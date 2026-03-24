# attribute the remainder of county level VMT to
# CTUs that don't have CTU level VMT data

source("R/_load_pkgs.R")
library(lme4) # for mixed-effects model
library(Metrics)

source("_transportation/data-raw/vmt_model_data.R")
set.seed(24601)

# prepare data -----
unique_years <- unique(ctu_pop_jobs_vmt$inventory_year) %>% length()
n_forecast_years <- 2050 - 2022

# which CTUs need VMT data, regardless of the years they are needed?
ctu_missing <- ctu_pop_jobs_vmt %>%
  # filter to only have observations where daily_vmt is NA
  filter(is.na(daily_vmt)) %>%
  # any COCTU with any na daily_vmt will need to be modeled
  select(
    coctu_id_gnis, ctu_name_full, ctu_name_full_county,
    geoid, county_name
  ) %>%
  unique()

# which CTUs come in with some data, but not all?
ctu_pop_jobs_vmt %>%
  filter(!is.na(daily_vmt)) %>%
  group_by(
    coctu_id_gnis, ctu_name_full, ctu_name_full_county,
    geoid, county_name
  ) %>%
  count() %>%
  arrange(n) %>%
  filter(
    n < unique_years,
    n > n_forecast_years
  )


# which CTUs have a complete dataset?
ctu_complete <- ctu_pop_jobs_vmt %>%
  filter(!is.na(daily_vmt)) %>%
  group_by(
    coctu_id_gnis, ctu_name_full, ctu_name_full_county,
    geoid, county_name
  ) %>%
  count() %>%
  filter(n >= unique_years) %>%
  unique()

testthat::expect_equal(
  length(unique(ctu_pop_jobs_vmt$coctu_id_gnis)),
  nrow(ctu_complete) + nrow(ctu_missing)
)

df <- ctu_pop_jobs_vmt %>%
  filter(
    # filter to only data prior to 2022
    # our RTDM data starts in 2023
    inventory_year <= 2022
  ) %>%
  mutate(
    # log_vmt = log(daily_vmt + 1),
    log_vmt = if_else(!is.na(daily_vmt), log(daily_vmt), NA_real_),
    log_pop = log(total_pop + 1),
    log_emp = log(total_jobs + 1), # avoid log(0)
    log_hh = log(total_households + 1),
    log_centerline_miles = log(centerline_miles + 1),
    emp_pop_ratio = total_jobs / total_pop,
    emp_hh_ratio = total_jobs / total_households
  )


# find the marginal VMT that is unaccounted for when we
# sum up from the COCTU to CO level
county_marginal_vmt <- df %>%
  # only use COCTUs that have a complete set from 2010 to 2022
  filter(!is.na(daily_vmt)) %>%
  # filter(coctu_id_gnis %in% ctu_complete$coctu_id_gnis) %>%
  group_by(county_name, geoid, inventory_year) %>%
  summarize(
    sum_daily_vmt = sum(daily_vmt, na.rm = T),
    county_daily_vmt = first(county_daily_vmt),
    marginal_vmt = county_daily_vmt - sum_daily_vmt,
    n = n(),
    .groups = "keep"
  )

# CHECKPOINT 1: Check for negative marginal VMT at county level
cli::cli_alert_info("CHECKPOINT 1: County marginal VMT (affects which COCTUs will get negative VMT)")
negative_marginal <- county_marginal_vmt %>%
  filter(marginal_vmt < 0) %>%
  mutate(pct_over = (abs(marginal_vmt) / county_daily_vmt) * 100)

if (nrow(negative_marginal) > 0) {
  cli::cli_alert_warning("Found {nrow(negative_marginal)} county-year combinations with negative marginal_vmt")
  cli::cli_alert_info("Counties with sum(COCTU VMT) > County VMT:")
  print(negative_marginal %>%
    select(county_name, inventory_year, county_daily_vmt, sum_daily_vmt, marginal_vmt, pct_over) %>%
    arrange(desc(abs(marginal_vmt))))
  
  # Identify which COCTUs will be affected
  affected_coctus <- df %>%
    filter(is.na(daily_vmt)) %>%
    inner_join(negative_marginal %>% select(county_name, geoid, inventory_year),
              by = c("county_name", "geoid", "inventory_year")) %>%
    select(coctu_id_gnis, ctu_name_full_county, county_name, inventory_year) %>%
    distinct()
  
  cli::cli_alert_warning("{nrow(affected_coctus)} COCTUs will receive negative scale factors:")
  print(affected_coctus %>% head(20))
} else {
  cli::cli_alert_success("No negative marginal_vmt - all COCTUs will have positive scales")
}

testthat::test_that("county_marginal_vmt should not have negative values", {
  testthat::expect_true(
    all(county_marginal_vmt$marginal_vmt >= 0, na.rm = TRUE),
    info = paste0(
      "Negative marginal_vmt found for: ",
      paste(negative_marginal$county_name, negative_marginal$inventory_year, collapse = ", ")
    )
  )
})

# create training dataset
n_designations <- 0
n_counties <- 0

# make sure training dataset has all seven counties
# and all 9 Imagine 2050 designation
while (n_designations != length(df$imagine_designation %>% unique()) |
  n_counties < 5) {
  full_ctus <- df %>%
    filter(!coctu_id_gnis %in% ctu_missing$coctu_id_gnis) %>%
    filter(!is.na(daily_vmt) & log_vmt > 1, ) %>%
    sample_frac(size = 0.6)

  train <- df %>%
    filter(coctu_id_gnis %in% full_ctus$coctu_id_gnis)

  n_designations <- length(train$imagine_designation %>% unique())
  n_counties <- length(train$county_name %>% unique())
}

# Create model -----
m <- lmer(log_vmt ~ log_pop + log_hh + log_emp + I(inventory_year - min(inventory_year)) + imagine_designation + (1 | county_name),
  data = train, REML = TRUE
)

summary(m, correlation = T)
# broom::tidy(m)
# equatiomatic::extract_eq(m, wrap = T) %>% print()

plot(m)

# apply model to original df table
pred_df <- df %>%
  ungroup() %>%
  mutate(
    pred_log_vmt = predict(object = m, newdata = df, allow.new.levels = TRUE), # includes random effects when available
    pred_vmt = exp(pred_log_vmt), # back to VMT
    resid_log = log_vmt - pred_log_vmt, # residuals in log space
    resid_vmt = daily_vmt - pred_vmt # residuals in original VMT units
  )

# CHECKPOINT 2: Check for negative predicted VMT values
cli::cli_alert_info("CHECKPOINT 2: Raw model predictions for COCTUs")
negative_pred <- pred_df %>%
  filter(pred_vmt < 0)

if (nrow(negative_pred) > 0) {
  cli::cli_alert_warning("Found {nrow(negative_pred)} COCTUs with negative pred_vmt")
  cli::cli_alert_info("This should NEVER happen (exp() always positive)")
  print(negative_pred %>%
    select(ctu_name_full_county, inventory_year, pred_log_vmt, pred_vmt) %>%
    head(10))
} else {
  cli::cli_alert_success("All {nrow(pred_df)} COCTU predictions are positive (as expected)")
}

testthat::test_that("pred_df should not have negative pred_vmt values", {
  testthat::expect_true(
    all(pred_df$pred_vmt >= 0, na.rm = TRUE),
    info = paste0("Negative pred_vmt found for ", nrow(negative_pred), " observations")
  )
})


# apply model to training dataset
train_post <- train %>%
  ungroup() %>%
  mutate(
    pred_log_vmt = predict(object = m, newdata = train, allow.new.levels = TRUE), # includes random effects when available
    pred_vmt = exp(pred_log_vmt), # back to VMT
    resid_log = log_vmt - pred_log_vmt, # residuals in log space
    resid_vmt = daily_vmt - pred_vmt # residuals in original VMT units
  )

# print model statistics from training data
cat("RMSE (log scale):", rmse(train_post$log_vmt, train_post$pred_log_vmt), "\n")
cat("RMSE (raw VMT):", rmse(train_post$daily_vmt, train_post$pred_vmt), "\n")
cat("MAE (raw VMT):", mae(train_post$daily_vmt, train_post$pred_vmt), "\n")
cat("R² (log scale):", cor(train_post$log_vmt, train_post$pred_log_vmt)^2, "\n")

# plot model
plot_ly(
  data = train_post,
  x = ~pred_log_vmt,
  y = ~resid_log,
  color = ~imagine_designation,
  type = "scatter",
  mode = "markers",
  opacity = 0.7,
  size = 4,
  hovertext = ~ paste0(
    ctu_name_full_county, "<br>",
    inventory_year, "<br>",
    "pred_log_vmt = ", round(pred_log_vmt, digits = 4), "<br>",
    "resid_log_vmt = ", round(resid_log, digits = 4)
  )
) %>%
  plotly_layout(
    main_title = "Residuals vs. Fitted, training data",
    x_title = "Predicted (log scale)",
    y_title = "Residuals (log scale)",
    legend_title = "Imagine designation"
  )


plot_ly(
  data = pred_df,
  x = ~pred_log_vmt,
  y = ~resid_log,
  color = ~imagine_designation,
  type = "scatter",
  mode = "markers",
  opacity = 0.7,
  size = 4,
  hovertext = ~ paste0(
    ctu_name_full_county, "<br>",
    inventory_year, "<br>",
    "pred_log_vmt = ", round(pred_log_vmt, digits = 4), "<br>",
    "resid_log_vmt = ", round(resid_log, digits = 4)
  )
) %>%
  plotly_layout(
    main_title = "Residuals vs. Fitted, all data",
    x_title = "Predicted (log scale)",
    y_title = "Residuals (log scale)",
    legend_title = "Imagine designation"
  )



plot_ly(
  data = pred_df,
  x = ~pred_vmt,
  y = ~resid_vmt,
  color = ~imagine_designation,
  type = "scatter",
  mode = "markers",
  opacity = 0.7,
  size = 4,
  hovertext = ~ paste0(
    ctu_name_full_county, "<br>",
    inventory_year, "<br>",
    "pred_vmt = ", round(pred_vmt, digits = 4), "<br>",
    "resid_vmt = ", round(resid_vmt, digits = 4)
  )
) %>%
  plotly_layout(
    main_title = "Residuals vs. Fitted, all data",
    x_title = "Predicted daily_vmt",
    y_title = "Residuals log scale",
    legend_title = "Imagine designation"
  )

plot_ly(
  data = train_post,
  name = "CTU-Year",
  x = ~log_vmt,
  y = ~pred_log_vmt,
  type = "scatter",
  mode = "markers",
  size = 5,
  marker = list(opacity = 0.4),
  hovertext = ~ paste0(
    ctu_name_full_county, "<br>",
    inventory_year, "<br>",
    "log_vmt = ", round(log_vmt, digits = 4), "<br>",
    "pred_log_vmt = ", round(pred_log_vmt, digits = 4)
  )
) %>%
  add_trace(
    name = "1:1 line",
    inherit = FALSE,
    x = c(min(train_post$log_vmt, na.rm = TRUE), max(train_post$log_vmt, na.rm = TRUE)),
    y = c(min(train_post$log_vmt, na.rm = TRUE), max(train_post$log_vmt, na.rm = TRUE)),
    type = "scatter",
    mode = "lines",
    line = list(color = "black")
  ) %>%
  plotly_layout(
    main_title = "Observed vs Predicted (log), training data",
    y_title = "log(Predicted VMT)",
    x_title = "log(Observed VMT)"
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    ),
    yaxis = list(
      type = "log"
    )
  )


train_post %>%
  plot_ly(
    type = "scatter",
    mode = "markers",
    # name = "CTU-Year",
    x = ~daily_vmt,
    y = ~pred_vmt,
    color = ~ctu_name_full_county,
    opacity = 0.8,
    size = 4,
    hovertext = ~ paste0(
      ctu_name_full_county, "<br>",
      inventory_year, "<br>",
      "Daily VMT: ", scales::comma(daily_vmt), "<br>",
      "Pred Daily VMT: ", scales::comma(pred_vmt)
    )
  ) %>%
  add_trace(
    name = "1:1",
    x = c(1, max(train_post$pred_vmt, train_post$daily_vmt)),
    y = c(1, max(train_post$pred_vmt, train_post$daily_vmt)),
    inherit = FALSE,
    type = "scatter",
    mode = "lines",
    line = list(color = "gray")
  ) %>%
  plotly_layout(
    main_title = "Training dataset",
    x_title = "Observed",
    y_title = "Predicted"
  )

fig_model_performance <-
  pred_df %>%
  # filter(coctu_id_gnis %in% ctu_missing$coctu_id_gnis) %>%
  plot_ly(
    source = "fig-vmt-model-performance",
    type = "scatter",
    mode = "markers",
    x = ~daily_vmt,
    y = ~pred_vmt,
    color = ~ctu_name_full_county,
    opacity = 0.8,
    size = 4,
    hoverinfo = "text",
    hovertext = ~ paste0(
      ctu_name_full_county, "<br>",
      inventory_year, "<br>",
      "Observed Daily VMT: ", scales::comma(daily_vmt, accuracy = 1), "<br>",
      "Predicted Daily VMT: ", scales::comma(pred_vmt, accuracy = 1)
    )
  ) %>%
  add_trace(
    name = "1:1 Correlation",
    x = c(1, max(pred_df$pred_vmt, pred_df$daily_vmt, na.rm = T)),
    y = c(1, max(pred_df$pred_vmt, pred_df$daily_vmt, na.rm = T)),
    inherit = FALSE,
    type = "scatter",
    mode = "lines",
    line = list(color = "gray")
  ) %>%
  plotly_layout(
    main_title = "City VMT model performance, prior to county scaling",
    x_title = "Observed",
    y_title = "Predicted",
    legend_title = "County-CTU",
    subtitle = "Markers above correlation line indicate model over-prediction,<br>while markers under correlation line indicate under-prediction"
  )


fig_model_performance

saveRDS(fig_model_performance, "_transportation/data/fig_model_performance.RDS")

# # what new information do we want to glean from the counties?
ranefs <- lme4::ranef(m)$county_name %>%
  tibble::rownames_to_column("county_name") %>%
  rename(random_intercept = `(Intercept)`)
ranefs <- ranefs[order(ranefs$random_intercept), ]

plot_ly(
  data = ranefs,
  x = ~random_intercept,
  y = ~county_name,
  type = "bar",
  orientation = "h",
  marker = list(color = "steelblue")
) %>%
  layout(yaxis = list(categoryarray = "total descending")) %>%
  plotly_layout(
    main_title = "Group-level Effects",
    y_title = "Group",
    x_title = "Random Intercept (log VMT)"
  )

# Scale predictions to county level ----------------------------
## Total county vmt ----
## first, try to find benchmark from TOTAL county VMT
## this will make the overall trend appear to decrease over time
##
## as more CTUs come online, more of the county's VMT is accounted for.
## This means that the amount of VMT that needs to be allocated to CTUs
## with missing data is decreasing over time, but the total county VMT
## (generally) continues to rise.

# create scaling factor
bench <- pred_df %>%
  group_by(county_name, geoid, inventory_year) %>%
  summarise(
    county_daily_vmt = first(county_daily_vmt),
    sum_pred_vmt = sum(pred_vmt, na.rm = TRUE),
    n_cities = n(),
    .groups = "keep"
  ) %>%
  ungroup() %>%
  mutate(
    scale = case_when(
      is.na(county_daily_vmt) ~ NA_real_,
      sum_pred_vmt <= 0 ~ NA_real_,
      TRUE ~ county_daily_vmt / sum_pred_vmt
    )
  )

# apply scaling factor
pred_df_bench <- pred_df %>%
  left_join(bench %>% select(county_name, inventory_year, geoid, scale, sum_pred_vmt),
    by = c("county_name", "inventory_year", "geoid")
  ) %>%
  mutate(
    pred_vmt_bench = case_when(
      !is.na(scale) ~ pred_vmt * scale,
      TRUE ~ pred_vmt # if no county control, keep model prediction
    ),
    # if there is MnDOT VMT, use it, otherwise use the benched predictions
    final_city_vmt = if_else(!is.na(daily_vmt), daily_vmt, pred_vmt_bench),
    vmt_source = ifelse(!is.na(daily_vmt), "MnDOT VMT Reports", "MetC Modeled")
  )

# summarize differences in total county VMT
pred_df_bench_diff <- pred_df_bench %>%
  group_by(county_name, inventory_year) %>%
  summarize(
    # n_ctus = count(),
    county_daily_vmt = first(county_daily_vmt),
    sum_vmt_bench = sum(pred_vmt_bench),
    sum_final_vmt = sum(final_city_vmt),
    observed_pred_diff = county_daily_vmt - sum_final_vmt,
    observed_pred_bench_diff = county_daily_vmt - sum_vmt_bench,
    observed_pred_diff_pct = observed_pred_diff / county_daily_vmt,
    n_cities = n(),
    .groups = "keep"
  ) %>%
  ungroup()

# The totals here come out different than expected, because we are
# scaling all predictions, even those that we don't need to predict for!
pred_df_bench_diff %>%
  plot_ly(
    type = "scatter",
    mode = "lines+markers",
    x = ~inventory_year,
    y = ~observed_pred_diff_pct,
    color = ~county_name
  )

## Marginal/missing VMT -----
## find benchmark based on missing/gap VMT from counties

pred_from_na_bench <- pred_df %>%
  # find the observations/predicted values that need to be benched
  # these are ones without a complete time series from 2010 to 2022
  filter(is.na(daily_vmt)) %>%
  # join with the marginal/missing county VMT tabulation
  left_join(
    county_marginal_vmt,
    join_by(inventory_year, geoid, county_name, county_daily_vmt)
  ) %>%
  group_by(county_name, geoid, inventory_year) %>%
  summarise(
    county_marginal_vmt = first(marginal_vmt),
    sum_pred_vmt = sum(pred_vmt, na.rm = TRUE),
    n_cities = n(),
    .groups = "keep"
  ) %>%
  ungroup() %>%
  mutate(
    scale = case_when(
      is.na(county_marginal_vmt) ~ NA_real_,
      sum_pred_vmt <= 0 ~ NA_real_,
      # scale is the amount of county marginal vmt divided by
      # the total predicted VMT of CTUs marginal data
      TRUE ~ county_marginal_vmt / sum_pred_vmt
    )
  )

# CHECKPOINT 3: Check for negative scale factors
cli::cli_alert_info("CHECKPOINT 3: Scale factors applied to COCTUs")
negative_scale <- pred_from_na_bench %>%
  filter(scale < 0)

if (nrow(negative_scale) > 0) {
  cli::cli_alert_warning("Found {nrow(negative_scale)} county-year combinations with negative scale factors")
  cli::cli_alert_info("These negative scales will be applied to COCTUs in affected counties:")
  print(negative_scale %>%
    select(county_name, inventory_year, n_cities, county_marginal_vmt, sum_pred_vmt, scale) %>%
    arrange(scale))
  
  cli::cli_text("")
  cli::cli_alert_info("Formula: scale = county_marginal_vmt / sum_pred_vmt")
  cli::cli_alert_info("When county_marginal_vmt < 0, scale becomes negative")
  cli::cli_alert_info("This negative scale will multiply ALL modeled COCTUs in that county-year")
} else {
  cli::cli_alert_success("All scale factors are positive")
}

testthat::test_that("pred_from_na_bench should not have negative scale factors", {
  testthat::expect_true(
    all(pred_from_na_bench$scale >= 0, na.rm = TRUE),
    info = paste0(
      "Negative scale found for: ",
      paste(negative_scale$county_name, negative_scale$inventory_year, collapse = ", ")
    )
  )
})

# apply scaling factor
pred_df_na_bench <- pred_df %>%
  left_join(
    pred_from_na_bench %>%
      select(county_name, inventory_year, geoid, scale, sum_pred_vmt),
    by = c("county_name", "inventory_year", "geoid")
  ) %>%
  mutate(
    # apply scaling to all predictions
    pred_vmt_bench = pred_vmt * scale,
    # if there is MnDOT VMT, use it, otherwise use the benched predictions
    final_city_vmt = if_else(!is.na(daily_vmt), daily_vmt, pred_vmt_bench),
    final_vmt_source = ifelse(!is.na(daily_vmt), "MnDOT VMT Reports", "MetC Modeled"),

    # determine the scaling factor value for each observation
    # if MnDOT, there is no scaling at all
    # if predicted, then it uses the established scale
    county_ctu_scaling_factor = ifelse(final_vmt_source == "MnDOT VMT Reports", 1, scale)
  )

# CHECKPOINT 4: Check for negative final_city_vmt values
cli::cli_alert_info("CHECKPOINT 4: Final COCTU VMT values after scaling")
negative_final <- pred_df_na_bench %>%
  filter(final_city_vmt < 0)

if (nrow(negative_final) > 0) {
  cli::cli_alert_danger("FOUND {nrow(negative_final)} COCTUs with NEGATIVE VMT!")
  cli::cli_text("")
  
  # Show summary by county
  coctu_summary <- negative_final %>%
    group_by(county_name) %>%
    summarize(
      n_coctus = n_distinct(coctu_id_gnis),
      n_years = n_distinct(inventory_year),
      min_vmt = min(final_city_vmt),
      .groups = "drop"
    ) %>%
    arrange(min_vmt)
  
  cli::cli_alert_info("Affected counties:")
  print(coctu_summary)
  
  cli::cli_text("")
  cli::cli_alert_info("COCTUs with most negative VMT:")
  print(negative_final %>%
    select(coctu_id_gnis, ctu_name_full_county, county_name, inventory_year, 
           pred_vmt, scale, pred_vmt_bench, final_city_vmt) %>%
    arrange(final_city_vmt) %>%
    head(30))
  
  # Trace back to show the problem chain
  cli::cli_text("")
  cli::cli_alert_info("Root cause chain for these COCTUs:")
  cli::cli_ul(c(
    paste0("Negative county marginal_vmt in ", 
           nrow(negative_scale), " county-years"),
    "→ Creates negative scale factors",
    paste0("→ Multiplied by positive pred_vmt = negative final_city_vmt for ", 
           n_distinct(negative_final$coctu_id_gnis), " unique COCTUs")
  ))
} else {
  cli::cli_alert_success("All {nrow(pred_df_na_bench)} COCTUs have non-negative VMT")
}

testthat::test_that("pred_df_na_bench should not have negative final_city_vmt", {
  testthat::expect_true(
    all(pred_df_na_bench$final_city_vmt >= 0, na.rm = TRUE),
    info = paste0(
      "Negative final_city_vmt found for ",
      nrow(negative_final),
      " observations. ",
      "Unique COCTUs affected: ",
      n_distinct(negative_final$coctu_id_gnis)
    )
  )
})


# these totals should match exactly
pred_df_na_bench_diff <- pred_df_na_bench %>%
  group_by(county_name, geoid, inventory_year) %>%
  summarize(
    county_daily_vmt = first(county_daily_vmt),
    sum_pred_vmt_bench = sum(pred_vmt_bench),
    sum_final_vmt = sum(final_city_vmt),
    observed_pred_diff = county_daily_vmt - sum_final_vmt,
    n_cities = n(),
    .groups = "keep"
  )

pred_df_na_bench_diff %>%
  filter(!observed_pred_diff <= 0.0001)


pred_df_na_bench %>%
  filter(coctu_id_gnis %in% ctu_missing$coctu_id_gnis) %>%
  plot_ly(
    type = "scatter",
    mode = "lines+markers",
    x = ~inventory_year,
    y = ~final_city_vmt,
    color = ~ctu_name_full_county,
    symbol = ~final_vmt_source,
    symbols = c(
      "circle-open",
      "circle"
    ),
    marker = list(size = 9),
    opacity = 0.7
  )



pred_df_na_bench %>%
  select(
    geoid, coctu_id_gnis, ctu_name_full_county,
    inventory_year, final_city_vmt, final_vmt_source, vmt_source
  ) %>%
  bind_rows(ctu_pop_jobs_vmt %>%
    select(inventory_year, coctu_id_gnis,
      ctu_name_full_county, geoid,
      final_city_vmt = daily_vmt, vmt_source
    ) %>%
    filter(inventory_year >= 2023)) %>%
  mutate(vmt_source_final = ifelse(is.na(final_vmt_source),
    vmt_source, final_vmt_source
  )) %>%
  group_by(ctu_name_full_county) %>%
  plot_ly(
    type = "scatter",
    mode = "lines+markers",
    x = ~inventory_year,
    y = ~final_city_vmt,
    color = ~ctu_name_full_county,
    symbol = ~vmt_source_final,
    # symbols = c(
    # "circle-open",
    # "circle"),
    marker = list(size = 9),
    hoverinfo = "text",
    hovertext = ~ paste0(
      ctu_name_full_county, "<br>",
      inventory_year, "<br>",
      scales::comma(final_city_vmt, accuracy = 1), "<br>",
      vmt_source_final
    ),
    opacity = 0.7
  ) %>%
  plotly_layout()

# conclusion -----
# our deciding factor for whether to use the NA benchmark vs total benchmark
# is Fort Snelling. MnDOT does not provide data for Fort Snelling, which
# includes MSP airport, a major job center. There are very few
# population or households, but lots of jobs. The 2023 model
# outputs are 981,750 (nearly 1 million daily VMT).
# Using the marginal county VMT differences, we get a 2022 modeled
# VMT for Fort Snelling of  around 700,000, while if we use the total
# county VMT, we get only around 350,000.
# We would expect most of Hennepin County's marginal/missing VMT
# to be at the MSP airport, because it is such a large hub of activity.
# Thus, we will use the marginal county VMT as our benchmark.


# combine COCTUs into CTUS and plot
pred_df_na_bench %>%
  group_by(ctu_name, ctu_name_full, inventory_year) %>%
  summarize(
    total_households = sum(total_households),
    total_jobs = sum(total_jobs),
    total_pop = sum(total_pop),
    final_city_vmt = sum(final_city_vmt),
    vmt_source = paste0(unique(vmt_source), collapse = ", "),
    final_vmt_source = paste0(unique(final_vmt_source), collapse = ", ")
  ) %>%
  plot_ly(
    type = "scatter",
    mode = "lines+markers",
    x = ~inventory_year,
    y = ~final_city_vmt,
    color = ~ctu_name_full,
    symbol = ~final_vmt_source,
    # symbols = c(
    # "circle-open",
    # "circle"),
    marker = list(size = 9),
    opacity = 0.7
  )


# wrap up

mndot_vmt_ctu_gap_filled <- pred_df_na_bench %>%
  select(
    -starts_with("log"),
    -ends_with("ratio"),
    -ends_with("log"),
    -pred_log_vmt,
    -pred_vmt,
    -sum_pred_vmt,
    -pred_vmt_bench,
    -resid_vmt,
    -scale
  ) %>%
  bind_rows(ctu_pop_jobs_vmt %>%
    mutate(
      final_city_vmt = daily_vmt,
      final_vmt_source = vmt_source
    ) %>%
    filter(inventory_year >= 2023)) %>%
  select(
    inventory_year, coctu_id_gnis,
    geoid, gnis, ctu_name_full_county, county_ctu_scaling_factor,
    final_city_vmt, final_vmt_source
  ) %>%
  arrange(coctu_id_gnis, inventory_year)

# CHECKPOINT 5: Final check for negative VMT in output dataset
cli::cli_alert_info("CHECKPOINT 5: Final output dataset validation")
negative_final_output <- mndot_vmt_ctu_gap_filled %>%
  filter(final_city_vmt < 0)

if (nrow(negative_final_output) > 0) {
  cli::cli_alert_danger("CRITICAL: {nrow(negative_final_output)} COCTU-years with negative VMT in final output")
  cli::cli_text("")
  
  # Group by COCTU to show full time series for affected units
  cli::cli_alert_info("Unique COCTUs affected: {n_distinct(negative_final_output$coctu_id_gnis)}")
  
  affected_coctu_list <- negative_final_output %>%
    group_by(coctu_id_gnis, ctu_name_full_county) %>%
    summarize(
      years_affected = paste(inventory_year, collapse = ", "),
      min_vmt = min(final_city_vmt),
      avg_scale = mean(county_ctu_scaling_factor, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(min_vmt)
  
  print(affected_coctu_list)
  
  cli::cli_text("")
  cli::cli_alert_info("Detailed COCTU-year records (worst cases):")
  print(negative_final_output %>%
    select(coctu_id_gnis, ctu_name_full_county, inventory_year, 
           county_ctu_scaling_factor, final_city_vmt, final_vmt_source) %>%
    arrange(final_city_vmt) %>%
    head(30))
} else {
  cli::cli_alert_success("All {nrow(mndot_vmt_ctu_gap_filled)} COCTU-year records have non-negative VMT")
}

testthat::test_that("Final output should not have negative VMT values", {
  testthat::expect_true(
    all(mndot_vmt_ctu_gap_filled$final_city_vmt >= 0, na.rm = TRUE),
    info = paste0(
      "CRITICAL: Negative VMT in final output for ",
      nrow(negative_final_output),
      " COCTU-year observations across ",
      n_distinct(negative_final_output$coctu_id_gnis),
      " unique COCTUs"
    )
  )
})

# DIAGNOSTIC SUMMARY -----
cli::cli_h1("Diagnostic Summary: Root Cause Analysis")

# Analyze the root cause of negative VMT values
if (nrow(negative_marginal) > 0) {
  cli::cli_h2("Issue 1: Negative Marginal VMT at County Level")
  cli::cli_alert_info("Root cause: Sum of CTU-level VMT exceeds county-level VMT")
  cli::cli_alert_info("This happens when:")
  cli::cli_ul(c(
    "County VMT data quality is poor",
    "CTU VMT is over-reported or double-counted",
    "Spatial misalignment between CTU and county boundaries",
    "Different reporting methodologies between CTU and county data"
  ))
  
  cli::cli_text("")
  cli::cli_alert_info("Counties with negative marginal VMT:")
  
  negative_summary <- negative_marginal %>%
    mutate(
      pct_over = (abs(marginal_vmt) / county_daily_vmt) * 100
    ) %>%
    arrange(desc(abs(marginal_vmt)))
  
  print(negative_summary %>%
    select(county_name, inventory_year, county_daily_vmt, sum_daily_vmt, marginal_vmt, pct_over) %>%
    mutate(across(where(is.numeric), ~ round(., 2))))
}

if (nrow(negative_scale) > 0) {
  cli::cli_h2("Issue 2: Negative Scale Factors")
  cli::cli_alert_info("Consequence: Negative marginal VMT creates negative scale factors")
  cli::cli_alert_info("Scale formula: scale = county_marginal_vmt / sum_pred_vmt")
  cli::cli_alert_info("When county_marginal_vmt < 0, scale becomes negative")
  cli::cli_text("")
}

if (nrow(negative_final) > 0) {
  cli::cli_h2("Issue 3: Negative Final City VMT")
  cli::cli_alert_info("Result: Negative scale applied to predictions creates negative VMT")
  cli::cli_alert_info("Formula: final_city_vmt = pred_vmt * scale")
  cli::cli_text("")
  cli::cli_alert_info("COCTUs with negative VMT (sample):")
  
  negative_detail <- negative_final %>%
    arrange(final_city_vmt) %>%
    head(20) %>%
    select(ctu_name_full_county, county_name, inventory_year, pred_vmt, scale, final_city_vmt)
  
  print(negative_detail %>%
    mutate(across(where(is.numeric), ~ round(., 2))))
}

cli::cli_h2("Solutions")
cli::cli_ul(c(
  "Option 1: Set negative marginal VMT to zero (assumes county VMT is the constraint)",
  "Option 2: Use positive scaling only (set negative scales to NA or 1)",
  "Option 3: Investigate data quality issues in affected counties",
  "Option 4: Use alternative benchmarking approach for problematic counties",
  "Option 5: Apply floor constraint (final_city_vmt = max(0, pred_vmt_bench))"
))

cli::cli_text("")
cli::cli_alert_info("Recommended approach: Combine options 1 and 5")
cli::cli_alert_info("  1. Set negative marginal_vmt to zero during calculation")
cli::cli_alert_info("  2. Apply floor constraint to prevent negative final values")
cli::cli_text("")


saveRDS(mndot_vmt_ctu_gap_filled, "_transportation/data/mndot_vmt_ctu_gap_filled.RDS")
saveRDS(m, "_transportation/data/vmt_gap_fill_model.RDS")


mndot_vmt_ctu_gap_filled_meta <- ctu_pop_jobs_vmt_meta %>%
  filter(Column %in% names(mndot_vmt_ctu_gap_filled)) %>%
  bind_rows(
    tibble::tribble(
      ~Column, ~Class, ~Description,
      "county_ctu_scaling_factor", class(mndot_vmt_ctu_gap_filled$county_ctu_scaling_factor), "Scaling factor applied to CTU VMT prediction",
      "ctu_name_full_county", class(mndot_vmt_ctu_gap_filled$ctu_name_full_county), "City, city class, and county names",
      "final_city_vmt", class(mndot_vmt_ctu_gap_filled$final_city_vmt), "Daily VMT for given CTU and year",
      "final_vmt_source", class(mndot_vmt_ctu_gap_filled$final_vmt_source), paste0(
        "Data source for given CTU-year. One of ",
        paste0(unique(mndot_vmt_ctu_gap_filled$final_vmt_source), collapse = ", ")
      ),
    )
  ) %>%
  arrange(match(Column, names(mndot_vmt_ctu_gap_filled)))

saveRDS(mndot_vmt_ctu_gap_filled_meta, "_transportation/data/mndot_vmt_ctu_gap_filled_meta.RDS")


mndot_vmt_county_marginals <- county_marginal_vmt %>%
  ungroup() %>%
  select(-county_name, -n) %>%
  select(geoid, inventory_year,
    sum_ctu_vmt = sum_daily_vmt,
    county_daily_vmt,
    marginal_vmt
  )

saveRDS(mndot_vmt_county_marginals, "_transportation/data/mndot_vmt_county_marginals.RDS")

mndot_vmt_county_marginals_meta <- mndot_vmt_ctu_gap_filled_meta %>%
  filter(Column %in% names(county_marginal_vmt)) %>%
  bind_rows(
    tibble::tribble(
      ~Column, ~Class, ~Description,
      "sum_ctu_vmt", class(mndot_vmt_county_marginals$sum_ctu_vmt), "Total daily VMT in all CTUs in the given county",
      "county_daily_vmt", class(mndot_vmt_county_marginals$county_daily_vmt), "Total county daily VMT, MnDOT reported",
      "marginal_vmt", class(mndot_vmt_county_marginals$marginal_vmt), "Difference in MnDOT reported county VMT and CTU VMT total "
    )
  )


saveRDS(mndot_vmt_county_marginals_meta, "_transportation/data/mndot_vmt_county_marginals_meta.RDS")
