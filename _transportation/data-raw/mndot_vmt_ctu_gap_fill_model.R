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


pred_df %>%
  # filter(coctu_id_gnis %in% ctu_missing$coctu_id_gnis) %>%
  plot_ly(
    type = "scatter",
    mode = "markers",
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
    x = c(1, max(pred_df$pred_vmt, pred_df$daily_vmt, na.rm = T)),
    y = c(1, max(pred_df$pred_vmt, pred_df$daily_vmt, na.rm = T)),
    inherit = FALSE,
    type = "scatter",
    mode = "lines",
    line = list(color = "gray")
  ) %>%
  plotly_layout(
    main_title = "Full dataset",
    x_title = "Observed",
    y_title = "Predicted"
  )

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
    vmt_source = ifelse(!is.na(daily_vmt), "MnDOT", "MetC Modeled")
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
    final_vmt_source = ifelse(!is.na(daily_vmt), "MnDOT", "MetC Modeled"),
    
    # determine the scaling factor value for each observation
    # if MnDOT, there is no scaling at all
    # if predicted, then it uses the established scale
    county_ctu_scaling_factor = ifelse(final_vmt_source == "MnDOT", 1, scale)
  )


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
  group_by(ctu_name_full_county) %>%
  plot_ly(
    type = "scatter",
    mode = "lines+markers",
    x = ~inventory_year,
    y = ~final_city_vmt,
    color = ~ctu_name_full_county,
    symbol = ~vmt_source,
    # symbols = c(
    # "circle-open",
    # "circle"),
    marker = list(size = 9),
    opacity = 0.7
  )

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
    symbol = ~vmt_source,
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
  select(inventory_year, coctu_id_gnis, geoid, gnis, county_ctu_scaling_factor, final_city_vmt, final_vmt_source) %>%
  arrange(coctu_id_gnis, inventory_year)


saveRDS(mndot_vmt_ctu_gap_filled, "_transportation/data/mndot_vmt_ctu_gap_filled.RDS")
saveRDS(m, "_transportation/data/vmt_gap_fill_model.RDS")


mndot_vmt_ctu_gap_filled_meta <- ctu_pop_jobs_vmt_meta %>%
  filter(Column %in% names(mndot_vmt_ctu_gap_filled)) %>%
  bind_rows(
    tibble::tribble(
      ~Column, ~Class, ~Description,
      "county_ctu_scaling_factor", class(mndot_vmt_ctu_gap_filled$county_ctu_scaling_factor), "Scaling factor applied to CTU VMT prediction",
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
         marginal_vmt)

saveRDS(mndot_vmt_county_marginals, "_transportation/data/mndot_vmt_county_marginals.RDS")

mndot_vmt_county_marginals_meta <- mndot_vmt_ctu_gap_filled_meta %>% 
  filter(Column %in% names(county_marginal_vmt)) %>% 
  bind_rows(
    tibble::tribble(
      ~Column, ~Class, ~Description,
      "sum_ctu_vmt", class(mndot_vmt_county_marginals$sum_ctu_vmt), "Total daily VMT in all CTUs in the given county",
      "county_daily_vmt", class(mndot_vmt_county_marginals$county_daily_vmt), "Total county daily VMT, MnDOT reported ",
      "marginal_vmt", class(mndot_vmt_county_marginals$marginal_vmt), "Difference in MnDOT reported county VMT and CTU VMT total "
    )
  )
  

saveRDS(mndot_vmt_county_marginals_meta, "_transportation/data/mndot_vmt_county_marginals_meta.RDS")
