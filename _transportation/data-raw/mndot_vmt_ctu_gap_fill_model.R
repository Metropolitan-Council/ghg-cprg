# attribute the remainder of county level VMT to 
# CTUs that don't have CTU level VMT data

source("R/_load_pkgs.R")
library(lme4)        # for mixed-effects model
library(Metrics)

source("_transportation/data-raw/vmt_model_data.R")
set.seed(24601)

total_ctu_per_county <- ctu_coctu_index %>% 
  select(geoid, county_name, coctu_id_gnis, ctu_name_full_county) %>% 
  unique() %>% 
  group_by(geoid, county_name) %>% 
  count()

# which CTUs need help?
ctu_missing <- ctu_pop_jobs_vmt %>% 
  filter(is.na(daily_vmt)) %>% 
  select(coctu_id_gnis, ctu_name_full, ctu_name_full_county,
         geoid, county_name) %>% 
  unique()


ctu_complete <- ctu_pop_jobs_vmt %>% 
  filter(!is.na(daily_vmt)) %>% 
  group_by(coctu_id_gnis, ctu_name_full, ctu_name_full_county,
           geoid, county_name) %>% 
  count() %>% 
  filter(n >= 41) %>% 
  unique()

testthat::expect_equal(length(unique(ctu_pop_jobs_vmt$coctu_id_gnis)), nrow(ctu_complete) + nrow(ctu_missing)  )


# ctu_pop_jobs_vmt %>% 
#   filter(coctu_id_gnis %in% ctu_missing$coctu_id_gnis) %>% 
#   plot_ly(
#     color = ~ctu_name_full_county,
#     x= ~inventory_year,
#     y = ~daily_vmt
#   )


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
county_missing_vmt <- df %>% 
  filter(coctu_id_gnis %in% ctu_complete$coctu_id_gnis) %>% 
  group_by(county_name, geoid, inventory_year) %>% 
  summarize(
    sum_daily_vmt = sum(daily_vmt, na.rm = T),
    county_daily_vmt = first(county_daily_vmt),
    missing_vmt = county_daily_vmt - sum_daily_vmt,
    n = n(),
    .groups = "keep"
    
  )

# create training dataset
n_designations <- 0
n_counties <- 0

# make sure training dataset has all seven counties
# and all 9 Imagine 2050 designation
while(n_designations != length(df$imagine_designation %>% unique()) |
      n_counties < 5){
  
  full_ctus <- df %>% 
    filter(!coctu_id_gnis %in% ctu_missing$coctu_id_gnis) %>% 
    filter(!is.na(daily_vmt) & log_vmt > 1,
           # ctu_name != "Minneapolis"
           # ctu_name != "Saint Paul"
    ) %>% 
    # group_by(coctu_id_gnis) %>% 
    # count() %>% 
    # filter(n >= 12) %>% 
    # ungroup() %>% 
    sample_frac(size = 0.6)
  
  train <- df %>%
    filter(coctu_id_gnis %in% full_ctus$coctu_id_gnis)
  
  n_designations <- length(train$imagine_designation %>% unique())
  n_counties <- length(train$county_name %>% unique())
}

# Create model -----
m <- lmer(log_vmt ~ log_pop + log_hh + log_emp + I(inventory_year - min(inventory_year)) + imagine_designation +  (1 | county_name), 
          data = train, REML = TRUE)

summary(m, correlation = T)
# broom::tidy(m)
# equatiomatic::extract_eq(m, wrap = T) %>% print()

plot(m)

# apply model to original df table
pred_df <- df %>%
  ungroup() %>% 
  mutate(
    pred_log_vmt = predict(object = m, newdata = df, allow.new.levels = TRUE), # includes random effects when available
    pred_vmt = exp(pred_log_vmt),           # back to VMT
    resid_log = log_vmt - pred_log_vmt,     # residuals in log space
    resid_vmt = daily_vmt - pred_vmt        # residuals in original VMT units
  ) 


# apply model to training dataset
train_post <- train %>% 
  ungroup() %>% 
  mutate(
    pred_log_vmt = predict(object = m, newdata = train, allow.new.levels = TRUE), # includes random effects when available
    pred_vmt = exp(pred_log_vmt),           # back to VMT
    resid_log = log_vmt - pred_log_vmt,     # residuals in log space
    resid_vmt = daily_vmt - pred_vmt        # residuals in original VMT units
  ) 

# print model statistics
cat("RMSE (log scale):", rmse(train_post$log_vmt, train_post$pred_log_vmt), "\n")
cat("RMSE (raw VMT):", rmse(train_post$daily_vmt, train_post$pred_vmt) , "\n")
cat("MAE (raw VMT):", mae(train_post$daily_vmt, train_post$pred_vmt), "\n")
cat("R² (log scale):", cor(train_post$log_vmt, train_post$pred_log_vmt)^2, "\n")

# plot model 
plot_ly(
  data=  train_post,
  x = ~pred_log_vmt,
  y = ~resid_log,
  color = ~imagine_designation,
  type = "scatter",
  mode = "markers",
  opacity = 0.7,
  size = 4,
  hovertext = ~paste0(
    ctu_name_full_county, "<br>",
    inventory_year ,"<br>",
    "pred_log_vmt = ", round(pred_log_vmt, digits = 4), "<br>",
    "resid_log_vmt = ", round(resid_log, digits = 4)
  )
) %>% 
  plotly_layout(
    main_title = "Residuals vs. Fitted, training data",
    x_title = "Predicted log(daily_vmt)",
    y_title = "Residuals (log scale)",
    legend_title = "Imagine designation"
  )


plot_ly(
  data= pred_df,
  x = ~pred_log_vmt,
  y = ~resid_log,
  color = ~imagine_designation,
  type = "scatter",
  mode = "markers",
  opacity = 0.7,
  size = 4,
  hovertext = ~paste0(
    ctu_name_full_county, "<br>",
    inventory_year ,"<br>",
    "pred_log_vmt = ", round(pred_log_vmt, digits = 4), "<br>",
    "resid_log_vmt = ", round(resid_log, digits = 4)
  )
) %>% 
  plotly_layout(
    main_title = "Residuals vs. Fitted, all data",
    x_title = "Predicted log(daily_vmt)",
    y_title = "Residuals (log scale)",
    legend_title = "Imagine designation"
  )



plot_ly(
  data= pred_df,
  x = ~pred_vmt,
  y = ~resid_vmt,
  color = ~imagine_designation,
  type = "scatter",
  mode = "markers",
  opacity = 0.7,
  size = 4,
  hovertext = ~paste0(
    ctu_name_full_county, "<br>",
    inventory_year ,"<br>",
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
  name= "CTU-Year",
  x = ~log_vmt,
  y = ~pred_log_vmt,
  type = "scatter",
  mode = "markers",
  size = 5,
  marker = list(opacity = 0.4),
  hovertext = ~paste0(
    ctu_name_full_county, "<br>",
    inventory_year, "<br>",
    "log_vmt = ", round(log_vmt, digits = 4), "<br>",
    "pred_log_vmt = ", round(pred_log_vmt, digits = 4)
  )
) %>%
  add_trace(
    name = "1:1",
    inherit = FALSE,
    x = c(min(train_post$log_vmt, na.rm = TRUE), max(train_post$log_vmt, na.rm = TRUE)),
    y = c(min(train_post$log_vmt, na.rm = TRUE), max(train_post$log_vmt, na.rm = TRUE)),
    type = "scatter",
    mode = "lines",
    line = list(color = "black")
  ) %>%
  plotly_layout(main_title = "Observed vs Predicted (log), training data",
                y_title = "log(Predicted VMT)",
                x_title = "log(Observed VMT)") %>% 
  layout(
    xaxis = list(
      type = "log"
    ),
    yaxis = list(
      type = "log"
    ))


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
    hovertext = ~paste0(
      ctu_name_full_county, "<br>",
      inventory_year,"<br>",
      "Daily VMT: ", scales::comma(daily_vmt), "<br>",
      "Pred Daily VMT: ", scales::comma(pred_vmt)
    )
  ) %>% 
  add_trace(
    name = "1:1",
    x = c(1,max(train_post$pred_vmt, train_post$daily_vmt)),
    y = c(1,max(train_post$pred_vmt, train_post$daily_vmt)),
    inherit = FALSE,
    type = "scatter",
    mode = "lines",
    line = list(color = "gray")
  ) %>% 
  plotly_layout(main_title = "Training dataset",
                x_title = "Observed",
                y_title = "Predicted")


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
    hovertext = ~paste0(
      ctu_name_full_county, "<br>",
      inventory_year,"<br>",
      "Daily VMT: ", scales::comma(daily_vmt), "<br>",
      "Pred Daily VMT: ", scales::comma(pred_vmt)
    )
  ) %>% 
  add_trace(
    name = "1:1",
    x = c(1,max(pred_df$pred_vmt, pred_df$daily_vmt, na.rm = T)),
    y = c(1,max(pred_df$pred_vmt, pred_df$daily_vmt, na.rm = T)),
    inherit = FALSE,
    type = "scatter",
    mode = "lines",
    line = list(color = "gray")
  ) %>% 
  plotly_layout(main_title = "Full dataset",
                x_title = "Observed",
                y_title = "Predicted")
# 
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
  plotly_layout(main_title = "Group-level Effects",
                y_title = "Group",
                x_title = "Random Intercept (log VMT)")

# Scale predictions to county level ----------------------------
## find benchmark from TOTAL county VMT

bench <- pred_df %>%
  # filter(coctu_id_gnis %in% ctu_complete$coctu_id_gnis) %>% 
  group_by(county_name, geoid, inventory_year) %>%
  summarise(
    county_daily_vmt = first(county_daily_vmt), # from county_df
    sum_pred_vmt = sum(pred_vmt, na.rm = TRUE),
    n_cities = n(),
    .groups = "keep"
  ) %>%
  ungroup() %>%
  mutate(
    scale = case_when(
      is.na(county_daily_vmt) ~ NA_real_, # can't scale if no county control
      sum_pred_vmt <= 0 ~ NA_real_, # handle degenerate
      TRUE ~ county_daily_vmt / sum_pred_vmt
    )
  )

pred_df_bench <- pred_df %>%
  left_join(bench %>% select(county_name, inventory_year, geoid, scale, sum_pred_vmt),
            by = c("county_name", "inventory_year", "geoid")) %>%
  mutate(
    pred_vmt_bench = case_when(
      !is.na(scale) ~ pred_vmt * scale,
      TRUE ~ pred_vmt  # if no county control, keep model prediction
    ),
    final_city_vmt = if_else(!is.na(daily_vmt), daily_vmt, pred_vmt_bench),
    vmt_source = ifelse(!is.na(daily_vmt), "MnDOT", "MetC Modeled")
  )

pred_df_bench_diff <- pred_df_bench %>%
  group_by(county_name, inventory_year) %>%
  summarize(
    # n_ctus = count(),
    county_daily_vmt = first(county_daily_vmt),
    sum_vmt_bench = sum(pred_vmt_bench),
    sum_final_vmt = sum(final_city_vmt),
    observed_pred_diff = county_daily_vmt - sum_final_vmt,
    observed_pred_bench_diff = county_daily_vmt - sum_vmt_bench,
    n_cities = n(),
    .groups = "keep"
  ) %>% 
  ungroup()

pred_df_bench_diff %>% 
  plot_ly(
    type = "scatter",
    mode = "lines+markers",
    x = ~inventory_year,
    y = ~observed_pred_diff,
    color = ~county_name
  )

## find benchmark based on missing/gap VMT from counties -----
## this could make the overall trend appear to decrease over time
## as more CTUs come online, more of the county's VMT is accounted for.
## 
pred_from_na_bench <- pred_df %>% 
  # filter(is.na(daily_vmt)) %>%
  filter(!coctu_id_gnis %in% ctu_complete$coctu_id_gnis) %>%
  left_join(county_missing_vmt,
            join_by(inventory_year, geoid, county_name, county_daily_vmt)) %>% 
  group_by(county_name, geoid, inventory_year) %>%
  summarise(
    county_missing_vmt = first(missing_vmt), # from county_df
    sum_pred_vmt = sum(pred_vmt, na.rm = TRUE),
    n_cities = n(),
    .groups = "keep"
  ) %>%
  ungroup() %>%
  mutate(
    scale = case_when(
      is.na(county_missing_vmt) ~ NA_real_, # can't scale if no county control
      sum_pred_vmt <= 0 ~ NA_real_, # handle degenerate
      # scale is the amount of county missing vmt divided by 
      # the total predicted VMT of CTUs missing data
      TRUE ~ county_missing_vmt / sum_pred_vmt
    )
  )

pred_from_na_bench %>% 
  select(county_name, geoid, n_cities) %>% 
  unique() %>% 
  left_join(total_ctu_per_county)

pred_df_na_bench <- pred_df %>% 
  left_join(pred_from_na_bench %>% select(county_name, inventory_year, geoid, scale, sum_pred_vmt),
            by = c("county_name", "inventory_year", "geoid")) %>%
  mutate(
    pred_vmt_bench = case_when(
      # is.na(daily_vmt) ~ pred_vmt * scale,
      # !is.na(daily_vmt) ~ daily_vmt,
      TRUE ~ pred_vmt * scale  # if no county control, keep model prediction
    ),
    final_city_vmt = if_else(!is.na(daily_vmt), daily_vmt, pred_vmt_bench),
    vmt_source = ifelse(final_city_vmt == daily_vmt, "MnDOT", "MetC Modeled")
  )


# credit river city and blaine are our two problem

# these totals should match exactly
pred_df_na_bench %>% 
  group_by(county_name, geoid, inventory_year) %>% 
  summarize(
    county_daily_vmt = first(county_daily_vmt),
    sum_pred_vmt_bench = sum(pred_vmt_bench),
    sum_final_vmt = sum(final_city_vmt),
    observed_pred_diff = county_daily_vmt - sum_final_vmt,
    n_cities = n(),
    .groups = "keep"
  ) %>% 
  filter(observed_pred_diff != 0) 


pred_df_na_bench %>% 
  filter(coctu_id_gnis %in% ctu_missing$coctu_id_gnis) %>%
  plot_ly(
    type = "scatter",
    mode = "lines+markers",
    x = ~inventory_year,
    y = ~final_city_vmt,
    color = ~ctu_name_full_county,
    symbol = ~vmt_source,
    symbols = c(
      "circle-open",
      "circle"),
    marker = list(size = 9),
    opacity = 0.7
  ) 


pred_df_na_bench %>% 
  select(geoid, coctu_id_gnis, ctu_name_full_county, inventory_year, final_city_vmt, vmt_source) %>% 
  bind_rows(ctu_pop_jobs_vmt %>% 
              select(inventory_year, coctu_id_gnis, ctu_name_full_county, geoid, final_city_vmt = daily_vmt, vmt_source) %>% 
              filter(inventory_year>= 2023)) %>% 
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


# TODO 
# - Check AGAIN if county level VMT sums up (having replaced predicted values 
# with the observed values)
# - Gut check various cities

