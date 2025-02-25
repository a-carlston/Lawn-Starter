
source("./00_functions/general.R", local = "general" <- new.env())

required_packages <- c("googlesheets4", "dplyr", "stringr", "tidyr", "vroom", "readxl")
# 
# remotes::install_github("business-science/modeltime")
# remotes::install_github("business-science/modeltime.ensemble")
# remotes::install_github("business-science/modeltime.resample")
# remotes::install_github("business-science/timetk")


# Machine Learning
library(tidymodels)
library(modeltime)
library(modeltime.ensemble)
library(modeltime.resample)

# Time Series
library(timetk)

# Core
library(tidyverse)





# Check and Install only (do not load)
general$install_packages(required_packages, load_packages = FALSE)


# Read queue list from Google Sheets -----
voice_volume <- readxl::read_xlsx("G:/Shared drives/WFM/Sandbox/voice_daily.xlsx") |> 
  filter(date >= as.Date("2022-01-01"))


voice_volume <- voice_volume |>
  dplyr::group_by(Staff_group, weekday = lubridate::wday(date, label = TRUE, abbr = FALSE)) |> 
  mutate(date = as.Date(date, origin = "1899-12-30",),
         # volume = log1p(volume),
         clean_volume = ts_clean_vec(volume, period = 7),
         weekday_avg  = slider::slide_dbl(clean_volume, mean, .before = 1, .after = 1, na.rm = TRUE)
         ) |>
  ungroup() |> 
  pivot_longer(c("volume", "clean_volume", "weekday_avg")) 





voice_volume|> 
  group_by(Staff_group) |>
  plot_time_series(
    .date_var   = date,
    .value      = value,
    .color_var  = name,
    .facet_vars = Staff_group,
    .facet_ncol = 2, 
    .smooth = F
  )

max_date <- max(voice_volume$date)

FORECAST_HORIZON <- as.numeric(as.Date("2025-12-31") - max_date)


# Process Data ------------------------------------------------------------

full_data_tbl <- voice_volume %>%
  filter(name == "weekday_avg") %>%
  select(date, Staff_group, Actuals = value) %>%
  group_by(Staff_group) %>%
  future_frame(
    .date_var   = date,
    .length_out = FORECAST_HORIZON,
    .bind_data  = TRUE
  ) %>%
  ungroup() %>%
  
  # Consolidate IDs
  mutate(id = fct_drop(Staff_group))




# Training Data
data_prepared_tbl <- full_data_tbl %>%
  filter(!is.na(Actuals))

data_prepared_tbl %>%
  group_by(Staff_group) %>%
  timetk::tk_summary_diagnostics()

# Forecast Data
future_tbl <- full_data_tbl %>%
  filter(is.na(Actuals))

future_tbl %>%
  group_by(Staff_group) %>%
  timetk::tk_summary_diagnostics()



# 4.0 SPLITTING ----

# Panel Data Splitting
splits <- data_prepared_tbl %>%
  time_series_split(
    date_var   = date,
    assess     = "2 months",
    cumulative = TRUE
  )

training(splits) %>%
  pull(date) %>%
  range()


# 5.0 PREPROCESSOR ----
recipe_spec_1 <- recipe(Actuals ~ ., training(splits)) %>%
  # First create time series features
  step_timeseries_signature(date) %>%
  
  # Remove unnecessary time components
  step_rm(matches("(.iso$)|(.xts$)|(hour)|(minute)|(second)|(am.pm)")) %>%
  
  # Normalize numeric features
  step_normalize(date_index.num, date_year) %>%
  
  # Add interaction
  step_interact(~ date_index.num:date_year) %>%
  
  # Manually add Fourier terms for all periods
  # step_mutate(
  #   # Weekly seasonality (7 days)
  #   date_sin7_K1 = sin(2 * pi * date_index.num / 7),
  #   date_cos7_K1 = cos(2 * pi * date_index.num / 7),
  #   date_sin7_K2 = sin(4 * pi * date_index.num / 7),
  #   date_cos7_K2 = cos(4 * pi * date_index.num / 7),
  #   
  #   Bi-weekly seasonality (14 days)
  #   date_sin14_K1 = sin(2 * pi * date_index.num / 14),
  #   date_cos14_K1 = cos(2 * pi * date_index.num / 14),
  #   date_sin14_K2 = sin(4 * pi * date_index.num / 14),
  #   date_cos14_K2 = cos(4 * pi * date_index.num / 14),
  #   
  #   # Monthly seasonality (30 days)
  #   date_sin30_K1 = sin(2 * pi * date_index.num / 30),
  #   date_cos30_K1 = cos(2 * pi * date_index.num / 30),
  #   date_sin30_K2 = sin(4 * pi * date_index.num / 30),
  #   date_cos30_K2 = cos(4 * pi * date_index.num / 30),
  #   
  #   Quarterly seasonality (90 days)
  #   date_sin90_K1 = sin(2 * pi * date_index.num / 90),
  #   date_cos90_K1 = cos(2 * pi * date_index.num / 90),
  #   date_sin90_K2 = sin(4 * pi * date_index.num / 90),
  #   date_cos90_K2 = cos(4 * pi * date_index.num / 90),
  #   
  #   # Yearly seasonality (365 days)
  #   date_sin365_K1 = sin(2 * pi * date_index.num / 365),
  #   date_cos365_K1 = cos(2 * pi * date_index.num / 365),
  #   date_sin365_K2 = sin(4 * pi * date_index.num / 365),
  #   date_cos365_K2 = cos(4 * pi * date_index.num / 365)
  # ) %>%
  
  # One-hot encode categorical variables
  step_dummy(all_nominal(), one_hot = TRUE)

recipe_spec_1 %>% prep() %>% juice() %>% glimpse()

recipe_spec_2 <- recipe_spec_1 %>%
  update_role(date, new_role = "ID")

recipe_spec_1 %>% prep() %>% summary()
recipe_spec_2 %>% prep() %>% summary()



# 6.0 MODELS ----

# * Prophet w/ Regressors -----
wflw_fit_prophet <- workflow() %>%
  add_model(
    prophet_reg(seasonality_daily = T, seasonality_weekly = T, seasonality_yearly = T) %>% set_engine("prophet") |> set_mode("regression")
  ) %>%
  add_recipe(recipe_spec_1) %>%
  fit(training(splits))

# * XGBoost ----
wflw_fit_xgboost <- workflow() %>%
  add_model(
    boost_tree() %>% set_engine("xgboost") |> set_mode("regression")
  ) %>%
  add_recipe(recipe_spec_2) %>%
  fit(training(splits))

# * Random Forest ----
wflw_fit_rf <- workflow() %>%
  add_model(
    rand_forest() %>% set_engine("ranger") |> set_mode("regression")
  ) %>%
  add_recipe(recipe_spec_2) %>%
  fit(training(splits))

# * SVM ----
wflw_fit_svm <- workflow() %>%
  add_model(
    svm_rbf() %>% set_engine("kernlab") |> set_mode("regression")
  ) %>%
  add_recipe(recipe_spec_2) %>%
  fit(training(splits))

# * Prophet Boost ----
wflw_fit_prophet_boost <- workflow() %>%
  add_model(
    prophet_boost(
      seasonality_daily  = T,
      seasonality_weekly = T,
      seasonality_yearly = T
    ) %>%
      set_engine("prophet_xgboost")
  ) %>%
  add_recipe(recipe_spec_1) %>%
  fit(training(splits))

# 7.0 MODELTIME WORKFLOW ----

# * Modeltime Table ----
submodels_tbl <- modeltime_table(
  # wflw_fit_prophet,
  wflw_fit_xgboost,
  wflw_fit_rf
  # wflw_fit_svm,
  # wflw_fit_prophet_boost
)

# * Calibrate Testing Data ----
submodels_calibrated_tbl <- submodels_tbl %>%
  modeltime_calibrate(testing(splits))

# * Measure Test Accuracy ----
submodels_calibrated_tbl %>% modeltime_accuracy()

# * Visualize Test Forecast ----
submodels_calibrated_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = data_prepared_tbl,
    keep_data   = TRUE
  ) %>%
  group_by(Staff_group) %>%
  plot_modeltime_forecast(.conf_interval_show = F,
                          .facet_ncol = 2
  )


# * Refit on Full Training Dataset -----
submodels_refit_tbl <- submodels_calibrated_tbl %>%
  modeltime_refit(data_prepared_tbl)

# * Visualize Submodel Forecast ----
submodels_refit_tbl %>%
  modeltime_forecast(
    new_data    = future_tbl,
    actual_data = data_prepared_tbl,
    keep_data   = TRUE
  ) %>%
  group_by(Staff_group) %>%
  plot_modeltime_forecast(.facet_ncol = 2)

# 8.0 ENSEMBLE ----

# * Make Ensemble ----
ensemble_fit_mean <- submodels_tbl %>%
  # filter(!.model_id %in% c(1)) %>%
  ensemble_average(type = "mean")

# * Modeltime Table ----
ensemble_tbl <- modeltime_table(
  ensemble_fit_mean
)

# * Ensemble Test Accuracy ----
ensemble_tbl %>%
  combine_modeltime_tables(submodels_tbl) %>%
  modeltime_accuracy(testing(splits))

# * Ensemble Test Forecast ----
ensemble_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = data_prepared_tbl,
    keep_data   = TRUE
  ) %>%
  group_by(Staff_group) %>%
  plot_modeltime_forecast()

# * Refit Ensemble ----
ensemble_refit_tbl <- ensemble_tbl %>%
  modeltime_refit(data_prepared_tbl)

# * Visualize Ensemble Forecast -----
forecast_complete <- ensemble_refit_tbl %>%
  modeltime_forecast(
    new_data    = future_tbl,
    actual_data = data_prepared_tbl,
    keep_data   = TRUE
  ) 
# %>%
#   mutate(
#     year = lubridate::year(.index),
#     month = lubridate::month(.index),
#     .value = expm1(.value),
#     Actuals = expm1(Actuals),
#     units = expm1(Units_log)
#   ) 


forecast_complete |> 
  group_by(Staff_group) |> 
  plot_modeltime_forecast()



# forecast_complete |> 
#   group_by(month, year) |> 
#   summarise(volume = sum(.value, na.rm = T)) |> 
#   arrange(year, month) |> tibble::view()

# Forecast_Finishing_Updates ----------------------------------------------

# * add holidays ----
final_forecast <- voice_volume %>%
  filter(name == "volume") %>%
  select(date, Staff_group, Actuals = value) %>%
  group_by(Staff_group) %>%
  future_frame(
    .date_var   = date,
    .length_out = FORECAST_HORIZON,
    .bind_data  = TRUE
  ) %>%
  ungroup() |>
  # dplyr::filter(name == "volume") |>
  # dplyr::left_join(daily_units |> select(Date = date, all_of(.country)), by = "Date") |>
  dplyr::left_join(forecast_complete |> select(date, Staff_group = id, forecast = .value) |> dplyr::filter(date >= as.Date("2025-01-01")), by = c("date", "Staff_group")) |>
  dplyr::mutate(forecast = dplyr::case_when(
    forecast < 5 ~ 0,
    TRUE ~ forecast
  )) |> 
  dplyr::mutate(forecast = case_when(
    date >= as.Date("2025-02-18") & date <= as.Date("2025-05-25") ~ forecast * 1.15,
    date >= as.Date("2025-05-27") & date <= as.Date("2025-11-30") ~ forecast * 1.05,
    # Date >= as.Date("2025-12-08") & Date <= as.Date("2025-12-31") ~ Forecast * 1.35,
    TRUE ~ forecast
  )) |> 
dplyr::mutate(forecast = case_when(
  date >= as.Date("2025-04-06") & date <= as.Date("2025-05-18") ~ forecast * 1.05,
  TRUE ~ forecast
))
# |> 
  # dplyr::mutate(Forecast = case_when(
  #   Date >= as.Date("2025-11-15") & Date <= as.Date("2025-12-31") & wday(Date, label = TRUE, week_start = 1) == "Mon" ~ Forecast * 1.20,
  #   TRUE ~ Forecast
  # ))
# |>
#   dplyr::left_join(master_holiday_factors, by = c("Continent", "Country", "Date")) |>
#   dplyr::mutate(Forecast = case_when(
#     !is.na(Factor) ~ Factor * Forecast,
#     TRUE ~ Forecast
#   ))
# 
# 
# ## * add hoop ----
# open_weekday <- hms::as_hms("07:00:00")
# close_weekday <- hms::as_hms("20:00:00")
# open_saturday <- hms::as_hms("07:00:00")
# close_saturday <- hms::as_hms("18:00:00")
# open_sunday <- NA
# close_sunday <- NA
# 
# 
# final_forecast <- final_forecast |> 
#   dplyr::mutate(
#     Open = case_when(
#       wday(Date, label = TRUE, week_start = 1) %in% c("Mon", "Tue", "Wed", "Thu", "Fri") ~ open_weekday,
#       wday(Date, label = TRUE, week_start = 1) == "Sat" ~ open_saturday,
#       wday(Date, label = TRUE, week_start = 1) == "Sun" ~ open_sunday
#     ),
#     Close = case_when(
#       wday(Date, label = TRUE, week_start = 1) %in% c("Mon", "Tue", "Wed", "Thu", "Fri") ~ close_weekday,
#       wday(Date, label = TRUE, week_start = 1) == "Sat" ~ close_saturday,
#       wday(Date, label = TRUE, week_start = 1) == "Sun" ~ close_sunday
#     )
#   ) 
# 
# 
final_forecast <- final_forecast |>
  rename(Brand = Staff_group, Date = date) |> 
  mutate(Continent = "North America", Country = "Untied States") |> 
  dplyr::group_by(Continent, Country, Brand, Date) |>
  dplyr::summarise(volume = sum(Actuals, na.rm = TRUE),
                   forecast = sum(forecast, na.rm = TRUE), .groups = "drop") |>
  general$lag_by_weekdays(column_name = "volume", years_back = 2) |> 
  mutate(channel = "voice") |> 
  rename_with(tolower) |> 
  select(continent:brand,channel,everything(),-lagged_date)
  
  
  
  final_forecast |> 
  tidyr::pivot_longer(c("forecast", "volume", "lag_volume_2024", "lag_volume_2023")) |>
  dplyr::filter(date >= as.Date("2025-01-01")) |>
  timetk::plot_time_series(.date_var = date, .value = value, .color_var = name, .facet_vars = brand, .facet_ncol = 2, .smooth = F)


  
  
  final_forecast |> 
    filter(brand != "Retention", date >=as.Date("2025-01-01")) |> 
    group_by(date) |> 
    summarise(volume = sum(volume)) |> tibble::view()




# Get the current year and month
current_year <- format(Sys.Date(), "%Y")
current_month <- format(Sys.Date(), "%m")
channel <- "voice"


# Construct the file name dynamically
file_name <- paste0("00_data/00_voice_forecasts/",channel, "_forecasts_", current_year, "-", current_month,"-15", ".xlsx")


# Export the data to Excel
writexl::write_xlsx(final_forecast, path = file_name)

