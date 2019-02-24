library(tidymodels)

library(AmesHousing)
ames <-
  make_ames() %>%
  dplyr::select(-matches("Qu"))
nrow(ames)

# Make sure that you get the same random numbers
set.seed(4595)
data_split <- rsample::initial_split(ames, strata = "Sale_Price")
data_split

ames_train <- rsample::training(data_split)

# let's do it for one
simple_lm <- lm(log10(Sale_Price) ~ Longitude + Latitude, data = ames_train)
simple_lm_values <- broom::augment(simple_lm)

# let's do it for 10
set.seed(2453)
cv_splits <- rsample::vfold_cv(
  data = ames_train,
  v = 10,
  strata = "Sale_Price"
)

cv_splits

pryr::object_size(ames_train)
pryr::object_size(cv_splits)

# set up models -----------------------------------------------------------
draft <-
  parsnip::linear_reg()

spec_lm <-
  draft %>%
  parsnip::set_engine(engine = "lm")
spec_lm

spec_stan <-
  draft %>%
  parsnip::set_engine("stan", chains = 2, iter = 100)
spec_stan


# fit ---------------------------------------------------------------------
geo_form <- log10(Sale_Price) ~ Latitude + Longitude
fit_model <- function(split, spec, formula = geo_form) {
  parsnip::fit(
    object = spec,
    formula = formula,
    data = rsample::analysis(split) # <- pulls out training set
  )
}

compute_pred <- function(split, model) {
  # Extract the assessment set
  assess <- rsample::assessment(split) %>%
    dplyr::mutate(Sale_Price_Log = log10(Sale_Price))

  # Compute predictions (a df is returned)
  pred <- predict(model, new_data = assess)

  dplyr::bind_cols(assess, pred)
}

cv_splits <-  cv_splits %>%
  dplyr::mutate(
    models_lm   = purrr::map(splits, fit_model, spec_lm),
    pred_lm     = purrr::map2(splits, models_lm, compute_pred),
    models_stan = purrr::map(splits, fit_model, spec_stan),
    pred_stan   = purrr::map2(splits, models_stan, compute_pred)
  )

# performance -------------------------------------------------------------
compute_perf <- function(pred_df) {
  # Create a function that calculates
  # rmse and rsq and returns a data frame
  numeric_metrics <- yardstick::metric_set(yardstick::rmse, yardstick::rsq)

  numeric_metrics(
    pred_df,
    truth = Sale_Price_Log,
    estimate = .pred
  )
}

cv_splits <- cv_splits %>%
  dplyr::mutate(perf_lm   = purrr::map(pred_lm,   compute_perf),
                perf_stan = purrr::map(pred_stan, compute_perf))

cv_splits %>%
  tidyr::unnest(perf_lm, perf_stan, .sep = "_") %>%
  dplyr::select(id, .metric = perf_lm_.metric, dplyr::ends_with(".estimate")) %>%
  tidyr::gather(engine, value, -id, -.metric) %>%
  dplyr::group_by(.metric, engine) %>%
  dplyr::summarise(
    .avg = mean(value),
    .sd = sd(value)
  )

holdout_results <-
  cv_splits %>%
  tidyr::unnest(pred_lm) %>%
  dplyr::mutate(.resid = Sale_Price_Log - .pred)


