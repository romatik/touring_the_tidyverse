library(tidymodels)

library(AmesHousing)
ames <-
  make_ames() %>%
  dplyr::select(-matches("Qu"))
nrow(ames)

set.seed(4595)
data_split <- rsample::initial_split(ames, strata = "Sale_Price")
data_split

ames_train <- rsample::training(data_split)
nrow(ames_train)

# let's do it for 1
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

# fit on analysis part
fit_model <- function(split, spec, formula = geo_form) {
  parsnip::fit(
    object = spec,
    formula = formula,
    data = rsample::analysis(split) # <- pulls out training set
  )
}

# predict on assessment part
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
  # rmse and rsq and returns a _data frame_
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

# dials -------------------------------------------------------------------
ames_train_log <- ames_train %>%
  dplyr::mutate(Sale_Price_Log = log10(Sale_Price))

spec_knn <- parsnip::nearest_neighbor(neighbors = 2) %>%
  parsnip::set_engine("kknn")
spec_knn

fit_knn <- parsnip::fit(spec_knn, geo_form, ames_train_log)
fit_knn

# Predict on the same data you train with
repredicted <- fit_knn %>%
  predict(new_data = ames_train_log) %>%
  dplyr::bind_cols(ames_train_log) %>%
  dplyr::select(.pred, Sale_Price_Log)
repredicted

repredicted %>%
  yardstick::rsq(
    truth = Sale_Price_Log,
    estimate = .pred
  )

cv_splits <- cv_splits %>%
  dplyr::mutate(
    # same functions
    models_knn = purrr::map(splits, fit_model, spec_knn),
    pred_knn = purrr::map2(splits, models_knn, compute_pred),
    perf_knn = purrr::map(pred_knn, compute_perf)
  )


# Unnest & compute resampled performance estimates
cv_splits %>%
  tidyr::unnest(perf_knn) %>%
  dplyr::group_by(.metric) %>%
  dplyr::summarise(
    .estimate_mean = mean(.estimate),
    .estimate_sd = sd(.estimate)
  )

# let's compare lm and kknn
extract_rmse <- function(perf_list) {
  perf_list %>%
    dplyr::bind_rows() %>%
    dplyr::filter(.metric == "rmse") %>%
    dplyr::pull(.estimate)
}

rmse_lm <- extract_rmse(cv_splits$perf_lm)
rmse_knn <- extract_rmse(cv_splits$perf_knn)

rs_comp <- data.frame(
  rmse = c(rmse_lm, rmse_knn),
  Model = rep(c("Linear\nRegression", "2-NN"), each = nrow(cv_splits)),
  Resample = cv_splits$id
)

ggplot(rs_comp, aes(x = Model, y = rmse, group = Resample, col = Resample)) +
  geom_point() +
  geom_line() +
  theme(legend.position = "none")

t.test(rmse_lm, rmse_knn, paired = TRUE)


# can we do better? -------------------------------------------------------
#    ├── Create a set of candidate tuning parameter values
#    └── For each resample
#    │   ├── Split the data into analysis and assessment sets
#    │   ├── [preprocess data]
#    │   ├── For each tuning parameter value
#    │   │   ├── Fit the model using the analysis set
#    │   │   └── Compute the performance on the assessment set and save
#    ├── For each tuning parameter value, average the performance over resamples
#    ├── Determine the best tuning parameter value
#    └── Create the final model with the optimal parameter(s) on the training set

# Parameter object for `neighbors`
dials::neighbors

# Number of neighbors varies from 1-20
param_grid <-
  dials::neighbors %>%
  dials::range_set(c(1, 20)) %>%
  dials::grid_regular(levels = 20)

dplyr::glimpse(param_grid)

# Declare `neighbors` as varying
spec_knn_varying <- parsnip::nearest_neighbor(
  neighbors = parsnip::varying()
) %>%
  parsnip::set_engine("kknn") %>%
  parsnip::set_mode("regression")
spec_knn_varying

param_grid <-
  param_grid %>%
  dplyr::mutate(
    specs = merge(., spec_knn_varying)
  )
param_grid

print(param_grid, n = 4)
param_grid$specs[[20]]


# building the algorithm --------------------------------------------------
#    │   │   ├── Fit the model using the analysis set
#    │   │   └── Compute the performance on the assessment set and save
fit_one_spec_one_split <- function(spec, split) {
  mod <- fit_model(split, spec)
  pred_df <- compute_pred(split, mod)
  perf_df <- compute_perf(pred_df)
  # pull out only rmse
  perf_df %>%
    dplyr::filter(.metric == "rmse") %>%
    dplyr::pull(.estimate)
}

fit_one_spec_one_split(
  param_grid$specs[[6]],  # Six neighbors
  cv_splits$splits[[9]]  # Ninth Fold
)

#    │   ├── For each tuning parameter value
#    │   │   ├── Run `fit_one_spec_one_split()`
fit_all_specs_one_split <- function(split, spec_df) {
  spec_df %>%
    dplyr::mutate(
      rmse = purrr::map_dbl(
        specs,
        fit_one_spec_one_split,
        split = split
      )
    )
}

fit_all_specs_one_split(
  cv_splits$splits[[1]],
  param_grid
) %>%
  print(n = 5)

#    └── For each resample
#    │   ├── Run `fit_all_specs_one_split()`
fit_all_specs_all_splits <- function(split_df, spec_df) {
  split_df %>%
    dplyr::mutate(
      spec_perf = purrr::map(
        splits,
        fit_all_specs_one_split,
        spec_df = spec_df
      )
    ) %>%
    dplyr::select(splits, id, spec_perf)
}

resampled_grid <- fit_all_specs_all_splits(
  split_df = cv_splits,
  spec_df = param_grid
)

resampled_grid %>%
  dplyr::slice(1:6)

# Keep the unnested version
unnested_grid <-
  resampled_grid %>%
  tidyr::unnest(spec_perf) %>%
  dplyr::select(-specs)

unnested_grid %>%
  dplyr::slice(1:6)

rmse_by_neighbors <-
  unnested_grid %>%
  dplyr::group_by(neighbors) %>%
  dplyr::summarize(rmse = mean(rmse))

ggplot(rmse_by_neighbors, aes(x = neighbors, y = rmse)) +
  geom_point() +
  geom_line()

best_neighbors <-
  unnested_grid %>%
  dplyr::group_by(id) %>%
  dplyr::summarize(neighbors = neighbors[which.min(rmse)],
                   rmse      = rmse[which.min(rmse)])

ggplot(rmse_by_neighbors, aes(x = neighbors, y = rmse)) +
  geom_point() +
  geom_line() +
  geom_line(data = unnested_grid,
            aes(group = id, col = id),
            alpha = .2, lwd = 1) +
  geom_point(data = best_neighbors,
             aes(col = id),
             alpha = .5, cex = 2) +
  theme(legend.position = "none")

best_neighbor_value <-
  rmse_by_neighbors %>%
  dplyr::filter(rmse == min(rmse)) %>%
  dplyr::pull(neighbors)

best_spec <-
  param_grid %>%
  dplyr::filter(neighbors == best_neighbor_value) %>%
  dplyr::pull(specs) %>%
  .[[1]]
best_spec

parsnip::fit(best_spec, geo_form, ames_train)

