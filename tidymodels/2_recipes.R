library(tidymodels)
theme_set(theme_bw())

library(AmesHousing)
ames <- make_ames() %>%
  dplyr::select(-matches("Qu"))

set.seed(4595)
data_split <- rsample::initial_split(ames, strata = "Sale_Price")

ames_train <- rsample::training(data_split)
ames_test  <- rsample::testing(data_split)

set.seed(2453)
cv_splits <- rsample::vfold_cv(ames_train, v = 10, strata = "Sale_Price")

mod_rec <- recipes::recipe(Sale_Price ~ Longitude + Latitude, data = ames_train) %>%
  recipes::step_log(Sale_Price, base = 10)
mod_rec

mod_rec <- recipes::recipe(
  Sale_Price ~ Longitude + Latitude + Neighborhood,
  data = ames_train                                                             # only train data is used!
) %>%
  recipes::step_log(Sale_Price, base = 10) %>%
  # Lump factor levels that occur in
  # <= 5% of data as "other"
  recipes::step_other(Neighborhood, threshold = 0.05) %>%
  # Create dummy variables for _any_ factor variables
  recipes::step_dummy(all_nominal())

# recipe  -->  prepare   --> bake/juice
# (define) --> (estimate) -->  (apply)
mod_rec_trained <- recipes::prep(mod_rec, training = ames_train, verbose = TRUE) # retain = TRUE
mod_rec_trained
recipes::juice(mod_rec_trained)

ames_test_dummies <- recipes::bake(mod_rec_trained, new_data = ames_test)
names(ames_test_dummies)

# Make the example data using data from caret --------------------
library(caret)
data(segmentationData)

segmentationData <-
  segmentationData %>%
  dplyr::select(EqSphereAreaCh1, PerimCh1, Class, Case) %>%
  setNames(c("PredictorA", "PredictorB", "Class", "Case")) %>%
  dplyr::mutate(Class = factor(ifelse(Class == "PS", "One", "Two")))

bivariate_data_train <-
  segmentationData %>%
  dplyr::filter(Case == "Train") %>%
  dplyr::select(-Case)

bivariate_data_test  <-
  segmentationData %>%
  dplyr::filter(Case == "Test") %>%
  dplyr::select(-Case)

ggplot(bivariate_data_test,
       aes(x = PredictorA,
           y = PredictorB,
           color = Class)) +
  geom_point(alpha = .3, cex = 1.5) +
  theme(legend.position = "top")

bivariate_pca <-
  recipes::recipe(Class ~ PredictorA + PredictorB, data = bivariate_data_train) %>%
  recipes::step_BoxCox(all_predictors()) %>%
  recipes::step_center(all_predictors()) %>%
  recipes::step_scale(all_predictors()) %>%
  recipes::step_pca(all_predictors()) %>%
  recipes::prep(training = bivariate_data_train, verbose = FALSE)

pca_test <- recipes::bake(bivariate_pca, new_data = bivariate_data_test)

pca_rng <- extendrange(c(pca_test$PC1, pca_test$PC2))

ggplot(pca_test, aes(x = PC1, y = PC2, color = Class)) +
  geom_point(alpha = .2, cex = 1.5) +
  theme(legend.position = "top") +
  xlim(pca_rng) + ylim(pca_rng) +
  xlab("Principal Component 1") + ylab("Principal Component 2")

# Back to Ames -------------------------------------------------------
## how to create interactions

price_breaks <- (1:6)*(10^5)
ggplot(
  ames_train,
  aes(x = Year_Built, y = Sale_Price)
) +
  geom_point(alpha = 0.4) +
  scale_x_log10() +
  scale_y_continuous(
    breaks = price_breaks
  ) +
  geom_smooth(method = "loess")

library(MASS) # to get robust linear regression model

ggplot(
  ames_train,
  aes(x = Year_Built,
      y = Sale_Price)
) +
  geom_point(alpha = 0.4) +
  scale_y_continuous(
    breaks = price_breaks
  ) +
  facet_wrap(~ Central_Air, nrow = 2) +
  geom_smooth(method = "rlm")

mod1 <- lm(log10(Sale_Price) ~ Year_Built + Central_Air, data = ames_train)
mod2 <- lm(
  log10(Sale_Price) ~ Year_Built + Central_Air + Year_Built:Central_Air,
  data = ames_train
)
anova(mod1, mod2)

recipes::recipe(Sale_Price ~ Year_Built + Central_Air, data = ames_train) %>%
  recipes::step_log(Sale_Price) %>%
  recipes::step_dummy(Central_Air) %>%
  recipes::step_interact(~ starts_with("Central_Air"):Year_Built) %>%
  recipes::prep(training = ames_train) %>%
  recipes::juice() %>%
  dplyr::slice(153:157)

lin_terms <- recipes::recipe(Sale_Price ~ Bldg_Type + Neighborhood + Year_Built +
                      Gr_Liv_Area + Full_Bath + Year_Sold + Lot_Area +
                      Central_Air + Longitude + Latitude,
                    data = ames_train) %>%
  recipes::step_log(Sale_Price, base = 10) %>%
  recipes::step_BoxCox(Lot_Area, Gr_Liv_Area) %>%                               # highly skewed
  recipes::step_other(Neighborhood, threshold = 0.05)  %>%                      # a lot of small categories
  recipes::step_dummy(all_nominal()) %>%
  recipes::step_interact(~ starts_with("Central_Air"):Year_Built)
lin_terms

nonlin_terms <- lin_terms %>%
  recipes::step_bs(Longitude, Latitude, options = list(df = 5))                 # nonlinear interaction, so using B-splines
nonlin_terms

ggplot(ames_train,
       aes(x = Longitude, y = Sale_Price)) +
  geom_point(alpha = .5) +
  geom_smooth(
    method = "lm",
    formula = y ~ splines::bs(x, 5),
    se = FALSE
  ) +
  scale_y_log10()

ggplot(ames_train,
       aes(x = Latitude, y = Sale_Price)) +
  geom_point(alpha = .5) +
  geom_smooth(
    method = "lm",
    formula = y ~ splines::bs(x, 5),
    se = FALSE
  ) +
  scale_y_log10()

# prepper is a wrapper that will use analysis set from rsample
cv_splits <- cv_splits %>%
  dplyr::mutate(nonlin_terms = purrr::map(splits, recipes::prepper, recipe = nonlin_terms),
                lin_terms    = purrr::map(splits, recipes::prepper, recipe = lin_terms))
cv_splits$nonlin_terms[[1]]
cv_splits$lin_terms[[1]]

# fitting the model
spec_lm <- parsnip::linear_reg() %>%
  parsnip::set_engine("lm")

lm_fit_rec <- function(rec_obj, formula) {
  # juice contains prepped version of the dataset
  parsnip::fit(spec_lm, formula, data = recipes::juice(rec_obj))
}

cv_splits <- cv_splits %>%
  dplyr::mutate(
    nonlin_models = purrr::map(
      nonlin_terms,
      lm_fit_rec,
      Sale_Price ~ .
    ),
    lin_models    = purrr::map(
      lin_terms,
      lm_fit_rec,
      Sale_Price ~ .
    )
  )

broom::glance(cv_splits$nonlin_models[[1]]$fit)
broom::glance(cv_splits$lin_models[[1]]$fit)

# the split object (to get the assessment data)
# the recipe object (to process the data)
# the linear model (for predictions)
assess_predictions <- function(split, recipe, model) {
  raw_assessment <- rsample::assessment(split)
  processed <- recipes::bake(recipe, new_data = raw_assessment)
  model %>%
    predict(new_data = processed) %>%
    dplyr::bind_cols(processed) %>%
    dplyr::mutate(
      # Sale_Price is already logged by the recipe
      .resid = Sale_Price - .pred,
      # Save the original row number of the data
      .row = as.integer(split, data = "assessment")
    )
}


# compute the summary statistics ------------------------------------------
cv_splits <- cv_splits %>%
  dplyr::mutate(
    nonlin_pred = purrr::pmap(
      list(
        split  = splits,
        recipe = nonlin_terms,
        model  = nonlin_models
      ),
      assess_predictions
    ),
    lin_pred    = purrr::pmap(
      list(
        split  = splits,
        recipe = lin_terms,
        model  = lin_models
      ),
      assess_predictions
    )
  )

cv_splits %>%
  tidyr::unnest(nonlin_pred) %>%
  dplyr::group_by(id) %>%
  yardstick::metrics(truth = Sale_Price, estimate = .pred) %>%
  dplyr::group_by(.metric) %>%
  dplyr::summarise(
    resampled_estimate = mean(.estimate)
  )

cv_splits %>%
  tidyr::unnest(lin_pred) %>%
  dplyr::group_by(id) %>%
  yardstick::metrics(truth = Sale_Price, estimate = .pred) %>%
  dplyr::group_by(.metric) %>%
  dplyr::summarise(
    resampled_estimate = mean(.estimate)
  )


# visually inspecting results ---------------------------------------------
assess_nonlin_pred <- cv_splits %>%
  tidyr::unnest(nonlin_pred) %>%
  dplyr::mutate(
    Sale_Price = 10^Sale_Price,
    .pred = 10^.pred
  )

ggplot(assess_nonlin_pred,
       aes(x = Sale_Price,
           y = .pred)) +
  geom_abline(lty = 2) +
  geom_point(alpha = .4)  +
  geom_smooth(se = FALSE, col = "red") +
  scale_y_continuous(limits = c(0.5e5, 6e5)) +
  ggtitle("Nonlinear terms included")

assess_lin_pred <- cv_splits %>%
  tidyr::unnest(lin_pred) %>%
  dplyr::mutate(
    Sale_Price = 10^Sale_Price,
    .pred = 10^.pred
  )

ggplot(assess_lin_pred,
       aes(x = Sale_Price,
           y = .pred)) +
  geom_abline(lty = 2) +
  geom_point(alpha = .4)  +
  geom_smooth(se = FALSE, col = "red") +
  scale_y_continuous(limits = c(0.5e5, 6e5)) +
  ggtitle("Only linear terms")

