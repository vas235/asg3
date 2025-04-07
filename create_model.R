library(lspline)
library(glmtoolbox)


data <- readRDS("data-2013-2023-final-vars.rds")

start_year <- 2013
end_year <- 2023

# to allow the GEE model to converge
# scale the weights by dividing by their mean
# this should not invalidate the weights
data$scaled_weights <- data$adjusted_weights / mean(data$adjusted_weights)


# reorder povlev 4 to make interpretting the output easier
data$povlev4 <- relevel(data$povlev4, ref = "400% FPL or greater")


# glmgee function from glmtoolbox
# this is the final model code
gee_model <- glmgee(
  formula = providers_binary ~ lspline(diagnosis_year_scaled, 6) * (race4 + povlev4 + k2q35a_1_years_numeric),
  family = binomial(link = "logit"),  # logistic regression
  data = data,  # Use the dataset
  weights = data$scaled_weights,  # use survey weights
  id = data$clusters,  # clustered data
  corstr = "Exchangeable",  # correlation structure: exchangeable
  maxit = 500,  # max iterations for convergence
  trace = FALSE
)


summary(gee_model)