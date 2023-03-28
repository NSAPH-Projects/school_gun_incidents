## Functions to get regression results ----

get_models <- function(df, model = "logistic", covariate_names){
  if (model == "logistic"){
    model <- glm(y ~ ., 
                 data = df[, c("y", "a", covariate_names)], 
                 family = "binomial")
  } else if (model == "negbin"){
    model <- glm.nb(y ~ .,
                    data = df[, c("y", "a", covariate_names)])
  } else message("model must be `logistic` or `negbin`")
  return(model)
}