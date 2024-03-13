################################################################################
#                     Problem functions for `batchtools`
#                 (they generate the data and train the NN)
################################################################################

# Numerical problems -----------------------------------------------------------
syn_numerical <- function(data, job, n, p,
                          beta = "positive",
                          sample_type = "normal",
                          dgp_type = "linear",
                          scale_type = "scale_none",
                          beta0 = 0,
                          corr = 0,
                          mean = "zeros",
                          outcome_type = "regression",
                          n_test = 500,
                          nn_layers = 3,
                          nn_units = 256,
                          nn_act.fct = "relu", ...) {
  
  # Get attributes for the DGP
  mean <- get_mean(mean, p)
  sigma <- get_sigma(corr, p)
  beta <- get_beta(beta, p)
  
  print_args(as.list(environment()))
  
  # Get sample function
  sample_fun <- get_sample_fun(sample_type, p = p, mean = mean, sigma = sigma)
  dgp_fun <- get_dgp_fun(dgp_type, beta = beta, beta0 = beta0)
  
  # Create data
  data <- get_dataset(sample_fun, dgp_fun, n, n_test, preprocess_type = scale_type,
                      outcome_type = outcome_type)
  
  # Train model
  instance <- train_model(p, nn_units, nn_layers, data, outcome_type, 
                          nn_act.fct, seed = job$seed)
  
  # Add beta and outcome type
  instance$beta <- beta
  instance$outcome_type <- outcome_type
  
  instance
}

# Cetegorical problems ---------------------------------------------------------
syn_categorical <- function(data, job, n, p,
                            beta = "positive",
                            n_levels = 2,
                            level_beta = "random",
                            encode_type = "encode_label",
                            level_probs = "equal",
                            outcome_type = "regression",
                            n_test = 500,
                            nn_layers = 3,
                            nn_units = 256,
                            nn_act.fct = "relu", ...) {
  
  # Get attributes for the DGP
  beta <- get_beta(beta, p)
  level_beta <- get_level_beta(level_beta, n_levels)
  level_probs <- get_level_probs(level_probs, n_levels)
  
  print_args(as.list(environment()))
  
  # Get sample function
  sample_fun <- get_sample_fun("categorical", p = p, n_levels = n_levels,
                               level_probs = level_probs)
  dgp_fun <- get_dgp_fun("categorical", beta = beta, n_levels = n_levels, 
                         level_beta = level_beta, beta0 = beta0)
  
  # Create data
  data <- get_dataset(sample_fun, dgp_fun, n, n_test, preprocess_type = encode_type,
                      n_levels = n_levels)
  
  # Train model
  instance <- train_model(length(data$cor_groups), nn_units, nn_layers, data, 
                          outcome_type, nn_act.fct, seed = job$seed)
  
  # Add beta
  instance$beta <- beta
  instance$outcome_type <- outcome_type
  
  instance
}


# Real data problems -----------------------------------------------------------
get_realdata <- function(data, job, 
                         ds_name = "bike_sharing",
                         scale_type = "scale_none",
                         nn_units = 256,
                         nn_layers = 3, 
                         nn_act.fct = "relu", ...) {
  
  data <- switch(as.character(ds_name),
                 bike_sharing = get_bike_sharing_ds(scale_type),
                 boston_housing = get_boston_housing_ds(scale_type),
                 german_credit = get_german_credit_ds(scale_type),
                 compas = get_compas_ds(scale_type),
                 stop("Unknown dataset name: ", ds_name))
  
  # Train model
  instance <- train_model(length(data$cor_groups), nn_units, nn_layers, data, 
                          data$outcome_type, nn_act.fct, seed = job$seed)
  
  instance
}



################################################################################
#                       Helper functions
################################################################################
get_mean <- function(mean, p) {
  switch (as.character(mean),
          zeros = rep(0, p),
          random = runif(p, min = -2, max = 2),
          stop("Unknown value of 'mean': '", mean, "'!")
  )
}

get_sigma <- function(corr, p) {
  res <- diag(runif(p, 0.9, 1.1))
  
  for (i in seq_len(p)) {
    if (i %% 2 == 1 &  i != p) {
      res[i, i +1] <- corr
      res[i + 1, i] <- corr
    }
  }
  
  res
}

get_beta <- function(beta, p) {
  switch (as.character(beta),
          swapping = rep(c(1, 0), length.out = p),
          equal = rep(1, length.out = p),
          increasing = seq(1/p, 1, length.out = p),
          grouped = rep(c(0.1, 0.4, 1), each = p %/% 3, length.out = p),
          random = runif(p, min = -1, max = 1),
          rep(as.numeric(beta), length.out = p))
          
}

get_level_beta <- function(level_beta, n_levels) {
  switch(as.character(level_beta),
         mixed_random = runif(n_levels, min = -1, max = 1),
         mixed = seq(-1, 1, length.out = n_levels),
         stop("Unknown value of 'level_beta': '", level_beta, "'!"))
}

get_level_probs <- function(level_probs, n_levels) {
  switch(as.character(level_probs),
         equal = rep(1 / n_levels, n_levels),
         increasing = seq(1, n_levels) / sum(seq(1, n_levels)))
}

print_args <- function(args) {
  cli_h2("Problem description")
  
  if (!is.null(args$sample_type)) {
    cli_bullets(c(
      "*" = paste0("Outcome: {.val {args$outcome_type}}"),
      "*" = paste0("Number of samples: {.val {args$n}}"),
      "*" = paste0("Number of variables: {.val {args$p}}"),
      "*" = paste0("Sample type: {.val {args$sample_type}}"),
      "*" = paste0("Mean: {.val {args$mean}}"),
      "*" = paste0("DGP type: {.val {args$dgp_type}}"),
      "*" = paste0("Scale type: {.val {args$scale_type}}"),
      "*" = paste0("Intercept: {.val {args$beta0}}"),
      "*" = paste0("Beta coefficients: {.val {args$beta}}"),
      "*" = paste0("Number of layers: {.val {args$nn_layers}}"),
      "*" = paste0("Number of units: {.val {args$nn_units}}"),
      "*" = paste0("Activation: {.val {args$nn_act.fct}}")
    ))
  } else {
    cli_bullets(c(
      "*" = paste0("Outcome: {.val {args$outcome_type}}"),
      "*" = paste0("Number of samples: {.val {args$n}}"),
      "*" = paste0("Number of variables: {.val {args$p}}"),
      "*" = paste0("Number of levels: {.val {args$n_levels}}"),
      "*" = paste0("Encode type: {.val {args$encode_type}}"),
      "*" = paste0("Beta coefficients: {.val {args$beta}}"),
      "*" = paste0("Level beta coefficients: {.val {args$level_beta}}"),
      "*" = paste0("Number of layers: {.val {args$nn_layers}}"),
      "*" = paste0("Number of units: {.val {args$nn_units}}"),
      "*" = paste0("Activation: {.val {args$nn_act.fct}}")
    ))
  }
}
