################################################################################
#                                 Problems
################################################################################
syn_numerical <- function(data, job, n, p,
                          beta = "positive",
                          sample_type = "normal",
                          dgp_type = "linear",
                          scale_type = "scale_none",
                          beta0 = 0,
                          mean = "zeros",
                          n_test = 500,
                          nn_layers = 3,
                          nn_units = 256,
                          nn_act.fct = "relu", ...) {
  
  # Get attributes for the DGP
  mean <- get_mean(mean, p)
  sigma <- diag(p)
  beta <- get_beta(beta, p)
  outcome_type <- "regression"
  
  print_args(as.list(environment()))
  
  # Get sample function
  sample_fun <- get_sample_fun(sample_type, p = p, mean = mean, sigma = sigma)
  dgp_fun <- get_dgp_fun(dgp_type, beta = beta, beta0 = beta0)
  
  # Create data
  data <- get_dataset(sample_fun, dgp_fun, n, n_test, preprocess_type = scale_type)
  
  # Train model
  instance <- train_model(p, nn_units, nn_layers, data, outcome_type, 
                          nn_act.fct)
  
  instance
}


syn_categorical <- function(data, job, n, p,
                            beta = "positive",
                            n_levels = 2,
                            level_beta = "random",
                            encode_type = "encode_label",
                            level_probs = "equal",
                            n_test = 500,
                            nn_layers = 3,
                            nn_units = 256,
                            nn_act.fct = "relu", ...) {
  
  # Get attributes for the DGP
  beta <- get_beta(beta)
  level_beta <- get_level_beta(level_beta, n_levels)
  level_probs <- get_level_probs(level_probs, n_levels)
  outcome_type <- "regression"
  
  print_args(as.list(environment()))
  
  # Get sample function
  sample_fun <- get_sample_fun("categorical", p = p, n_levels = n_levels,
                               level_probs = level_probs)
  dgp_fun <- get_dgp_fun("categorical", beta = beta, n_levels = n_levels, 
                         level_beta = level_beta)
  
  # Create data
  data <- get_dataset(sample_fun, dgp_fun, n, n_test, preprocess_type = encode_type,
                      n_levels = n_levels)
  
  # Train model
  instance <- train_model(length(data$cor_groups), nn_units, nn_layers, data, 
                          outcome_type, nn_act.fct)
  
  instance
}


syn_uninformative_cont <- function(data, job, n, p,
                                   sample_type = "normal",
                                   dgp_type = "linear",
                                   scale_type = "scale_none",
                                   beta0 = 0,
                                   mean = "zeros",
                                   n_test = 500,
                                   n_layers = 2,
                                   n_units = 64,
                                   act.fct = "relu", ...) {
  
  # Set attributes
  n_cpus <- 1
  n_workers <- 0
  outcome_type <- "regression"
  mean <- switch (as.character(mean),
                  zeros = rep(0, p),
                  range_random = sample(seq(-2, 2, length.out = p)),
                  random = runif(p, min = -2, max = 2),
                  stop("Unknown value of 'mean': '", mean, "'!")
  )
  sigma <- diag(p)
  shape <- 1.5 # only for sample_type == "weibull"
  beta <- c(-1, 1, rep(0, p - 2))
  
  cli_h2("Problem description")
  cli_bullets(c(
    "*" = paste0("Number of samples: {.val {n}}"),
    "*" = paste0("Number of variables: {.val {p}}"),
    "*" = paste0("Sample type: {.val {sample_type}}"),
    "*" = paste0("Mean: {.val {mean}}"),
    "*" = paste0("DGP type: {.val {dgp_type}}"),
    "*" = paste0("Scale type: {.val {scale_type}}"),
    "*" = paste0("Intercept: {.val {beta0}}"),
    "*" = paste0("Number of layers: {.val {n_layers}}"),
    "*" = paste0("Activation: {.val {act.fct}}")
  ))
  
  # Get sample function
  sample_fun <- get_sample_fun(sample_type, p = p, mean = mean, sigma = sigma, 
                               shape = shape)
  dgp_fun <- get_dgp_fun(dgp_type, beta = beta, beta0 = beta0, 
                         sample_type = sample_type, mean = mean)
  
  # Create data
  data <- get_dataset(sample_fun, dgp_fun, n, n_test, preprocess_type = scale_type)
  
  # Train model
  instance <- train_model(p, n_units, n_layers, data, outcome_type, 
                          act.fct, n_cpus, n_workers)
  
  instance
}

syn_uninformative_cat <- function(data, job, n, p,
                                  n_levels = 2,
                                  level_beta = "mixed",
                                  encode_type = "encode_label",
                                  level_probs = "equal",
                                  n_test = 500,
                                  n_layers = 2,
                                  n_units = 64,
                                  act.fct = "relu", ...) {
  
  # Set attributes
  n_cpus <- 1
  n_workers <- 0
  outcome_type <- "regression"
  beta <- c(-1, 1, rep(0, p - 2))
  level_beta <- switch(as.character(level_beta),
                       range_random = sample(seq(-1, 1, length.out = n_levels)),
                       pos_random = runif(p),
                       neg_ramdom = runif(p, min = -1, max = 0),
                       mixed_random = runif(p, min = -1, max = 1),
                       pos = seq(1/n_levels, 1, length.out = n_levels),
                       neg = seq(-1, -1/n_levels, length.out = n_levels),
                       mixed = seq(-1, 1, length.out = n_levels),
                       stop("Unknown value of 'level_beta': '", level_beta, "'!"))
  
  level_probs <- switch (as.character(level_probs),
                         equal = rep(1 / n_levels, n_levels),
                         increasing = seq(1, n_levels) / sum(seq(1, n_levels))
  )
  
  cli_h2("Problem description")
  cli_bullets(c(
    "*" = paste0("Number of samples: {.val {n}}"),
    "*" = paste0("Number of variables: {.val {p}}"),
    "*" = paste0("Number of levels: {.val {n_levels}}"),
    "*" = paste0("Encode type: {.val {encode_type}}"),
    "*" = paste0("Beta coefficients: {.val {beta}}"),
    "*" = paste0("Level beta coefficients: {.val {level_beta}}"),
    "*" = paste0("Number of layers: {.val {nn_layers}}"),
    "*" = paste0("Number of units: {.val {nn_units}}"),
    "*" = paste0("Activation: {.val {nn_act.fct}}")
  ))
  
  # Get sample function
  sample_fun <- get_sample_fun("categorical", p = p, n_levels = n_levels,
                               level_probs = level_probs)
  dgp_fun <- get_dgp_fun("categorical", beta = beta, n_levels = n_levels, 
                         level_beta = level_beta)
  
  # Create data
  data <- get_dataset(sample_fun, dgp_fun, n, n_test, preprocess_type = encode_type,
                      n_levels = n_levels)
  
  # Train model
  instance <- train_model(length(data$cor_groups), n_units, n_layers, data, outcome_type, 
                          act.fct, n_cpus, n_workers)
  
  instance
}


################################################################################
#                       Helper functions
################################################################################
get_mean <- function(mean, p) {
  switch (as.character(mean),
          zeros = rep(0, p),
          range = seq(-1, 1, length.out = p),
          range_random = sample(seq(-1, 1, length.out = p)),
          random = runif(p, min = -1, max = 1),
          stop("Unknown value of 'mean': '", mean, "'!")
  )
}

get_beta <- function(beta, p) {
  switch (as.character(beta),
          equal = rep(1, length.out = p),
          positive = seq(1/p, 1, length.out = p),
          negative = -seq(1/p, 1, length.out = p),
          mixed = seq(-1, 1, length.out = p),
          positive_strong = seq(0.25, 1, length.out = p),
          stop("Unknown value of 'beta': '", beta, "'!")
  )
}

get_level_beta <- function(level_beta, n_levels) {
  switch(as.character(level_beta),
         range_random = sample(seq(-1, 1, length.out = n_levels)),
         pos_random = runif(n_levels),
         neg_ramdom = runif(n_levels, min = -1, max = 0),
         mixed_random = runif(n_levels, min = -1, max = 1),
         pos = seq(1/n_levels, 1, length.out = n_levels),
         neg = seq(-1, -1/n_levels, length.out = n_levels),
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
