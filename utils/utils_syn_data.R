################################################################################
#                         Get the dataset
################################################################################

get_dataset <- function(sample_fun, dgp_fun, n, n_test, preprocess_type = "scale_none",
                        n_levels = NULL, outcome_type = "regression") {
  cli_progress_step("Creating dataset...", "Dataset created!")
  # Sample data
  train_data <- as.matrix(sample_fun(n))
  val_data <- as.matrix(sample_fun(as.integer(0.3 * n)))
  test_data <- as.matrix(sample_fun(n_test))
  
  # For numerical data, we use the scaling of the train data
  if (startsWith(as.character(preprocess_type), "scale_")) {
    scale_args <- get_scale_args(train_data)
  } else {
    scale_args <- NULL
  }
  
  # Get preprocess function
  preprocess_fun <- get_preprocess_fun(preprocess_type, scale_args = scale_args,
                                       n_levels = n_levels)
  
  # Define outcome function
  if (outcome_type == "regression") {
    out_fun <- function(data, n) {
      dgp_fun(data)$lp + rnorm(n)
    }
  } else if (outcome_type == "classification") {
    center_point <- median(dgp_fun(train_data)$lp)
    out_fun <- function(data, n) {
      lp <- dgp_fun(data)$lp - center_point
      as.factor(rbinom(n, size = 1, prob = plogis(lp)))
    }
  }
  
  # Create train data
  train <- list(
    x = preprocess_fun(train_data),
    y = out_fun(train_data, n)
  )
  
  # Create validation data
  valid <- list(
    x = preprocess_fun(val_data), 
    y = out_fun(val_data, as.integer(0.3 * n))
  )
  
  # Create test data
  test <- list(
    x = preprocess_fun(test_data),
    y = out_fun(test_data, n_test)
  )
  
  # Create ground truth
  cor_groups <- rep(seq_len(ncol(train_data)), 
                    each = ncol(train$x) %/% ncol(train_data))
  imp_local <- dgp_fun(test_data)$effects
  
  list(train = train, valid = valid, test = test, cor_groups = cor_groups,
       imp_local = imp_local)
}


################################################################################
#                   Data Generating Processes for Numeric Data
################################################################################
get_dgp_fun <- function(type, beta, beta0 = 0, n_levels = NULL, level_beta = NULL) {
  switch (as.character(type),
          linear = function(x) dgp_linear(x, beta = beta, beta0 = beta0),
          linear_and_squared = function(x) dgp_lin_squared(x, beta = beta, beta0 = beta0),
          linear_and_squared_2 = function(x) dgp_lin_squared(x, beta = beta, beta0 = beta0, squared_idx = 10:20),
          pwlinear = function(x) dgp_pwlinear(x, beta = beta, beta0 = beta0),
          squared = function(x) dgp_squared(x, beta = beta, beta0 = beta0),
          smooth = function(x) dgp_smooth(x, beta = beta, beta0 = beta0),
          gaussian = function(x) dgp_gaussian(x, beta = beta, beta0 = beta0),
          cos = function(x) dgp_cos(x, beta = beta, beta0 = beta0),
          nonlinear = function(x) dgp_nonlinear(x, beta = beta, beta0 = beta0),
          categorical = function(x) dgp_categorical(x, beta = beta, n_levels = n_levels,
                                                    level_beta = level_beta),
          stop("Unknown data generating process: '", type, "'")
  )
}

dgp_linear <- function(x, beta, beta0) {
  effects <- t(t(x) * beta)
  list(lp = rowSums(effects) + beta0, effects = effects)
}

dgp_lin_squared <- function(x, beta, beta0, squared_idx = 2) {
  x[, squared_idx] <- cos(x[, squared_idx]*0.4 *pi) * exp(-(0.7 * x[, squared_idx])^2) * 1.768237
  effects <- t(t(x) * beta)
  list(lp = rowSums(effects) + beta0, effects = effects)
}

dgp_pwlinear <- function(x, beta, beta0) {
  res <- (
    (x < -2.75) * (-1) +
    (x >= -2.75 & x < -2.25) * (x + 1.75) +
    (x >= -2.25 & x < -1.75) * (-0.5) +
    (x >= -1.75 & x < -1.25) * (x + 1.25) +
    (x >= -1.25 & x < -0.75) * 0 +
    (x >= -0.75 & x < -0.25) * (x + 0.75) +
    (x >= -0.25 & x < 0.25) * 0.5 +
    (x >= 0.25 & x < 0.75) * (-x + 0.75) +
    (x >= 0.75 & x < 1.25) * 0 +
    (x >= 1.25 & x < 1.75) * (-x + 1.25) +
    (x >= 1.75 & x < 2.25) * (-0.5) +
    (x >= 2.25 & x < 2.75) * (-x + 1.75) +
    (x >= 2.75) * (-1)) * 2
    
    
  effects <- t(t(res) * beta)
  list(lp = rowSums(effects) + beta0, effects = effects)
}

dgp_smooth <- function(x, beta, beta0) {
  
  res <- cos(x*0.4 *pi) * exp(-(0.7*x)^2) * 1.768237
  effects <- t(t(res) * beta)
  list(lp = rowSums(effects) + beta0, effects = effects)
}

dgp_squared <- function(x, beta, beta0) {
  effects <- t(t(x**2) * beta)
  list(lp = rowSums(effects) + beta0, effects = effects)
}

dgp_gaussian <- function(x, beta, beta0) {
  effects <- t(t(exp(-x**2)) * 2 * beta)
  list(lp = rowSums(effects) + beta0, effects = effects)
}

dgp_cos <- function(x, beta, beta0) {
  effects <- t(t(cos(x * pi) * x) * beta)
  list(lp = rowSums(effects) + beta0, effects = effects)
}

dgp_nonlinear <- function(x, beta, beta0) {
  
  effects <- ifelse(abs(x) < qnorm(0.75), 1, -1)
  effects <- t(t(effects) * beta)
  
  list(lp = rowSums(effects) + beta0, effects = effects)
}

dgp_categorical <- function(x, beta, n_levels, level_beta) {
  p <- ncol(x)
  beta <- rep(beta, each = n_levels) * level_beta
  effects <- t(t(encode_onehot(x, n_levels)) * beta)
  dim(effects) <- c(nrow(effects), n_levels, p)
  effects <- apply(effects, c(1,3), sum)
  
  list(lp = rowSums(effects), effects = effects)
}

################################################################################
#                           Sample functions
################################################################################
get_sample_fun <- function(type, p, mean = rep(0, p), sigma = diag(p),
                           n_levels = 2, level_probs = rep(1 / n_levels, n_levels)) {
  switch (as.character(type),
          normal = function(n) sample_normal(n, p = p, mean = mean, sigma = sigma),
          mixednormal = function(n) sample_mixednormal(n, p = p, mean = mean, sigma  = sigma),
          uniform = function(n) sample_uniform(n, p = p, mean = mean),
          weibull = function(n) sample_weibull(n, p = p, mean = mean),
          exponential = function(n) sample_exp(n, p = p, mean = mean),
          categorical = function(n) sample_categorical(n, p = p, n_levels = n_levels,
                                                       level_probs = level_probs),
          stop("Unknown sample function: '", type, "'")
  )
}

sample_normal <- function(n, p, mean, sigma) {
  rmvnorm(n, mean = mean, sigma = sigma)
}

sample_mixednormal <- function(n, p, mean, sigma) {
  n_half <- n %/% 2
  res <- rbind(
    rmvnorm(n_half, mean = mean - 1.5, sigma = 0.5 * sigma),
    rmvnorm(n - n_half, mean = mean + 1.5, sigma = 0.5 * sigma)
  )
  
  res[sample(nrow(res)), ]
}

sample_uniform <- function(n, p, mean) {
  matrix(runif(n * p, min = -1, max = 1) + mean, nrow = n, byrow = TRUE)
}

sample_exp <- function(n, p, mean) {
  matrix(rexp(n * p) + mean - 1, nrow = n, byrow = TRUE)
}

sample_weibull <- function(n, p, mean) {
  matrix(rweibull(n * p, shape = 1.5) + mean - gamma(1 + 1 / 1.5), 
         nrow = n, byrow = TRUE)
}

sample_categorical <- function(n, p, n_levels, level_probs) {
  values <- sample(LETTERS[1:n_levels], n * p, 
                   replace = TRUE, 
                   prob = level_probs)
  matrix(values, nrow = n, byrow = TRUE)
}

################################################################################
#                         Preprocess functions
################################################################################

get_preprocess_fun <- function(type, scale_args = NULL, n_levels = NULL) {
  switch (as.character(type),
          scale_minmax = function(x) scale_min_max(x, scale_args = scale_args),
          scale_robust_minmax = function(x) scale_robust_min_max(x, scale_args = scale_args),
          scale_maxabs = function(x) scale_max_abs(x, scale_args = scale_args),
          scale_zscore = function(x) scale_zscore(x, scale_args = scale_args),
          scale_normalize = function(x) scale_normalize(x, scale_args = scale_args),
          scale_none = function(x) x,
          encode_onehot = function(x) encode_onehot(x, n_levels = n_levels),
          encode_dummy = function(x) encode_dummy(x, n_levels = n_levels),
          encode_effect = function(x) encode_effect(x, n_levels = n_levels),
          encode_label = function(x) encode_label(x, n_levels = n_levels),
          encode_binary = function(x) encode_binary(x, n_levels = n_levels),
          stop("Unknown preprocess function: '", type, "'")
  )
}

# Numeric scaling functions ----------------------------------------------------
get_scale_args <- function(x) {
  list(
    min = apply(x, 2, min),
    max = apply(x, 2, max),
    max_abs = apply(x, 2, function(x) max(abs(x))),
    mean = colMeans(x),
    median = apply(x, 2, median),
    iqr = apply(x, 2, stats::IQR),
    sd = apply(x, 2, sd)
  )
}

scale_min_max <- function(x, scale_args) {
  t((t(x) - scale_args$min) / (scale_args$max - scale_args$min))
}

scale_robust_min_max <- function(x, scale_args) {
  t((t(x) - scale_args$min) / scale_args$iqr)
}

scale_max_abs <- function(x, scale_args) {
  t(t(x) / scale_args$max_abs)
}

scale_zscore <- function(x, scale_args) {
  t((t(x) - scale_args$mean) / scale_args$sd)
}

scale_normalize <- function(x, scale_args) {
  t((t(x) - scale_args$mean) / (scale_args$max - scale_args$min))
}

# Categorical encoding functions -----------------------------------------------
encode_onehot <- function(x, n_levels) {
  fun <- function(a, n_levels) {
    a <- factor(a, levels = LETTERS[1:n_levels])
    a <- as.numeric(a) - 1
    res <- rep(0, n_levels)
    res[a + 1] <- 1
    
    res
  }
  
  res <- t(apply(x, c(1, 2), fun, n_levels = n_levels, simplify = FALSE))
  matrix(unlist(res), nrow = nrow(x), byrow = TRUE)
}

encode_dummy <- function(x, n_levels) {
  fun <- function(a, n_levels) {
    a <- factor(a, levels = LETTERS[1:n_levels])
    a <- as.numeric(a) - 1
    res <- rep(0, n_levels - 1)
    if (a != n_levels - 1) res[a + 1] <- 1
    
    res
  }
  
  res <- t(apply(x, c(1, 2), fun, n_levels = n_levels, simplify = FALSE))
  matrix(unlist(res), nrow = nrow(x), byrow = TRUE)
}

encode_effect <- function(x, n_levels) {
  fun <- function(a, n_levels) {
    a <- factor(a, levels = LETTERS[1:n_levels])
    a <- as.numeric(a) - 1
    res <- rep(0, n_levels - 1)
    if (a != n_levels - 1) {
      res[a + 1] <- 1
    } else {
      res <- res - 1
    }
    
    res
  }
  
  res <- t(apply(x, c(1, 2), fun, n_levels = n_levels, simplify = FALSE))
  matrix(unlist(res), nrow = nrow(x), byrow = TRUE)
}

encode_label <- function(x, n_levels) {
  fun <- function(a, n_levels) {
    a <- factor(a, levels = LETTERS[1:n_levels])
    as.numeric(a) - 1
  }
  
  res <- t(apply(x, c(1, 2), fun, n_levels = n_levels, simplify = FALSE))
  matrix(unlist(res), nrow = nrow(x), byrow = TRUE)
}

encode_binary <- function(x, n_levels) {
  fun <- function(a, n_levels) {
    a <- factor(a, levels = LETTERS[1:n_levels])
    a <- as.numeric(a) - 1
    bits <- ceiling(log(n_levels) / log(2))
    
    bitI <- function(x) lapply(x, function(x) {
      b <- as.numeric(substr(as.character(rev(intToBits(x))), 2L, 2L))
      b[((length(b) + 1 - bits):length(b))]
    })
    
    do.call("rbind", args = bitI(a))
  }
  
  res <- t(apply(x, c(1, 2), fun, n_levels = n_levels, simplify = FALSE))
  matrix(unlist(res), nrow = nrow(x), byrow = TRUE)
}
