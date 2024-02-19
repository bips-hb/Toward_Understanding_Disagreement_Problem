################################################################################
#                         Get the dataset
################################################################################

get_dataset <- function(sample_fun, dgp_fun, n, n_test, preprocess_type = "scale_none",
                        n_levels = NULL) {
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
  
  # Create train data
  train <- list(
    x = preprocess_fun(train_data),
    y = dgp_fun(train_data)$lp + rnorm(n)
  )
  
  # Create validation data
  valid <- list(
    x = preprocess_fun(val_data), 
    y = dgp_fun(val_data)$lp + rnorm(as.integer(0.3 * n))
  )
  
  # Create test data
  test <- list(
    x = preprocess_fun(test_data),
    y = dgp_fun(test_data)$lp + rnorm(n_test)
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

get_dgp_fun <- function(type, beta, beta0 = 0, n_levels = NULL,
                        level_beta = NULL, sample_type = NULL, mean = NULL) {
  switch (as.character(type),
    linear = function(x) dgp_linear(x, beta = beta, beta0 = beta0),
    squared = function(x) dgp_squared(x, beta = beta, beta0 = beta0, mean = mean,
                                      sample_type = sample_type),
    exp = function(x) dgp_exp(x, beta = beta, beta0 = beta0),
    cos = function(x) dgp_cos(x, beta = beta, beta0 = beta0, mean = mean, 
                              sample_type = sample_type),
    nonlinear = function(x) dgp_nonlinear(x, beta = beta, beta0 = beta0, 
                                          sample_type = sample_type,
                                          mean = mean),
    categorical = function(x) dgp_categorical(x, beta = beta, n_levels = n_levels,
                                              level_beta = level_beta),
    stop("Unknown data generating process: '", type, "'")
  )
}

dgp_linear <- function(x, beta, beta0) {
  effects <- t(t(x) * beta)
  list(lp = rowSums(effects) + beta0, effects = effects)
}

dgp_squared <- function(x, beta, beta0, mean, sample_type) {
  scale <- switch (as.character(sample_type),
    uniform = (1.5/ qunif(0.98, min = -1, max = 1))**2,
    normal = (1.5/qnorm(0.98))**2
  )
  effects <- t(((t(x) - mean)**2 * scale  - 1) * beta)
  list(lp = rowSums(effects) + beta0, effects = effects)
}

dgp_exp <- function(x, beta, beta0) {
  effects <- t(t(exp(x)) * beta)
  list(lp = rowSums(effects) + beta0, effects = effects)
}

cos_trunc <- function(x, trunc_limit = 1.5) {
  ifelse(abs(x) < trunc_limit, 
         cos(x * pi * 1.5 * (1/trunc_limit))* 1.25, 0)
}

dgp_cos <- function(x, beta, beta0, mean, sample_type) {
  trunc_limit <- switch (as.character(sample_type),
    uniform = qunif(0.98, -1, 1),
    normal = qnorm(0.98)
  )
  
  effects <- t(cos_trunc(t(x) - mean, trunc_limit) * beta)
  list(lp = rowSums(effects) + beta0, effects = effects)
}

dgp_nonlinear <- function(x, beta, beta0, sample_type, mean) {
  x_centered <- t(t(x) - mean)
  
  effects <- switch (as.character(sample_type),
    normal = ifelse(abs(x_centered) < qnorm(0.75), 1, -1),
    uniform = ifelse(abs(x_centered) < 0.5, 1, -1),
    weibull = ifelse(abs(x_centered) < qweibull(0.25, shape = 1.5) |
                       abs(x_centered) > qweibull(0.75, shape = 1.5), 1, -1)
  )
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
                           shape = 1.5, n_levels = 2, 
                           level_probs = rep(1 / n_levels, n_levels)) {
  switch (as.character(type),
    normal = function(n) sample_normal(n, p = p, mean = mean, sigma = sigma),
    uniform = function(n) sample_uniform(n, p = p, mean = mean),
    weibull = function(n) sample_weibull(n, p = p, mean = mean, shape = shape),
    categorical = function(n) sample_categorical(n, p = p, n_levels = n_levels,
                                                 level_probs = level_probs),
    stop("Unknown sample function: '", type, "'")
  )
}

sample_normal <- function(n, p, mean, sigma) {
  rmvnorm(n, mean = mean, sigma = sigma)
}

sample_uniform <- function(n, p, mean) {
  matrix(runif(n * p, min = -1, max = 1) + mean, nrow = n, byrow = TRUE)
}

sample_weibull <- function(n, p, mean, shape) {
  matrix(rweibull(n * p, shape = shape) + mean, nrow = n, byrow = TRUE)
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
    scale_maxabs = function(x) scale_max_abs(x, scale_args = scale_args),
    scale_zscore = function(x) scale_zscore(x, scale_args = scale_args),
    scale_normalize = function(x) scale_normalize(x, scale_args = scale_args),
    scale_none = function(x) x,
    encode_onehot = function(x) encode_onehot(x, n_levels = n_levels),
    encode_dummy = function(x) encode_dummy(x, n_levels = n_levels),
    encode_effect = function(x) encode_effect(x, n_levels = n_levels),
    encode_label = function(x) encode_label(x, n_levels = n_levels),
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
    sd = apply(x, 2, sd)
  )
}

scale_min_max <- function(x, scale_args) {
  t((t(x) - scale_args$min) / (scale_args$max - scale_args$min))
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
