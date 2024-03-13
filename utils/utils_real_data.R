################################################################################
#                         Utility functions for
#                             Real Datasets
################################################################################

# Utils for real datasets ------------------------------------------------------
get_bike_sharing_ds <- function(scale_type = "scale_zscore") {
  data <- read.csv(here("utils/datasets/bike_sharing.csv"))
  data <- data[, c(-1, -2, -4, -5, -11, -14, -15)]
  
  # Get the preprocess function
  scale_fun <- get_scale_fun(scale_type)
  
  # Scale numeric variables
  data$temp <- scale_fun(data$temp)
  data$hum <- scale_fun(data$hum)
  data$windspeed <- scale_fun(data$windspeed)
  #data$cnt <- data$cnt #scale_fun(data$cnt)
  
  # Set binary and categorical variables
  data$season <- as.factor(as.character(data$season))
  data$holiday <- as.factor(as.character(data$holiday))
  data$weekday <- as.factor(as.character(data$weekday))
  data$workingday <- as.factor(as.character(data$workingday))
  data$weathersit <- as.factor(as.character(data$weathersit))
  
  # Prepare and return data
  prepare_real_data(data, split = 0.2, y = 9, binary = c(2, 4), 
                    categorical = c(1,3 ,5), outcome_type = "regression")
}

get_boston_housing_ds <- function(scale_type = "scale_zscore") {
  library(mlbench)
  data("BostonHousing")
  data <- BostonHousing
  
  # Get scale function
  scale_fun <- get_scale_fun(scale_type)
  
  # Scale continuous variables
  data$crim <- scale_fun(data$crim)
  data$zn <- scale_fun(data$zn)
  data$indus <- scale_fun(data$indus)
  data$nox <- scale_fun(data$nox)
  data$rm <- scale_fun(data$rm)
  data$age <- scale_fun(data$age)
  data$dis <- scale_fun(data$dis)
  data$rad <- scale_fun(data$rad)
  data$tax <- scale_fun(data$tax)
  data$ptratio <- scale_fun(data$ptratio)
  data$b <- scale_fun(data$b)
  data$lstat <- scale_fun(data$lstat)
  #data$medv <- scale_fun(data$medv)
  
  # Set binary and categorical variables
  data$chas <- as.factor(as.character(data$chas))
  
  # Prepare and return dataset
  prepare_real_data(data, split = 0.2, y = 14, binary = c(4), 
                    categorical = NULL, outcome_type = "regression")
}

get_compas_ds <- function(scale_type = "scale_zscore") {
  library(mlr3fairness)
  data("compas", package = "mlr3fairness")
  data <- compas
  
  # Get scale function
  scale_fun <- get_scale_fun(scale_type)
  
  # Scale numerical variables
  data$age <- scale_fun(data$age)
  data$priors_count <- scale_fun(data$priors_count)
  data$days_b_screening_arrest <- scale_fun(data$days_b_screening_arrest)
  data$decile_score  <- scale_fun(data$decile_score)
  data$length_of_stay  <- scale_fun(data$length_of_stay)
  
  # Set factors for binary and categorical variables
  data$c_charge_degree <- as.factor(data$c_charge_degree)
  data$race <- as.factor(data$race)
  data$age_cat <- as.factor(data$age_cat)
  data$score_text <- as.factor(data$score_text)
  data$sex <- as.factor(data$sex)
  data$is_recid <- NULL
  data$two_year_recid <- as.factor(data$two_year_recid)
  
  prepare_real_data(data.frame(data), split = 0.2, y = 10, binary = c(2, 6), 
                    categorical = c(3, 4, 5), outcome_type = "classification")
}


# Dataset preparation ----------------------------------------------------------
prepare_real_data <- function(data, split, y, binary = NULL, categorical = NULL,
                              outcome_type = "regression") {
  # Order columns (outcome, numerical, binary, categorical)
  data <- cbind(data[, y, drop = FALSE], data[, -c(y, binary, categorical)], 
                data[, binary, drop = FALSE], data[, categorical, drop = FALSE])
  # Shuffle data
  data <- data[sample.int(nrow(data)), ]
  feat_names <- colnames(data)[-1]
  
  # Encode binary and categorical variables
  if (!is.null(binary)) {
    binary <- seq_along(binary) + ncol(data) - length(categorical) - length(binary)
    for (i in binary){
      data[, i] <- as.numeric(data[, i]) - 1
    }
  }
  cat_data <- NULL
  if (!is.null(categorical)) {
    # function for one-hot encoding
    encode_oh <- function(x, n_levels) {
      fun <- function(a, n_levels) {
        a <- as.numeric(a) - 1
        res <- rep(0, n_levels)
        res[a + 1] <- 1
        
        res
      }
      
      res <- t(apply(x, c(1, 2), fun, n_levels = n_levels, simplify = FALSE))
      matrix(unlist(res), nrow = nrow(x), byrow = TRUE)
    }
    
    categorical <- seq_along(categorical) + ncol(data) - length(categorical)
    cat_vars <- list()
    for (i in categorical) {
      encode_fun <- function(x) encode_oh(x, length(unique(data[, i])))
      cat_vars[[i]] <- encode_fun(matrix(as.numeric(data[, i]), ncol = 1) - 1)
    }
    cat_data <-  do.call("cbind", args = cat_vars)
    data <- data[, -c(categorical)]
  } else {
    categorical <- ncol(data) + 1
    cat_vars <- list()
  }
  
  if (outcome_type == "classification") {
    data <- data.frame(
      y = as.factor(as.numeric(as.factor(data[, 1])) - 1), 
      as.matrix(data[, -1]))
  } else {
    data <- data.frame(as.matrix(data))
  }
  
  # Train-Val-Test split
  test_idx <- seq(from = 0, to = as.integer(nrow(data) * split))
  val_idx <- seq(from = ceiling(nrow(data) * split), 
                 to = as.integer(nrow(data) * (split + 0.15)))
  train_idx <- seq(from = ceiling(nrow(data) * (split + 0.15)), 
                   to = nrow(data))
  
  # Add categorical data
  data <- if (!is.null(cat_data)) cbind(data, cat_data) else data
  rownames(data) <- NULL
  times <- unlist(lapply(cat_vars, ncol))
  times <- if (is.null(times)) 0 else times
  
  list(
    train = list(x = data[train_idx, -1], y = data[train_idx, 1]),
    valid = list(x = data[val_idx, -1], y = data[val_idx, 1]),
    test = list(x = data[test_idx, -1], y = data[test_idx, 1]),
    cor_groups = c(seq_len(min(categorical) - 2), rep(categorical - 1, times)),
    outcome_type = outcome_type,
    feat_names = feat_names
  )
}


# Utility functions ------------------------------------------------------------
get_scale_fun <- function(scale_type) {
  if (as.character(scale_type == "scale_zscore")) {
    scale_fun <- function(x) (x - mean(x)) / sd(x)
  } else if (as.character(scale_type) == "scale_none") {
    scale_fun <- identity
  } else if (as.character(scale_type) == "scale_maxabs") {
    scale_fun <- function(x) x / max(abs(x))
  } else {
    stop("Unknown scale type: '", scale_type, "'!")
  }
  
  scale_fun
}
