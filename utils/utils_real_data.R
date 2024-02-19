################################################################################
#                         Utility functions for
#                             Real Datasets
################################################################################

# Utils for real datasets ------------------------------------------------------
get_bike_sharing_ds <- function() {
  data <- read.csv(here("utils/datasets/bike_sharing.csv"))
  data <- data[, c(-1, -2, -4, -5, -11, -14, -15)]
  data$season <- as.factor(as.character(data$season))
  data$cnt <- data$cnt / 1000
  data$holiday <- as.factor(as.character(data$holiday))
  data$weekday <- as.factor(as.character(data$weekday))
  data$workingday <- as.factor(as.character(data$workingday))
  data$weathersit <- as.factor(as.character(data$weathersit))
  
  prepare_real_data(data, split = 0.2, y = 9, binary = c(2, 4), 
                    categorical = c(1,3 ,5))
}

get_bike_sharing_ds <- function() {
  data <- read.csv("utils/datasets/bike_sharing.csv")
  data <- data[, c(-1, -2, -4, -5, -11, -14, -15)]
  data$season <- as.factor(as.character(data$season))
  #data$temp <- 47 * data$temp - 8
  data$cnt <- data$cnt / 1000
  data$holiday <- as.factor(as.character(data$holiday))
  data$weekday <- as.factor(as.character(data$weekday))
  data$workingday <- as.factor(as.character(data$workingday))
  data$weathersit <- as.factor(as.character(data$weathersit))
  
  prepare_real_data(data, split = 0.2, y = 9, binary = c(2, 4), 
                    categorical = c(1,3 ,5))
}

get_boston_housing_ds <- function() {
  library(mlbench)
  data("BostonHousing")
  
  data <- BostonHousing
  prepare_real_data(data, split = 0.2, y = 14, binary = c(4), 
                    categorical = NULL)
}

prepare_real_data <- function(data, split, y, binary = NULL, categorical = NULL) {
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
  
  data <- as.matrix(data)
  
  # Train-Val-Test split
  test_idx <- seq(from = 0, to = as.integer(nrow(data) * split))
  val_idx <- seq(from = ceiling(nrow(data) * split), 
                 to = as.integer(nrow(data) * (split + 0.15)))
  train_idx <- seq(from = ceiling(nrow(data) * (split + 0.15)), 
                   to = nrow(data))
  
  # Add categorical data
  data <- cbind(data, cat_data)
  rownames(data) <- NULL
  times <- unlist(lapply(cat_vars, ncol))
  times <- if (is.null(times)) 0 else times
  
  list(
    train = list(x = data[train_idx, -1], y = data[train_idx, 1]),
    valid = list(x = data[val_idx, -1], y = data[val_idx, 1]),
    test = list(x = data[test_idx, -1], y = data[test_idx, 1]),
    cor_groups = c(seq_len(min(categorical) - 2), rep(categorical - 1, times)),
    feat_names = feat_names
  )
}

