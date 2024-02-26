################################################################################
#                             Utility functions
################################################################################

# Create torch dataset ---------------------------------------------------------
get_torch_dataset <- dataset(
  name = "torch_dataset",
  
  initialize = function(df) {
    x <- as.matrix(df$x)
    if (is.factor(df$y)) {
      y <- as.numeric(as.numeric(df$y) - 1)
    } else {
      y <- as.numeric(df$y)
    }
    self$df <- as.matrix(data.frame(y,x))
  },
  
  .getitem = function(i) {
    list(x = torch_tensor(self$df[i, -1]),  
         y = torch_tensor(self$df[i, 1]))
  },
  
  .length = function() {
    nrow(self$df)
  }
)

# Model training ---------------------------------------------------------------
train_model <- function(p, n_units, n_layers, dataset, outcome_type, act.fct = "relu",
                        n_cpus = 1, n_workers = 1) {
  # Set seed for reproducibility
  #set.seed(42)
  torch_manual_seed(42)
  tryCatch(torch_set_num_interop_threads(n_cpus), error = function(c) NULL)
  torch_set_num_threads(n_cpus)
  
  
  # Train a dense model on this dataset
  layers <- get_dense_layers(p, n_units, n_layers, act.fct)
  
  # Adjust bias vector of last layer
  if (outcome_type == "regression") {
    layers[[length(layers)]]$bias <- layers[[length(layers)]]$bias * 0 +
      median(dataset$train$y)
  }
  
  # Create torch dataloaders
  cli_progress_step("Creating data loaders...", "Crating data loaders!")
  train_dl <- get_torch_dataset(dataset$train) %>%
    dataloader(batch_size = min(256, length(dataset$train$y)), shuffle = TRUE,
               num_workers = n_workers)
  valid_dl <- get_torch_dataset(dataset$valid) %>%
    dataloader(batch_size = min(256, length(dataset$valid$y)), shuffle = FALSE,
               num_workers = n_workers)
  test_dl <- get_torch_dataset(dataset$test) %>%
    dataloader(batch_size = min(256, length(dataset$test$y)), shuffle = FALSE,
               num_workers = n_workers)
  
  mode <- if (outcome_type == "regression") "min" else "max"
  
  cli_progress_step("Fitting model...", "Fitting model!")
  fitted_model <- get_model %>%
    setup(
      loss = if (outcome_type == "regression") nn_mse_loss() else nn_bce_loss(),
      optimizer = optim_adam,
      metrics = if (outcome_type == "regression") list(luz_metric_mse()) else list(luz_metric_binary_accuracy())
    ) %>%
    set_hparams(layers = layers, outcome_type = outcome_type) %>%
    set_opt_hparams(lr = 0.01) %>%
    fit(train_dl,
        epochs = 200,
        valid_data = valid_dl,
        verbose = FALSE,
        callbacks = c(
          luz_callback_keep_best_model(
            monitor = "valid_loss",
            mode = mode),
          luz_callback_early_stopping(
            monitor = "valid_loss",
            patience = 10,
            mode = mode),
          luz_callback_lr_scheduler(
            torch::lr_step, 
            step_size = 30,
            gamma = 0.2))
    )
  
  
  metrics <- get_metrics(fitted_model)
  metrics <- metrics[metrics$epoch == max(metrics$epoch), ]
  cli_text("Model trained for {metrics$epoch[1]} epochs.")
  cli_text("Train loss: {signif(metrics$value[1])}   Train metric: {signif(metrics$value[2])}")
  cli_text("Valid loss: {signif(metrics$value[3])}   Valid metric: {signif(metrics$value[4])}")
  
  # Evaluate model
  error <- fitted_model %>%
    luz::evaluate(data = test_dl) %>%
    get_metrics()
  
  error <- as.numeric(error[2,2])
  cli_text("Test loss: {signif(error)}")
  
  if (outcome_type == "regression") {
    # Calculate R²
    preds <- fitted_model %>% predict(newdata = test_dl) %>% 
      torch_squeeze() %>% as.numeric()
    y <- test_dl$dataset$df[, "y"]
    r_squared <- 1 - mean((preds - y)**2) / mean((y - mean(y))**2)
    cli_text("R² value: {signif(r_squared)}")
  } else if (outcome_type == "classification") {
    preds <- fitted_model %>% predict(newdata = test_dl) %>% 
      torch_squeeze() %>% as.numeric()
    preds <- as.factor(ifelse(preds < 0.5, 0, 1))
    y <- as.factor(test_dl$dataset$df[, "y"])
    r_squared <- confusionMatrix(preds, reference = y)$byClass[["F1"]]
  }
  
  list(model = fitted_model$model,
       error = error,
       r_squared_test = r_squared,
       r_squared_true = 1 - 1 / var(train_dl$dataset$df[, "y"]),
       num_inputs = p,
       outcome_type = outcome_type,
       dataset = dataset)
}

# Defining model ---------------------------------------------------------------
get_model <- nn_module(
  "Model_wrapper",
  initialize = function(layers, outcome_type) {
    if (outcome_type == "classification") {
      layers <- append(layers, list(nn_sigmoid()))
    }
    self$model <- do.call(nn_sequential, layers)
  },
  
  forward = function(x) {
    self$model(x)
  }
)

get_act <- function(act.fct) {
  if (act.fct == "relu") {
    res <- nn_relu()
  } else if (act.fct == "tanh") {
    res <- nn_tanh()
  } else if (act.fct == "elu") {
    res <- nn_elu()
  } else if (act.fct == "softplus") {
    res <- nn_softplus()
  }
  
  res
}

get_dense_layers <- function(p, n_units, n_layers, act.fct = "relu") {
  
  # Create layers
  if (n_layers == 1) {
    layer_units <- c(p, 1)
  } else {
    layer_units <- n_units / 2^(0:(n_layers - 2))
    layer_units[layer_units < 32] <- 32
    layer_units[-1] <- pmin(256, layer_units[-1])
    layer_units <- c(p, layer_units, 1)
  }
  
  cli_bullets(c(
    "*" = paste0("Neural Network architecture: ", paste0(layer_units, collapse = " -> "))
  ))
  
  layers <- lapply(seq_len(length(layer_units) - 1), function(i) {
    list(
      nn_linear(layer_units[i], layer_units[i + 1]),
      if (i + 1 != length(layer_units)) get_act(act.fct),
      if (i + 1 != length(layer_units)) nn_dropout(p = 0.4)
    )
  })
  
  unlist(layers)
}


binary_enc <- function(a, num_levels) {
  bits <- floor(log(num_levels) / log(2))
  powers <- 2^(bits:0)
  bitwAnd(a,powers)/powers
}

get_encoding_fun <- function(encode_type, num_levels) {
  encode_FUN <- switch(as.character(encode_type),
                       one_hot = one_hot_enc,
                       dummy = dummy_enc,
                       effect = effect_enc,
                       label = label_enc, 
                       binary = binary_enc)
  
  function(x) {
    res <- t(apply(x, c(1, 2), encode_FUN, num_levels = num_levels, simplify = FALSE))
    matrix(unlist(res), nrow = nrow(x), byrow = TRUE)
  }
}

# Other utility functions ------------------------------------------------------
combine_features <- function(res_1, cat_feat, FUN = identity) {
  if (!is.matrix(res_1)) res_1 <- matrix(res_1, nrow = 1)
  res <- lapply(seq_len(max(cat_feat)), 
                function(i) rowSums(FUN(res_1[, which(cat_feat == i), drop = FALSE])))
  do.call("cbind", args = res)
}