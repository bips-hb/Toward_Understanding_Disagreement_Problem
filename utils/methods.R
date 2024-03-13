################################################################################
#                           Method wrapper
################################################################################

# Vanilla Gradient -------------------------------------------------------------
gradient_wrapper <- function(instance, times_input, saliency = FALSE, ignore_last_act = TRUE) {
  
  library(innsight)
  start_time <- Sys.time()
  
  # Convert model
  invisible(capture.output({
    converter <- convert(instance$model$model, instance$num_inputs)
  }, type = "message"))
  
  # Apply method
  result <- run_grad(converter, instance$data$test$x, times_input = times_input,
                     verbose = FALSE, ignore_last_act = ignore_last_act)
  
  # Get results
  result <- get_result(result)
  dim(result) <- dim(result)[-length(dim(result))]
  if (saliency) {
    result <- (result)**2
  }
  
  if (times_input) {
    method_name <- "GxI"
  } else {
    method_name <- "Grad"
  }
  if (saliency) {
    method_name <- "Saliency"
  }
  
  gc()
  
  list(result = result, 
       hyperp = data.frame(
          method_name = method_name,
          method_grp = "Gradient-based",
          time = as.numeric(Sys.time() - start_time, units = "secs")
        )
  )
}

# SmoothGrad -------------------------------------------------------------------
smoothgrad_wrapper <- function(instance, times_input, K, noise_level, ignore_last_act = TRUE) {
  
  library(innsight)
  start_time <- Sys.time()
  
  # Convert model
  invisible(capture.output({
    converter <- convert(instance$model$model, instance$num_inputs)
  }, type = "message"))
  
  # Apply method
  result <- run_smoothgrad(converter, instance$data$test$x,
                           times_input = times_input,
                           n = K,
                           noise_level = noise_level,
                           verbose = FALSE,
                           ignore_last_act = ignore_last_act)
  
  # Get results
  result <- get_result(result)
  dim(result) <- dim(result)[-length(dim(result))]
  
  if (times_input) {
    method_name <- "SGxI"
  } else {
    method_name <- "SG"
  }
  
  gc()
  
  list(result = result,
       hyperp = data.frame(
          method_name = method_name,
          method_grp = "Gradient-based",
          time = as.numeric(Sys.time() - start_time, units = "secs")
        )
  )
}

# IntegratedGradient -----------------------------------------------------------
intgrad_wrapper <- function(instance, n, x_ref, ignore_last_act = TRUE) {
  
  library(innsight)
  x_ref_name <- x_ref
  
  if (x_ref == "zeros") {
    x_ref <- NULL
  } else if (x_ref == "mean") {
    x_ref <- matrix(colMeans(instance$data$test$x), nrow = 1)
  } else {
    stop("Unknown value of 'x_ref'!")
  }
  
  start_time <- Sys.time()
  
  # Convert model
  invisible(capture.output({
    converter <- convert(instance$model$model, instance$num_inputs)
  }, type = "message"))
  
  # Apply method
  result <- run_intgrad(converter, instance$data$test$x,
                        x_ref = x_ref, n = n, verbose = FALSE, 
                        ignore_last_act = ignore_last_act)
  
  # Get results
  result <- get_result(result)
  dim(result) <- dim(result)[-length(dim(result))]
  
  gc()
  
  list(result = result,
       hyperp = data.frame(
          method_name = paste0("IntGrad (", x_ref_name, ")"),
          method_grp = "Gradient-based",
          time = as.numeric(Sys.time() - start_time, units = "secs")
        )
  )
}

# ExpectedGradient -------------------------------------------------------------
expgrad_wrapper <- function(instance, n, ignore_last_act = TRUE) {
  
  library(innsight)
  data_ref <- instance$data$test$x
  start_time <- Sys.time()
  
  # Convert model
  invisible(capture.output({
    converter <- convert(instance$model$model, instance$num_inputs)
  }, type = "message"))
  
  # Apply method
  result <- run_expgrad(converter, instance$data$test$x,
                        data_ref = data_ref, n = n, verbose = FALSE, 
                        ignore_last_act = ignore_last_act)
  
  # Get results
  result <- get_result(result)
  dim(result) <- dim(result)[-length(dim(result))]
  
  gc()
  
  list(result = result,
       hyperp = data.frame(
          method_name = "ExpGrad",
          method_grp = "Gradient-based",
          time = as.numeric(Sys.time() - start_time, units = "secs")
        )
  )
}

# LRP --------------------------------------------------------------------------
lrp_wrapper <- function(instance, rule_name, rule_param, ignore_last_act = TRUE) {
  
  library(innsight)
  start_time <- Sys.time()
  
  # Convert model
  invisible(capture.output({
    converter <- convert(instance$model$model, instance$num_inputs)
  }, type = "message"))
  
  # Apply method
  result <- run_lrp(converter, instance$data$test$x, rule_name = rule_name,
                    rule_param = rule_param, verbose = FALSE, 
                    ignore_last_act = ignore_last_act)
  
  # Get results
  result <- get_result(result)
  dim(result) <- dim(result)[-length(dim(result))]
  
  if (rule_name == "simple") {
    method_name <- paste0("LRP-0")
  } else if (rule_name == "epsilon") {
    method_name <- paste0("LRP-ε (", rule_param, ")")
  } else if (rule_name == "alpha_beta") {
    method_name <- paste0("LRP-αβ (", rule_param, ")")
  } else {
    stop("Unknown LRP rule!")
  }
  
  gc()
  
  list(result = result,
       hyperp = data.frame(
          method_name = method_name,
          method_grp = "LRP",
          time = as.numeric(Sys.time() - start_time, units = "secs")
        )
  )
}

# DeepLift ---------------------------------------------------------------------
deeplift_wrapper <- function(instance, rule_name, x_ref, ignore_last_act = TRUE) {
  
  library(innsight)
  x_ref_name <- x_ref
  
  if (x_ref == "zeros") {
    x_ref <- NULL
  } else if (x_ref == "mean") {
    x_ref <- matrix(colMeans(instance$data$test$x), nrow = 1)
  } else {
    stop("Unknown value of 'x_ref'!")
  }
  
  start_time <- Sys.time()
  
  # Convert model
  invisible(capture.output({
    converter <- convert(instance$model$model, instance$num_inputs)
  }, type = "message"))
  
  # Apply method
  result <- run_deeplift(converter, instance$data$test$x,
                         rule_name = as.character(rule_name),
                         x_ref = x_ref, verbose = FALSE, 
                         ignore_last_act = ignore_last_act)
  
  # Get results
  result <- get_result(result)
  dim(result) <- dim(result)[-length(dim(result))]
  
  if (rule_name == "rescale") {
    method_name <- paste0("DeepLift-RE (", x_ref_name, ")")
  } else if (rule_name == "reveal_cancel") {
    method_name <- paste0("DeepLift-RC (", x_ref_name, ")")
  } else {
    stop("Unknown DeepLift rule!")
  }
  
  gc()
  
  list(result = result,
       hyperp = data.frame(
          method_name = method_name,
          method_grp = "DeepLift",
          time = as.numeric(Sys.time() - start_time, units = "secs")
        )
  )
}

# DeepSHAP ---------------------------------------------------------------------
deepshap_wrapper <- function(instance, rule_name, ignore_last_act = TRUE) {
  
  library(innsight)
  data_ref <- instance$data$test$x
  start_time <- Sys.time()
  
  # Convert model
  invisible(capture.output({
    converter <- convert(instance$model$model, instance$num_inputs)
  }, type = "message"))
  
  # Apply method
  result <- run_deepshap(converter, instance$data$test$x,
                         rule_name = as.character(rule_name),
                         data_ref = data_ref, 
                         limit_ref = 50, verbose = FALSE, 
                         ignore_last_act = ignore_last_act)
  
  # Get results
  result <- get_result(result)
  dim(result) <- dim(result)[-length(dim(result))]
  
  if (rule_name == "rescale") {
    method_name <- paste0("DeepSHAP-RE")
  } else if (rule_name == "reveal_cancel") {
    method_name <- paste0("DeepSHAP-RC")
  } else {
    stop("Unknown DeepSHAP rule!")
  }
  
  gc()
  
  list(result = result,
       hyperp = data.frame(
          method_name = method_name,
          method_grp = "DeepLift",
          time = as.numeric(Sys.time() - start_time, units = "secs")
        )
  )
}


# Shapley Values --------------------------------------------------------------
shap_wrapper <- function(instance, nsim = 20, ignore_last_act = TRUE) {
  library(innsight)
  data_ref <- instance$data$test$x
  start_time <- Sys.time()
  
  # Apply method
  result <- run_shap(instance$model$model, instance$data$test$x,
                     data_ref = data_ref, nsim = nsim)
  # Get results
  result <- get_result(result)
  dim(result) <- dim(result)[-length(dim(result))]
  
  gc()
  
  list(result = result,
       hyperp = data.frame(
          method_name = "SHAP",
          method_grp = "Agnostic",
          time = as.numeric(Sys.time() - start_time, units = "secs")
        )
  )
}

# LIME ------------------------------------------------------------------------
lime_wrapper <- function(instance, ignore_last_act = TRUE, nperm = 200) {
  library(innsight)
  data_ref <- instance$data$test$x
  start_time <- Sys.time()
  
  # Apply method
  result <- run_lime(instance$model$model, instance$data$test$x,
                     data_ref = data_ref,
                     n_permutations = nperm)
  # Get results
  result <- get_result(result)
  dim(result) <- dim(result)[-length(dim(result))]
  
  gc()
  
  list(result = result,
       hyperp = data.frame(
          method_name = "LIME",
          method_grp = "Agnostic",
          time = as.numeric(Sys.time() - start_time, units = "secs")
        )
  )
}