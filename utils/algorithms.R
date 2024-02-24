################################################################################
#                               Algorithms
################################################################################
apply_methods <- function(data, job, instance, compare_type = "cor", method_df = NULL) {
  source(here("utils/methods.R"))
  
  cli_progress_step("Running feature attribution methods...", 
                    "Running feature attribution methods done!")
  
  res_list <- lapply(names(method_df), run_method, instance = instance,
                     method_args = method_df)
  res_list <- unlist(res_list, recursive = FALSE)
  
  if (compare_type == "attributions") {
    res <- lapply(res_list, compare_raw, instance = instance)
    res <- do.call("rbind", args = res)
  } else if (compare_type == "correlation") {
    res <- lapply(res_list, compare_corelation, instance = instance)
    res <- do.call("rbind", args = res)
  } else if (compare_type == "uninformative") {
    res <- lapply(res_list, compare_uninformative, instance = instance)
    res <- do.call("rbind", args = res)
  } else if (compare_type == "global") {
    res <- lapply(res_list, compare_global, instance = instance)
    res <- do.call("rbind", args = res)
  }
  
  res$method_name <- factor(res$method_name, levels = METHOD_LEVELS)
  
  cli_end()
  
  # Fit linear model as reference
  model <- lm(y ~ ., data = as.data.frame(instance$dataset$train))
  lm_mse <- mean((instance$dataset$test$y - 
                    predict(model, 
                            newdata = as.data.frame(instance$dataset$test)))**2)
  lm_rsquared <- summary(model)$r.squared
  
  data.table(res, model_error = instance$error, lm_error = lm_mse, lm_rsquared = lm_rsquared,
             r_squared = instance$r_squared_test, r_squared_true = instance$r_squared_true)
}

################################################################################
#                     Compare functions
################################################################################
compare_corelation <- function(result, instance) {
  # Combine correlated features
  cor_groups <- instance$dataset$cor_groups
  res <- combine_features(result$result, cor_groups, FUN = base::identity)
  
  cor_fun <- function(x, y, i) {
    if (sd(x) == 0 || sd(y) == 0) {
      warning("Standard diviation is zero in method '",
              result$hyperp$method_name, "' for feature 'X", i,
              "'! Returning 'NA' instead!")
      res <- NA
    } else {
      res <- cor(x, y)
    }
    
    res
  }
  
  cor_local <- unlist(lapply(seq_len(ncol(res)), 
                function(i) cor_fun(res[, i], instance$dataset$imp_local[, i], i)))
  
  data.table(cor = cor_local, 
             feature = paste0("X", seq_len(ncol(res))),
             result$hyperp)
}

compare_uninformative <- function(result, instance) {
  
  # Get informative features
  info_idx <- which(instance$beta != 0)
  
  # Combine correlated features
  cor_groups <- instance$dataset$cor_groups
  res <- combine_features(result$result, cor_groups, FUN = base::identity)
  
  perc_uninform <- mean(rowSums(abs(res[, -info_idx])) / rowSums(abs(res)), 
                        na.rm = TRUE)

  data.table(percent_to_uninformative = perc_uninform, result$hyperp)
}

compare_global <- function(result, instance) {
  # Combine correlated features
  cor_groups <- instance$dataset$cor_groups
  res <- combine_features(result$result, cor_groups, FUN = base::identity)
  
  data.table(auc = auc((instance$beta == 0) * 1, (rank(colMeans(abs(res))) > 10) * 1),
             result$hyperp)
}


compare_raw <- function(result, instance) {
  
  # If `cor_groups` is NULL, we assume there are no correlations
  cat_feat <- instance$dataset$cor_groups
  if (is.null(cat_feat)) {
    cat_feat <- seq_len(instance$num_inputs)
  }
  
  # Aggregate correlated features
  res <- combine_features(result$result, cat_feat, FUN = base::identity)
  
  res <- data.frame(
    attribution = c(res),
    feature = rep(paste0("X", seq_len(ncol(res))), each = nrow(res)),
    attribution_true = NA
  )
  if (!is.null(instance$dataset$imp_local)) {
    res$attribution_true = c(instance$dataset$imp_local)
  }
  
  fun <- function(x, y) {
    cor_total <- cor(x, y)
    unlist(lapply(seq_along(x), function(i) cor(x[-i], y[-i]))) - cor_total
  }
  
  # Transform to data.table
  res <- data.table(res)
  res[, cor_error := fun(attribution, attribution_true),
      by = c("feature")]
  
  cbind(res, result$hyperp)
}

################################################################################
#                             Helper functions
################################################################################
run_method <- function(method_name, instance, method_args) {
  args <- method_args[[method_name]]
  wrapper_name <- paste0(tolower(method_name), "_wrapper")
  lapply(args, function(arg) {
    arg$instance <- instance
    do.call(wrapper_name, args = arg)
  })
}

METHOD_LEVELS <- c(
  "LIME", "SHAP",
  "Saliency", "SG", "Grad",
  "SGxI", "GxI",
  "LRP-0", "LRP-ε (0.1)", "LRP-αβ (0.5)", "LRP-αβ (1)", 
  "LRP-αβ (1.5)",
  "DeepSHAP-RC", "DeepSHAP-RE", "ExpGrad",
  "DeepLift-RC (mean)", "DeepLift-RE (mean)", "IntGrad (mean)", 
  "DeepLift-RC (zeros)", "DeepLift-RE (zeros)", "IntGrad (zeros)" 
)
