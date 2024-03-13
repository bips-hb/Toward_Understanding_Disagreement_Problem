################################################################################
#                               Algorithms
################################################################################
apply_methods <- function(data, job, instance, compare_type = "correlation", 
                          method_df = NULL, ignore_last_act = TRUE) {
  source(here("utils/methods.R"))
  
  cli_progress_step("Running feature attribution methods...", 
                    "Running feature attribution methods done!")
  
  res_list <- lapply(names(method_df), run_method, instance = instance,
                     method_args = method_df, ignore_last_act = ignore_last_act)
  res_list <- unlist(res_list, recursive = FALSE)
  
  if (compare_type == "attributions") {
    res <- lapply(res_list, compare_raw, instance = instance)
    res <- do.call("rbind", args = res)
  } else if (compare_type == "correlation") {
    res <- lapply(res_list, compare_corelation, instance = instance)
    res <- do.call("rbind", args = res)
  } else if (compare_type == "F1_score") {
    res <- lapply(res_list, compare_f1_score, instance = instance)
    res <- do.call("rbind", args = res)
  } else if (compare_type == "raw_matrix") {
    res <- lapply(res_list, compare_raw_matrix, instance = instance)
    res <- do.call("rbind", args = res)
  }
  
  res$method_name <- factor(res$method_name, levels = METHOD_LEVELS)
  
  cli_end()
  
  # Fit linear or logistic model as reference
  if (instance$outcome_type == "regression") {
    model <- lm(y ~ ., data = as.data.frame(instance$dataset$train))
    preds <- predict(model, newdata = as.data.frame(instance$dataset$test))
    error_metric_ref <- mean((instance$dataset$test$y - preds)**2)
    error_ref <- summary(model)$r.squared
  } else if (instance$outcome_type == "classification") {
    model <- glm(y~., family = binomial(link='logit'), 
                 data = as.data.frame(instance$dataset$train))
    
    pred <- predict(model, newdata = as.data.frame(instance$dataset$test), 
                    type = "response")
    pred <- as.factor(ifelse(pred < 0.5, 0, 1))
    error_metric_ref <- mean(pred == instance$dataset$test$y)
    error_ref <- confusionMatrix(pred, reference = as.factor(as.numeric(instance$dataset$test$y) - 1))$byClass[["F1"]]
  }
  
  
  data.table(res, 
             error_metric = instance$error_metric, 
             error_metric_ref = error_metric_ref, 
             error_ref = error_ref,
             error = instance$error, 
             r_squared_true = instance$r_squared_true)
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
             exp_mean = colMeans(res), 
             feature = paste0("X", seq_len(ncol(res))),
             result$hyperp)
}

compare_f1_score <- function(result, instance) {
  cor_groups <- instance$dataset$cor_groups
  res <- combine_features(result$result, cor_groups, FUN = base::identity)
  p <- ncol(res)
  
  # Calculate rankings
  pred <- (t(apply(abs(res), 1, rank, ties.method = "random")) > p %/% 2) * 1
  ref <- factor((instance$beta != 0) * 1)
  
  prec <- apply(pred, 1, function(a) caret::precision(factor(a), ref))
  rec <- apply(pred, 1, function(a) caret::recall(factor(a), ref))
  f1 <- apply(pred, 1, function(a) caret::F_meas(factor(a), ref))
  
  data.table(f1_score = mean(f1), precision = mean(prec),
             recall = mean(rec), result$hyperp)
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

compare_raw_matrix <- function(result, instance) {
  cor_groups <- instance$dataset$cor_groups
  res <- combine_features(result$result, cor_groups, FUN = base::identity)
  
  cbind(res, result$hyperp)
}

################################################################################
#                             Helper functions
################################################################################
run_method <- function(method_name, instance, method_args, ignore_last_act = TRUE) {
  args <- method_args[[method_name]]
  wrapper_name <- paste0(tolower(method_name), "_wrapper")
  lapply(args, function(arg) {
    arg$instance <- instance
    arg$ignore_last_act <- ignore_last_act
    do.call(wrapper_name, args = arg)
  })
}

combine_features <- function(res_1, cat_feat, FUN = identity) {
  if (!is.matrix(res_1)) res_1 <- matrix(res_1, nrow = 1)
  res <- lapply(seq_len(max(cat_feat)), 
                function(i) rowSums(FUN(res_1[, which(cat_feat == i), drop = FALSE])))
  do.call("cbind", args = res)
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
