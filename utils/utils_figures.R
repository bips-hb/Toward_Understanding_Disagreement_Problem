################################################################################
#                 Utility functions for final FIGURES
################################################################################

# Section 3 --------------------------------------------------------------------
create_distribution_fig <- function(result_all, result_local) {
  # Crate plot for the distribution of attribution values
  p1 <- ggplot(result_all) +
    geom_jitter(aes(x = feature, y = attribution, color = attribution), size = 0.2) +
    geom_violin(aes(x = feature, y = attribution), scale = "width", alpha = 0) +
    facet_grid(cols = vars(method_name), scales = "free") +
    scale_color_gradient2(low = "#3A3A98", mid = "white", high = "#832424", limits = c(-2, 2),
                          oob = scales::squish) +
    geom_hline(yintercept = 0, color = "gray40") + 
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    xlab(NULL) + ylab("Attribution") + guides(color = "none")
  
  # Create plot for a local explanation
  p2 <- ggplot(result_local) +
    geom_bar(aes(x = feature, y = attribution, fill = attribution), stat = "identity") +
    facet_grid(cols = vars(method_name), scales = "free") +
    geom_hline(yintercept = 0, color = "gray40") +
    scale_fill_gradient2(low = "#3A3A98", mid = "white", high = "#832424", limits = c(-2, 2),
                         oob = scales::squish) +
    theme(strip.background.x = element_blank(),
          strip.text.x = element_blank()) +
    guides(fill = "none") + xlab("Feature") + ylab("Attribution")
  
  # Combine both plots
  suppressWarnings({
    plot_grid(p1, p2, ncol = 1, align = "v", rel_heights = c(3, 2))
  })
}


# Section 4: Model error plot --------------------------------------------------
create_modelerror_fig <- function(result, fig_prefix) {
  res_cont <- result[endsWith(problem, "cont")]
  res_cat <- result[endsWith(problem, "cat")]
  
  # Model loss plots
  p1 <- ggplot(res_cont) +
    geom_boxplot(aes(x = data_type, y = model_error, colour = preprocess_type)) +
    geom_boxplot(aes(x = data_type, y = lm_error), color = "gray", alpha = 0.5) +
    facet_grid(cols = vars(facet_lab), scales = "free_x") +
    geom_hline(yintercept = 1) +
    scale_colour_colorblind() +
    theme(legend.position = "top", legend.text = element_text(margin = margin(r = 20)),
          legend.title = element_blank(), legend.margin = margin(), legend.text.align = 0) +
    xlab("Effect type") + ylab("Mean Squared Error (MSE)") 
  
  p2 <- ggplot(res_cat) +
    geom_boxplot(aes(x = data_type, y = model_error, colour = preprocess_type)) +
    geom_boxplot(aes(x = data_type, y = lm_error), color = "gray", alpha = 0.5) +
    facet_grid(cols = vars(facet_lab), scales = "free_x", space = "free_x") +
    geom_hline(yintercept = 1) +
    scale_colour_colorblind() +
    theme(legend.position = "top", legend.box="vertical", 
          legend.text = element_text(margin = margin(r = 20)),
          legend.title = element_blank(), 
          legend.margin = margin(), legend.text.align = 0) +
    xlab("Number of levels") + ylab(NULL) 
  
  # Combine both plots
  suppressWarnings({
    p <- plot_grid(p1, p2, align = "h", ncol = 2)
  })
  plot(p)
  
  # Save figure
  ggsave(here(paste0("figures/", fig_prefix, "_error_mse.pdf")), 
         width = 15, height = 6)
  
  # Model R² error
  p1 <- ggplot(res_cont) +
    geom_boxplot(aes(x = data_type, y = r_squared, colour = preprocess_type)) +
    geom_boxplot(aes(x = data_type, y = lm_rsquared), color = "gray", alpha = 0.5) +
    facet_grid(cols = vars(facet_lab), scales = "free_x") +
    geom_hline(yintercept = 1) +
    scale_colour_colorblind() +
    theme(legend.position = "top", legend.text = element_text(margin = margin(r = 20)),
          legend.title = element_blank(), legend.margin = margin(), legend.text.align = 0) +
    xlab("Effect type") + ylab("R-squared") + ylim(c(0, 1))
  
  p2 <- ggplot(res_cat) +
    geom_boxplot(aes(x = data_type, y = r_squared, colour = preprocess_type)) +
    geom_boxplot(aes(x = data_type, y = lm_rsquared), color = "gray", alpha = 0.5) +
    facet_grid(cols = vars(facet_lab), scales = "free_x", space = "free_x") +
    geom_hline(yintercept = 1) +
    scale_colour_colorblind() +
    theme(legend.position = "top", legend.box="vertical", 
          legend.text = element_text(margin = margin(r = 20)),
          legend.title = element_blank(), 
          legend.margin = margin(), legend.text.align = 0) +
    xlab("Number of levels") + ylab(NULL) + ylim(c(0, 1))
  
  # Combine both plots
  suppressWarnings({
    p <- plot_grid(p1, p2, align = "h", ncol = 2)
  })
  plot(p)
  
  # Save figure
  ggsave(here(paste0("figures/", fig_prefix, "_error_rsquared.pdf")), 
         width = 15, height = 6)
} 

# Section 4: Preprocess figures ------------------------------------------------
create_preprocess_fig <- function(result) {
  
  # Calculate mean value
  res_tmp <- result[, .(mean = mean(cor, na.rm = TRUE), 
                        sd = sd(cor, na.rm = TRUE),
                        n_NA = sum(is.na(cor))), 
                    by = c("problem", "method_name", "method_grp", "data_type", 
                           "preprocess_type", "paper_grp", "n_levels")]
  res_tmp[, label := ifelse(n_NA == 0, paste0(round(mean, 2)), paste0(round(mean, 2), "*"))]
  res_tmp$n_levels <- factor(res_tmp$n_levels, levels = sort(unique(res_tmp$n_levels)))
  
  p1 <- ggplot(res_tmp[problem == "Prep_cont"]) +
    geom_tile(aes(x = preprocess_type, y = method_name, fill = mean)) +
    geom_point(aes(x = preprocess_type, y = method_name, size = sd), alpha = 0.5, color = "gray") +
    facet_grid(cols = vars(data_type), rows = vars(paper_grp), scales = "free", 
               space = "free", switch = "y") + 
    labs(x = "Preprocessing", y = NULL) +
    scale_fill_gradient2(low = scales::muted("blue"), high = scales::muted("red"), limits = c(-1, 1)) +
    geom_text(aes(x = preprocess_type, y = method_name, 
                  label = label), color = "gray10", size = 5) +
    scale_y_discrete(expand = c(-0.5,0.5)) +
    scale_x_discrete(expand = c(-0.5,0.5)) +
    guides(fill = "none", size = "none") +
    theme(panel.spacing = unit(0, "lines"),
          axis.text.x = element_text(angle = 45, vjust = 0.6)) +
    scale_size_area(max_size = 10)
  
  p2 <- ggplot(res_tmp[problem == "Prep_cat"]) +
    geom_tile(aes(x = n_levels, y = method_name, fill = mean)) +
    geom_point(aes(x = n_levels, y = method_name, size = sd), alpha = 0.5, color = "gray") +
    facet_grid(cols = vars(preprocess_type), rows = vars(paper_grp), scales = "free", 
               space = "free", switch = "y") + 
    labs(x = "Number of levels", y = NULL) +
    scale_fill_gradient2(low = scales::muted("blue"), high = scales::muted("red"), limits = c(-1, 1)) +
    geom_text(aes(x = n_levels, y = method_name, 
                  label = label), color = "gray10") +
    scale_y_discrete(expand = c(0,0)) +
    scale_x_discrete(expand = c(0,0)) +
    guides(fill = "none", size = "none") +
    theme(panel.spacing = unit(0, "lines"))
  
  # Combine both plots
  suppressWarnings({
    p <- plot_grid(p1, p2, align = "h", ncol = 2)
  })
  plot(p)
  
  # Save figure
  ggsave(here(paste0("figures/Prep_correlation.pdf")), 
         width = 20, height = 8)
  
}

# Section 4: Faithfulness figures ----------------------------------------------
create_faithfulness_fig <- function(result) {
  fun <- function(x) {
    res <- vector(mode = "character", length(x))
    res[x %in% c("X1", "X2", "X3", "X4")] <- "Low effect"
    res[x %in% c("X5", "X6", "X7", "X8")] <- "Medium effect"
    res[x %in% c("X9", "X10", "X11", "X12")] <- "Strong effect"
    
    factor(res, levels = c("Low effect", "Medium effect", "Strong effect"))
  }
  result[, feat_grp := fun(feature)]
  result$n_levels <- factor(result$n_levels, levels = sort(unique(result$n_levels)))
  
  p1 <- ggplot(result[problem == "Faith_cont" & preprocess_type == "Z-score"]) +
    geom_boxplot(aes(x = cor, y = method_name, color = feat_grp), 
                 outlier.size = 0.2, outlier.alpha = 0.1) + 
    facet_grid(cols = vars(data_type), rows = vars(paper_grp), 
               space = "free_y", scales = "free", switch = "y") +
    scale_colour_colorblind() +
    theme(legend.position = "top", 
          panel.spacing = unit(0, "lines"),
          legend.title = element_blank()) +
    scale_x_continuous(trans = "atanh", breaks = c(-1, -0.5, 0, 0.5, 0.75, 0.9, 0.95, 0.99)) +
    geom_vline(xintercept = 0, color = "gray20") +
    xlab("Correlation") + ylab("")
  
  p2 <- ggplot(result[problem == "Faith_cat"]) +
    geom_boxplot(aes(x = cor, y = method_name, color = feat_grp), 
                 outlier.size = 0.2, outlier.alpha = 0.1) + 
    facet_grid(cols = vars(data_type), rows = vars(paper_grp), 
               space = "free_y", scales = "free_y", switch = "y") +
    scale_colour_colorblind() +
    theme(legend.position = "top", 
          panel.spacing = unit(0, "lines"),
          legend.title = element_blank()) +
    scale_x_continuous(trans = "atanh", breaks = c(-1, -0.5, 0, 0.5, 0.75, 0.9, 0.95, 0.99)) +
    geom_vline(xintercept = 0, color = "gray20") +
    xlab("Correlation") + ylab("")
  
  # Combine both plots
  suppressWarnings({
    p <- plot_grid(p1, p2, align = "h", ncol = 2)
  })
  plot(p)
  
  # Save figure
  ggsave(here(paste0("figures/Faith_correlation.pdf")), 
         width = 22, height = 8)
}


# Section 4: Robustness --------------------------------------------------------
create_robustness_p_fig <- function(result) {
  
  res_tmp <- result[, .(
    mean = mean(percent_to_uninformative, na.rm = TRUE),
    q1 = quantile(percent_to_uninformative, probs = 0.25, na.rm = TRUE),
    q3 = quantile(percent_to_uninformative, probs = 0.75, na.rm = TRUE),
    n_NA = sum(is.na(percent_to_uninformative))
  ),  by = c("problem", "p", "data_type", "facet_lab", "method_name",
             "paper_grp", "n_levels")]
  res_tmp$n_levels <- factor(res_tmp$n_levels, levels = sort(unique(res_tmp$n_levels)))
  
  p1 <- ggplot(res_tmp[problem == "Robust_p_cont"]) +
    geom_line(aes(x = as.numeric(p), y = mean, color = method_name)) +
    geom_ribbon(aes(x = as.numeric(p), ymin = q1, ymax = q3, fill = method_name), alpha = 0.1) +
    facet_grid(cols = vars(paper_grp), space = "free_y", scales = "free") +
    xlab("Number of variables") + ylab("Ratio of uninformative") +
    theme(panel.spacing = unit(0, "lines")) +
    guides(color = "none", fill = "none") +
    geom_hline(yintercept = 0, color = "darkgray")
  
  p2 <- ggplot(res_tmp[problem == "Robust_p_cat"]) +
    geom_line(aes(x = as.numeric(p), y = mean, color = method_name, linetype = n_levels)) +
    geom_ribbon(aes(x = as.numeric(p), ymin = q1, ymax = q3, fill = method_name, linetype = n_levels), alpha = 0.1) +
    facet_grid(cols = vars(paper_grp), space = "free_y", scales = "free") +
    xlab("Number of variables") + ylab("Ratio of uninformative") +
    theme(panel.spacing = unit(0, "lines")) +
    geom_hline(yintercept = 0, color = "darkgray")
  
  # Combine both plots
  suppressWarnings({
    legend <- get_legend(p2 + theme(legend.position = "right"))
    
    p2 <- p2  +
      guides(color = "none", fill = "none", linetype = "none")
    
    p <- plot_grid(p1, p2, legend, align = "hv", nrow = 1,
              rel_widths = c(0.44, 0.44, 0.12))
  })
  plot(p)
  
  # Save figure
  ggsave(here(paste0("figures/Robust_p_uninformative.pdf")), 
         width = 22, height = 8)
}

################################################################################
#                  Utility functions for getting the results
################################################################################
get_and_prepare_results <- function(file.dir, conf.file = here("utils/config.R")) {
  
  # Load the registry and plain jobPars and results
  loadRegistry(file.dir = file.dir, conf.file = conf.file)
  res <- reduceResultsDataTable()
  jobPars <- getJobPars(ids = res$job.id)
  jobPars$algo.pars <- lapply(jobPars$algo.pars, function(x) x[names(x) != "method_df"])
  jobPars <- batchtools::flatten(jobPars)
  
  # Prepare the variables and set labels/factors
  jobPars[, `:=`(
    data_type = prepare_dgptype(dgp_type, n_levels, encode_type),
    dgp_type = NULL)]
  
  jobPars[, `:=`(
          preprocess_type = prepare_preprocesstype(scale_type, encode_type),
          scale_type = NULL, encode_type = NULL)]
  
  jobPars[, facet_lab := set_facetlabs(data_type, n_levels)]
  
  # Get the model errors -------------------------------------------------------
  fun <- function(x) unique(x[, c("model_error", "lm_error", "lm_rsquared", 
                                  "r_squared", "r_squared_true")])
  result_error <- lapply(res$result, fun)
  result_error <- cbind(jobPars, rbindlist(result_error))
  
  # Prepare results based on correlation ---------------------------------------
  jobPars_corr <- jobPars[compare_type == "correlation", ]
  result_corr <- res[jobPars_corr$job.id]$result
  
  if (length(result_corr) != 0) {
    # Repeat rows jobPars_corr according to the number of rows in the results
    reps <- rep(seq_len(nrow(jobPars_corr)), unlist(lapply(result_corr, nrow)))
    jobPars_corr <- jobPars_corr[reps, ]
    
    # Combine all together
    result_corr <- cbind(jobPars_corr, rbindlist(result_corr))
    result_corr[, paper_grp := add_paper_grp(method_name)]
  }
  
  # Prepare results based on uninformative features ----------------------------
  jobPars_uninform <- jobPars[compare_type == "uninformative", ]
  result_uninform <- res[jobPars_uninform$job.id]$result
  
  if (length(result_uninform) != 0) {
    # Repeat rows jobPars_uninform according to the number of rows in the results
    reps <- rep(seq_len(nrow(jobPars_uninform)), unlist(lapply(result_uninform, nrow)))
    jobPars_uninform <- jobPars_uninform[reps, ]
    
    # Combine all together
    result_uninform <- cbind(jobPars_uninform, rbindlist(result_uninform))
    result_uninform[, paper_grp := add_paper_grp(method_name)]
  }
  
  list(
    res_error = result_error,
    res_corr = result_corr,
    res_uninform = result_uninform
  )
}


prepare_preprocesstype <- function(x_scale, x_encode) {
  res <- vector(mode = "character", length(x_scale))
  
  # No scaling
  res[x_scale == "scale_none"] <- "No scaling"
  # Z-score scaling
  res[x_scale == "scale_zscore"] <- "Z-score"
  # Min-Max scaling
  res[x_scale == "scale_minmax"] <- "Min-max"
  # Max-Abs scaling
  res[x_scale == "scale_maxabs"] <- "Max-abs"
  # Normalize scaling
  res[x_scale == "scale_normalize"] <- "Normalize"
  
  # Label encoding
  res[x_encode == "encode_label"] <- "Label"
  # One-hot encoding
  res[x_encode == "encode_onehot"] <- "One-hot"
  # Dummy encoding
  res[x_encode == "encode_dummy"] <- "Dummy"
  # Effect encoding
  res[x_encode == "encode_effect"] <- "Effect"
  
  # Set as factor
  factor(res, 
    levels = c("No scaling", "Z-score", "Min-max", "Max-abs", "Normalize",
               "Label", "One-hot", "Dummy", "Effect"))
}

prepare_dgptype <- function(x_dgp, x_levels, x_encode) {
  res <- vector(mode = "character", length(x_dgp))
  
  # Linear data
  res[x_dgp == "linear"] <- "Linear"
  # Piece-wise data
  res[x_dgp == "pwlinear"] <- "Piece-wise linear"
  # Squared
  res[x_dgp == "squared"] <- "Squared"
  # Cosine
  res[x_dgp == "cos"] <- "Cosine"
  # Non-Continuous
  res[x_dgp == "nonlinear"] <- "Non-continuous"
  # Smooth
  res[x_dgp == "smooth"] <- "Smooth"
  
  # Categorical variables
  res[!is.na(x_levels)] <- paste0(na.omit(x_levels))
  
  # Binary variables
  res[x_levels == 2 & x_encode == "encode_label"] <- "Binary"
  
  
  # Set as factor
  all_levels <- sort(unique(x_levels))
  factor(res, 
         levels = c("Linear", "Piece-wise linear", "Squared", "Smooth", "Cosine",
                    "Non-continuous", "Binary", 
                    paste0(all_levels)))
}

set_facetlabs <- function(x_prep, x_levels) {
  res <- rep("Continuous", length(x_prep))
  
  res[x_prep != "Binary" & !is.na(x_levels)] <- "Categorical"
  res[x_prep == "Binary"] <- "Binary"
  
  factor(res, levels = c("Continuous", "Binary", "Categorical"))
}


add_paper_grp <- function(method_names) {
  res <- vector(mode = "character", length(method_names))
  
  # Group 1
  grp_1 <- c("Grad", "SG", "Saliency")
  res[method_names %in% grp_1] <- "Group 1"
  
  # Group 2
  grp_2 <- c("GxI", "SGxI", "LRP-0", "LRP-ε (0.1)", "LRP-αβ (0.5)", 
             "LRP-αβ (1)", "LRP-αβ (1.5)")
  res[method_names %in% grp_2] <- "Group 2"
  
  # Group 3
  grp_3 <- c("IntGrad (zeros)", "IntGrad (mean)", "DeepLift-RE (zeros)",
             "DeepLift-RE (mean)", "DeepLift-RC (zeros)", "DeepLift-RC (mean)")
  res[method_names %in% grp_3] <- "Group 3"
  
  # Group 4
  grp_4 <- c("DeepSHAP-RE", "DeepSHAP-RC", "ExpGrad", "SHAP")
  res[method_names %in% grp_4] <- "Group 4"
  
  factor(res, levels = c("Group 1", "Group 2", "Group 3", "Group 4"))
}





