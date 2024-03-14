################################################################################
#
#                 Utility functions for final FIGURES
#
################################################################################

################################################################################
#             SECTION 3: Understanding the Explanation’s Distribution
################################################################################
create_distribution_fig <- function(result_all, result_local) {
  # Crate plot for the distribution of attribution values
  p1 <- ggplot(result_all) +
    geom_jitter(aes(x = feature, y = attribution, color = attribution), size = 0.2) +
    geom_violin(aes(x = feature, y = attribution), scale = "width", alpha = 0) +
    facet_grid(cols = vars(method_name), scales = "free") +
    scale_color_gradient2(low = "#3A3A98", mid = "white", high = "#832424", limits = c(-1.5, 1.5),
                          oob = scales::squish) +
    geom_hline(yintercept = 0, color = "gray40") + 
    theme(axis.text.x = element_blank()) +
    xlab(NULL) + ylab("Attribution") + guides(color = "none")
  
  # Create plot for a local explanation
  p2 <- ggplot(result_local) +
    geom_bar(aes(x = feature, y = attribution, fill = attribution), stat = "identity") +
    facet_grid(cols = vars(method_name), scales = "free") +
    geom_hline(yintercept = 0, color = "gray40") +
    scale_fill_gradient2(low = "#3A3A98", mid = "white", high = "#832424", limits = c(-1.5, 1.5),
                         oob = scales::squish) +
    theme(strip.background.x = element_blank(),
          strip.text.x = element_blank()) +
    guides(fill = "none") + xlab(NULL) + ylab("Attribution")
  
  # Combine both plots
  suppressWarnings({
    plot_grid(p1, p2, ncol = 1, align = "v", rel_heights = c(3, 2))
  })
}


################################################################################
#                   SECTION 4.1 Impact of Data Preprocessing
################################################################################
create_preprocess_fig <- function(result) {
  
  # Create folder
  if (!dir.exists(here("figures"))) {
    dir.create(here("figures"))
  }
  
  # Calculate mean value
  res_tmp <- result[, .(mean = mean(cor, na.rm = TRUE), 
                        sd = sd(cor, na.rm = TRUE),
                        n_NA = sum(is.na(cor))), 
                    by = c("problem", "method_name", "method_grp", "data_type", 
                           "preprocess_type", "paper_grp", "n_levels")]
  res_tmp[, label := ifelse(n_NA == 0, paste0(round(mean, 2)), paste0(round(mean, 2), "*"))]
  res_tmp$n_levels <- factor(res_tmp$n_levels, levels = sort(unique(res_tmp$n_levels)))
  
  # Remove LRP-0 (since it is identical to GxI)
  res_tmp <- res_tmp[method_name != "LRP-0"]
  labels <- levels(res_tmp$method_name)
  labels[labels == "GxI"] <- "GxI / LRP-0"
  res_tmp$method_name <- factor(res_tmp$method_name, 
                                levels = levels(res_tmp$method_name),
                                labels = labels)
  
  
  p <- ggplot(res_tmp) +
    geom_tile(aes(x = preprocess_type, y = method_name, fill = mean)) +
    geom_point(aes(x = preprocess_type, y = method_name, size = sd), 
               alpha = 0.9, color = "gray10") +
    facet_grid(cols = vars(data_type), rows = vars(paper_grp), scales = "free", 
               space = "free", switch = "y") +
    scale_fill_gradient2(low = scales::muted("blue"), high = scales::muted("red"), 
                         limits = c(-1, 1)) +
    scale_y_discrete(expand = c(0,0)) +
    scale_x_discrete(expand = c(0,0), guide = guide_axis(angle = 30)) +
    labs(x = NULL, y = NULL, fill = "Feature correlation (mean)", 
         size = "Feature correlation (sd)") + 
    scale_size_area(max_size = 7) +
    guides(fill = guide_colorbar(barwidth = 30, title.position = "top"),
           size = guide_legend(title.position = "top")) +
    theme_publish(base_family = "serif", base_size = 15, base_linewidth = 0.5)
  
  ggsave(here("figures/Sec_4_1_Preprocessing.pdf"), 
         width = 12, height = 8, plot = p)
}


################################################################################
#                   SECTION 4.2 Faithfulness of Effects
################################################################################
create_faithfulness_fig <- function(result) {
  
  # Remove methods with zero baseline
  removed_methods <- c("IntGrad (zeros)", "LRP-0", "DeepLift-RE (zeros)",
                       "DeepLift-RC (zeros)", "LRP-ε (0.1)")
  result <- result[!(method_name %in% removed_methods)] 
  
  
  # Create folder
  if (!dir.exists(here("figures"))) {
    dir.create(here("figures"))
  }
  
  # Group the features and combine GxI and LRP-0
  fun <- function(x) {
    res <- vector(mode = "character", length(x))
    res[x %in% c("X1", "X2", "X3", "X4")] <- "Weak"
    res[x %in% c("X5", "X6", "X7", "X8")] <- "Medium"
    res[x %in% c("X9", "X10", "X11", "X12")] <- "Strong"
    
    factor(res, levels = c("Weak", "Medium", "Strong"))
  }
  result[, feat_grp := fun(feature)]
  result$n_levels <- factor(result$n_levels, levels = sort(unique(result$n_levels)))
  labels <- levels(result$method_name)
  labels[labels == "GxI"] <- "GxI / LRP-0"
  result$method_name <- factor(result$method_name, 
                               levels = levels(result$method_name),
                               labels = labels)
  
  
  # Create plot
  p <- ggplot(result) +
    annotate("rect", xmin = 0.7, xmax = 1.02, ymin = -Inf, ymax = Inf, 
             fill = "gray70", alpha = 0.2) +
    geom_boxplot(aes(x = cor, y = method_name, fill = feat_grp), 
                 outlier.size = 0.3, outlier.alpha = 0.3) + 
    facet_grid(cols = vars(data_type), rows = vars(paper_grp), 
               space = "free_y", scales = "free_y", switch = "y") +
    scale_fill_manual(values = rev(pal_npg()(3))) +
    geom_vline(xintercept = 0, color = "gray20") +
    labs(x = "Feature correlation with ground-truth", fill = "Feature effect", y = NULL) +
    theme_publish(base_family = "serif", base_size = 15, base_linewidth = 0.5) +
    theme(panel.grid.minor.x =element_line(color="lightgray", linewidth=0.5)) +
    coord_cartesian(xlim = c(-0.5, 1)) +
    theme(legend.text = element_text(size = 16),
          legend.key.size = unit(17, "pt"),
          legend.title = element_text(size = 16))
  
  ggsave(here("figures/Sec_4_2_Faithfulness.pdf"), 
         width = 12, height = 8, plot = p)
}

################################################################################
#             SECTION 4.3 Beyond Feature Attribution Toward Importance
################################################################################
create_beyond_attribution_fig <- function(result) {
  
  # Select subset of methods
  methods <- c("Saliency", "SG", "GxI", "SGxI", "IntGrad (mean)", 
               "DeepLift-RC (mean)", "SHAP", "ExpGrad", "DeepSHAP-RC")
  result <- result[method_name %in% methods]
  
  # Calulate the mean F1-score over all repetitions
  res_tmp <- result[, .(mean = mean(f1_score, na.rm = TRUE)), 
                    by = c("n", "method_name", "data_type", "paper_grp", 
                           "preprocess_type")]
  
  # Combine identical methods
  # LRP-0 = GxI
  # Saliency = Grad
  labels <-  methods
  labels[labels == "GxI"] <- "GxI / LRP-0"
  labels[labels == "Saliency"] <- "Saliency / Grad"
  res_tmp$method_name <- factor(res_tmp$method_name, 
                               levels = methods,
                               labels = labels)
  
  # Create plot
  colors <- pal_npg()(9)
  p <- ggplot(res_tmp[preprocess_type != "No scaling"]) +
    geom_line(aes(x = n, y = mean, color = method_name, linetype = paper_grp)) +
    geom_point(aes(x = n, y = mean, color = method_name, shape = paper_grp)) +
    facet_grid(cols = vars(data_type), scale = "free") +
    theme_publish(base_family = "serif", base_size = 15, base_linewidth = 0.5) +
    geom_hline(yintercept = 0.5, linetype = "longdash") +
    scale_color_manual(values = colors) +
    scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +
    scale_shape_manual(values = c(1,2,3,4)) +
    theme(panel.grid.minor = element_line(colour="lightgray")) +
    guides(color = "none", shape = "none", linetype = "none") +
    ylim(c(0.5, 1)) +
    labs(y = "F1-score", x = "Sample size", shape = NULL, linetype = NULL,
         color = "Method")
  
  leg1 <- get_legend(ggplot(res_tmp[paper_grp == "Group 1"]) +
                       theme_publish(base_family = "serif", base_size = 15, base_linewidth = 0.5) +
                       theme(legend.direction = "vertical") +
    geom_line(aes(x = n, y = mean, color = method_name), linetype = "solid") +
    geom_point(aes(x = n, y = mean, color = method_name), shape = 1) +
    scale_color_manual(values = colors[1:2])+
    labs(color = "Group 1"))
  
  leg2 <- get_legend(ggplot(res_tmp[paper_grp == "Group 2"]) +
                       theme_publish(base_family = "serif", base_size = 15, base_linewidth = 0.5) + 
                       theme(legend.direction = "vertical") +
    geom_line(aes(x = n, y = mean, color = method_name), linetype = "dashed") +
    geom_point(aes(x = n, y = mean, color = method_name), shape = 2) +
    scale_color_manual(values = colors[3:4])+
    labs(color = "Group 2"))
  
  leg3 <- get_legend(ggplot(res_tmp[paper_grp == "Group 3"]) +
                       theme_publish(base_family = "serif", base_size = 15, base_linewidth = 0.5) + 
                       theme(legend.direction = "vertical") +
                       geom_line(aes(x = n, y = mean, color = method_name), linetype = "dotted") +
                       geom_point(aes(x = n, y = mean, color = method_name), shape = 3) +
                       scale_color_manual(values = colors[5:6])+
                       labs(color = "Group 3"))
  
  leg4 <- get_legend(ggplot(res_tmp[paper_grp == "Group 4"]) +
                       theme_publish(base_family = "serif", base_size = 15, base_linewidth = 0.5) + 
                       theme(legend.direction = "vertical") +
                       geom_line(aes(x = n, y = mean, color = method_name), linetype = "dotdash") +
                       geom_point(aes(x = n, y = mean, color = method_name), shape = 4) +
                       scale_color_manual(values = colors[7:9]) +
                       guides(color = guide_legend(ncol = 2)) +
                       labs(color = "Group 4"))
  
  legend <- plot_grid(grid::nullGrob(), leg1, leg2, leg3, leg4, grid::nullGrob(), nrow = 1, align = "hv",
                      axis = "lr", rel_widths = c(0.3, 1, 1, 1, 1, 0.5))
  p <- plot_grid(p, legend, nrow = 2, rel_heights = c(6, 1))
  
  ggsave(here("figures/Sec_4_3_Beyond_Attribution.pdf"), 
         width = 12, height = 7, plot = p)
  
}

################################################################################
#                        Appendix: Model Performance
################################################################################
create_table_1 <- function(result) {
  res_tab <- result[problem %in% c("Prep_cont", "Faith_cont")]
  res_tab_linear <- res_tab[, .(r2 = paste0(round(mean(error_ref), digits = 2), " ± ", round(sd(error_ref), digits = 2))),
                            by = c("data_type", "problem")]
  res_tab <- res_tab[, .(r2 = paste0(round(mean(error), digits = 2), " ± ", round(sd(error), digits = 2))), 
                     by = c("preprocess_type", "data_type", "problem")]
  res_tab <- dcast(res_tab, problem + preprocess_type ~ data_type, value.var = "r2")
  res_tab_linear <- dcast(res_tab_linear, problem ~ data_type, value.var = "r2")
  res_tab_linear$preprocess_type <- "(Linear model)"
  res_tab <- rbind(res_tab, res_tab_linear)
  res_tab <- res_tab[c(2, 3, 4, 6, 1, 5), ]
  res_tab <- kbl(res_tab, "pipe", booktabs = TRUE) %>%
    kable_classic() %>%
    add_header_above(c(" " = 1, " " = 1, "Effect type" = 3)) %>%
    collapse_rows(columns = 1, valign = "top")
  
  res_tab
}

create_table_2 <- function(result) {
  res_tab <- result[problem %in% c("Prep_cat", "Faith_cat")]
  res_tab_linear <- res_tab[, .(r2 = paste0(round(mean(error_ref), digits = 2), " ± ", round(sd(error_ref), digits = 2))),
                            by = c("preprocess_type", "problem")]
  res_tab <- res_tab[, .(r2 = paste0(round(mean(error), digits = 2), " ± ", round(sd(error), digits = 2))), 
                     by = c("preprocess_type", "data_type", "problem")]
  res_tab <- dcast(res_tab, problem + data_type ~ preprocess_type, value.var = "r2")
  res_tab_linear <- dcast(res_tab_linear, problem ~ preprocess_type, value.var = "r2")
  res_tab_linear$data_type <- "(Linear model)"
  res_tab <- rbind(res_tab, res_tab_linear)
  res_tab <- res_tab[c(3, 4, 6, 1, 2, 5), ]
  res_tab <- kbl(res_tab, "pipe", booktabs = TRUE) %>%
    kable_classic() %>%
    add_header_above(c(" " = 1, " " = 1, "Encoding" = 4)) %>%
    collapse_rows(columns = 1, valign = "top")
  
  res_tab
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
  
  if (!("n_levels" %in% names(jobPars))) {
    jobPars$n_levels <- NA
  }
  if (!("encode_type" %in% names(jobPars))) {
    jobPars$encode_type <- NA
  }
  
  # Prepare the variables and set labels/factors
  jobPars[, `:=`(
    data_type = prepare_dgptype(dgp_type, n_levels, encode_type),
    dgp_type = NULL)]
  
  jobPars[, `:=`(
          preprocess_type = prepare_preprocesstype(scale_type, encode_type),
          scale_type = NULL, encode_type = NULL)]
  
  jobPars[, facet_lab := set_facetlabs(data_type, n_levels)]
  
  # Get the model errors -------------------------------------------------------
  fun <- function(x) unique(x[, c("error_metric", "error_metric_ref", "error", 
                                  "error_ref", "r_squared_true")])
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
  
  # Prepare results based on f1 score ------------------------------------------
  jobPars_f1 <- jobPars[compare_type == "F1_score", ]
  result_f1 <- res[jobPars_f1$job.id]$result
  
  if (length(result_f1) != 0) {
    # Repeat rows jobPars_f1 according to the number of rows in the results
    reps <- rep(seq_len(nrow(jobPars_f1)), 
                unlist(lapply(result_f1, function(x) if(is.null(x)) 0 else nrow(x))))
    jobPars_f1 <- jobPars_f1[reps, ]
    
    # Combine all together
    result_f1 <- cbind(jobPars_f1, rbindlist(result_f1))
    result_f1[, paper_grp := add_paper_grp(method_name)]
  }
  
  list(
    res_error = result_error,
    res_corr = result_corr,
    res_f1 = result_f1
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
  # Effect binary
  res[x_encode == "encode_binary"] <- "Binary"
  
  # Set as factor
  factor(res, 
    levels = c("No scaling", "Z-score", "Min-max", "Max-abs", "Normalize",
               "Label", "One-hot", "Dummy", "Effect", "Binary"))
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
                    paste0(all_levels)),
         labels = c("Linear", "Piece-wise linear", "Squared", "Smooth", "Cosine",
                    "Non-continuous", "Binary", 
                    paste0("Cat. (", all_levels, ")")))
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






