################################################################################
#              SCRIPT FOR REPRODUCING THE FIGURES IN THE PAPER                 #
#              "Understanding Feature Attribution Methods                      #  
#                            on Tabular Data"                                  #
#                                                                              #
#                               SECTION 3:                                     #
#               "Understanding the Explanation's Distribution"                 #
#                                                                              #
################################################################################

# Load required libraries
library("torch")
library("luz")
library("data.table")
library("here")
library("cli")
library("ggplot2")
library("ggridges")
library("cowplot")
library("sysfonts")
library("showtext")

# Load LaTeX font (Latin modern), only relevant for setting the fonts as in the
# paper, but requires the latinmodern-math font
font_add("LModern_math", here("utils/latinmodern-math.otf"))
showtext_auto()

# Load helper functions
source(here("utils/utils_torch.R"))
source(here("utils/utils_syn_data.R"))
source(here("utils/utils_real_data.R"))
source(here("utils/algorithms.R"))
source(here("utils/utils_figures.R"))

# Function for saving figures
save_fig <- function(name, ...) {
  fig_dir <- here("figures/")
  ggsave(paste0(fig_dir, name, ".pdf"), ...)
}

# Set ggplot2 theme
theme_set(
  theme_bw(base_size = 18, base_family = "LModern_math", base_line_size = 1) +
    theme(
      axis.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold", size = 16),
      axis.text.y = element_text(size = 13),
      panel.spacing=unit(0,"lines"),
      plot.margin = unit(c(0, 0, 0, 0), "cm")
    )
)

# Choose the methods to be used for this section
method_df <- list(
  Gradient = list(list(times_input = FALSE), list(times_input = TRUE)),
  SmoothGrad = list(list(times_input = FALSE, K = 50, noise_level = 0.2)),
  IntGrad = list(
    list(n = 50, x_ref = "zeros"),
    list(n = 50, x_ref = "mean")),
  ExpGrad = list(list(n = 50)),
  LRP = list(
    list(rule_name = "simple", rule_param = 0),
    list(rule_name = "epsilon", rule_param = 0.1),
    list(rule_name = "alpha_beta", rule_param = 1),
    list(rule_name = "alpha_beta", rule_param = 1.5)),
  DeepLIFT = list(
    list(rule_name = "rescale", "zeros"),
    list(rule_name = "rescale", "mean"),
    list(rule_name = "reveal_cancel", "zeros"),
    list(rule_name = "reveal_cancel", "mean")),
  DeepSHAP = list(
    list(rule_name = "rescale"),
    list(rule_name = "reveal_cancel"))
)

################################################################################
#                           Bike Sharing Dataset
################################################################################
set.seed(42)
torch_manual_seed(42)

# Set hyperparameters
n_units <- 256
n_layers <- 3
act.fct <- "relu"

# Get and preprocess the dataset
data <- get_bike_sharing_ds()

# Train model
instance <- train_model(length(data$cor_groups), n_units, n_layers, data, 
                        "regression", act.fct)

# Apply feature attribution methods
result <- apply_methods(instance = instance, compare_type = "attributions", 
                        method_df = method_df)

# Generate plot
methods <- c("DeepSHAP (reveal-cancel)", "DeepLift (rescale, zeros)",  
             "LRP (alpha-beta, 1.5)", "LRP (alpha-beta, 1)", 
             "ExpectedGradient", "IntegratedGradient (mean)",
             "Gradient x Input", "Gradient")
ggplot(result[feature %in% c("X1", "X7")]) +
  geom_density_ridges_gradient(aes(x = attribution, y = method_name, fill = after_stat(x)), 
                               scale = 2, rel_min_height = 0.01, alpha = 0.8) +
  facet_grid(cols = vars(feature), scales = "free",
             labeller = labeller(feature = c(X1 = "Temperature (normalized)", 
                                             X7 = "Weekday"))) +
  scale_fill_gradient2(low = "#3A3A98", high = "#832424", limits = c(-1.5, 1.5),
                       oob = scales::squish) +
  geom_vline(xintercept = 0, color = "gray40") +
  xlab("Attribution") + ylab("") +
  guides(fill = "none") +
  theme(text = element_text(family = "LModern_math", size = 14))

# Save plot
save_fig("Sec_3_bike_sharing", width = 8, height = 5)

################################################################################
#                       Section 3: Running Example
################################################################################
set.seed(42)
torch_manual_seed(42)

# Set hyperparameters
n_units <- 256
n_layers <- 3
act.fct <- "relu"

# Define attributes
n <- 3000
n_test <- 1000
p <- 4

# Which dataset instance should be visualized?
idx <- 22

# Define data generating process (y = x1 + x2 + x3^2, x4 - 0.5)
# and sample function
dgp_fun <- function(x) {
  effects <- cbind(x[ ,1], x[ ,2], x[ ,3]^2, x[ ,4])
  list(lp = rowSums(effects), effects = effects)
}
sample_fun <- function(n) {
  cbind(rnorm(n), rnorm(n) + 2, runif(n, min = -1, max = 2), rbinom(n, 1, 0.4)) 
}

# Create data, train model and apply attribution methods
data <- get_dataset(sample_fun, dgp_fun, n, n_test, preprocess_type = "scale_none")
instance <- train_model(p, n_units, n_layers, data, "regression", act.fct)
result <- apply_methods(instance = instance, compare_type = "attributions",
                        method_df = method_df)
n_x <- n_test * p

# Create plot for the first group ----------------------------------------------
# i.e, Gradient and SmoothGrad
res <- result[method_name %in% c("Grad", "SG")]
res$method_name <- factor(res$method_name, levels = c("Grad", "SG"), 
                          labels = c("Gradient (Grad)", "SmoothGrad (SG)"))
res_instance <- res[seq(1, n_x * 2, by = n_test) + idx, ]

# Create plot
create_distribution_fig(res, res_instance)

# Save plot
save_fig("Sec_3_group_1", width = 8.5, height = 6)

# Create plot for the second group ---------------------------------------------
# i.e., Gradient x Input, LRP
methods <- c("GxI", "LRP-αβ (1)", "LRP-αβ (1.5)")
res <- result[method_name %in% methods]
res$method_name <- factor(res$method_name, levels = methods,
                          labels = c("Gradient x Input (GxI)", "LRP-αβ (α = 1)", "LRP-αβ (α = 1.5)"))
res_instance <- res[seq(1, n_x * 3, by = n_test) + idx, ]

# Create plot
create_distribution_fig(res, res_instance)

# Save plot
save_fig("Sec_3_group_2", width = 8.5, height = 6)

# Create plot for the third group ----------------------------------------------
# Integrated Gradient, DeepLIFT 
names <- c("IntGrad (zeros)", "IntGrad (mean)")
res <- result[method_name %in% names]
res$method_name <- factor(res$method_name, levels = names)
res_instance <- res[seq(1, n_x * 2, by = n_test) + idx, ]

# Create plot
create_distribution_fig(res, res_instance)

# Save plot
save_fig("Sec_3_group_3", width = 8.5, height = 6)


names <- c("IntGrad (zeros)", "DeepLift-RE (zeros)", "DeepLift-RC (zeros)",
           "IntGrad (mean)", "DeepLift-RE (mean)", "DeepLift-RC (mean)")
res <- result[method_name %in% names]
res$method_name <- factor(res$method_name, levels = names)
res_instance <- res[seq(1, n_x * 6, by = n_test) + idx, ]

# Create plot
create_distribution_fig(res, res_instance)

# Save plot
save_fig("App_Sec_3_group_3_extended", width = 17, height = 6)


# Create plot for the fourth group ----------------------------------------------
# DeepSHAP and ExpectedGradient
names <- c("DeepSHAP-RC", "ExpGrad")
res <- result[method_name %in% names]
res$method_name <- factor(res$method_name, levels = names)
res_instance <- res[seq(1, n_x * 2, by = n_test) + idx, ]

# Create plot
create_distribution_fig(res, res_instance)

# Save plot
save_fig("Sec_3_group_4", width = 8, height = 6)
