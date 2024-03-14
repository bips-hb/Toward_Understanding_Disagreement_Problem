################################################################################
#              SCRIPT FOR REPRODUCING THE FIGURES IN THE PAPER                 #
#              "Toward Understanding the Disagreement Problem in               #
#                   Neural Network Feature Attribution"                        #
#                                                                              #
#                               SECTION 3:                                     #
#               "Understanding the Explanation's Distribution"                 #
#                                                                              #
################################################################################

# Load required libraries
library("torch")
library("luz")
library("innsight")
library("data.table")
library("here")
library("cli")
library("ggplot2")
library("cowplot")
library("envalysis")
library("sysfonts")
library("showtext")

# Load LaTeX font (Latin modern), only relevant for setting the fonts as in the
# paper, but requires the latinmodern-math font
font_add("LModern_math", here("utils/latinmodern-math.otf"))
showtext_auto()

# Load helper functions
source(here("utils/utils_torch.R"))
source(here("utils/utils_syn_data.R"))
source(here("utils/algorithms.R"))
source(here("utils/utils_figures.R"))

# Function for saving figures
save_fig <- function(name, ...) {
  fig_dir <- here("figures/")
  if (!dir.exists(fig_dir)) dir.create(fig_dir)
  ggsave(paste0(fig_dir, name, ".pdf"), ...)
}

# Set ggplot2 theme
theme_set(
  theme_publish(base_size = 20, base_family = "serif", base_line_size = 2)
)

# Choose the methods to be used for this section
method_df <- list(
  Gradient = list(list(times_input = FALSE), list(times_input = TRUE)),
  SmoothGrad = list(list(times_input = FALSE, K = 100, noise_level = 0.2)),
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
    list(rule_name = "reveal_cancel")),
  SHAP = list(list(nsim = 50))
)

################################################################################
#                       Section 3: Running Example
################################################################################
set.seed(42)
torch_manual_seed(42)

# Set hyperparameters
n_units <- 128
n_layers <- 3
act.fct <- "relu"

# Define attributes
n <- 2000
n_test <- 1000
p <- 4

# Which dataset instance should be visualized?
idx <- 60

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
save_fig("Sec_3_group_1", width = 8.5, height = 5.5)

# Create plot for the second group ---------------------------------------------
# i.e., Gradient x Input, LRP
methods <- c("GxI", "LRP-αβ (1)", "LRP-αβ (1.5)")
res <- result[method_name %in% methods]
res$method_name <- factor(res$method_name, levels = methods,
                          labels = c("GxI / LRP-0", "LRP-αβ (α = 1)", "LRP-αβ (α = 1.5)"))
res_instance <- res[seq(1, n_x * 3, by = n_test) + idx, ]

# Create plot
create_distribution_fig(res, res_instance)

# Save plot
save_fig("Sec_3_group_2", width = 8.5, height = 5.5)

# Create plot for the third group ----------------------------------------------
# Integrated Gradient, DeepLIFT 
names <- c("IntGrad (zeros)", "IntGrad (mean)")
res <- result[method_name %in% names]
res$method_name <- factor(res$method_name, levels = names)
res_instance <- res[seq(1, n_x * 2, by = n_test) + idx, ]

# Create plot
create_distribution_fig(res, res_instance)

# Save plot
save_fig("Sec_3_group_3", width = 8.5, height = 5.5)

# Create plot for the fourth group ----------------------------------------------
# DeepSHAP and ExpectedGradient
names <- c("DeepSHAP-RC", "ExpGrad")
res <- result[method_name %in% names]
res$method_name <- factor(res$method_name, levels = names)
res_instance <- res[seq(1, n_x * 2, by = n_test) + idx, ]

# Create plot
create_distribution_fig(res, res_instance)

# Save plot
save_fig("Sec_3_group_4", width = 8.5, height = 5.5)
