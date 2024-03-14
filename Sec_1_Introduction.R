################################################################################
#              SCRIPT FOR REPRODUCING THE FIGURES IN THE PAPER                 #
#              "Toward Understanding the Disagreement Problem in               #
#                   Neural Network Feature Attribution"                        #
#                                                                              #
#                               SECTION 1:                                     #
#                    "Introduction" (COMPAS dataset)                           #
#                                                                              #
################################################################################
library("innsight")
library("torch")
library("luz")
library("mlr3fairness")
library("data.table")
library("here")
library("cli")
library("caret")
library("ggplot2")
library("envalysis")

# Set seeds
set.seed(42)
torch_manual_seed(42)

# Load helper functions
source(here("utils/utils_torch.R"))
source(here("utils/algorithms.R"))
source(here("utils/utils_real_data.R"))

# Choose the methods to be used for this section
method_df <- list(
  Gradient = list(list(times_input = FALSE), list(times_input = TRUE)),
  SmoothGrad = list(list(times_input = FALSE, K = 50, noise_level = 0.1)),
  IntGrad = list(list(n = 50, x_ref = "zeros")),
  ExpGrad = list(list(n = 50)),
  DeepLIFT = list(list(rule_name = "reveal_cancel", "mean")),
  DeepSHAP = list(list(rule_name = "rescale")),
  SHAP = list(list(nsim = 50))
)

# Set hyperparameters
n_units <- 256
n_layers <- 4
act.fct <- "relu"

# Get and preprocess the dataset
data <- get_compas_ds(scale_type = "scale_zscore")

# Train model
instance <- train_model(length(data$cor_groups), n_units, n_layers, data,
                        "classification", act.fct)

# Apply feature attribution methods
result <- apply_methods(instance = instance, compare_type = "raw_matrix",
                        method_df = method_df)

# Define grid for pair-wise comparison 
method_names <- c("Grad", "SG", "GxI", "IntGrad (zeros)", "DeepLift-RC (mean)",
                  "ExpGrad", "DeepSHAP-RE", "SHAP")
method_names <- factor(method_names, levels = method_names)
rank_cor <- expand.grid(method_names, method_names, 
                        cor = 0, rank_ag = 0, rank_cor = 0)


# Calculate the feature correlation, rank agreement (top 2 features) and
# rank correlation for each combination of methods
for (i in seq_len(nrow(rank_cor))) {
  x1 <- result[method_name == rank_cor$Var1[i]][, 1:10] 
  x2 <- result[method_name == rank_cor$Var2[i]][, 1:10] 
  
  # Correlation
  a <- unlist(lapply(seq_len(ncol(x1)), function(k) cor(x1[ ,..k], x2[, ..k])))
  rank_cor$cor[i] <- mean(a, na.rm = TRUE)
  
  # Rank agreement (k = 2)
  k <- 2 
  a1 <- t(apply(x1, 1, rank)) <= k
  a2 <- t(apply(x2, 1, rank)) <= k
  rank_cor$rank_ag[i] <- mean(rowSums(a1 * a2) / k)
  
  # Rank correlation
  a <- unlist(lapply(seq_len(nrow(x1)), function(k) cor(rank(x1[k, ]), rank(x2[k, ]))))
  rank_cor$rank_cor[i] <- mean(a, na.rm = TRUE)
}


# Transform it into a long format and set factors/labels
res <- reshape2::melt(rank_cor, id.vars = c("Var1", "Var2"),
            measure.vars = c("cor", "rank_cor", "rank_ag"))

labels <- levels(res$Var1)
labels[labels == "GxI"] <- "GxI / LRP-0"
res$Var1 <- factor(res$Var1, levels = levels(res$Var1), labels = labels)
res$Var2 <- factor(res$Var2, levels = rev(levels(res$Var2)),
                   labels = rev(labels))
res$variable <- factor(res$variable, levels = levels(res$variable),
                       labels = c("Correlation", "Rank Correlation", "Rank Agreement (k = 2)"))

# Create the plot
ggplot(res) +
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
  scale_fill_gradient2(high = scales::muted("red"), low = scales::muted("blue"), limits = c(-1,1)) +
  scale_x_discrete(guide = guide_axis(angle = 30), expand = c(0,0)) +
  scale_y_discrete(expand = c(0, 0)) +
  facet_grid(cols = vars(variable)) +
  theme_publish(base_family = "serif", base_size = 18) +
  guides(fill = guide_colorbar(barheight = 20)) +
  theme(legend.position = "right") +
  labs(x = NULL, y = NULL, fill = "")


# Save plot
if (!dir.exists(here("figures/"))) dir.create(here("figures/"))
ggsave(paste0("figures/Sec_1_COMPAS.pdf"), width = 16, height = 5)
