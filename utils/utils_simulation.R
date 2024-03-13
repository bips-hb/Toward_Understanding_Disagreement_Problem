################################################################################
#
#               Utility functions and settings for the simulations
#
################################################################################

# Hyperparameters of the feature attribution methods ---------------------------
METHOD_DF <- list(
  Gradient = list(
    list(times_input = FALSE),
    list(times_input = FALSE, saliency = TRUE),
    list(times_input = TRUE)),
  SmoothGrad = list(
    list(times_input = FALSE, K = 50, noise_level = 0.2),
    list(times_input = TRUE, K = 50, noise_level = 0.2)),
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
#                               Experiments
################################################################################

# 4.1 Impact of Data Preprocessing ---------------------------------------------

# Continuous variables
Prep_cont <- expand.grid(
  n = 4000,
  n_test = 1000,
  p = 12,
  beta = 1,
  mean = "random",
  sample_type = "normal",
  dgp_type = c("linear", "pwlinear", "nonlinear"),
  scale_type = c("scale_none", "scale_zscore", "scale_maxabs"), 
  nn_units = c(256),
  nn_layers = c(3),
  nn_act.fct = c("relu")
)

# Binary/Categorical variables
Prep_cat <- expand.grid(
  n = 2000,
  n_test = 1000,
  p = 12,
  n_levels = c(4, 12),
  beta = 0.5,
  level_beta = "mixed",
  level_probs = "equal",
  encode_type = c("encode_label", "encode_onehot", "encode_dummy", "encode_binary"),
  nn_units = c(128),
  nn_layers = c(3),
  nn_act.fct = c("relu")
)

# 4.2 Faithfulness of Effects --------------------------------------------------

# Continuous variables
Faith_cont <- expand.grid(
  n = 4000,
  n_test = 1000,
  p = 12,
  beta = "grouped",
  mean = "random",
  sample_type = "normal",
  dgp_type = c("linear", "pwlinear", "nonlinear"),
  scale_type = c("scale_zscore"),
  nn_units = c(256),
  nn_layers = c(3),
  nn_act.fct = c("relu")
)

# Binary/Categorical variables
Faith_cat <- rbind(
  expand.grid( # Categorical variables
    n = 2000,
    n_test = 1000,
    p = 12,
    n_levels = c(4),
    beta = "grouped",
    level_beta = "mixed",
    level_probs = "equal",
    encode_type = c("encode_onehot"),
    nn_units = c(128),
    nn_layers = c(3),
    nn_act.fct = c("relu")
  ),
  expand.grid( # Binary variables
    n = 2000,
    n_test = 1000,
    p = 12,
    n_levels = c(2),
    beta = "grouped",
    level_beta = "mixed",
    level_probs = "equal",
    encode_type = c("encode_label"),
    nn_units = c(128),
    nn_layers = c(3),
    nn_act.fct = c("relu")
  )
)

# 4.2 Beyond Feature Attribution -----------------------------------------------

BeyondA_cont <- expand.grid(
  n = seq(200, 5000,  by = 400),
  n_test = 1000,
  p = 20,
  beta = "swapping",
  mean = "random",
  sample_type = "normal",
  dgp_type = c("linear", "nonlinear"), 
  scale_type = c("scale_zscore"), 
  nn_units = c(256),
  nn_layers = c(3),
  nn_act.fct = c("relu")
)

BeyondA_cat <- expand.grid(
  n = c(100, seq(200, 2000,  by = 150)),
  n_test = 1000,
  p = 20,
  n_levels = c(4),
  beta = "swapping",
  level_beta = "mixed",
  level_probs = "equal",
  encode_type = c("encode_onehot"),
  nn_units = c(128),
  nn_layers = c(3),
  nn_act.fct = c("relu")
)