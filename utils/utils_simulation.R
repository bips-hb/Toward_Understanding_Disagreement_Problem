################################################################################
#
#               Utility functions and settings for the simulations
#
################################################################################

# Definition of the methods to be applied --------------------------------------
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
  SHAP = list(list(nsim = 20))
)


################################################################################
#                               Experiments
################################################################################

# Preprocessing ----------------------------------------------------------------

# Continuous variables
Prep_cont <- expand.grid(
  n = 3000,
  n_test = 1000,
  p = 12,
  beta = 0.75,
  mean = "range",
  sample_type = "normal",
  dgp_type = c("linear", "pwlinear", "nonlinear"), 
  scale_type = c("scale_none", "scale_zscore", "scale_minmax"), 
  nn_units = c(256),
  nn_layers = c(3),
  nn_act.fct = c("relu")
)

# Binary/Categorical variables
Prep_cat <- expand.grid(
  n = 1500,
  n_test = 1000,
  p = 12,
  n_levels = c(4, 12),
  beta = 0.5,
  level_beta = "mixed",
  level_probs = "equal",
  encode_type = c("encode_label", "encode_onehot", "encode_dummy"),
  nn_units = c(128),
  nn_layers = c(3),
  nn_act.fct = c("relu")
)

# Faithfulness -----------------------------------------------------------------

# Continuous variables
Faith_cont <- expand.grid(
  n = 3000,
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
    n = 1500,
    n_test = 1000,
    p = 12,
    n_levels = c(10),
    beta = "grouped",
    level_beta = "mixed",
    level_probs = "equal",
    encode_type = c("encode_onehot"),
    nn_units = c(128),
    nn_layers = c(3),
    nn_act.fct = c("relu")
  ),
  expand.grid( # Binary variables
    n = 1500,
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

# Robustness -------------------------------------------------------------------

# Continuous variables
Robust_p_cont <- expand.grid(
  n = 1500,
  n_test = 1000,
  p = seq(4, 30, by = 4),
  beta = "first_two",
  mean = "random",
  sample_type = "normal",
  dgp_type = "linear",
  scale_type = c("scale_none", "scale_zscore", "scale_minmax"),
  nn_units = c(256),
  nn_layers = c(3),
  nn_act.fct = c("relu")
)

# Binary/Categorical variables
Robust_p_cat <- expand.grid()
  #n = 1500,
  #n_test = 1000,
  #p = seq(4, 30, by = 4),
  #n_levels = c(4), #, 8, 12),
  #beta = "first_two",
  #level_beta = "mixed",
  #level_probs = "equal",
  #encode_type = "encode_onehot",
  #nn_units = c(128),
  #nn_layers = c(3),
  #nn_act.fct = c("relu")
#)

# Robustness for correlated features
Robust_corr <- expand.grid(
  n = 3000,
  n_test = 1000,
  p = 20,
  corr = c(seq(0, 0.9, by = 0.05), 0.95), 
  beta = "swapping",
  mean = "random",
  sample_type = "normal",
  dgp_type = "linear_and_squared_2",
  scale_type = "scale_zscore",
  nn_units = c(256),
  nn_layers = c(3),
  nn_act.fct = c("relu")
)