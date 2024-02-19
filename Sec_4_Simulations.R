################################################################################
#                                 Simulations
################################################################################
library("batchtools")
library("data.table")
library("here")
library("ggplot2")
library("cowplot")
library("ggthemes")

# Set seed
set.seed(42)
data.table::setDTthreads(10)

# Global attributes
n_cpus <- 25
n_reps <- 50
reg_name <- "Simulations"
reg_dir <- here(file.path("registries", reg_name))
required_pkgs <- c("innsight", "luz", "torch", "mvtnorm", "cli", "here", 
                   "data.table")

# Create `batchtools` Registry
if (!file.exists(here("registries"))) dir.create(here("registries"))
unlink(reg_dir, recursive = TRUE) # delete old simulations
makeExperimentRegistry(
  file.dir = reg_dir,
  conf.file = here("utils/config.R"),
  packages = required_pkgs,
  source = here(c("utils/algorithms.R", "utils/problems.R",
                  "utils/utils_syn_data.R", "utils/utils_real_data.R",
                  "utils/utils_torch.R")),
  seed = 42)

################################################################################
#                       Simulation: Preprocessing
################################################################################

# Define methods
method_df <- list(
  Gradient = list(
    list(times_input = FALSE),
    list(times_input = FALSE, abs = TRUE),
    list(times_input = TRUE)),
  SmoothGrad = list(list(times_input = FALSE, K = 50, noise_level = 0.2)),
  IntGrad = list(
    list(n = 50, x_ref = "zeros"),
    list(n = 50, x_ref = "mean")),
  ExpGrad = list(list(n = 50)),
  LRP = list(
    list(rule_name = "simple", rule_param = 0),
    list(rule_name = "epsilon", rule_param = 0.1),
    list(rule_name = "alpha_beta", rule_param = 0.5),
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

# Problems --------------------------------------------------------------------- 
addProblem(name = "Prep_cont", fun = syn_numerical, seed = 42)
addProblem(name = "Prep_cat", fun = syn_categorical, seed = 43)

# Algorithms -------------------------------------------------------------------
addAlgorithm(name = "Correlation", fun = get_algo_fun(method_df))

# Experiments ------------------------------------------------------------------

#
# Continuous variables
#
Prep_cont <- expand.grid(
  n = 3000,
  n_test = 1000,
  p = 12,
  beta = "equal",
  mean = "range",
  sample_type = "normal",
  dgp_type = c("linear", "pwlinear", "squared", "cos", "nonlinear"),
  scale_type = c("scale_none", "scale_zscore" , "scale_minmax", "scale_maxabs"),
  nn_units = 256,
  nn_layers = 3,
  nn_act.fct = "relu"
)

#
# Binary/Categorical variables
#
Prep_cat <- expand.grid(
  n = 3000,
  n_test = 1000,
  p = 12,
  n_levels = c(2, 6, 10, 20),
  beta = "equal",
  level_beta = "mixed",
  level_probs = "equal",
  encode_type = c("encode_label", "encode_onehot", "encode_dummy", "encode_effect"),
  nn_units = 256,
  nn_layers = 3,
  nn_act.fct = "relu"
)

prob_design <- list(Prep_cont = Prep_cont, Prep_cat = Prep_cat)

# 
# Define Algorithms and add Experiments
#
algo_design <- list(Correlation = expand.grid(compare_type = "correlation"))
addExperiments(prob_design, algo_design, repls = n_reps)
summarizeExperiments()

# Test jobs --------------------------------------------------------------------
testJob(id = 1)

# Submit -----------------------------------------------------------------------
submitJobs(resources = list(name = reg_name,
                            ncpus = 1, memory = 6000, walltime = 10*24*3600,
                            max.concurrent.jobs = 40))
waitForJobs()

