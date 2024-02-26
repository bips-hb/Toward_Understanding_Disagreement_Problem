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
n_cpus <- 20
n_reps <- 10
reg_name <- "Simulations_final2"
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

# Load experiments
source(here("utils/utils_simulation.R"))

################################################################################
#                       Simulation: Preprocessing
################################################################################

# Select methods
# (remove model-agnostic approaches)
prep_method_df <- METHOD_DF
prep_method_df[["SHAP"]] <- NULL

# Problems --------------------------------------------------------------------- 
addProblem(name = "Prep_cont", fun = syn_numerical, seed = 42)
addProblem(name = "Prep_cat", fun = syn_categorical, seed = 43)

# Algorithms -------------------------------------------------------------------
addAlgorithm(name = "Correlation", fun = apply_methods)

# Combine all to the problem design
Prep_prob_design <- list(Prep_cont = Prep_cont, Prep_cat = Prep_cat)

# Define Algorithms and add Experiments
Prep_algo_design <- list(
  Correlation = expand.grid(
    compare_type = "correlation",
    method_df = list(prep_method_df)))

################################################################################
#                       Simulation: Faithfulness
################################################################################

# Apply all methods
faith_method_df <- METHOD_DF

# Problems --------------------------------------------------------------------- 
addProblem(name = "Faith_cont", fun = syn_numerical, seed = 44)
addProblem(name = "Faith_cat", fun = syn_categorical, seed = 45)

# Algorithms -------------------------------------------------------------------
addAlgorithm(name = "Correlation", fun = apply_methods)

# Combine all to the problem design
Faith_prob_design <- list(Faith_cont = Faith_cont, Faith_cat = Faith_cat)

# Define Algorithms and add Experiments
Faith_algo_design <- list(
  Correlation = expand.grid(
    compare_type = "correlation", 
    method_df = list(faith_method_df)))

################################################################################
#                       Simulation: Robustness
################################################################################

# Select methods
# (remove model-agnostic approaches)
robust_method_df <- METHOD_DF
robust_method_df[["SHAP"]] <- NULL

# Problems --------------------------------------------------------------------- 
addProblem(name = "Robust_p_cont", fun = syn_numerical, seed = 46)
addProblem(name = "Robust_p_cat", fun = syn_categorical, seed = 47)
addProblem(name = "Robust_corr", fun = syn_numerical, seed = 48)

# Algorithms -------------------------------------------------------------------
addAlgorithm(name = "Uninformative", fun = apply_methods)

# Combine all to the problem design
Robust_prob_design <- list(Robust_p_cont = Robust_p_cont, 
                           Robust_p_cat = Robust_p_cat)

# Define Algorithms and add Experiments
Robust_algo_design <- list(
  Uninformative = expand.grid(compare_type = "uninformative", 
                              method_df = list(robust_method_df)))

################################################################################
#                         Add all experiments 
###############################################################################

#addExperiments(Prep_prob_design, Prep_algo_design, repls = n_reps)
#addExperiments(Faith_prob_design, Faith_algo_design, repls = n_reps)
addExperiments(Robust_prob_design, Robust_algo_design, repls = n_reps)
addExperiments(list(Robust_corr = Robust_corr), Robust_algo_design, repls = n_reps)

summarizeExperiments()

# Test jobs --------------------------------------------------------------------
#testJob(id = 277)

# Submit -----------------------------------------------------------------------
submitJobs(resources = list(name = reg_name,
                            ncpus = 1, memory = 6000, walltime = 10*24*3600,
                            max.concurrent.jobs = 40))
waitForJobs()

################################################################################
#                     Show results and create outputs
################################################################################
library("ggplot2")
library("cowplot")
library("ggthemes")
library("sysfonts")
library("showtext")

# Load LaTeX font (Latin modern), only relevant for setting the fonts as in the
# paper, but requires the latinmodern-math font
font_add("LModern_math", here("utils/latinmodern-math.otf"))
showtext_auto()

source(here("utils/utils_figures.R"))

# Set ggplot2 theme
theme_set(
  theme_bw(base_size = 18, base_family = "LModern_math", base_line_size = 1) +
    theme(
      axis.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold", size = 16),
      axis.text.y = element_text(size = 13)
    )
)

# Load and prepare results from registry ---------------------------------------
res <- get_and_prepare_results(reg_dir, here("utils/config.R"))
res_error <- res$res_error

# Preprocessing ----------------------------------------------------------------

# Crate and save the model error plots
create_modelerror_fig(res_error[problem %in% c("Prep_cont", "Prep_cat")], "Prep")

# Create results plot
res_prep <- res$res_corr[problem %in% c("Prep_cont", "Prep_cat")]
create_preprocess_fig(res_prep)

# Faithfulness -----------------------------------------------------------------

# Crate and save the model error plots
create_modelerror_fig(res_error[problem %in% c("Faith_cont", "Faith_cat")], "Faith")

# Create results plot
res_faith <- res$res_corr[problem %in% c("Faith_cont", "Faith_cat")]
create_faithfulness_fig(res_faith)

# Robustness: Number of uninformative variables --------------------------------

# Crate and save the model error plots
create_modelerror_fig(res_error[problem %in% c("Robust_p_cont", "Robust_p_cat")], "Robust")

# Create results plot
res_robust_p <- res$res_uninform[problem %in% c("Robust_p_cont")]
create_robustness_p_fig(res_robust_p)


res_robust_corr <- res$res_uninform[problem %in% c("Robust_corr", "Robust_corr")]
create_robustness_corr_fig(res_robust_corr)
