################################################################################
#              SCRIPT FOR REPRODUCING THE FIGURES IN THE PAPER                 #
#              "Toward Understanding the Disagreement Problem in               #
#                   Neural Network Feature Attribution"                        #
#                                                                              #
#                               SECTION 4:                                     #
#               "Do Feature Attribution Methods Attribute?"                    #
#                                                                              #
################################################################################
library("batchtools")
library("data.table")
library("here")

# Set seed
set.seed(42)
data.table::setDTthreads(10)

# Global attributes
n_cpus <- 35
reg_name <- "Sec_4_Simulations"
reg_dir <- here(file.path("registries", reg_name))
required_pkgs <- c("innsight", "luz", "torch", "mvtnorm", "cli", "here", 
                   "data.table")

# !!! FOR REPRODUCING THE RESULTS WITHPUT RUNNING THE SIMULATIONS AGAIN, SKIP
# !!! THE FOLLOWING PART AND GO TO THE END OF THIS SKRIPT 
# (i.e., skip to this <==========)

# Create `batchtools` registry
if (!file.exists(here("registries"))) dir.create(here("registries"))
#unlink(reg_dir, recursive = TRUE) # delete old simulations
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
#                     4.1 Impact of Data Preprocessing
################################################################################

# Select methods (remove SHAP)
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
#                       4.2 Faithfulness of Effects
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
#               4.3 Beyond Feature Attribution Toward Importance
################################################################################

# Select methods
beyondA_method_df <- METHOD_DF

# Problems --------------------------------------------------------------------- 
addProblem(name = "BeyondA_cont", fun = syn_numerical, seed = 46)
addProblem(name = "BeyondA_cat", fun = syn_categorical, seed = 47)

# Algorithms -------------------------------------------------------------------
addAlgorithm(name = "F1_score", fun = apply_methods)

# Combine all to the problem design
BeyondA_prob_design <- list(BeyondA_cont = BeyondA_cont, BeyondA_cat = BeyondA_cat)

# Define Algorithms and add Experiments
BeyondA_algo_design <- list(
  F1_score = expand.grid(compare_type = "F1_score",
                         method_df = list(beyondA_method_df)))

################################################################################
#                         Add all experiments 
################################################################################

addExperiments(Prep_prob_design, Prep_algo_design, repls = 200)
addExperiments(Faith_prob_design, Faith_algo_design, repls = 200)
addExperiments(BeyondA_prob_design, BeyondA_algo_design, repls = 500)

summarizeExperiments()

# Test jobs --------------------------------------------------------------------
testJob(id = 1)

# Submit -----------------------------------------------------------------------
submitJobs(resources = list(name = reg_name,
                            ncpus = 1, memory = 6000, walltime = 10*24*3600,
                            max.concurrent.jobs = 40))
waitForJobs()

################################################################################
#                               Create figures
################################################################################
library("ggplot2")
library("cowplot")
library("ggsci")
library("envalysis")
library("sysfonts")
library("showtext")
library("kableExtra")

# Load LaTeX font (Latin modern), only relevant for setting the fonts as in the
# paper, but requires the latinmodern-math font
font_add("LModern_math", here("utils/latinmodern-math.otf"))
showtext_auto()

source(here("utils/utils_figures.R"))

# Load and prepare results from registry ---------------------------------------
res <- get_and_prepare_results(reg_dir, here("utils/config.R"))

# USE The FOLLOWING CODE, IF YOU WANT TO USE OUR SAVED RESULTS <========================
#res <- readRDS(here("results/results.rds"))

# Save results
saveRDS(res, file = here("results.rds"))

# 4.1 Impact of Data Preprocessing ---------------------------------------------
res_prep <- res$res_corr[problem %in% c("Prep_cont", "Prep_cat")]
create_preprocess_fig(res_prep)

# 4.2 Faithfulness of Effects --------------------------------------------------
res_faith <- res$res_corr[problem %in% c("Faith_cont", "Faith_cat")]
create_faithfulness_fig(res_faith)

# 4.3 Beyond Feature Attribution -----------------------------------------------
res_beyond <- res$res_f1[problem %in% c("BeyondA_cont", "BeyondA_cat")]
create_beyond_attribution_fig(res_beyond)

# Appendix: Show model performance ---------------------------------------------

# Create Table 1
table_1 <- create_table_1(res$res_error)
save_kable(table_1, file = here("figures/Sec_App_table_1.html"))

# Create Table 2
table_2 <- create_table_2(res$res_error)
save_kable(table_2, file = here("figures/Sec_App_table_2.html"))


