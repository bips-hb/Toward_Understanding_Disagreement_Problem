################################################################################
#                       Section 5: Real Data
################################################################################
library("batchtools")
library("data.table")
library("here")

# Set seed
set.seed(42)
data.table::setDTthreads(10)

# Global attributes
n_cpus <- 10
n_reps <- 1
reg_name <- "Real_Data"
reg_dir <- here(file.path("registries", reg_name))
required_pkgs <- c("innsight", "luz", "torch", "mvtnorm", "cli", "here", 
                   "data.table", "mlbench")

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

# Problems ---------------------------------------------------------------------
addProblem(name = "Datasets", fun = get_realdata, seed = 42)

# Algorithms -------------------------------------------------------------------
addAlgorithm(name = "Attribution", fun = apply_methods)

# Experiments ------------------------------------------------------------------

Datasets <- expand.grid(c(
  global_attr,
  ds_name = list(c("boston_housing", "bike_sharing"))
))

prob_design <- list(Datasets = Datasets)

# 
# Define Algorithms and add Experiments
#
algo_design <- list(Attribution = expand.grid(compare_type = "attributions"))
addExperiments(prob_design, algo_design, repls = n_reps)
summarizeExperiments()

# Test jobs --------------------------------------------------------------------
#testJob(id = 1)

# Submit -----------------------------------------------------------------------
submitJobs(resources = list(name = reg_name,
                            ncpus = 1, memory = 6000, walltime = 10*24*3600,
                            max.concurrent.jobs = 40))
waitForJobs()

# Get results ------------------------------------------------------------------
loadRegistry(reg_dir, writeable = FALSE, conf.file = here("utils/config.R"))
res <- reduceResultsDataTable()
jobPars <- batchtools::flatten(getJobPars(ids = res$job.id))

# Correlation results
jobPars <- jobPars[rep(seq_len(nrow(jobPars)), unlist(lapply(res$result, nrow))), ]
result <- cbind(jobPars, rbindlist(res$result))
keep_methods <- c("SHAP", "DeepSHAP (reveal-cancel)", "DeepLift (rescale, zeros)", 
                  "DeepLift (reveal-cancel, mean)",
                  "ExpectedGradient", "IntegratedGradient (mean)",
                  "IntegratedGradient (zeros)", "Gradient x Input", "Gradient",
                  "LRP (alpha-beta, 1.5)", "LRP (alpha-beta, 1)", "LRP (simple, 0)")
result <- result[method_name %in% keep_methods]

res_cor <- lapply(unique(result$job.id), function(i) {
  res <- result[job.id == i]
  names <- unique(res$method_name)
  a <- expand.grid(names, names)
  mat <- sapply(seq_len(nrow(names_grid)), function(k) {
    res_feat <- lapply(unique(res$feature), function(feat) {
      cor(res[method_name == names_grid[k, 1] & feature == feat]$attribution,
          res[method_name == names_grid[k, 2] & feature == feat]$attribution)
    })
    mean(unlist(res_feat))
  })
  cbind(a, cor = mat, dataset = unique(res$ds_name), act.fct = unique(res$act.fct))
})

res_cor <- do.call("rbind", args = res_cor)

# Create plots -----------------------------------------------------------------
library(ggplot2)
library(cowplot)
library(ggthemes)


ggplot(res_cor) +
  geom_raster(aes(x = Var1, y = Var2, fill = cor)) +
  facet_grid(cols = vars(dataset), rows = vars(act.fct)) +
  scale_fill_gradient2(low = scales::muted("blue"), high = scales::muted("red")) +
  theme(axis.text.x = element_text(angle = 45, vjust= 0.8))
ggsave("figures/real_data.pdf", width = 20, height = 20)

# Model error
p1 <- ggplot(res_error_cont) +
  geom_boxplot(aes(x = dgp_type, y = error, colour = as.factor(scale_type))) +
  geom_boxplot(aes(x = dgp_type, y = lm_error), color = "gray", alpha = 0.5) +
  facet_grid(cols = vars(facet_lab)) + 
  theme_bw() +
  geom_hline(yintercept = 1) +
  scale_colour_colorblind() +
  theme(legend.position = "top", legend.title = element_blank()) +
  xlab("DGP") + ylab("Mean Squared Error(MSE)")

p2 <- ggplot(res_error_cat) +
  geom_boxplot(aes(x = as.factor(n_levels), y = error, colour = encode_type)) +
  geom_boxplot(aes(x = as.factor(n_levels), y = lm_error), color = "gray", alpha = 0.5) +
  facet_grid(cols = vars(facet_lab), scales = "free_x", space = "free_x") + 
  theme_bw() +
  geom_hline(yintercept = 1) +
  scale_colour_colorblind() +
  theme(legend.position = "top", legend.title = element_blank()) +
  xlab("Number of Levels") + ylab(NULL)

plot_grid(p1, p2)

ggsave("figures/preprocessing_error.pdf", width = 7.5, height = 5.5)

# Preprocessing results --------------------------------------------------------
p1 <- ggplot(result_cont) +
  geom_boxplot(aes(x = cor, y = method_name, color = scale_type), 
               outlier.size = 0.2, outlier.alpha = 0.1) + 
  facet_grid(cols = vars(dgp_type), rows = vars(method_grp), 
             space = "free_y", scales = "free", switch = "y") +
  scale_colour_colorblind() +
  theme_bw() +
  theme(legend.position = "top", legend.title = element_blank()) +
  geom_vline(xintercept = 0) +
  xlab("Correlation") + ylab("")

p2 <- ggplot(result_cat) +
  geom_boxplot(aes(x = cor, y = method_name, color = encode_type), 
               outlier.size = 0.2, outlier.alpha = 0.1) + 
  facet_grid(cols = vars(facet_lab), rows = vars(method_grp), 
             space = "free_y", scales = "free", switch = "y") +
  scale_colour_colorblind() +
  theme_bw() +
  theme(legend.position = "top", legend.title = element_blank()) +
  geom_vline(xintercept = 0) +
  xlab("Correlation") + ylab("")

plot_grid(p1, p2)

ggsave("figures/preprocessing_cor.pdf", width = 14, height = 8)
