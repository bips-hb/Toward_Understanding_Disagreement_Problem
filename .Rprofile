# For e.g. XGBoost
Sys.setenv(OMP_NUM_THREADS = 1)
Sys.setenv(OMP_THREAD_LIMIT = 1)
# Unsure, MKL is an Intel-specific thing
Sys.setenv(MKL_NUM_THREADS = 1)
# Package-specific settings
try(data.table::setDTthreads(1))
