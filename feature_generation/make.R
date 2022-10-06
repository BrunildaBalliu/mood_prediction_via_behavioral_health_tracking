# Source packages
source("./packages.R")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

## Load R files
lapply(list.files("./R", recursive=TRUE, include.dirs = FALSE, full.names = TRUE), source)

# Set future to use multiple cores - multisession for Windows, multicore for Linux
future::plan(future::multiprocess)

sys_os = Sys.info()['sysname']
print(paste0("Running on OS: ", sys_os))

if(sys_os == "Windows") {  #Running on local PC
  ## LOCAL multicore processing
  options(
    clustermq.scheduler = "multiprocess"
    # Created by drake_hpc_template_file("sge_clustermq.tmpl"):
  )
  drakecache = drake_cache()
} else if(sys_os == "Linux") {  #Running on HPC
  options(
    clustermq.scheduler = "sge",
    clustermq.template = "sge_clustermq_hoffman.tmpl"
  )
  print('Using scratch (temporary) drake cache for processing')
  drakecache = new_cache(path = file.path(Sys.getenv('SCRATCH'),'drake'))
} else{  # All other OSs
  options(
    clustermq.scheduler = "multiprocess"
  )
  drakecache = drake_cache()
}

tmp <- drake_cache(drakecache$path)

print(paste0("Using drake cache located at: ", drakecache$path))



make(
  pull_subject_ids,
  pull_data, 
  preprocess_data,
  extract_features,
  
  
  # Limit the number of branches during development and testing (dynamic branches only)
  # max_expand = 6,
  lock_envir = FALSE,
  
  # keep_going = TRUE,
  
  ## Parallelism - FUTURE
  # parallelism = "future",
  # jobs = 5,

  ## Parallelism - CLUSTERMQ
  parallelism = "clustermq",
  jobs = 40, 
  caching = "worker",

  ## Speed up options
  # verbose = 0L, # Console messages can pile up runtime.
  # log_progress = FALSE, # drake_progress() will be useless.
  # log_build_times = FALSE, # build_times() will be useless.
  # recoverable = FALSE, # make(recover = TRUE) cannot be used later.
  # history = FALSE, # drake_history() cannot be used later.
  # session_info = FALSE, # drake_get_session_info() cannot be used later.

  ## Force rerun
  # trigger = drake::trigger(condition = TRUE), # Force all targets to run
  cache = drakecache,
  log_make = "drake.log"
)
