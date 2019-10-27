library(parallel)
library(microbenchmark)


set.seed(150)

# check the number of cores
num_cores  <- detectCores(logical = TRUE)  # ---> 8 cores
# num_cores  <- detectCores(logical = FALSE) # ---> 4 cores


# create the cluster
cll <- makeCluster(num_cores)

str(cll)

# get cluster IDs
clusterCall(cll, Sys.getpid)

# Stop the cluster
stopCluster(cll)
