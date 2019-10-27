library(parallel)
library(microbenchmark)


set.seed(150)

outside <-  "Outside Variable"

printt <-  function(){print(outside)}

# check the number of cores
num_cores  <- detectCores(logical = TRUE)  # ---> 8 cores
# num_cores  <- detectCores(logical = FALSE) # ---> 4 cores


# create the cluster of socket type
# the core are initialize without any variables, they are empty
cll <- makeCluster(num_cores, type = "PSOCK")

# create the cluster of Fork type
# cll <- makeCluster(num_cores, type = "FORK")
# fork clusters are not supported on Windows
# The cluster nodes under FORK are loaded with all global variabales
# but not updated with local variables after initiation

inside <-  "Inside Variable"

str(cll)

# get cluster IDs
clusterCall(cll, Sys.getpid)


# Run the print on each node
# the cores can't find the objects
clusterCall(cll, printt)

clusterCall(cll , function(){print(inside)})

# Stop the cluster
stopCluster(cll)

