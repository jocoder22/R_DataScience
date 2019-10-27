library(parallel)
library(microbenchmark)
library(janeaustenr)

outside <-  "Outside Variable"
printt <-  function(x){print(x)}

# create cluster of parallel workers
workers <- makeCluster(detectCores(logical = TRUE))

str(workers)
# AS cluster especially of type = "PSOCK" the default is created with empty environmental variable
# we need to initialise or load them with variable, functions, and library. This decrease the number
# of communication between master and workers, increasing efficiency

# using clusterExport to initialize and load clusters with variable from the master

clusterExport(workers, "outside")

clusterCall(workers, function()print(outside))




# using clusterCall to initialize and load clusters with functions and library from the master

clusterCall(workers, function()library(janeaustenr))
clusterCall(workers, function(i) emma[i], 15:18)


# using clusterEvalQ to initialize and load clusters with functions and library from the master

clusterEvalQ(workers, function()library(janeaustenr))
clusterCall(workers, function(i) emma[i], 15:18)


# Stop the cluster
stopCluster(workers)


