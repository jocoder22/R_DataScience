library(parallel)
library(microbenchmark)


set.seed(150)

outside <-  "Outside Variable"

printt <-  function(){print(outside)}
printt <-  function(){print(inside)}
# check the number of cores
num_cores  <- detectCores(logical = TRUE)  # ---> 8 cores
# num_cores  <- detectCores(logical = FALSE) # ---> 4 cores


# create the cluster
cll <- makeCluster(num_cores, type = "PSOCK")

inside <-  "Inside Variable"

str(cll)

# get cluster IDs
clusterCall(cll, Sys.getpid)


clusterCall(cll, printt)

# Stop the cluster
stopCluster(cll)




