library(parallel)


# check the number of cores
num_cores  <- detectCores(logical = FALSE)


# create the cluster
cll <- makeCluster(num_cores)


stdev  <- function(series){
  sd(series)
}


number_reps <- 4000
n_number_reps <- 100000

# calculate the standard deviation

std <- clusterApply(cll, x = rep(n_number_reps, number_rep),
                    fun = stdev)

hist(std)