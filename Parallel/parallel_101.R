library(parallel)


# check the number of cores
num_cores  <- detectCores(logical = FALSE)


# create the cluster
cll <- makeCluster(num_cores)


stdev  <- function(num){
  series <- rnorm(num)
  sd(series)
}


number_reps <- 4000
n_number_reps <- 100000

# calculate the standard deviation
# time the process
t0  <- Sys.time()
std <- clusterApply(cll, x = rep(n_number_reps, number_reps),
                    fun = stdev)

t1  <- Sys.time()

print(t1 - t0)

hist(unlist(std))
