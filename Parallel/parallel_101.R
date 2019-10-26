library(parallel)
library(microbenchmark)


set.seed(10)

# check the number of cores
# num_cores  <- detectCores(logical = TRUE)
num_cores  <- detectCores(logical = FALSE)

# create the cluster
cll <- makeCluster(num_cores)


stdev  <- function(num){
  series <- rnorm(num)
  sd(series)
}



number_reps <- 400
n_number_reps <- 30000

# calculate the standard deviation
# time the process
t0  <- Sys.time()
std <- clusterApply(cll, x = rep(n_number_reps, number_reps),
                    fun = stdev)

t1  <- Sys.time()

print(t1 - t0)

hist(unlist(std))


# compare with sapply
tk <- rep(n_number_reps, number_reps)

t2  <- Sys.time()
std2 <- sapply(tk, stdev)

t3 <- Sys.time()

print(t3 - t2)

hist(std2)


# compared with for loop
# Compute series to store the number
res <- rep(NA, number_reps)

t4 <- Sys.time()
for(i in seq_len(number_reps)) {
  
  res[i] <- stdev(n_number_reps)
}


t5 <- Sys.time()

print(t5 - t4)

# plot the outcome
hist(res)



# create n, m
n  <- 4e7
m <- 10

# set up the sequential model
sd_squential <- function(n, m){
  
  result <- rep(NA, m)
  
  for(i in seq_len(m)) {
 
    # result[i] <- sd(rnorm(n))
    
    result[i] <- stdev(n)
  }
  
}



# create parallel model
sd_parallel <- function(n, m){
  
  cllr <- rep(n, m)

  clusterApply(cll, x = cllr, fun = stdev)
  
}


# create sapply sequential model
sapply_squential <- function(n, m){
  
  spp <- rep(n, m)
  
  sapply(spp, stdev)
  
}


# compute benchmark
microbenchmark(
  
  sd_squential(n, m),
  sd_parallel(n, m),
  sapply_squential(n, m),
  
  times = 1,
  
  unit = "s"
  
)


