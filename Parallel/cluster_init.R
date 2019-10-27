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



# create the book_ variables on the master
book_emma  <- janeaustenr::emma
book_pers  <- janeaustenr::persuasion


# create cluster of  3 parallel workers
workers3 <- makeCluster(3)

# using clusterEvalQ to initialize and load clusters with functions and library from the master

clusterEvalQ(workers3, {library(janeaustenr)
  library(stringr)
  austenbooks <- function() austen_books()$book %>%
    unique()  %>% as.character()
  inside <- "Inside Variable"})

clusterExport(workers3, "book_pers")
clusterCall(workers3, function(i) austenbooks()[i], -4)
clusterCall(workers3, function() austen_books())
clusterCall(workers3, function() print(emma))
clusterCall(workers3, function() print(book_pers))
clusterCall(workers3, function() print(inside))


# Stop the cluster
stopCluster(workers3)