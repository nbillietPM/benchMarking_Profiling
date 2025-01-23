###Parallellization###

library(parallel)

"
The parallel package in R offers two main types of parallelisation:

    FORK: Predominantly used on Unix-based systems (including Linux and Mac OS),
    with FORK a parent process creates child processes that are a copy of the
    parent process. The key advantage of FORK is that it shares memory objects 
    between the processes, which can lead to significant efficiencies when dealing 
    with large objects.

    PSOCK: Used on all systems, including Windows, PSOCK creates a set of independent
    R processes and communicates between them using sockets. Each PSOCK worker is a 
    separate R process, and there is no memory sharing between workers resulting in a 
    higher memory overhead compared to FORK.
"

testCL <- makeCluster(4, type="PSOCK")

sort_vector <- function(n) {
  vec <- sample(1:n, n, replace = TRUE)
  sorted_vec <- sort(vec)
  return(sorted_vec)
}


print("Serial Monte Carlo Pi Estimation")
system.time(lapply(rep(1e6, 4),sort_vector))
"
> system.time(lapply(rep(1e6, 4),sort_vector))
   user  system elapsed 
   0.25    0.02    0.26 
"
print("Parallellized Monte Carlo Pi Estimation")
system.time(parLapply(testCL, rep(1e6, 4), sort_vector))
"
> system.time(parLapply(testCL, rep(1e6, 4), sort_vector))
   user  system elapsed 
   0.02    0.00    0.09 
"



my_list <- 1:10
n <- 3
chunk_indices <- cut(seq_along(my_list), breaks = n, labels = FALSE)
chunk_indices
split_list <- split(my_list, chunk_indices)
print(split_list)
parLapply(testCL, rep(10, 4), sort_vector)

