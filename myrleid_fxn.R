## Function based on the data.table::rleid function, but instead changes the count after a specified number of steps (n)
# Generates an ID column which repeats the same ID value over a specified number of changes in the value of an input vector - originally written to identify relative week IDs based on any possible 7 day of the year sequences 

myrleid <- function(x, n) {
  y <- rle(x)$lengths
  xx <- aggregate(y,
                  # list(rep(1:(length(y) %/% n+1), each=n, len=length(y))),
                  list(gl(ceiling(length(y)/n), n)[1:length(y)]),
                  sum)[,2]
  xxx <- rep(seq_along(xx), times=xx)
  return(xxx)
}


