complete <- function(directory, id = 1:332) {
  good <- numeric(length = length(id))
  
  nloop <- 0
  for(i in id){
    nloop <- nloop + 1
    tab <- read.csv(sprintf('%s/%.3d.csv', directory, i))
    good[nloop] <- sum(complete.cases(tab))
  }
  
  data.frame(id, nobs = good)
}