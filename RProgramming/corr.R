corr <- function(directory, threshold = 0) {
  comp <- complete(directory)
  good <- comp$nobs >= threshold & comp$nobs != 0
  c <- numeric(length = sum(good))
  
  nloop <- 0
  for(i in which(good)) {
    nloop <- nloop + 1
    tab <- read.csv(sprintf('%s/%.3d.csv', directory, i))
    val <- complete.cases(tab)
    c[nloop] = cor(tab$sulfate[val],tab$nitrate[val])
  }
  c
}