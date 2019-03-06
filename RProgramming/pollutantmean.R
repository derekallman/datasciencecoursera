pollutantmean <- function(directory, pollutant, id = 1:332) {
  psum <- 0
  nval <- 0
  
  for(i in id) {
    tab <- read.csv(sprintf('%s/%.3d.csv', directory, i))
    vec <- tab[pollutant]
    bad <- is.na(vec)
    psum <- psum + sum(vec[!bad])
    nval <- nval + sum(!bad)
  }
  
  psum/nval
}