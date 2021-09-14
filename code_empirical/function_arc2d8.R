
arc2d8 <- function(x) {
  z <- 0:7
  fdir_d8 <- 2^z
  fdir_arc <- fdir_d8[c(8, 1:7)]
  
  for(i in seq_len(length(fdir_d8))) {
    x[x == fdir_arc[i]] <- fdir_d8[i]
  }
  
  return(x)  
}