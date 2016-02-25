beta <- function(vr,i,j) {
  if (i == 1 & j ==1) {
    return(c(vr[2]/vr[1],vr[1]*(1-(vr[2]/vr[1])^2)))
  } else if (i > j) {
    return(c(beta(vr,i-1,j)[1]-beta(vr,i,i)[1]*beta(vr,i-1,i-j)[1],NA))
  } else {
    sum1 <- 0
    for (k in 1:(i-1)) {
      sum1 <- sum1 + beta(vr,i-1,k)[1]*vr[i-k+1]
    }
    bss <- (vr[i+1] - sum1)/((beta(vr,i-1,i-1)[2]))
    sig <- beta(vr,i-1,i-1)[2]*(1-bss^2)
    return(c(bss,sig))
  }
}
