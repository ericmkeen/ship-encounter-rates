# Functions - summary statistics

library(dplyr)
library(boot)

bootci <- function(vals,B=5000){
  #vals <- val1 ; vals
  samps <- c()
  for(i in 1:B){
    sampi <- sample(vals,size=length(vals),replace=TRUE)
    samps[i] <- mean(sampi)
  }
  lci <- quantile(samps,.025)
  uci <- quantile(samps,.975)
  return(list(lci=lci,uci=uci))
}

pval <- function(val1, val2, B=10000){
  vals1 <- sample(val1,size=B,replace=TRUE)
  vals2 <- sample(val2,size=B,replace=TRUE)
  diffs <- vals1 - vals2

  p1 <- length(which(diffs>0)) / length(diffs) # If significant = val 1 is larger than val2
  p2 <- length(which(diffs<0)) / length(diffs) # If significant = val 2 is larger than val1

  par(mar=c(4.2,4.2,3,1))
  hist(diffs,col="grey70",border="grey60",main="val1 - val2",xlab="Differences")
  abline(v=0,lty=3,col="steelblue")

  return(list(diffs=diffs,p1=p1,p2=p2))
}
