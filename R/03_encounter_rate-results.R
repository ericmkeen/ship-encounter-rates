#########################################################
#########################################################
# ENCOUNTER RATES - RESULTS - 2018 and 2030
# Eric M. Keen, v. June 2021
#########################################################
#########################################################

library(rstudioapi)
library(truncnorm)
library(solartime)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("function-stats.R")

# Load data
pass <- readRDS("../results/emp/2018-passenger.rds")
tanker <- readRDS("../results/emp/2018-tanker.rds")
prtank <- readRDS("../results/emp/2030-tanker.rds")


#########################################################
#########################################################
# Functions

hist_enc <- function(enc,shade="black",add=FALSE){
  par(mar=c(4.2,4.2,1,.5))
  hist(enc,
       breaks=seq(0,25,by=1),
       col=adjustcolor(shade,alpha.f=.3),
       border=adjustcolor(shade,alpha.f=.1),
       main=NULL,
       xlab="Encounters per 100 trials",
       add=add)
}

hist_prox <- function(prox,shade="black",add=FALSE){
  par(mar=c(4.2,4.2,1,.5))
  hist(prox[prox > 0],
       breaks=seq(0,1500,by=10),
       xlim=c(0,1200),
       ylim=c(0,1800),
       col=adjustcolor(shade,alpha.f=.4),
       border=NA,
       main=NULL,,add=add,
       xlab="Closest proximitiy (m) between whale and vessel")
}


#########################################################
#########################################################
# Results

#pdf(file="../figures/dists/dists.pdf",height=8,width=6)
par(mfrow=c(2,1))
hist_enc(pass$encounter_tally,shade="black",add=FALSE) # Passenger ships
hist_enc(tanker$encounter_tally,shade="darkblue",add=TRUE) # Cargo + tanker
hist_enc(prtank$encounter_tally,shade="darkorange",add=TRUE) # Cargo + tanker - projected

hist_prox(pass$summaries$proximity_m,shade="black",add=FALSE) # Passenger ships
hist_prox(tanker$summaries$proximity_m,shade="darkblue",add=TRUE) # Cargo + tanker
hist_prox(prtank$summaries$proximity_m,shade="darkorange",add=TRUE) # Cargo + tanker - projected

par(mfrow=c(1,1))
#dev.off()

#########################################################
#########################################################
# Stats

mrs <- pass$encounter_tally
mrs <- tanker$encounter_tally
mrs <- prtank$encounter_tally

mean(mrs)
bootci(mrs)
quantile(mrs,0.05)
quantile(mrs,0.95)

pval(val1=pass$encounter_tally,
     val2=tanker$encounter_tally)

pval(val1=pass$encounter_tally,
     val2=prtank$encounter_tally)

pval(val1=tanker$encounter_tally,
     val2=prtank$encounter_tally)

#########################################################
#########################################################
# by Eric Keen @ SWAG, (c) 2021
