#########################################################
#########################################################
# ENCOUNTER RATES - RESULTS - 2018 and 2030
# Eric M. Keen, v. October 2021
#########################################################
#########################################################

library(rstudioapi)
library(truncnorm)
library(solartime)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("function-results.R")
source("function-stats.R")

#########################################################
#########################################################
# Load data

raw_pass <- readRDS("../results/emp/2018-passenger.rds")
raw_tanker <- readRDS("../results/emp/2018-tanker.rds")
raw_prtank <- readRDS("../results/emp/2030-tanker.rds")


#########################################################
#########################################################
# Process results

pass <- process_results(raw_pass)
tanker <- process_results(raw_tanker)
prtank <- process_results(raw_prtank)


#########################################################
#########################################################
# Histogram

#pdf(file="../figures/er/er.pdf",height=4,width=6)
par(mfrow=c(1,1))
hist_enc(pass$encounters,shade="black",add=FALSE) # Passenger ships
hist_enc(tanker$encounters,shade="darkblue",add=TRUE) # Cargo + tanker
hist_enc(prtank$encounters,shade="darkorange",add=TRUE) # Cargo + tanker - projected
par(mfrow=c(1,1))
#dev.off()

#########################################################
#########################################################
# Comparisons

pval(val1=pass$encounters,
     val2=tanker$encounters)

pval(val1=pass$encounters,
     val2=prtank$encounters)

pval(val1=tanker$encounters,
     val2=prtank$encounters)

#########################################################
#########################################################
# by Eric Keen @ SWAG, (c) 2021
