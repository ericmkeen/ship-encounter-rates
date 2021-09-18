#########################################################
#########################################################
# ENCOUNTER RATE - 2018 - CARGO SHIP + TANKER
#########################################################
#########################################################

library(rstudioapi)
library(truncnorm)
library(solartime)

# Set working directory to folder that this R file is in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load functions
source("function-simulator.R")
source("function-ais.R")

# Prep ais data
source('00_ais.R')
head(ais)

#########################################################
#########################################################
# PARAMETERS & USER INPUTS 

results_filename <- '../results/emp/2018-tanker-test.rds'
iterations <- 1000

# Traffic 
traffic <- ais_filter(type_ops=c("cargo ship","tanker"),ais=ais)
traffic
nrow(traffic)
v.ship = traffic$sog ; v.ship
l.ship = traffic$length ; l.ship
w.ship = .25*l.ship # dist
params.ship <- data.frame(v.ship,l.ship,w.ship) ; params.ship

# Whale
n=1000 # size of distributions
v.whale = truncnorm::rtruncnorm(n,0,2.63,1.3611,.5) # meters per sec dist # based on total mean + SD from Hendricks et al 2021
delta.sd = truncnorm::rtruncnorm(n,0,90,30,10) # dist # drawn from Hendricks et al 2021
l.whale = truncnorm::rtruncnorm(n,0,40,18.60375,1.649138) # meters # based mean + SD from Keen et al 2021 (UAS)
w.whale = .2074*l.whale # meters # based on fluke / body length ratio in Keen et al 2021 (UAS)

#########################################################
#########################################################
# ENCOUNTER RATE SIMULATOR

encounter_tally <- c() # simple tally of imminent encounters
summaries <- data.frame() # summaries of each iteration
records <- list() # list of detailed info for each imminent encounter

# Setup a multi-pane plot to watch results
par(mfrow=c(3,3))

# Loop through iterations  
for(b in 1:iterations){
  
  # Run simulator
  sim_b <- encounter_simulator(params.ship=params.ship,
                               v.whale=v.whale,
                               l.whale=l.whale,
                               w.whale=w.whale,
                               delta.sd=delta.sd,
                               B=100,
                               toplot=FALSE)
  
  # Summary of each iteration
  summary_b <- sim_b$summary
  summary_b$iteration <- b
  summary_b
  
  # Add to summary df for all iterations
  summaries <- rbind(summaries, summary_b)
  
  # Note number of imminent encounters that occurred in this iteration
  encounters <- which(summary_b$encounter==1)
  tot_encounters <- length(encounters) 
  encounter_tally <- c(encounter_tally,tot_encounters)
  
  # Details for each iteration
  records_b <- sim_b$records
  
  # Get records for runs that results in an encounter
  if(tot_encounters > 0){
    encounter_records <- records_b[encounters]
    length(encounter_records)
    records <- c(records, encounter_records)
  }
  
  # Save results to RDS in each iteration to ensure work is never lost
  results_list <- list(encounter_tally,
                       summaries,
                       records)
  saveRDS(results_list,
          file=results_filename)
  
  # Print status report
  print(paste0(Sys.time()," | Run ",b," | ",tot_encounters," imminent encounter(s) ..."))
  
}

#########################################################
#########################################################
# Visualize & store result

mrs <- readRDS(results_filename)
head(mrs$encounters)
head(mrs$proximities)

mrs
par(mfrow=c(2,1))
par(mar=c(4.2,4.2,1,1)) ; hist(mrs$encounters, main='Imminent encounter rates') 
par(mar=c(4.2,4.2,1,1)) ; hist(mrs$proximities$proximity_m,breaks=30)

#########################################################
#########################################################
# by Eric Keen @ SWAG, (c) 2021