#########################################################
#########################################################
# ENCOUNTER RATE - 2023 - CARGO SHIP + TANKER
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

results_filename <- '../results/emp/2023-tanker.rds'
iterations <- 1000

# Traffic 
traffic <- ais_filter(type_ops=c("cargo ship","tanker"),ais=ais)
traffic

params.ship <- traffic %>% 
  dplyr::select(v.ship = sog,
                l.ship = length) %>%
  dplyr::mutate(w.ship = 0.25*l.ship) 

# Add projected traffic
new_transits <- 750
v.ship <- rep(5.144,times=new_transits)# 10 knots or 0.5144 m/s
l.ship <- rep(300,times=new_transits)
w.ship <- l.ship*0.25
projected.traffic <- data.frame(v.ship,l.ship,w.ship)
params.ship <- rbind(params.ship, projected.traffic)

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
  results_list <- list(encounter_tally = encounter_tally,
                       summaries = summaries,
                       records = records)
  saveRDS(results_list,
          file=results_filename)
  
  # Print status report
  print(paste0(Sys.time()," | Run ",b," | ",tot_encounters," imminent encounter(s) ..."))
  
}

#########################################################
#########################################################
# Visualize & store result

mrs <- readRDS(results_filename)

mrs
par(mar=c(4.2,4.2,1,1)) ; hist(mrs, main='Imminent encounter rates') 
par(mar=c(4.2,4.2,1,1)) ; hist(df$proximity_m,breaks=seq(0,(1.1*max(df$proximity_m)),length=30))
par(mar=c(4.2,4.2,1,1)) ; hist(df$whale_hdg,breaks=seq(0,(1.1*max(df$whale_hdg)),length=30))
par(mar=c(4.2,4.2,1,1)) ; hist(df$ship_hdg,breaks=seq(0,(1.1*max(df$ship_hdg)),length=30))

#########################################################
#########################################################
# by Eric Keen @ SWAG, (c) 2021