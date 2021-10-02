#########################################################
#########################################################
# ENCOUNTER RATE - 2018 - PASSENGER SHIPS
#########################################################
#########################################################

library(rstudioapi)
library(truncnorm)
library(solartime)
library(dplyr)
library(DescTools)

# Set working directory to folder that this R file is in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load functions
source("function-simulator.R")
source("function-ais.R")

# Prep ais data
source('00_ais.R')
head(ais)

# Load whale defaults
source('00_whale.R')

# Load whale defaults
source('00_whale.R')


#########################################################
#########################################################
# PARAMETERS & USER INPUTS

results_filename <- '../results/emp/2018-passenger.rds'
iterations <- 500

# Filter traffic
traffic <- ais_filter(type_ops=c("passenger ship"),ais=ais)
traffic

params.ship <- traffic %>%
  dplyr::select(v.ship = sog,
                l.ship = length) %>%
  dplyr::mutate(w.ship = 0.15*l.ship)


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
                               save_records=FALSE,
                               speedy=TRUE,
                               verbose=FALSE,
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

  # Refresh results storage object
  results_list <- list(encounter_tally = encounter_tally,
                       summaries = summaries)

  if(save_records){
    # Details for each iteration
    records_b <- sim_b$records

    # Get records for runs that results in an encounter
    if(tot_encounters > 0){
      encounter_records <- records_b[encounters]
      length(encounter_records)
      records <- c(records, encounter_records)
    }

    results_list$records <- records
  }

  # Save results object to RDS in each iteration to ensure work is never lost
  saveRDS(results_list,
          file=results_filename)

  # Print status report
  message(Sys.time()," | Iteration ",b," | ",tot_encounters," imminent encounter(s) ...")

}

#########################################################
#########################################################
# by Eric Keen @ SWAG, (c) 2021
