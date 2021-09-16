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

results_filename <- '../results/emp/2018-tanker.rds'
iterations <- 1000

# Traffic 
traffic <- ais_filter(type_ops=c("cargo ship","tanker"),ais=ais)
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

df <- data.frame() # details
mrs <- c() # imminent encounters
for(b in 1:iterations){
  mr <- encounter_simulator(params.ship=params.ship,
                            v.whale=v.whale,
                            l.whale=l.whale,
                            w.whale=w.whale,
                            delta.sd=delta.sd,
                            B=100,
                            toplot=FALSE)
  mr
  df <- rbind(df, mr)
  ieb <- length(which(mr$proximity_m <= 0)) # store N imminent encounters for this b
  mrs <- c(mrs,ieb) # store N imminent encounters
  
  # Save to RDS
  getwd()
  saveRDS(list(encounters=mrs,
               proximities=df),
          file=results_filename)
  print(paste0(Sys.time()," | Run ",b," | ",ieb," imminent encounter(s) ..."))
}

#########################################################
#########################################################
# Visualize & store result

mrs
par(mar=c(4.2,4.2,1,1)) ; hist(mrs, main='Imminent encounter rates') 
par(mar=c(4.2,4.2,1,1)) ; hist(df$proximity_m,breaks=seq(0,(1.1*max(df$proximity_m)),length=30))
par(mar=c(4.2,4.2,1,1)) ; hist(df$whale_hdg,breaks=seq(0,(1.1*max(df$whale_hdg)),length=30))
par(mar=c(4.2,4.2,1,1)) ; hist(df$ship_hdg,breaks=seq(0,(1.1*max(df$ship_hdg)),length=30))

#########################################################
#########################################################
# by Eric Keen @ SWAG, (c) 2021