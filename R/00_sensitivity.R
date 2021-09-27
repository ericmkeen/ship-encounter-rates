#########################################################
#########################################################
# PREP DEFAULT PARAMETERS for SENSITIVITY ANALYSIS
#########################################################
#########################################################

library(rstudioapi)
library(truncnorm)
library(solartime)

# Set working directory to folder that this R file is in
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load functions
source("function-simulator.R")
source("function-ais.R")

# Prep ais data
source('00_ais.R')
head(ais)

# Load whale defaults
source('00_whale.R')

# Traffic
traffic <- ais_filter(type_ops=c("passenger ship"),ais=ais)
v.ship = traffic$sog ; v.ship
l.ship = traffic$length ; l.ship
w.ship = .15*l.ship # dist
params.ship <- data.frame(v.ship,l.ship,w.ship) ; params.ship

# Iterations
iterations <- 100

