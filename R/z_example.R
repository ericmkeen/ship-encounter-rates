#########################################################
#########################################################
# ENCOUNTER RATE - EXAMPLE
# Eric M. Keen, v. June 2021
#########################################################
#########################################################
# Base analysis

library(rstudioapi)
library(truncnorm)
library(solartime)

# Set working directory to folder that this R file is in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("function-simulator.R")
source("function-ais.R")


#########################################################
#########################################################
# Prepare AIS data

# Read in raw data
ais <- read.csv("../data/ais/ais-2018.csv")
head(ais)
tail(ais)
nrow(ais)

# Modify data to have these exact fields and column names
ais <- data.frame(id=ais$ID,
                  type=ais$Type,
                  sog=(ais$SOG*0.5144), # meters per second
                  length=ais$Length,
                  year=ais$Year,
                  month=ais$month,
                  hour=as.numeric(gsub(':','',substr(ais$time,1,2))),
                  doy=as.numeric(strftime(ais$Local.Time,format="%j")),
                  time=as.POSIXct(ais$Local.Time),
                  x=ais$Longitude,
                  y=ais$Latitude)
nrow(ais)

# Remove duplicate entries of unique vessel IDs in the same hour
ais$id_hour <- paste0(ais$id,'-',ais$hour) ; ais$id_hour
ais <- ais[! duplicated(ais$id_hour),]
nrow(ais)
ais$id_hour <- NULL

# Add solar elevation data
ais$sol <- solartime::computeSunPositionDoyHour(doy=ais$doy,
                                 hour=ais$hour,
                                 latDeg=ais$y,
                                 longDeg=ais$x,
                                 timeZone=-7)[,3] * (180/pi)

# Use solar elevation to determine which events occur at night, which during day
ais$night <- 0
ais$night[ais$sol < 0] <- 1
ais$night

# Check out result
head(ais)
nrow(ais)

# Summarise AIS records
summ <- ais_summary(ais)
summ

#########################################################
#########################################################
# Run SIMULATOR - single parameter set

# Parameters ============================================

# Filter traffic by vessel type
traffic <- ais_filter(type_ops=c("cargo ship","tanker"),ais=ais)
traffic

# Define traffic parameters
v.ship = traffic$sog ; v.ship
l.ship = traffic$length ; l.ship
w.ship = .25*l.ship # dist
params.ship <- data.frame(v.ship,l.ship,w.ship) ; params.ship

# Whale parameters
n=1000
v.whale = truncnorm::rtruncnorm(n,0,2.63,1.3611,.5) # meters per sec dist # based on total mean + SD from Hendricks et al 2021
delta.sd = truncnorm::rtruncnorm(n,0,90,30,10) # dist # drawn from Hendricks et al 2021
l.whale = truncnorm::rtruncnorm(n,0,40,18.60375,1.649138) # meters # based mean + SD from Keen et al 2021 (UAS)
w.whale = .2074*l.whale # meters # based on fluke / body length ratio in Keen et al 2021 (UAS)

# Encounter simulator ===================================

B <- 100
df <- data.frame() # details
mrs <- c()
for(b in 1:B){
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
  print(paste0(Sys.time()," | Run ",b," | ",ieb," imminent encounter(s) ..."))
}

#########################################################
#########################################################
# Save results

head(df)
nrow(df)
unique(df$whale_ms)

# Visualize
mrs
par(mar=c(4.2,4.2,1,1)) ; hist(mrs, main='Imminent encounter rates') 
par(mar=c(4.2,4.2,1,1)) ; hist(df$proximity_m,breaks=seq(0,(1.1*max(df$proximity_m)),length=30))
par(mar=c(4.2,4.2,1,1)) ; hist(df$whale_hdg,breaks=seq(0,(1.1*max(df$whale_hdg)),length=30))
par(mar=c(4.2,4.2,1,1)) ; hist(df$ship_hdg,breaks=seq(0,(1.1*max(df$ship_hdg)),length=30))
sum(mrs)

# Save to RDS
getwd()
saveRDS(list(encounters=mrs,
             proximities=df),
        file="../results/emp/example.rds")





