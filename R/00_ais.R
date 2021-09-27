#########################################################
#########################################################
# PREP AIS DATA
#########################################################
#########################################################

library(rstudioapi)
library(truncnorm)
library(solartime)

# Read in data
ais <- read.csv("../data/ais/ais-2018.csv")
head(ais)
tail(ais)
nrow(ais)

# Format final dataframe
ais <- data.frame(id=ais$ID,
                  type=ais$Type,
                  sog=(ais$SOG*0.5144), # meters per second
                  length=ais$Length,
                  year=ais$Year,
                  month=ais$month,
                  hour=as.numeric(gsub(":","",substr(ais$time,1,2))),
                  doy=as.numeric(strftime(ais$Local.Time,format="%j")),
                  time=as.POSIXct(ais$Local.Time),
                  x=ais$Longitude,
                  y=ais$Latitude)

head(ais)
nrow(ais)

# Remove duplicate entries of unique vessel IDs in the same hour
ais$id_hour <- paste0(ais$id,'-',ais$hour) ; ais$id_hour
ais <- ais[! duplicated(ais$id_hour),]
nrow(ais)
ais$id_hour <- NULL

# Add solar elevation data
ais$sol <- computeSunPositionDoyHour(doy=ais$doy,
                                     hour=ais$hour,
                                     latDeg=ais$y,
                                     longDeg=ais$x,
                                     timeZone=-7)[,3] * (180/pi)
ais$night <- 0
ais$night[ais$sol < 0] <- 1

head(ais)

#########################################################
#########################################################
# by Eric Keen @ SWAG, (c) 2021