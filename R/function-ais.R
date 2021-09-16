# AIS formatting functions for encounter simulator

if(FALSE){
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

  # Data
  ais <- read.csv("../data/ais/ais-2018.csv")
  head(ais)
  tail(ais)
  nrow(ais)

  ais <- data.frame(id=ais$ID,
                    type=ais$Type,
                    sog=ais$SOG,
                    length=ais$Length,
                    year=ais$Year,
                    month=ais$month,
                    hour=as.numeric(substr(ais$time,1,2)),
                    doy=as.numeric(strftime(ais$Local.Time,format="%j")),
                    time=as.POSIXct(ais$Local.Time),
                    x=ais$Longitude,
                    y=ais$Latitude)

  head(ais)
  nrow(ais)

  # Add solar elevation data
  ais$sol <- computeSunPositionDoyHour(doy=ais$doy,
                                       hour=ais$hour,
                                       latDeg=ais$y,
                                       longDeg=ais$x,
                                       timeZone=-7)[,3] * (180/pi)
  ais$night <- 0
  ais$night[ais$sol < 0] <- 1

  head(ais)


}

#########################################################
#########################################################

ais_summary <- function(ais){
  # Build summary
  ids <- unique(ais$id) ; length(ids)
  df <- data.frame() ; i=1
  for(i in 1:length(ids)){
    idi <- ids[i] ; idi
    ai <- ais[ais$id==idi,]
    ai <- ai[ai$sog > 1,]
    dfi <- data.frame(id=idi,
                      n_loc=nrow(ai),
                      n_days=length(unique(ai$doy)),
                      type=tolower(ai$type[1]),
                      sog=mean(ai$sog,na.rm=TRUE),
                      length=mean(ai$length,na.rm=TRUE)
    )
    dfi
    df <- rbind(df,dfi)
  }

  df

  # Type summary
  types <- unique(df$type) ; types
  summ <- data.frame() ; i=1
  for(i in 1:length(types)){
    typi <- types[i] ; typi
    dfi <- df[df$type==typi,] ; dfi
    summi <- data.frame(type=typi,
                        n=nrow(dfi))
    summ <- rbind(summ,summi)
  }
  summ

  return(list(id=df,type=summ))
}

#summ <- ais_summary(ais)
#nrow(summ$id)
#summ$type

#########################################################
#########################################################

#type_ops <- c("passenger ship") ; type_ops
#summ <- summ$id

ais_filter <- function(type_ops,ais){
  df <- ais
  matches <- which(tolower(as.character(df$type)) %in% type_ops) ; matches
  dfi <- df[matches,] ; nrow(dfi)
  #dfi <- summ[matches,] ; nrow(dfi)
  dfi <- dfi[dfi$length > 0 & dfi$sog > 0,]
  sogs <- dfi$sog ; unique(sogs)
  lengths <- dfi$length ; unique(lengths)
  return(list(n=length(matches),
              sog=sogs,
              length=lengths))
}

#ais_filter(type_ops="passenger ship",ais=ais,summ=summ$id)

