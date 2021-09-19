#########################################################
#########################################################
# simulator  - encounter - functions
# Eric M. Keen, v. Sept 2020
#########################################################
#########################################################
# Base analysis

# Set working directory to folder that this R file is in
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#########################################################
#########################################################
# Establish arena

make.arena <- function(new=TRUE,r=564.18958354776){
  library(plotrix)
  #A <- pi*r^2 ; A
  par(mar=c(1,1,1,1))
  if(new){
    suppressWarnings(plot(1~1,col="white",xlim=c(-700,700),ylim=c(-700,700),axes=FALSE,ann=FALSE))
  }
  coords <- draw.circle(x=0,y=0,radius=r,nv=360,border="grey")
  coords
  arena <- data.frame(x=coords$x,y=coords$y)
  head(arena)
  return(arena)
}

# Arena
#coords <- make.arena()

# Buffer zone
#buffer <- make.arena(new=FALSE,r=750)

#########################################################
#########################################################
# Get ship course

#toplot <- TRUE

ship.course <- function(coords,toplot=TRUE){
  goods <- 1:nrow(coords)
  vi <- sample(goods,size=1) ; vi
  vstart <- coords[vi,] ; vstart
  if(toplot){points(vstart,pch=16)}

  diffs <- abs(vi - goods) ; diffs
  goods[diffs < 30] <- NA
  goods <- goods[!is.na(goods)]
  length(goods)

  #if(diffs < 30 | diffs > 330){nos <- c(330:360,1:30)
  #}else{nos <- (vi - 30):(vi+30)} ; nos
  #goods <- goods[-nos] ; length(goods)

  vi <- sample(goods,size=1) ; vi
  vend <- coords[vi,] ; vend
  if(toplot){points(vend,pch=1)}

  x1 <- vstart$x
  x2 <- vend$x
  y1 <- vstart$y
  y2 <- vend$y
  if(toplot){segments(x0=x1,x1=x2,y0=y1,y1=y2)}
  m <- sqrt((x2 - x1)^2 + (y2 - y1)^2)  ; m

  xint <- seq(x1,x2,length=1000) ; xint
  yint <- seq(y1,y2,length=1000) ; yint
  xylm <- lm(yint~xint)
  b <- xylm$coefficients[1] ; b
  a <- xylm$coefficients[2] ; a

  hdg <-  (atan2(y2 - y1, x2 - x1) * 180 / pi)  ; hdg
  #if(hdg < 0){hdg <- 360 + hdg} ; hdg

  return(list(m=m,hdg=hdg,a=a,b=b,x1=x1,x2=x2,y1=y1,y2=y2))
}

#########################################################
#########################################################
# Get ship timeline

#course <- ship.course(coords) ; course
#v = 5 # m/s

ship.timeline <- function(course,v){
  course
  m <- course$m ; m
  x1 <- course$x1
  x2 <- course$x2
  y1 <- course$y1
  y2 <- course$y2
  secs <- round( m / v ) ; secs

  xint <- seq(x1,x2,length=secs) ; xint
  yint <- seq(y1,y2,length=secs) ; yint
  #points(x=xint,y=yint,pch=16,cex=.3)

  t <- 1:secs
  xlm <- lm(xint~t)
  ylm <- lm(yint~t)

  tbuff <- -60:(secs+60)
  xbuff <- predict(xlm,newdata=data.frame(t=tbuff))
  ybuff <- predict(ylm,newdata=data.frame(t=tbuff))
  points(x=xbuff,y=ybuff,pch=16,cex=.2,col="grey")

  ins <- rep(1,times=length(tbuff))
  ins[tbuff < 0] <- 0
  ins[tbuff > secs] <- 0
  ins

  tl <- data.frame(t=tbuff,x=xbuff,y=ybuff,status=ins)
  return(tl)
}

#########################################################
#########################################################
# Draw ellipse function

draw1ellipse <- function(x, y, a = 1, b = 1, angle = 0, segment=NULL,
                         arc.only=TRUE, nv = 100, deg = TRUE, border=NULL, col=NA, lty=1, lwd=1, toplot=TRUE){
  if(is.null(segment)) {
    # set segment to full ellipse if not supplied
    if(deg) segment<-c(0,360)
    else segment<-c(0,2*pi)
  }
  # if input is in degrees
  if (deg) {
    angle <- angle * pi/180
    segment <- segment * pi/180
  }
  z <- seq(segment[1], segment[2], length = nv + 1)
  xx <- a * cos(z)
  yy <- b * sin(z)
  alpha <- xyangle(xx, yy, directed = TRUE, deg = FALSE)
  rad <- sqrt(xx^2 + yy^2)
  xp <- rad * cos(alpha + angle) + x
  yp <- rad * sin(alpha + angle) + y
  if (!arc.only) {
    xp <- c(x, xp, x)
    yp <- c(y, yp, y)
  }
  if(toplot){polygon(xp, yp, border=border, col=col, lty=lty, lwd=lwd, ...)}
  return(list(x=xp,y=yp))
}

## internal function for the internal function
xyangle <-function(x, y, directed = FALSE, deg = TRUE){
  if (missing(y)) {
    y <- x[,2]
    x <- x[,1]
  }
  out <- atan2(y, x)
  if (!directed)
    out <- out %% pi
  if (deg) # if output is desired in degrees
    out <- out * 180 / pi
  out
}


#########################################################
#########################################################
# Add ship dimensions

#sc  <- ship.course(coords) ; sc
#course <- sc
#v <- 5
#tl <- ship.timeline(course=sc,v=v)
#l <- 70
#w <- 20

ship.polys <- function(course,tl,l,w,toplot=TRUE){
  course
  hdg <- course$hdg ; hdg

  ships <- list()
  i=25
  for(i in 1:nrow(tl)){
    tli <- tl[i,] ; tli
    ei <- draw1ellipse(x=tli$x, y=tli$y, a = l/2, b = w/2, angle = hdg, nv = 30,toplot=FALSE)
    ei
    ships[[i]] <- list(t=tli$t,xy=ei)
  }

  if(toplot){polygon(ships[[.5*nrow(tl)]]$xy)}

  return(ships)
}

#tship <- ship.polys(course,tl,l,w)

#########################################################
#########################################################
#########################################################
#########################################################
#########################################################
#########################################################
# WHALES

#########################################################
#########################################################
# Get grid of starts

get.starts <- function(coords){
  x <- seq(-1000,1000,10) ; x
  y <- seq(-1000,1000,10) ; y
  grid <- expand.grid(x,y)
  names(grid) <- c("x","y") ; head(grid)
  nrow(grid)

  library(sp)
  sgrid = SpatialPoints(grid)

  # Arena
  P1 = Polygon(coords)
  Ps1 = Polygons(list(P1), ID = "a")
  SPs = SpatialPolygons(list(Ps1))

  ins <- sp::over(x=sgrid,y=SPs)
  grid <- grid[which(ins==1),] ; nrow(grid)

  #points(x=grid$x,y=grid$y,pch=16,cex=.2)
  return(grid)
}

#whalestarts <- get.starts(coords)


#########################################################
#########################################################
# Initiate whale

initiate.whale <- function(whalestarts,toplot=TRUE){

  # Starting point
  i <- sample(1:nrow(whalestarts),1) ; i
  wi <- whalestarts[i,] ; wi
  if(toplot){points(x=wi$x,y=wi$y,col="red",pch=16)}

  # Initial bearing
  bi <- sample(1:360,1) ; bi

  return(list(x=wi$x,y=wi$y,hdg=bi))
}

#w0 <- initiate.whale(whalestarts) ; w0

#########################################################
#########################################################
# Step whale

#v <- 1.3 # m/s = 4.7 kmh
#delta.mean <- 0 # change in bearing per 1 minute
#delta.sd <- 5 # change in bearing per 1 minute

step.whale <- function(w0,v,delta.mean=0,delta.sd,toplot=TRUE){
  dhdg <- rnorm(1,mean=delta.mean,sd=delta.sd) ; dhdg
  m <- v*60 ; m

  b0 <- w0$hdg  ; b0
  b0 <- b0*(pi/180) ; b0
  dhdg <- dhdg*(pi/180) ; dhdg
  b1 <- b0 + dhdg ; b1

  y <- w0$y + m*sin(b1) ; y
  x <- w0$x + m*cos(b1) ; x

  if(!is.finite(x)){x <- 1}

  xint <- seq(w0$x,x,length=60) ; xint
  yint <- seq(w0$y,y,length=60) ; yint

  if(toplot){points(x=xint,y=yint,pch=1,col="red",cex=.2)}
  if(toplot){points(x=x,y=y,pch=16,col="red",cex=1)}
  w1 <- list(x=xint,y=yint,hdg=(b1*180/pi))
  return(w1)
}

#step.whale(w0=w0,v=1.3,delta.sd=5)


#########################################################
#########################################################
# Whale track

#t <- nrow(tl)
#v <- 1.3 # m/s = 4.7 kmh
#delta.mean <- 0 # change in bearing per 1 minute
#delta.sd <- 5 # change in bearing per 1 minute
#w0

whale.trax <- function(w0,t,v,delta.mean=0,delta.sd){
  wi <- w0
  #steps <- ceiling(t/60)
  steps <- round(t/60)
  trax <- data.frame()
  i=1
  for(i in 1:steps){
    stepi <- step.whale(w0=wi,v=v,delta.sd=delta.sd) ; stepi
    si <- data.frame(step=i,x=stepi$x,y=stepi$y,hdg=stepi$hdg) ; si
    trax <- rbind(trax,si)
    wi <- list(x=tail(stepi$x,1),
               y=tail(stepi$y,1),
               hdg=stepi$hdg); wi
  }

  nrow(trax)
  trax$t <- 1:nrow(trax)
  return(trax)
}

#trax <- whale.trax(w0=w0,t=nrow(tl),v=1.3,delta.sd=5)

#########################################################
#########################################################
# Create whale polygons

#trax <- whale.trax(w0=w0,t=nrow(tl),v=1.3,delta.sd=5)
#l <- 20
#w <- 5

whale.polys <- function(trax,l,w,toplot=TRUE){
  trax

  whales <- list()
  i=25
  for(i in 1:nrow(trax)){
    traxi <- trax[i,] ; traxi
    ei <- draw1ellipse(x=traxi$x, y=traxi$y, a = l/2, b = w/2, angle = traxi$hdg, nv = 30,toplot=FALSE)
    ei
    whales[[i]] <- list(t=traxi$t,xy=ei)
  }

  if(toplot){polygon(whales[[.5*nrow(trax)]]$xy,col="purple")}

  return(whales)
}

#twhale <- whale.polys(trax,l,w)

#########################################################
#########################################################
# Determine overlap of polygons

#tship
#twhale

encounter.test <- function(tship,twhale,polys=FALSE){
  shipts <- c()
  for(i in 1:length(tship)){
    ti <- tship[[i]]$t
    shipts <- c(shipts,ti)
  }
  shipts
  ts <- data.frame(i=1:length(tship),t=shipts) ; ts

  whalets <- c()
  i=1
  for(i in 1:length(twhale)){
    twhale[[i]]
    ti <- twhale[[i]]$t
    whalets <- c(whalets,ti)
  }
  whalets
  tw <- data.frame(i=1:length(twhale),t=whalets) ; tw

  commont <- tw$t[which(tw$t %in% ts$t)] ; commont

  library(sf)

  i=1
  proximities <- c()
  whale_route <- ship_route <- data.frame()
  for(i in 1:length(commont)){
    ti <- commont[i]

    # Format for spatial overlap
    si <- tship[[i]]$xy ; si
    c1 = cbind(si$x, si$y)
    r1 = rbind(c1, c1[1, ])  # join
    sip <- st_polygon(list(r1)) ; sip

    wi <- twhale[[i]]$xy ; wi
    c1 = cbind(wi$x, wi$y)
    r1 = rbind(c1, c1[1, ])  # join
    wip <- st_polygon(list(r1))
    
    # Overlap test
    min_dist <- st_distance(sip,wip)
    proximities[i] <- min_dist
    
    sri <- data.frame(si) %>% 
      dplyr::summarize(x=mean(x),y=mean(y)) %>%
      dplyr::mutate(t=ti,proximity=min_dist)
    ship_route <- rbind(ship_route,sri)
    
    wri <- data.frame(wi) %>% 
      dplyr::summarize(x=mean(x),y=mean(y)) %>%
      dplyr::mutate(t=ti,proximity=min_dist)
    whale_route <- rbind(whale_route,wri)
  }
  
  proximities
  length(proximities)
  length(commont)
  mini <- which.min(proximities) ; mini
  
  # Proximity time series  =====================================================
  proximity_timeseries <- proximities

  # Closest proximity ==========================================================
  closest_distance <- proximities[mini] ; closest_distance

  # Positions at closest proximity  ============================================
  ship_at_closest <- tship[[mini]]$xy ; ship_at_closest
  whale_at_closest <- twhale[[mini]]$xy ; whale_at_closest
  
  # Whale heading & speed at closest proximity =================================

  if(mini==length(twhale)){
    wxy1 <- twhale[[mini-1]]$xy ; wxy1
    wxy2 <- twhale[[mini]]$xy ; wxy2
  }else{
    wxy1 <- twhale[[mini]]$xy ; wxy1
    wxy2 <- twhale[[mini+1]]$xy ; wxy2
  }
  x1 = wxy1$x[1] ; x2 = wxy2$x[1] ; y1 = wxy1$y[1] ; y2 = wxy2$y[1]
  dx <- (x2 - x1) ; dx
  dy <- (y2 - y1) ; dy

  # Get whale speed
  wms <- sqrt(dx^2 + dy^2) ; wms

  # Get whale heading
  library(matlib)
  p1 <- c(0,1) # simulate vertical line
  p2 <- c(dx,dy)
  theta <- as.numeric(angle(p1,p2))
  if(dx  < 0){theta <- 360 - theta}
  wtheta <- theta

  # Plot situation
  if(FALSE){
    scales <- c(abs(dx),abs(dy)) ; scales
    plot(1,type='n',
         xlim=c((x1 - max(scales)),(x1+max(scales))),
         ylim=c((y1 - max(scales)),(y1+max(scales))))
    text(x=x1,y=y1,label='xy1')
    text(x=x2,y=y2,label='xy2')
  }

  # Vessel heading & speed =====================================================

  if(mini==length(tship)){
    wxy1 <- tship[[mini-1]]$xy ; wxy1
    wxy2 <- tship[[mini]]$xy ; wxy2
  }else{
    wxy1 <- tship[[mini]]$xy ; wxy1
    wxy2 <- tship[[mini+1]]$xy ; wxy2
  }
  x1 = wxy1$x[1] ; x2 = wxy2$x[1] ; y1 = wxy1$y[1] ; y2 = wxy2$y[1]
  dx <- (x2 - x1) ; dx
  dy <- (y2 - y1) ; dy

  # Get ship speed
  sms <- sqrt(dx^2 + dy^2) ; sms

  # Get ship heading
  p1 <- c(0,1) # simulate vertical line
  p2 <- c(dx,dy)
  theta <- as.numeric(angle(p1,p2))
  if(dx  < 0){theta <- 360 - theta}
  stheta <- theta

  # Compile summary dataframe ==================================================
  dfi <- data.frame(encounter= ifelse(closest_distance==0,1,0),
                    proximity_m=closest_distance,
                    whale_ms=wms,
                    whale_hdg=wtheta,
                    ship_ms=sms,
                    ship_hdg=stheta,
                    speed_ratio = sms / wms)
  dfi
  
  # Compile proximity object ===================================================
  prox <- list()
  prox$summary <- dfi
  prox$ship_track <- ship_route
  prox$whale_track <- whale_route
  prox$proximity_timeseries <- proximity_timeseries
  prox$time_at_closest <- mini
  prox$ship_at_closest <- ship_at_closest
  prox$whale_at_closest <- whale_at_closest
  #prox$ship_polys <- tship
  #prox$whale_polys <- twhale
  
  
  if(FALSE){
    par(mfrow=c(2,1))
    par(mar=c(4.2,4.2,.5,.5))
    plot(prox$proximity_timeseries,type='l',xlab='Timestamp', ylab='Proximity (m)')
    abline(v=prox$time_at_closest,col='red',lty=3)
      
    make.arena()
    lines(x=prox$ship_track$x,
          y=prox$ship_track$y,
          col='red')
    lines(x=prox$whale_track$x,
          y=prox$whale_track$y,
          col='blue')
    
    points(x=ship_0$x, y=ship_0$y,type='l',col='firebrick')
    points(x=whale_0$x, y=whale_0$y,type='l',col='darkblue',lwd=2)
  }
  
  return(prox)
}



#########################################################
#########################################################
# Wrapper function to iterate

if(FALSE){
  # Parameters
  v.ship = 5 # m/s
  l.ship = 220 # meters
  w.ship = 50 # meters
  params.ship <- data.frame(v.ship,l.ship,w.ship)
  v.whale = 1.5 # m/s
  l.whale = 25 # meters
  w.whale = .2074 # meters
  delta.sd = 50 # degr
  toplot=TRUE
  B <- 10
  keep.records <- 'encounters'
}

encounter_simulator <- function(params.ship,
                                v.whale,l.whale,
                                w.whale,
                                delta.sd,
                                B=100,
                                toplot=TRUE){
  # Setup
  coords <- make.arena()
  whalestarts <- get.starts(coords)

  # Process
  b <- 1
  MR <- data.frame()
  records <- list()
  hits <- list()
  for(b in 1:B){
    coords <- make.arena()
    sc <- ship.course(coords)

    # Setup params
    ship_row <- 1
    if(nrow(params.ship)>1){ship_row <- sample(1:nrow(params.ship),1)}
    vs <- params.ship$v.ship[ship_row]
    lship <- params.ship$l.ship[ship_row]
    wship <- params.ship$w.ship[ship_row]
    v <- v.whale ; if(length(v.whale)>1){v <- sample(v.whale,1)}
    deltasd <- delta.sd ; if(length(delta.sd)>1){deltasd <- sample(delta.sd,1)}
    lwhale <- l.whale ; if(length(l.whale)>1){lwhale <- sample(l.whale,1)}
    wwhale <- lwhale * w.whale

    # Ship course & timing
    tl <- ship.timeline(course=sc,v=vs)
    tship <- ship.polys(course=sc,
                        tl=tl,
                        l=lship,
                        w=wship,
                        toplot=toplot)

    # Whale course & timing
    w0 <- initiate.whale(whalestarts)
    trax <- whale.trax(w0=w0,
                       t=nrow(tl),
                       v=v,
                       delta.sd=deltasd)
    twhale <- whale.polys(trax=trax,
                          l=lwhale,
                          w=wwhale,
                          toplot=toplot)

    # Was there an imminent encounter?
    encounter <- encounter.test(tship,twhale) 
    names(encounter)
    
    # Store run b
    encounter$summary$run <- b
    encounter$summary
    
    # Store summary
    MR <- rbind(MR,encounter$summary)
    
    # Store record
    records[[b]] <- encounter
  }

  result_list <- list(summary=MR,
                      records=records)
  return(result_list)
}


#########################################################
#########################################################
# Encounter analyst

# Takes a prox object

encounter_analyst <- function(prox){
  tship <- prox$ship_polys
  twhale <- prox$whale_polys
  
  par(mfrow=c(1,2))
  
  # Situation
  #make.arena()
  par(mar=c(.5,.5,3,.5))
  plot(1,type='n',xlim=c(-600,600),ylim=c(-600,600),axes=FALSE,ann=FALSE)
  title(main='Overview')
  lines(x=prox$ship_track$x,
        y=prox$ship_track$y,
        col='red')
  lines(x=prox$whale_track$x,
        y=prox$whale_track$y,
        col='blue')
  points(x=prox$ship_at_closest$x, y=prox$ship_at_closest$y,type='l',col='firebrick')
  points(x=prox$whale_at_closest$x, y=prox$whale_at_closest$y,type='l',col='darkblue',lwd=2)
  
  par(mar=c(4.2,4.2,3,.5))
  plot(prox$proximity_timeseries,ylim=c(0,1200),
       type='l',xlab='Timestamp', ylab='Proximity (m)',main='Proximity timeseries')
  abline(v=prox$time_at_closest,col='red',lty=3)
  
  par(mfrow=c(1,1))
}
