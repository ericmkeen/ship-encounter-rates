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

ship.course <- function(coords,toplot=FALSE){
  ymin_i <- which.min(coords$y) ; ymin_i
  ymax_i <- which.max(coords$y) ; ymax_i
  x1 <- coords$x[ymin_i]
  x2 <- coords$x[ymax_i]
  y1 <- coords$y[ymin_i]
  y2 <- coords$y[ymax_i]
  if(toplot){segments(x0=x1,x1=x2,y0=y1,y1=y2)}
  m <- y2 - y1
  hdg <- 0
  return(list(m=m,hdg=hdg,
              x1=x1,x2=x2,
              y1=y1,y2=y2))
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

  tl <- data.frame(t=tbuff,x=xbuff,y=ybuff,status=ins,v=v)
  #tl
  return(tl)
}

#########################################################
#########################################################
# Draw ellipse function

ellipse <- function(x,y,
                    width,height=width,theta=0,
                    npoints=10,plot=T) {
  theta <- -1*theta*(pi/180)
  a <- width/2
  b <- height/2
  xcoord <- seq(-a,a,length=npoints)
  ycoord.neg <- sqrt(b^2*(1-(xcoord)^2/a^2))
  ycoord.pos <- -sqrt(b^2*(1-(xcoord)^2/a^2))
  xx <- c(xcoord,xcoord[npoints:1])
  yy <- c(ycoord.neg,ycoord.pos)
  x.theta <- xx*cos(2*pi-theta)+yy*sin(2*pi-theta)+x
  y.theta <- yy*cos(2*pi-theta)-xx*sin(2*pi-theta)+y
  if(plot){polygon(x.theta,y.theta,density=0)}
  return(list(coords=data.frame(x=x.theta,y=y.theta),
                   center=c(x,y),
                   theta=theta))
}

#ellipse(x=0, y=-500, width=.15*200, height=200, theta=310)

#########################################################
#########################################################
# Add ship dimensions

#sc  <- ship.course(coords) ; sc
#course <- sc
#v <- 5
#tl <- ship.timeline(course=sc,v=v)
#l <- 200
#w <- 0.15*l

ship.polys <- function(course,tl,l,w,toplot=FALSE){
  course
  hdg <- course$hdg ; hdg
  ships <- list()
  i=100
  for(i in 1:nrow(tl)){
    tli <- tl[i,] ; tli
    x <- tli$x
    y <- tli$y
    ei <- ellipse(x=x, y=y, width=w, height=l, theta=hdg, plot=F)
    ships[[i]] <- list(t=tli$t,
                       v=tli$v,
                       hdg=hdg,
                       center=ei$center,
                       xy=ei$coords)
  }

  return(ships)
}

#make.arena()
#tship <- ship.polys(course,tl,l,w,toplot=TRUE)

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

initiate.whale <- function(whalestarts,toplot=FALSE){

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

step.whale <- function(w0,v,delta.mean=0,delta.sd,toplot=FALSE){
  m <- v*60 ; m
  dhdg <- rnorm(1,mean=delta.mean,sd=(delta.sd*(pi/180))) ; dhdg

  b0 <- w0$hdg  ; b0
  b0 <- b0 * (pi/180)
  b1 <- b0 + dhdg
  #b1*(180/pi)

  dy <- sin(b1+pi/2)*m ; dy
  dx <- cos(2*pi - b1 + pi/2)*m ; dx

  x <- w0$x + dx
  y <- w0$y + dy

  if(!is.finite(x)){x <- 1}

  xint <- seq(w0$x,x,length=60) ; xint
  yint <- seq(w0$y,y,length=60) ; yint

  if(toplot){points(x=xint,y=yint,pch=1,col="red",cex=.2)}
  if(toplot){points(x=x,y=y,pch=16,col="red",cex=1)}
  w1 <- list(x=xint,
             y=yint,
             hdg=(b1*(180/pi)) %% 360,
             v=v)
  #w1 <- list(x=xint,y=yint,hdg=(b1*180/pi),v=v)
  return(w1)
}

#step.whale(w0=w0,v=1.3,delta.sd=5)


#########################################################
#########################################################
# Whale track

#t <- 3000
#v <- 1.5 # m/s
#delta.mean <- 0 # change in bearing per 1 minute
#delta.sd <- 70 # change in bearing per 1 minute
#w0

whale.trax <- function(w0,t,v,delta.mean=0,delta.sd,toplot=FALSE){
  wi <- w0
  if(toplot){points(wi$x,wi$y,col='red',cex=1)}
  steps <- round(t/60)
  trax <- data.frame()
  i=1
  for(i in 1:steps){
    stepi <- step.whale(w0=wi,v=v,delta.sd=delta.sd) ; stepi
    si <- data.frame(step=i,x=stepi$x,y=stepi$y,hdg=stepi$hdg, v=v) ; si
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

whale.polys <- function(trax,l,w,toplot=FALSE){
  #trax
  whales <- list()
  i=25
  for(i in 1:nrow(trax)){
    traxi <- trax[i,] ; traxi
    x <- traxi$x
    y <- traxi$y
    hdg <- traxi$hdg ; hdg
    ei <- ellipse(x=x, y=y, width=w, height=l, theta=hdg, plot=F)
    whales[[i]] <- list(t=traxi$t,
                        v=traxi$v,
                        hdg=hdg,
                        center=ei$center,
                        xy=ei$coords)
  }
  return(whales)
}

#twhale <- whale.polys(trax,l,w,toplot=TRUE)

#########################################################
#########################################################

find_bow <- function(ei,hypo=300, toplot=FALSE){

  #ei <- tship[[1]]
  #hypo <- 300

  # Build heading arrow
  ctr <- ei$center
  x <- ctr[1]
  y <- ctr[2]
  theta <- (ei$hdg * (pi/180))

  dx <- cos(2*pi - theta + pi/2)*hypo ; dx
  dy <- sin(theta + pi/2)*hypo ; dy
  hdg_x <- seq(x,(x+dx),length=1000)
  hdg_y <- seq(y,(y+dy),length=1000)
  if(toplot){lines(hdg_x,hdg_y,col='grey')}

  # Find bow
  pythag <- function(c1,c2){
    c2 <- data.frame(x=hdg_x, y=hdg_y)
    dx <- abs(c1[1] - c2[1])
    dy <- abs(c1[2] - c2[2])
    sqrt(dx^2 + dy^2)
  }
  ds <- apply(ei$xy,1,pythag)
  bow_i <- which.min(unlist(lapply(ds,min))) ; bow_i
  bow <- ei$xy[bow_i,]
  if(toplot){points(x=bow$x, y=bow$y, col='firebrick', pch=16)}

  ei_return <- list(center=ctr,
                    hdg=ei$hdg,
                    hdg_line=data.frame(x=hdg_x, y=hdg_y),
                    theta=theta,
                    bow=bow)
  return(ei_return)
}

#########################################################
#########################################################
# Determine overlap of polygons

if(FALSE){
  coords <- make.arena()
  course <- ship.course(coords) ; course
  v = 5 # m/s
  sc  <- ship.course(coords) ; sc
  tl <- ship.timeline(course=sc,v=v)
  l <- 200
  w <- 0.15*l
  tship <- ship.polys(sc,tl,l,w,toplot=TRUE)
  whalestarts <- get.starts(coords)
  w0 <- initiate.whale(whalestarts) ; w0
  v <- 1.3 # m/s = 4.7 kmh
  delta.mean <- 0 # change in bearing per 1 minute
  delta.sd <- 30 # change in bearing per 1 minute
  trax <- whale.trax(w0=w0,t=nrow(tl),v=v,delta.sd=delta.sd)
  twhale <- whale.polys(trax,l,w)
}


encounter.test <- function(tship,twhale){

  library(sf)

  # Ship timestamps
  shipts <- sapply(tship,'[[',1)
  ts <- data.frame(i=1:length(tship),t=shipts)
  ts

  # Whale timestamps
  whalets <- sapply(twhale,'[[',1)
  tw <- data.frame(i=1:length(twhale),t=whalets)
  tw

  # Which timestamps do the two things share?
  commont <- tw$t[which(tw$t %in% ts$t)] ; commont
  commont

  # Subset tracks to those common timestamps
  tsi <- ts$i[ts$t %in% commont]
  tsi <- tship[tsi]

  twi <- tw$i[tw$t %in% commont]
  twi <- twhale[twi]

  #plot(1,type='n',xlim=c(-700,700),ylim=c(-700,700)) ; for(i in 1:length(twi)){lines(x=twi[[i]]$xy$x, y=twi[[i]]$xy$y,col=adjustcolor('black',alpha.f=.1))}

  # Get centers of ship and whales at each t
  ship_centers <- sapply(tsi,'[[',4)
  whale_centers <- sapply(twi,'[[',4)

  # Extract the ellipse shapes for those timestamps
  spoly <- lapply(tsi,function(tship){
    si <- tship$xy
    c1 = cbind(si$x, si$y)
    r1 = rbind(c1, c1[1, ])
    sip <- st_polygon(list(r1)) ; sip
    return(sip)
  })

  wpoly <- lapply(twi,function(twhale){
    si <- twhale$xy
    c1 = cbind(si$x, si$y)
    r1 = rbind(c1, c1[1, ])
    wip <- st_polygon(list(r1)) ; wip
    return(wip)
  })

  # Get distance between ship and vessel at each t
  proximities <- mapply(st_distance,spoly,wpoly)

  #par(mar=c(4,4,.5,.5))
  #plot(proximities,type='l')

  # Create encounter summary
  enc_summary <- data.frame(t = sapply(tsi,'[[',1),
                            meters = proximities,
                            ship_hdg = sapply(tsi,'[[',3),
                            ship_v = sapply(tsi,'[[',2),
                            ship_x = round(ship_centers[1,],3),
                            ship_y = ship_centers[2,],
                            whale_hdg = sapply(twi,'[[',3),
                            whale_v = sapply(twi,'[[',2),
                            whale_x = whale_centers[1,],
                            whale_y = whale_centers[2,])
  enc_summary

  # Get moment of closest proximity
  min_i <- which.min(proximities)
  ship_min <- tsi[[min_i]]
  whale_min <- twi[[min_i]]

  # Get polygon information at that time
  ship_min$bow <- find_bow(ship_min)
  whale_min$bow <- find_bow(whale_min)

  # Compile proximity object ===================================================
  prox <- list()
  prox$timeline <- enc_summary
  prox$ship <- ship_min
  prox$whale <- whale_min

  return(prox)
}



#########################################################
#########################################################
# Wrapper function to iterate

if(FALSE){
  # Parameters
  v.ship = 5 # m/s
  l.ship = 220 # meters
  w.ship = 30 # meters
  params.ship <- data.frame(v.ship,l.ship,w.ship)
  v.whale = 1.5 # m/s
  l.whale = 25 # meters
  w.whale = .2074 # meters
  delta.sd = 50 # degr
  toplot=TRUE
  B <- 10
}

encounter_simulator <- function(params.ship,
                                v.whale,l.whale,
                                w.whale,
                                delta.sd,
                                B=100,
                                toplot=TRUE,
                                plot_timeseries=FALSE){
  # Setup
  coords <- make.arena(new=toplot)
  whalestarts <- get.starts(coords)

  # Process
  b <- 1
  MR <- data.frame()
  records <- list()
  hits <- list()
  for(b in 1:B){
    coords <- make.arena(new=FALSE)
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
    w0 <- initiate.whale(whalestarts,toplot=FALSE)
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

    if(plot_timeseries){plot(encounter$timeline$meters,
                             ylim=c(0,1200),type='l',col='firebrick')}

    # Store run b
    encounter$summary <- data.frame(closest=min(encounter$timeline$meters),
                                    ship_v=vs,
                                    ship_l=lship,
                                    ship_w=wship,
                                    whale_v=v,
                                    whale_l=lwhale,
                                    whale_w=wwhale,
                                    whale_deltasd=deltasd)
    encounter$summary
    encounter$summary$encounter <- ifelse(encounter$summary$closest==0,1,0)
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

if(FALSE){
  prox <- records
  prox <- prox[[1]]
  length(prox)
  names(prox)
  prox$whale$bow$hdg
  encounter_analyst(prox)

  ei <- prox$whale$bow
}

encounter_analyst <- function(prox){
  ship <- prox$ship
  whale <- prox$whale
  tl <- prox$timeline

  par(mfrow=c(1,2))

  # Situation
  par(mar=c(.5,.5,3,.5))
  #make.arena()
  plot(1,type='n',xlim=c(-600,600),ylim=c(-600,600),axes=FALSE,ann=FALSE)
  title(main='Overview')

  # ship's course
  lines(x=tl$ship_x,
        y=tl$ship_y,
        col='red')

  # ship's position at closest proximity
  lines(x=ship$xy$x,
        y=ship$xy$y,
        col='firebrick')

  # ship's heading
  lines(x=ship$bow$hdg_line$x,
        y=ship$bow$hdg_line$y,
        col='grey')

  # whale's course
  lines(x=tl$whale_x,
        y=tl$whale_y,
        col='blue')

  # whale's polygon at closest proximity
  lines(x=whale$xy$x,
        y=whale$xy$y,
        col='blue')

  # whale's heading
  lines(x=whale$bow$hdg_line$x,
        y=whale$bow$hdg_line$y,
        col='grey')

  par(mar=c(4.2,4.2,3,.5))
  plot(tl$meters,ylim=c(0,1200),
       type='l',xlab='Timestamp', ylab='Proximity (m)',main='Proximity timeseries')
  abline(v=tl$t[which.min(tl$meters)],col='red',lty=3)

  par(mfrow=c(1,1))
}
