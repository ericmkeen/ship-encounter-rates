#########################################################
#########################################################
# ENCOUNTER RATES - FUNCTIONS- RESULTS
# Eric M. Keen, v. October 2021
#########################################################
#########################################################


process_results <- function(mr){
  #mr <- prtank

  mri <- mr$summaries
  mri$id <- 1:nrow(mri) # add identifier for each row
  head(mri)
  nrow(mri)

  # Add new column -- whale's fluke position
  mri$whale_dx <- cos(2*pi - (mri$whale_hdg*pi/180) + pi/2)*(mri$whale_l) ; mri$whale_dx
  mri$whale_x_fluke <- mri$whale_x + mri$whale_dx
  hist(mri$whale_x, breaks=20)
  hist(mri$whale_x_fluke, breaks=20)

  # Add new column -- whale's fractional ship position (0 = bow, 1 = stern)
  mri$ship_frac <- (mri$ship_y_bow - mri$whale_y) / (mri$ship_y_bow - mri$ship_y_stern)
  hist(mri$ship_frac)

  # Which misses are within a whale length?
  near_misses <- which(mri$encounter==0 & mri$closest < mri$whale_l) ; near_misses
  misses <- mri[near_misses,] ; nrow(misses)
  head(misses)
  hist(misses$whale_x,breaks=20)
  hist(misses$whale_x_fluke,breaks=20)
  hist(misses$ship_frac,breaks=20)

  # Which are actually strikes, given the whale's fluke position?
  # Fluke occurs within ship width
  # Body intersects ship at closest point
  i=1
  hit <- rep(FALSE,nrow(misses))
  for(i in 1:nrow(misses)){
    missi <- misses[i,]
    missi
    ship_rad <- round(missi$ship_w / 2)  ; ship_rad
    hit_test <- round(missi$whale_x_fluke) %in% (-1*ship_rad):ship_rad ; hit_test
    y_test <- round(missi$whale_y) %in% round(missi$ship_y_bow):round(missi$ship_y_stern); y_test
    if(all(c(hit_test,y_test))){hit[i] <- TRUE}
  }
  hit
  hits <- misses[hit,]

  # Update encounter status for those actual hits
  to_change <- which(mri$id %in% hits$id) ; to_change
  mri$encounter[to_change] <- 1

  # Re-tally encounters in each iteration
  mrs <- mri %>%
    group_by(iteration) %>%
    summarize(encounters=sum(encounter))

  # Plots
  par(mfrow=c(1,1))
  par(mar=c(4.2,4.2,1,1))
  hist(mrs$encounters,
       breaks=seq(0,20,by=1),
       main='Imminent encounter rates')

  # Statistics
  message('Mean = ',round(mean(mrs$encounters),digits=3))
  message('CI of the mean = ',paste(round(BootCI(mrs$encounters,FUN=mean)[2:3],digits=3),collapse=' - '))
  message('90% CI = ',paste(quantile(mrs$encounters,c(0.05,0.95)),collapse=' - '))
  message('SD = ',sd(mrs$encounters))

  return(mrs)
}


#########################################################
#########################################################


hist_enc <- function(enc,shade="black",add=FALSE){
  par(mar=c(4.2,4.2,1,.5))
  hist(enc,
       breaks=seq(0,20,by=1),
       col=adjustcolor(shade,alpha.f=.3),
       border=adjustcolor(shade,alpha.f=.1),
       main=NULL,
       xlab="Encounters per 100 trials",
       add=add)
}


#########################################################
#########################################################
# by Eric Keen @ SWAG, (c) 2021
