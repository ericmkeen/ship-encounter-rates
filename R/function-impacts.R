#########################################################
#########################################################
# Functions for impact assessment
# Eric M. Keen, v. June 2021
#########################################################
#########################################################

ship_strike_impacts <- function(total_route,
                                whale_density,
                                p_encounter,
                                p_surface,
                                p_avoidance,
                                p_lethality,
                                iterations=10000,
                                verbose=TRUE,
                                toplot=TRUE){

  # starting point ===============================================================

  if(FALSE){
    transits <- 31 # may - october
    route <- 37 # km
    total_route <- transits * route
    b <- 10000
    whale_density <- whale_norm
    p_encounter <- tanker$encounters / 100
    p_surface <- 0.62
    p_avoidance <- 0.55
    p_lethality <- 0.42
    verbose <- TRUE
  }

  # Handle inputs ===============================================================
  if(verbose){message('formatting inputs . . . ')}

  b <- iterations
  if(length(whale_density)==1){whale_density <- rep(whale_density,times=b)}
  if(length(p_encounter)==1){p_encounter <- rep(p_encounter,times=b)}
  if(length(p_surface)==1){p_surface <- rep(p_surface,times=b)}
  if(length(p_avoidance)==1){p_avoidance <- rep(p_avoidance,times=b)}
  if(length(p_lethality)==1){p_lethality <- rep(p_lethality,times=b)}

  # co-occurrences ===============================================================
  if(verbose){message('Calculating co-occurrences . . . ')}

  cooccurrence <- c()
  for(i in 1:b){
    cooccurrence[i] <- sum(ifelse(runif(total_route,min=0,max=1) < sample(whale_norm,size=total_route), 1, 0))
  }

  if(toplot){
    par(mfrow=c(3,2))
    hist(cooccurrence,main='Cooccurrences')
  }

  # encounters  ==================================================================
  if(verbose){message('Predicting encounters . . . ')}

  encounters <- c()
  for(i in 1:b){
    coi <- sample(cooccurrence,size=1) ; coi
    eri <- sample(p_encounter,size=1) ; eri
    encounters[i] <- sum(ifelse(runif(coi,min=0,max=1) < eri,1,0))
  }
  if(toplot){
    hist(encounters,main='Encounters')
  }
  table(encounters)
  mean(encounters)
  quantile(encounters,.025)
  quantile(encounters,.975)

  # fine scale rates =============================================================

  if(verbose){message('Predicting surface overlap events . . . ')}
  surfaces <- c() #==========================
  for(i in 1:b){
    enci <- sample(encounters,size=1) ; enci
    surfi <- 0
    if(enci > 0){
      p_surfi <- sample(p_surface,size=1)
      surfi <- sum(ifelse(runif(enci,min=0,max=1) < p_surfi,1,0))
    }
    surfaces[i] <- surfi
  }
  if(toplot){
    hist(surfaces,main='Surface overlaps')
  }
  table(surfaces)

  if(verbose){message('Predicting collisions  . . . ')}
  collisions <- c() #==========================
  for(i in 1:b){
    surfi <- sample(surfaces,size=1) ; surfi
    collidi <- 0
    if(surfi > 0){
      p_avoidi <- sample(p_avoidance,size=1)
      collidi <- sum(ifelse(runif(surfi,min=0,max=1) < (1 - p_avoidi),1,0))
    }
    collisions[i] <- collidi
  }
  if(toplot){
    hist(collisions,main='Collisions')
  }
  table(collisions)

  if(verbose){message('Predicting mortalities . . . ')}
  mortalities <- c() #==========================
  for(i in 1:b){
    collidi <- sample(collisions,size=1) ; collidi
    morti <- 0
    if(collidi > 0){
      p_lethali <- sample(p_lethality,size=1)
      morti <- sum(ifelse(runif(collidi,min=0,max=1) < p_lethali,1,0))
    }
    mortalities[i] <- morti
  }
  if(toplot){
    hist(mortalities,main='Mortalities')
    par(mfrow=c(1,1))
  }
  table(mortalities)

  ################################################################################
  # Compile summary list

  if(verbose){message('Compiling summary . . . ')}
  summ <- data.frame()
  varvec <- cooccurrence
  varsum <- data.frame(rate = 'cooccurrences',
                       mean=round(mean(varvec)), median = as.numeric(quantile(varvec,.5)),
                       q05 = as.numeric(quantile(varvec,.05)), q20 = as.numeric(quantile(varvec,.20)), q95 = as.numeric(quantile(varvec,.95)),
                       yr5_mean = round(5*sum(sample(varvec,size=1000)/1000)), yr10_mean = round(10*sum(sample(varvec,size=1000))/1000))
  summ <- rbind(summ,varsum) ; summ

  varvec <- encounters
  varsum <- data.frame(rate = 'encounters',
                       mean=round(mean(varvec)), median = as.numeric(quantile(varvec,.5)),
                       q05 = as.numeric(quantile(varvec,.05)), q20 = as.numeric(quantile(varvec,.20)), q95 = as.numeric(quantile(varvec,.95)),
                       yr5_mean = round(5*sum(sample(varvec,size=1000)/1000)), yr10_mean = round(10*sum(sample(varvec,size=1000))/1000))
  summ <- rbind(summ,varsum) ; summ

  varvec <- surfaces
  varsum <- data.frame(rate = 'surface overlap',
                       mean=round(mean(varvec)), median = as.numeric(quantile(varvec,.5)),
                       q05 = as.numeric(quantile(varvec,.05)), q20 = as.numeric(quantile(varvec,.20)), q95 = as.numeric(quantile(varvec,.95)),
                       yr5_mean = round(5*sum(sample(varvec,size=1000)/1000)), yr10_mean = round(10*sum(sample(varvec,size=1000))/1000))
  summ <- rbind(summ,varsum) ; summ

  varvec <- collisions
  varsum <- data.frame(rate = 'collisions',
                       mean=round(mean(varvec)), median = as.numeric(quantile(varvec,.5)),
                       q05 = as.numeric(quantile(varvec,.05)), q20 = as.numeric(quantile(varvec,.20)), q95 = as.numeric(quantile(varvec,.95)),
                       yr5_mean = round(5*sum(sample(varvec,size=1000)/1000)), yr10_mean = round(10*sum(sample(varvec,size=1000))/1000))
  summ <- rbind(summ,varsum) ; summ

  varvec <- mortalities
  varsum <- data.frame(rate = 'mortalities',
                       mean=round(mean(varvec)), median = as.numeric(quantile(varvec,.5)),
                       q05 = as.numeric(quantile(varvec,.05)), q20 = as.numeric(quantile(varvec,.20)), q95 = as.numeric(quantile(varvec,.95)),
                       yr5_mean = round(5*sum(sample(varvec,size=1000)/1000)), yr10_mean = round(10*sum(sample(varvec,size=1000))/1000))
  summ <- rbind(summ,varsum) ; summ

  mr <- list()
  mr$summary <- summ

  ################################################################################
  # Add details to list

  if(verbose){message('Finalizing results list . . . ')}

  mr$inputs <- list()
  mr$inputs$total_route <- total_route
  mr$inputs$whale_density <- whale_density
  mr$inputs$p_encounter <- p_encounter
  mr$inputs$p_surface <- p_surface
  mr$inputs$p_avoidance <- p_avoidance
  mr$inputs$p_lethality <- p_lethality

  mr$results <- list()
  mr$results$cooccurrences <- cooccurrence
  mr$results$encounters <- encounters
  mr$results$surfaces <- surfaces
  mr$results$collisions <- collisions
  mr$results$mortalities <- mortalities

  return(mr)
}
