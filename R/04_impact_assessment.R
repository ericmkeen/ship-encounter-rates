#########################################################
#########################################################
# IMPACT ASSESSMENT
# Eric M. Keen, v. June 2021
#########################################################
#########################################################

# Load data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("function-results.R")
source("function-stats.R")
source("function-impacts.R")

#########################################################
#########################################################
# Load data

raw_pass <- readRDS("../results/emp/2018-passenger.rds")
raw_tanker <- readRDS("../results/emp/2018-tanker.rds")
raw_prtank <- readRDS("../results/emp/2030-tanker.rds")


#########################################################
#########################################################
# Process results

pass <- process_results(raw_pass)
tanker <- process_results(raw_tanker)
prtank <- process_results(raw_prtank)


################################################################################
################################################################################
# Prepare whale density
# Convert density into a distribution, assuming pkg Distance returns a Gaussian estimate

# Reported from Keen et al 2021
whale_density <- 0.015 # may - october
whale_density_lci <- 0.009
whale_density_uci <- 0.023

#x <- rpois(n=10000,lambda=1)
sd_options <- seq(0.0027,0.0043,by=.000001) ; length(sd_options)
norm_options <- data.frame()
for(sdi in sd_options){
  x <- rnorm(n=10000,mean=whale_density,sd=sdi)
  lci <- quantile(x,.025) ; lci
  uci <- quantile(x,.975) ; uci
  ci_error <- abs(whale_density_lci - lci) + abs(whale_density_uci - uci)
  noi <- data.frame(mean=whale_density,
                    sd=sdi,
                    lci,uci,
                    error=ci_error)
  norm_options <- rbind(norm_options, noi)
}
plot(error ~ sd, data=norm_options, type='l')

best_fit <- norm_options[which.min(norm_options$error),]

hist(x)
abline(v=whale_density,col='red')
abline(v=whale_density_lci,lty=3,col='red')
abline(v=whale_density_uci,lty=3,col='red')
abline(v=best_fit$lci,lty=3,col='blue')
abline(v=best_fit$uci,lty=3,col='blue')

whale_norm <- rnorm(100000,mean=whale_density,sd=best_fit$sd)
hist(whale_norm)

################################################################################
################################################################################
# 2018 cargo / tanker

total_route <- 31 * 37  # transits * km of route
total_route
result <- ship_strike_impacts(total_route = total_route,
                              whale_density = whale_norm,
                              p_encounter = (tanker$encounters / 100),
                              p_surface = 0.62,
                              p_avoidance = 0.66, # 0.55
                              p_lethality = 0.42,
                              iterations = 100000)
result$inputs$total_route
result$summary

################################################################################
################################################################################
# 2030 cargo / tanker

total_route <- (31 * 37) + (750 * 21)  # transits * km of route
total_route
result <- ship_strike_impacts(total_route = total_route,
                              whale_density = whale_norm,
                              p_encounter = (prtank$encounters / 100),
                              p_surface = 0.62,
                              p_avoidance = 0.66, # 0.55
                              p_lethality = 0.57,
                              iterations = 100000,
                              hist_n=10)
result$inputs$total_route
result$summary

#########################################################
#########################################################
# by Eric Keen @ SWAG, (c) 2021

