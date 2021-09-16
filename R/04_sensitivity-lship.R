#########################################################
#########################################################
# ENCOUNTER RATE - SENSITIVITY SIMULATION - L SHIP
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

iterations <- 100

# Traffic 
traffic <- ais_filter(type_ops=c("passenger ship"),ais=ais)
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
# SENSITIVITY PARAMETER

filecore <- 'l-ship'
l.ships <- c(10,20,50,100,150,200,250,300,350)
vars <- l.ships

#########################################################
#########################################################
# SIMULATOR

for(varb in 1:length(vars)){
  vari <- vars[varb] ; vari
  l.ship <- vari
  params.ship <- data.frame(v.ship=v.ship,
                            l.ship=l.ship,
                            w.ship = 0.25*l.ship)
  # Encounter simulator
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
            file=paste0("../results/sim/",filecore,"-",vari,".rds"))
    print(paste0(Sys.time()," | ",filecore," ",vari," meters | Run ",b," | ",ieb," imminent encounter(s) ..."))
  }
  
}

#########################################################
#########################################################
# Plot

mrs <- data.frame()
vals <- c(20,50,100,150,200,250,300,350) ; vals

# Figure ================================================

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#pdf("../figures/sim/lship.pdf",width=4.5,height=4.5)
par(mar=c(4.2,4.2,.5,.5))
plot(1,type="n",ylim=c(0,15),xlim=c(20,350),axes=FALSE,
     ylab="Imminent encounters per 100 trials",
     xlab="Vessel length (m)")
axis(2,at=0:15,las=2)
axis(1,at=vals,cex.axis=.9)

for(i in vals){
  mr <- readRDS(file=paste0("../results/sim/lship-",i,".rds"))
  mr$encounters
  mri <- data.frame(val=i,enc=mr$encounters)
  mrs <- rbind(mrs,mri)

  points(x=mean(mri$val),
         y=mean(mri$enc),col="firebrick",pch="-",cex=4)

  points(x=jitter(mri$val,amount=5),
         y=jitter(mri$enc,amount=.2),
         pch=16,cex=.7,col=adjustcolor("black",alpha.f=.4))
}

#dev.off()


################################################################################
################################################################################
# Table

# Summary table: Mean and SD for each value  ===================================
mrsum <- mrs %>%
  group_by(val) %>%
  summarize(mean=mean(enc),
            sd=sd(enc),
            q05=quantile(enc,0.05),
            q95=quantile(enc,0.95),
            lci=bootci(enc)$lci,
            uci=bootci(enc)$uci)
mrsum


# Change per step ==============================================================
diffs <- mrsum$mean[2:nrow(mrsum)] - mrsum$mean[1:(nrow(mrsum)-1)] ; diffs
mean(diffs[-1])
sd(diffs[-1])

summary(lm(enc~val,data=mrs))


# Significance test ============================================================

# Test for significant difference between value with lowest mean encounters and value with highest
pval(val1=mrs$enc[mrs$val==20],
     val2=mrs$enc[mrs$val==350])

# Follow ups
pval(val1=mrs$enc[mrs$val==20],
     val2=mrs$enc[mrs$val==50])

pval(val1=mrs$enc[mrs$val==50],
     val2=mrs$enc[mrs$val==250])

pval(val1=mrs$enc[mrs$val==100],  # diff b/w current and projected (roughly)?
     val2=mrs$enc[mrs$val==350])

#########################################################
#########################################################
# by Eric Keen @ SWAG, (c) 2021