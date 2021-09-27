#########################################################
#########################################################
# ENCOUNTER RATE - SENSITIVITY SIMULATION - DELTA SD
#########################################################
#########################################################

# Set working directory to folder that this R file is in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load sensitivity analysis defaults
source('00_sensitivity.R')


#########################################################
#########################################################
# SENSITIVITY PARAMETER

filecore <- 'v-whale'
v.whales <- c(0.5,1,1.5,2,2.5,3,3.5,4) ; v.whales
vars <- v.whales


#########################################################
#########################################################
# SIMULATOR

for(varb in 1:length(vars)){
  vari <- vars[varb] ; vari

  # Encounter simulator
  df <- data.frame() # details
  mrs <- c() # imminent encounters
  for(b in 1:iterations){
    mr <- encounter_simulator(params.ship=params.ship,
                              v.whale=vari,
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
    print(paste0(Sys.time()," | ",filecore," ",vari," m/s | Run ",b," | ",ieb," imminent encounter(s) ..."))
  }

}

#########################################################
#########################################################
# Plot

mrs <- data.frame()
vals <- c(0.5,1,1.5,2,2.5,3,3.5,4) ; vals

#pdf("../figures/sim/vwhale.pdf",width=4.5,height=4.5)
par(mar=c(4.2,4.2,.5,.5))
plot(1,type="n",ylim=c(0,10),xlim=c(0.5,4),axes=FALSE,
     ylab="Imminent encounters per 100 trials",
     xlab="Whale speed (m/s)")
axis(2,at=0:10,las=2)
axis(1,at=vals)

for(i in vals){
  mr <- readRDS(file=paste0("../results/sim/vwhale-",i,".rds"))
  mr$encounters
  mri <- data.frame(val=i,enc=mr$encounters)
  mrs <- rbind(mrs,mri)

  points(x=mean(mri$val),
         y=mean(mri$enc),col="firebrick",pch="-",cex=4)

  points(x=jitter(mri$val,amount=.06),
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
pval(val1=mrs$enc[mrs$val==4],
     val2=mrs$enc[mrs$val==1])

#########################################################
#########################################################
# by Eric Keen @ SWAG, (c) 2021
