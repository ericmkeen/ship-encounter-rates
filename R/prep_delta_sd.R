
# Set working directory to folder that this R file is in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Get file list of acoustic tracks
subdir <- '../data/acoustic_tracks'
lf <- dir(subdir)
lf <- paste0(subdir,'/',lf)
lf

# Stage results dataframe
mr <- data.frame()

# Loop through each track
i=1
for(i in 1:length(lf)){

  # Status report
  message(i)

  # Read & format
  lfi <- lf[i] ; lfi
  dfi <- read.csv(lfi,header=FALSE,sep=" ")
  head(dfi)
  dfi[,c(1,2,4,6,7,9,10,12,13,15,17,18)] <- NULL
  head(dfi)
  names(dfi) <- c('utm_x','utm_y','speed','curvature','acc','bearing')
  head(dfi)

  # Each row is a second
  dfi$ss <- 1:nrow(dfi)

  # Summarize data by each minute
  dfi$mm <- floor(dfi$ss / 60)
  dfm <- dfi %>%
    group_by(mm) %>%
    summarize(brng = bearing[n()])

  # Get the change in bearing for each minute
  dfm$brng_pre <- c(NA, dfm$brng[1:(nrow(dfm)-1)])
  dfm <- dfm %>% mutate(diff = brng - brng_pre)
  #hist(dfm$diff)

  # Store results
  mri <- data.frame(id=i, file=lfi,
                    minutes = nrow(dfm),
                    mean_change = mean(dfm$diff,na.rm=TRUE),
                    sd_change = sd(dfm$diff,na.rm=TRUE))
  mri

  # Store results
  mr <- rbind(mr,mri)
}

# Filter to tracks with at least 30 minutes of
range(mr$minutes)
nrow(mr)
mr <- mr %>% filter(minutes > 30)
nrow(mr)

# Explore result
head(mr)
hist(mr$mean_change)
hist(mr$sd_change,breaks=20)

# Log-transform SDs
hist(log(mr$sd_change),breaks=20)

# Create distribution
lmean <- mean(log(mr$sd_change),na.rm=TRUE) ; lmean # 1.0424
lsd <- sd(log(mr$sd_change),na.rm=TRUE) ; lsd # 0.8247715

deltas <- rnorm(1000,lmean,lsd)
hist(deltas)

# Revert the log-transformation
hist(exp(deltas))















