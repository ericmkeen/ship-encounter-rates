#########################################################
#########################################################
# PREP WHALE DEFAULT PARAMETERS
#########################################################
#########################################################

# size of distributions
n=1000

# meters per sec dist # based on total mean + SD from Hendricks et al 2021
v.whale = truncnorm::rtruncnorm(n,0,2.63,1.3611,.5)

# Produce distribution of log-transformed values
# (see `pre_delta_sd.R`)
delta.sd = rnorm(n,
                 mean = 1.0424,
                 sd = 0.82477)

# Revert from log-transformed to actual SD of course change
delta.sd <- exp(delta.sd)

# based mean + SD from Keen et al 2021 (UAS)
l.whale = truncnorm::rtruncnorm(n,0,40,18.60375,1.649138) # meters

# based on fluke / body length ratio in Keen et al 2021 (UAS)
w.whale = .2074 # meters

#########################################################
#########################################################
# by Eric Keen @ SWAG, (c) 2021
