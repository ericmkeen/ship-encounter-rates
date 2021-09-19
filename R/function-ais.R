# AIS formatting functions for encounter simulator

if(FALSE){
  # Data to use for debugging
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  source('00_ais.R')
  head(ais)
  type_ops <- c("passenger ship") ; type_ops
}

#########################################################
#########################################################

ais_filter <- function(type_ops,
                       ais,
                       min_sog=2, 
                       min_length=5, 
                       xlims=c(-129.6,-129.3), 
                       ylims=c(52.8,53.35)){
  
  traffic <- ais
  
  # Filter to vessel type
  matches <- which(tolower(as.character(ais$type)) %in% type_ops) 
  traffic <- traffic[matches,] 
  nrow(traffic)
  
  # Filter to valid entries
  traffic <- traffic %>% dplyr::filter(length > min_length,
                                       sog > min_sog) 
  
  # Filter to study area
  traffic <- traffic %>% dplyr::filter(x > min(xlims),
                                       x < max(xlims),
                                       y > min(ylims),
                                       y < max(ylims))
  nrow(traffic)
  return(traffic)
  
}

#ais_filter(type_ops="passenger ship",ais=ais,summ=summ$id)

