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

ais_filter <- function(type_ops,ais){
  df <- ais
  
  # Filter to vessel type
  matches <- which(tolower(as.character(df$type)) %in% type_ops) ; matches
  dfi <- df[matches,] ; nrow(dfi)
  
  # Filter to valid entries
  dfi <- dfi[dfi$length > 0 & dfi$sog > 0,] 
  
  head(dfi)
  
  sogs <- dfi$sog ; unique(sogs)
  lengths <- dfi$length ; unique(lengths)
  return(list(n=length(matches),
              sog=sogs,
              length=lengths))
}

#ais_filter(type_ops="passenger ship",ais=ais,summ=summ$id)

