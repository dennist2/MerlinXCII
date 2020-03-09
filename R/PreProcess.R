#' PreProcess
#'
#'
#' @param raw The un-cleaned dataset you'd like to preprocess
#'
#'@export
#'



PreProcess <- function(raw){
  col <- colnames(raw)
  
  keep <- c(which(col=="X"|col=="Date"|col=="Team"|col=="ML"|col=="SP"|col=="MLT"|col=="MLTR"|col=="SPT"|col=="Opponent"|col=="SPTR"|col=="TmPts"|col=="OpPts"|col=="H1A0"|col=="D0F1"|col=="PtDiff"|col=="strWL"|col=="sprWL"|col=="Money.Line"))
  
  outp <- raw[,c(keep)]
  
  return(outp)
}

