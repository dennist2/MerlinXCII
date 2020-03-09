#' Model
#' 
#' @param train The dataset you'd like to train the model on (should come from MerlinXII::nba)
#' @description This function fits a model to the most recent NBA data from 2018 to the last full gameday. Write the line strWL <- MerlinXII::FitModel and the fitted glm will be assigned to an object called strWL
#' 
#' 
#' @export

Model <- function(train){
  mtext <- "glm(strWL~SP+ML+H1A0+SPTR:D0F1+MLTR:H1A0+ML:MLTR+MLT:SPT,data = train, family=binomial)"
  mt <- parse(text = mtext)
  model <- eval(mt)
  return(model)
}

