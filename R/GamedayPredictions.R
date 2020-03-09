#' GamedayPredictions
#' 
#' @param model the fit glm model used to make predictions
#' @param test the test set to make predictions on

#' @export

GamedayPredictions <- function(model,test){
  out <- list()
  
  z <- qnorm(.99)
  preds <- predict(model,test,type='response',se.fit=TRUE)
  low <- preds$fit - z*preds$se.fit
  high <- preds$fit + z*preds$se.fit
  
  if(is.null(test$strWL)==TRUE){
    tab <- data.frame(Date=test$Date,Team=test$Team,Lo=low,Avg=preds$fit,Hi=high,PrWL=ifelse(preds$fit>.5,"1","0"),
                      Opponent=test$Opponent,
                      Id=test$Identify)
    
    tab[3:5] <- round(tab[3:5],digits = 3)
  } else if(is.null(test$strWL)==FALSE){
    
    tab <- data.frame(Date=test$Date,Team=test$Team,Lo=low,Avg=preds$fit,Hi=high,PrWL=ifelse(preds$fit>.5,"1","0"),
                      TrWL=test$strWL,
                      Opp=test$Opponent,
                      Id=test$Identify)
    
    tab[3:5] <- round(tab[3:5],digits = 3)
  }
  
  byGame <- tab[order(tab$Id),]
  byGame <- byGame[,-8]
  
  Gnum1 <- seq(1,length(byGame$Team)/2,1)
  Gnum2 <- seq(1,length(byGame$Team)/2,1)
  Gn <- sort(c(Gnum1,Gnum2))
  
  byGame <- data.frame(Date=byGame$Date,GameNumber=Gn,byGame[,-1])
  
  byProbability <- tab[order(tab$Avg),]
  byProbability <- byProbability[,-8]
  
  return(list(ByProbability=byProbability,ByGame=byGame))
}
