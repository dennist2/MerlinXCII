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
  MerlinXCII::NBA_current
  if(is.null(test$strWL)==TRUE){
    test$Date <- format(test$Date,"%m/%d/%y")
    tab <- data.frame(Date=test$Date,
                      Team=test$Team,
                      Lo=low,
                      Avg=preds$fit,
                      Hi=high,
                      PrWL=ifelse(preds$fit>.5,"W","L"),
                      Location=ifelse(test$H1A0=="0","Away","Home"),Status=ifelse(test$D0F1=="0","Dog","Favorite"),
                      Opponent=test$Opponent,
                      Id=test$Identify)
    
    tab[3:5] <- round(tab[3:5],digits = 3)
  } else if(is.null(test$strWL)==FALSE){
    test$Date <- format(test$Date,"%m/%d/%y")
    tab <- data.frame(Date=test$Date,Team=test$Team,Lo=low,Avg=preds$fit,Hi=high,PrWL=ifelse(preds$fit>.5,"W","L"),Location=ifelse(test$H1A0=="0","Away","Home"),Status=ifelse(test$D0F1=="0","Dog","Favorite"),
                      TrWL=test$strWL,
                      Opp=test$Opponent,
                      Id=test$Identify)
    
    tab[3:5] <- round(tab[3:5],digits = 3)
  }
  
  byGame <- tab[order(tab$Id),]
  byGame <- byGame[,-10]
  
  Gnum1 <- seq(1,length(byGame$Team)/2,1)
  Gnum2 <- seq(1,length(byGame$Team)/2,1)
  Gn <- sort(c(Gnum1,Gnum2))
  
  byGame <- data.frame(Date=byGame$Date,Game=Gn,byGame[,-1])
  
  byProbability <- tab[order(tab$Avg),]
  byProbability <- byProbability[,-10]
  
  return(list(ByProbability=byProbability,ByGame=byGame))
}
