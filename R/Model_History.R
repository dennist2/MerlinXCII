#' Model_History
#' 
#' @param data the preprocessed dataset to calculate performance off
#' @param form the referenced formula from a glm model to fit when making predictions
#' @param n the number of days back to be calculated
#' 
#' @export

Model_History <- function(data,form,n){
  
  dates <- unique(data$Date)
  last <- length(dates)
  comb <- list()
  
  
  for(i in 1:n){
    latest <- dates[last-(i-1)]
    
    test <- data[which(data$Date==latest),]
    
    dex <- as.Date(data$Date,format="%Y-%m-%d") < as.Date(latest,format="%Y-%m-%d")
    
    train <- data[dex,]
    
    model <- stats::glm(form,data=train,family = binomial)
    print(model)
    
    
    
    preds <- stats::predict(model,test,type='response',se.fit=T)
    
    lo <- preds$fit - stats::qnorm(.99)*preds$se.fit
    hi <- preds$fit + stats::qnorm(.99)*preds$se.fit
    
    dum <- rep("0",length(preds$fit))
    dum[preds$fit>.5]="1"
    
    
    
    
    df <- data.frame(Date=test$Date,Team=test$Team,Lo=lo,Avg=preds$fit,Hi=hi,PrWL=dum,TrWL=test$strWL,TeamPts=test$TmPts,OppPts=test$OpPts,Opp=test$Opponent,WL=round(mean(dum==test$strWL),digits = 2))
    df[,3:5] <- round(df[,3:5],digits = 3)
    
    if(i==1){
      comb[[i]] <- df
      
      
    } else {
      df$PrWL <- as.character(df$PrWL)
      df$Date <- as.character(df$Date)  
      df$Team <- as.character(df$Team)
      df$Opp <- as.character(df$Opp)
      c <- colnames(df)
      
      out <- rbind(c,df)
      
      comb[[i]] <- out
    }
  }
  
  
  all <- do.call(rbind,comb)
  t <- table(all$PrWL,all$TrWL)
  
  all <- transform(all,WinLoss=ifelse(all$TrWL==all$PrWL,"Win","Loss"))
  
  
  return(list(HistoryTranscript=all,Matrix=t))
  
}

