#' PullData
#' 
#'@importFrom magrittr %>%
#'@name %>%
#'@rdname pipe
#' 
#' @export

PullData <- function(){
  
  
  link0 <- "https://www.wunderdog.com/lines-and-odds.html"
  
  webpage <- xml2::read_html(link0)
  Sys.sleep(2)
  print("Pulling Lines/Odds")
  draft_table <- rvest::html_nodes(webpage, 'table') %>% rvest::html_table(fill = TRUE)
  data <- draft_table[[1]]
  
  
  date <- data[1,1]
  date <- stringr::str_sub(date,start = 6)
  date <- as.Date(date,format = "%b. %d, %Y")
  
  cnames <- data[1,]
  data <- data[-c(1),]
  colnames(data) <- cnames
  
  
  d <- Sys.Date()+1
  ds <- format(d,"%b. %d")
  ds <- stringr::str_remove(ds,"0")
  
  
  locate_date <- stringr::str_locate(string = data[,1],pattern = as.character(ds))
  end <- c(which(is.na(locate_date[,1])==FALSE))
  
  
  if(purrr::is_empty(end)){
    print("only todays data is shown")
  } else {
    data <- data[1:(end-1),]
    
  }
  
  
  ats <- stringr::str_split(data$ATS,pattern = "[[()]]",simplify = T)
  
  data$ATS <- ats[,1]
  data$Total <- ats[,2]
  
  half <- stringr::str_split(data$`1st H`,pattern = "[[()]]",simplify = T)
  data$`1st H` <- half[,1]
  data$`1st H O/U` <- half[,2]
  data$Moneyline <- as.numeric(data$Moneyline)
  
  data$ATS <- as.numeric(data$ATS)
  data$Total <- as.numeric(data$Total)
  data$`1st H` <- as.numeric(data$`1st H`)
  data$`1st H O/U` <- as.numeric(data$`1st H O/U`)
  
  colnames(data)[c(4,6,7,8)] <- c("Spread","SpreadOdd","1H_Spread","1H_Odd")
  
  lines <- data.frame(Date=date,Teams=data$Team,MoneLine=data$Moneyline,Spread=data$Spread,Spread_Odd=data$SpreadOdd,Half_Spread=data$`1H_Spread`,Half_Odd=data$`1H_Odd`)
  
  colnames(lines)[2] <- "Team"
  
  
  link <- "https://www.wunderdog.com/public-consensus/nba.html"
  
  webpage <- xml2::read_html(link)
  Sys.sleep(2)
  print("Pulling Betting")
  draft_table <- rvest::html_nodes(webpage, 'table') %>% rvest::html_table(fill = TRUE)
  
  data <- draft_table[[4]]
  
  
  d <- Sys.Date()+1
  ds <- format(d,"%b. %d")
  ds <- stringr::str_remove(ds,"0")
  
  cnames <- data[3,]
  data <- data[-c(1,2,3),]
  colnames(data) <- cnames
  
  locate_date <- stringr::str_locate(string = data$Teams,pattern = ds)
  end <- c(which(is.na(locate_date[,1])==FALSE))
  
  
  if(purrr::is_empty(end)){
    print("only todays data is shown")
  } else {
    data <- data[1:(end-1),]
    
  }
  
  aways <- seq(1,length(data$Teams)-1,2)
  homes <- seq(2,length(data$Teams),2)
  
  
  data <- transform(data,Opponent="as")
  data$Opponent <- as.character(data$Opponent)
  
  tms1 <- data[aways,]$Teams
  tms2 <- data[homes,]$Teams
  
  
  data[aways,]$Opponent <- tms2
  data[homes,]$Opponent <- tms1
  
  data <- data.frame(Date=date,data,H1A0="0")
  data$H1A0 <- as.character(data$H1A0)
  data[homes,]$H1A0 <- "1"
  data[aways,]$H1A0 <- "0"
  
  keep <- data[,c(1,4,5,6,8,9,14,15),]
  
  keep$Percentage <- as.numeric(stringr::str_remove(keep$Percentage,"%"))
  keep$Percentage.2 <- as.numeric(stringr::str_remove(keep$Percentage.2,"%"))
  
  colnames(keep) <- c("Date","Team","SPT","SP","MLT","ML","Opponent","H1A0")
  keep$SPT <- as.numeric(keep$SPT)
  keep$MLT <- as.numeric(keep$MLT)
  
  
  data <- keep
  
  data <- data.frame(data,Identify="d",TotalMLT=1,TotalSPT=1,MLTR=1,SPTR=1)
  data$Identify <- as.character(data$Identify)
  data$Team <- as.character(data$Team)
  
  data
  data$Date <- as.Date(data$Date,format="%m/%d/%y")
  
  both <- merge(x=data,y=lines,by = c("Date","Team"))
  
  both$Opponent <- as.character(both$Opponent)
  
  data <- both
  for(i in 1:length(data$Team)){
    hold <- data[i,]
    
    l <- c(hold$Team,hold$Opponent)
    
    ls <- sort(l) 
    
    ident <- stringr::str_c(hold$Date,ls[1],ls[2])  
    
    data[i,]$Identify <- ident
    
  }
  data$SPT <- as.numeric(data$SPT)
  data$MLT <- as.numeric(data$MLT)
  
  un <- unique(data$Identify)
  
  for(i in 1:length(un)){
    
    sel <- c(which(data$Identify==un[i]))
    
    sele <- data[sel,]
    
    sele$TotalMLT <- sele$MLT[1]+sele$MLT[2]
    sele$TotalSPT <- sele$SPT[1]+sele$SPT[2]
    
    data[sel,] <- sele
  }
  test <- data
  test <- test[order(test$TotalMLT,decreasing = TRUE),]
  
  tm1 <- seq(1,length(test$Team)/2,1)
  tm2 <- seq(1,length(test$Team)/2,1)
  
  
  ranks <- c(tm1,tm2)
  ranks <- sort(ranks)
  
  test$MLTR <- ranks
  
  test <- test[order(test$TotalSPT,decreasing = TRUE),]
  
  test$SPTR <- ranks  
  
  test <- test[order(test$Team),]
  
  test$MoneLine <- as.numeric(test$MoneLine)
  
  
  tod <- transform(test,D0F1=ifelse(test$MoneLine>0,"0","1"))
  
  tod$D0F1 <- as.factor(tod$D0F1)
  tod$H1A0 <- as.factor(tod$H1A0)
  
  return(tod)
}
