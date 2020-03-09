## code to prepare `NBA_total` dataset goes here

NBA_total <-  read.csv(here::here("data-raw","NBA_latest.csv"))
NBA_total$H1A0 <- as.factor(NBA_total$H1A0)
NBA_total$D0F1 <- as.factor(NBA_total$D0F1)
usethis::use_data(NBA_total,overwrite = TRUE)
