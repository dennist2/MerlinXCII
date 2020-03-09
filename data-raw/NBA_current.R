## code to prepare `NBA_current` dataset goes here

NBA_current <-  read.csv(here::here("data-raw","NBA_current_season.csv"))
NBA_current$H1A0 <- as.factor(NBA_current$H1A0)
NBA_current$D0F1 <- as.factor(NBA_current$D0F1)
usethis::use_data(NBA_current,overwrite = TRUE)
