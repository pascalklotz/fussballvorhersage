
library(dplyr)
library(stringr)
library(readr)
library(tidyr)


setwd("/home/pascal/fussballvorhersage")
getwd()


teams_id <- read.csv("~/fussballvorhersage/fbref_scraping/teams_id.txt", sep=";")

page <- c("shooting","keeper","passing","passing_types","gca","defense","possession","misc")
saison <- "2025-2026"
filename <- "fbref_scraping/hist_2526.csv"

dat <- data.frame()
for (i in 1:nrow(teams_id)){
  datteam <- data.frame(teamname = teams_id[i,1])
  a <- paste0("https://fbref.com/en/squads/",teams_id[i,2],"/",saison, "/matchlogs/all_comps/schedule/",teams_id[i,4],"-Scores-and-Fixtures-All-Competitions")
  a2 <- xml2::read_html(a) %>% rvest::html_nodes("#matchlogs_for") %>% rvest::html_table()
  a3 <- a2[[1]]
  #colnames(a3) <- a3[1,]
  a3 <- a3[a3$Comp == "Bundesliga",]
  if(nrow(a3) > 0){
    datteam <- data.frame(spieltag = 1:34,datteam,a3)
    for (j in 1:length(page)){
      a <- paste0("https://fbref.com/en/squads/",teams_id[i,2],"/",saison, "/matchlogs/all_comps/",page[j],"/",teams_id[i,4],"-Match-Logs-All-Competitions")
      print(a)
      a2 <- xml2::read_html(a) %>% rvest::html_nodes("#matchlogs_for") %>% rvest::html_table()
      a3 <- a2[[1]]
      colnames(a3) <- a3[1,]
      a3 <- a3[a3$Comp == "Bundesliga",]
      #    a3 <- a3[2:35,]
      datteam <- merge(datteam,a3, by = "Round", all.x = T)
      Sys.sleep(15)
    }

    dat <- rbind(dat,datteam)
    print(nrow(dat))
  }
  
}
test <- dat
colnames(test) <- gsub(".x|.y|.1", "", colnames(test))
test <- test[,which(!duplicated(t(test)), arr.ind = T)]
write_csv(test, file = filename)

