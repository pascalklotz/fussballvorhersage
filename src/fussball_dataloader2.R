#utf8?
#install.packages('dplyr')
library(dplyr)
#install.packages('readr')
library(readr)
#install.packages('tidyr')

library(tidyr)
#install.packages("RCurl")
library(RCurl)

#install.packages("calendar")
library(calendar)

#install.packages("jsonlite")
library(jsonlite)

library(stringr)
library(rvest)

#library(tidyverse)

#install.packages("bigrquery")
#library(bigrquery)

#setwd("/home/pascalkloetzer/git/fussballvorhersage/fussballvorhersage")
getwd()

#raw_data <- getURL("https://api.openligadb.de/getmatchdata/bl1/2021")
#bundesliga <- fromJSON(raw_data)
#bundesliga

aktueller_spieltag <- readLines("https://www.google.com/calendar/ical/spielplan.1.bundesliga%40gmail.com/public/basic.ics")
#test
spielplan <- data.frame(DESC = ic_extract(aktueller_spieltag,"DESCRIPTION"),DATE = ic_extract(aktueller_spieltag,"DTSTART"),SPIEL = ic_extract(aktueller_spieltag,"SUMMARY"))
spielplan$DATE <- as.Date(spielplan$DATE)
spielplan <- spielplan[(grepl('1. Bundesliga',spielplan$DESC ))&(!grepl('\\(',spielplan$SPIEL)),]
#spielplan <- spielplan[(grepl('19. Spieltag',spielplan$DESC )),]
spielplan <- spielplan[spielplan$DATE >= Sys.Date(),]
spielplan


min_spieltag <- spielplan %>% summarize(min_date = min(DATE))
min_spieltag <- spielplan %>% filter(DATE == min_spieltag$min_date) %>% select(DESC) %>% unique()
spieltag <- spielplan[spielplan$DESC == min_spieltag$DESC,]
#spieltag <- spielplan

spieltag <- spieltag %>% separate(SPIEL,'\\-', into = c('HOME','AWAY'))
spieltag$HOME <- trimws(spieltag$HOME)
spieltag$AWAY <- trimws(spieltag$AWAY)
spieltag


#bigrquery::bq_auth(
# path = "pascalsprojekt-f56653190968.json"
#)
#sql = "SELECT * FROM `pascalsprojekt.fussballvorhersage.tean_names`"
#datasets <- bq_dataset("pascalsprojekt","fussballvorhersage")
#teamnames <- bq_table_download(bq_dataset_query(datasets,sql))

teamnames <- read_delim("raw/team_names.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
teamnames$id <- as.character(teamnames$id)
spieltag <- spieltag %>% left_join(teamnames, by = c("HOME" = "name_klar"))
spieltag <- spieltag %>% left_join(teamnames, by = c("AWAY" = "name_klar"))
spieltag <- spieltag[,c("name_data.x","name_data.y")]
names(spieltag) <- c("HomeTeam","AwayTeam")
spieltag








#D1_20210108 <- read_delim("D1_20210119.csv", 
#";", escape_double = FALSE, trim_ws = TRUE)

url = "http://www.football-data.co.uk/mmz4281/2122/D1.csv"
urlcsv <- getURL(url)
dat12 <- read_csv(urlcsv, col_types = cols(Date = col_date(format = "%d/%m/%Y"),
                                           Time = col_time(format = "%H:%M")),trim_ws = TRUE)



##http://www.football-data.co.uk/germanym.php
#dat12 <- read_csv(file = "D1_20210122.csv", col_types = cols(Date = col_date(format = "%d/%m/%Y"),
#                                                     Time = col_time(format = "%H:%M")),trim_ws = TRUE)




prodat <- spieltag


#prodat <- data.frame(HomeTeam = proddat[,1], AwayTeam = proddat[,2])

dat12 <- bind_rows(dat12,prodat)
dat12$saison <- "2122"

dat12 <- dat12 %>% select(Div:FTAG, saison, -Time) 

dat2021 <- read_csv("raw/D1_20202021.csv", col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                                            Time = col_time(format = "%H:%M")))
dat2021$saison <- "2021"
dat2021 <- dat2021 %>% select(Div:FTAG, saison, -Time)

dat11 <- read_csv("raw/D120192020.csv", col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                                         Time = col_time(format = "%H:%M")))
dat11$saison <- "1920"
dat11 <- dat11 %>% select(Div:FTAG, saison, -Time)
dat10 <- read_csv("raw/D1_20182019.csv", col_types = cols(Date = col_date(format = "%d/%m/%Y")))
dat10$saison <- "1819"
dat10 <- dat10 %>% select(Div:FTAG, saison) 
dat <- rbind(dat10, dat11, dat2021, dat12)
#dat

dat2 <- dat 
dat2$Spieltag <- (rep(0:((nrow(dat2)-1)/9)%%34 + 1,each = 9)) #modulo fuer 1 bis 34 je Saison
dat2$Spielid <- paste0(dat2$saison,"_",as.character(dat2$Spieltag), "_", as.character(rep(1:9,nrow(dat2)/9)))
dat2$FTHG <- as.integer(dat2$FTHG)
dat2$FTAG <- as.integer(dat2$FTAG)
dat2$Result <- dat2$FTHG - dat2$FTAG
#dat2$Result[873] <- 0
dat3 <- dat2 %>% select(Spieltag, Spielid, HomeTeam, AwayTeam, Result, saison)
datb <- dat3
dat3 <- datb


#Platzierung pro Spieltag pro Team
dat3$Gewinner <- "X"
dat3$Gewinner[dat3$Result > 0 & !is.na(dat$Result)] <- dat3$HomeTeam[dat3$Result > 0 & !is.na(dat$Result)]
dat3$Gewinner[dat3$Result < 0 & !is.na(dat$Result)] <- dat3$AwayTeam[dat3$Result < 0 & !is.na(dat$Result)]

datp <- dat3 %>%
  gather(HeimAusw, Team, HomeTeam:AwayTeam) %>% select(-HeimAusw) %>%
  arrange(saison, Spieltag) %>%
  mutate(Punkte = ifelse(Team == Gewinner,3,ifelse(Gewinner == "X",1,0))) %>% select(-Gewinner, -Spielid, -Result)

datp2 <- datp %>%
  group_by(saison, Team) %>% mutate(Punktzahl = cumsum(Punkte)) %>%
  ungroup() 

datp3 <- datp2 %>% mutate(Spieltag = Spieltag + 1) %>%
  group_by(Spieltag, saison) %>% mutate(Platzierung = rank(-Punktzahl, ties.method = "min")) %>% select(-Punktzahl, -Punkte)

#Platzierung pro Spieltag pro Team ENDE
datp3
TRUE






######dataloader_neu


page <- c("shooting","keeper","passing","passing_types","gca","defense","possession","misc")


teams <- read_csv("raw/teams2122.csv")
ids <- read_delim("raw/teams_id.txt", delim = ";")

a2122 <- teams %>% left_join(ids, on = "teams")
a2122$teams <- str_replace(a2122$teams," ","-")
team <- a2122
saison <- "2021-2022"


dat2122 <- data.frame()
for (i in 1:18){
  datteam <- data.frame(teamname = team[i,1])
  for (j in 1:length(page)){
    a <- paste0("https://fbref.com/en/squads/",team[i,2],"/",saison, "/matchlogs/all_comps/",page[j],"/",team[i,1],"-Match-Logs-All-Competitions")
    a2 <- xml2::read_html(a) %>% rvest::html_nodes("#matchlogs_for") %>% rvest::html_table()
    a3 <- a2[[1]]
    colnames(a3) <- a3[1,]
    a3 <- a3[a3$Comp == "Bundesliga",]
    #    a3 <- a3[2:35,]
    datteam <- data.frame(datteam,a3)
  }
  dat2122 <- rbind(dat2122,datteam)
}
write_csv2(dat2122, file = "raw/hist_data_2122_raw.csv")

dat__raw1819 <- read_csv2("raw/hist_data_1819_raw.csv")
dat__raw1819$saison <- "1819"
dat__raw1920 <- read_csv2("raw/hist_data_1920_raw.csv")
dat__raw1920$saison <- "1920"
dat__raw2021 <- read_csv2("raw/hist_data_2021_raw.csv")
dat__raw2021$saison <- "2021"
dat__raw2122 <- read_csv2("raw/hist_data_2122_raw.csv")
dat__raw2122$saison <- "2122"



dat_all <- rbind(dat__raw1819, dat__raw1920, dat__raw2021, dat__raw2122)
dat_sel <- dat_all %>% select(-contains("Notes"),-contains("Date."),-contains("Time."),-contains("Match.Report"),-contains("Comp."),-contains("Day."))%>%
  select(-contains("Venue."),-contains("Captain"),-contains("Formation"),-contains("Opponent"),-contains("Round."),-contains("Referee"),-contains("Result.")) %>%
  select(-contains("Attendance")) %>% select(-"Tkl.Int") %>% mutate(Result = GF - GA)


library(caret)
dat__chars <- dat_sel[,-c(1:8)] %>% select(!where(is.numeric)) %>% mutate_all(as.numeric)
dat__nums <- dat_sel %>% select(where(is.numeric))
dat__nums <- data.frame(dat__chars, dat__nums)
dat__nums <- dat__nums %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))          
dat__cor <- cor(dat__nums)
corrs <- findCorrelation(dat__cor, cutoff=0.80)
dat__numsocor <- dat__nums[,-corrs]
dat__ <- data.frame(dat_sel[,1:8], saison = dat_sel$saison ,dat__numsocor)

dat_stats <- dat__
dat_stats$spieltag <- paste0(dat_stats$saison,"_", str_replace(dat_stats$Round,'Matchweek ',''))
dat_stats <- dat_stats %>%
  select(-(Date:Round),-Result,-saison.1) %>%
  mutate(Venue = case_when(Venue == "Home" ~ 1,
                           Venue == "Away" ~ 0),
         Day = case_when(Day == "Mon" ~ 0,
                         Day == "Tue" ~ 1,
                         Day == "Wed" ~ 2,
                         Day == "Fri" ~ 4,
                         Day == "Sat" ~ 5,
                         Day == "Son" ~ 6))






dat4 <- data.frame(spiel = 1:(nrow(dat2)))
dat4$S1 <- dat4$spiel - 1
dat4$S2 <- dat4$S1 -1
dat4$S3 <- dat4$S2 -1
dat4$S4 <- dat4$S3 -1
dat4$S5 <- dat4$S4 -1
dat5 <- dat3  %>%
  left_join(dat4, by = c("Spieltag" = "spiel")) %>% select(-Gewinner)
#dat6 <- dat3 %>% select(Spieltag, Spielid, HomeTeam)

dat8 <- dat3 %>% select( -Gewinner, -Result) %>%
  gather(Side, value, HomeTeam:AwayTeam) %>% select(-Side)
dat8$Spieltagid <- paste0(dat8$saison,'_', dat8$Spieltag)


ids <- read_delim("raw/teams_id.txt", delim = ";")

dat5_ <- dat5 %>% left_join(ids, by = c("HomeTeam" = "team_sched")) %>% select(-id) %>%
  left_join(ids, by = c("AwayTeam" = "team_sched")) %>% select(-id) 
dat5_$HomeTeam <- dat5_$teams.x
dat5_$AwayTeam <- dat5_$teams.y
dat5_ <- dat5_ %>% select(-teams.x, -teams.y)
dat5_$HomeTeam <- str_replace(dat5_$HomeTeam," ","-")
dat5_$AwayTeam <- str_replace(dat5_$AwayTeam," ","-")
dat5 <- dat5_



dat8_ <- dat8 %>% left_join(ids, by = c("value" = "team_sched")) %>% select(-id)
dat8_$teams <- str_replace(dat8_$teams," ","-")
dat8_$value <- dat8_$teams
dat8_ <- dat8_ %>% select(-teams) 
dat_stats2 <- dat_stats %>% left_join(dat8_, by = c("spieltag" = "Spieltagid", "teams" = "value"))
dat_stats3 <- dat_stats2 %>% select( -saison.x, -saison.y, -spieltag, -Spieltag,)
dat8_ <- dat8_ %>% select(-Spieltagid)
dat8 <- dat8_

dat7 <- dat5 %>% #filter(!is.na(FTR)) %>% 
  left_join(dat8, by = c("HomeTeam" = "value", "S1" = "Spieltag", "saison")) %>%
  left_join(dat8, by = c("AwayTeam" = "value", "S1" = "Spieltag", "saison")) %>%
  left_join(dat8, by = c("HomeTeam" = "value", "S2" = "Spieltag", "saison")) %>%
  left_join(dat8, by = c("AwayTeam" = "value", "S2" = "Spieltag", "saison")) %>%
  left_join(dat8, by = c("HomeTeam" = "value", "S3" = "Spieltag", "saison")) %>%
  left_join(dat8, by = c("AwayTeam" = "value", "S3" = "Spieltag", "saison")) %>%
  left_join(dat8, by = c("HomeTeam" = "value", "S4" = "Spieltag", "saison")) %>%
  left_join(dat8, by = c("AwayTeam" = "value", "S4" = "Spieltag", "saison")) %>%
  left_join(dat8, by = c("HomeTeam" = "value", "S5" = "Spieltag", "saison")) %>%
  left_join(dat8, by = c("AwayTeam" = "value", "S5" = "Spieltag", "saison")) %>%
  select(-Spieltag,-(S1:S5), -saison)

#dat7 <- na.omit(dat7)

#dat2 <- dat2 %>% left_join(datp3, by = c("saison", "Spieltag", "HomeTeam" = "Team")) %>% left_join(datp3, by = c("saison","Spieltag","AwayTeam"= "Team"))

#dat2_1 <- dat2 %>% select(-Div, -Date, -Spieltag, -saison)  %>%
#  select(Spielid,HomeTeam, AwayTeam,Result,HS, AS, HST,AST, Platzierung.x, Platzierung.y)
#colnames(dat7) <- c("Spielid", "FTR",paste0(c("Spielid_","Side_"),rep(c("H","A"), times = ncol(dat7)-2, each = 2), "_", rep(1:((ncol(dat7)-2)/4), each = 4)))
colnames(dat7) <- c("Spielid","Hometeam","Awayteam", "FTR",paste0(c("Spielid_"),rep(c("H","A"), times = ncol(dat7)-2, each = 1), "_", rep(1:((ncol(dat7)-2)/2), each = 2)))

rm(dat, dat10,dat2021 ,dat11, dat12, dat2, dat3, dat4, dat5, dat6, dat8, datb, datp, datp2, datp3)

dat2_1 <- dat_stats3



dat9 <- dat7 %>%
  
  #   filter(!is.na(dat7[,ncol(dat7)])) %>%
  left_join(dat2_1,by = c("Spielid_H_1" = "Spielid", "Hometeam" = "teams")) %>%
  left_join(dat2_1,by = c("Spielid_A_1" = "Spielid", "Awayteam" = "teams")) %>%
  left_join(dat2_1,by = c("Spielid_H_2" = "Spielid", "Hometeam" = "teams")) %>%
  left_join(dat2_1,by = c("Spielid_A_2" = "Spielid", "Awayteam" = "teams")) %>%
  left_join(dat2_1,by = c("Spielid_H_3" = "Spielid", "Hometeam" = "teams")) %>%
  left_join(dat2_1,by = c("Spielid_A_3" = "Spielid", "Awayteam" = "teams")) %>%
  left_join(dat2_1,by = c("Spielid_H_4" = "Spielid", "Hometeam" = "teams")) %>%
  left_join(dat2_1,by = c("Spielid_A_4" = "Spielid", "Awayteam" = "teams")) %>%
  left_join(dat2_1,by = c("Spielid_H_5" = "Spielid", "Hometeam" = "teams")) %>%
  left_join(dat2_1,by = c("Spielid_A_5" = "Spielid", "Awayteam" = "teams")) %>%
  select(-matches("Spielid_._."))




dat10 <- dat9 %>% separate(Spielid, remove = F ,into = c("Saison", "Spieltag", "Spielnummer"), sep = "_") %>% 
  mutate(Spieltag = as.integer(Spieltag)) %>%arrange(Saison, Spieltag) %>%
  select(-Saison, -Spieltag, -Spielnummer) %>% 
  select(-contains("Hometeam."), -contains("Awayteam."))

#dat10 <- dat10[!duplicated(dat10),]
#dat10 <- dat10[-(dim(dat10)[1]-(8:0)),]
#dat10 <- dat10 %>% replace(is.na(.),0)
converter <- dat10 %>% select(-(Spielid:FTR)) %>% mutate_all(funs(as.integer(.)))
dat10 <- bind_cols(dat10 %>% select((Spielid:FTR)), converter)




#(dat10)
#colnames(dat10)
dat10 <- dat10 %>%
  group_by(Hometeam) %>%  
  mutate(across(4:(ncol(dat10)-1), ~ ifelse(is.na(.),median(.,na.rm=TRUE), .)))


prod <- dat10[is.na(dat9$FTR),]
prod
dat10 <- dat10 %>% filter(!is.na(FTR))




write_csv2(dat10, file = "data/loaded_data.csv")
write_csv2(prod, file = "data/prod_data.csv")
