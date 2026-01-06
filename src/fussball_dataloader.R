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

#install.packages("bigrquery")
library(bigrquery)

#setwd("git/fussballvorhersage/fussballvorhersage")
getwd()

raw_data <- getURL("https://api.openligadb.de/getmatchdata/bl1/2021")
bundesliga <- fromJSON(raw_data)
#bundesliga

test <- readLines("https://www.google.com/calendar/ical/spielplan.1.bundesliga%40gmail.com/public/basic.ics")
#test
spielplan <- data.frame(DESC = ic_extract(test,"DESCRIPTION"),DATE = ic_extract(test,"DTSTART"),SPIEL = ic_extract(test,"SUMMARY"))
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

dat12 <- dat12 %>% select(Div:AR, saison) %>% select(-HTHG,-HTAG,-HTR, -Time)

dat2021 <- read_csv("raw/D1_20202021.csv", col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                                                                                         Time = col_time(format = "%H:%M")))
dat2021$saison <- "2021"
dat2021 <- dat2021 %>% select(Div:AR, saison) %>% select(-HTHG,-HTAG,-HTR, -Time)

dat11 <- read_csv("raw/D120192020.csv", col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                                                                                         Time = col_time(format = "%H:%M")))
dat11$saison <- "1920"
dat11 <- dat11 %>% select(Div:AR, saison) %>% select(-HTHG,-HTAG,-HTR, -Time)
dat10 <- read_csv("raw/D1_20182019.csv", col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                                                                                          Time = col_time(format = "%H:%M")))
dat10$saison <- "1819"
dat10 <- dat10 %>% select(Div:AR, saison) %>% select(-HTHG,-HTAG,-HTR)
dat <- rbind(dat10, dat11, dat2021, dat12)
#dat
avg_home <- dat %>%
    filter(!is.na(FTR)) %>%
    select(-Div,-Date, -AwayTeam, -FTR, -saison) %>%
    group_by(HomeTeam) %>%
    summarise(across(everything(), list(median)))
colnames(avg_home) <- gsub('_1','',colnames(avg_home))
avg_away <- dat %>%
    filter(!is.na(FTR)) %>%
    select(-Div,-Date, -HomeTeam, -FTR, -saison) %>%
    group_by(AwayTeam) %>%
    summarise(across(everything(), list(median)))
colnames(avg_away) <- gsub('_1','',colnames(avg_away))




dat2 <- dat 
dat2$Spieltag <- (rep(0:((nrow(dat2)-1)/9)%%34 + 1,each = 9)) #modulo fuer 1 bis 34 je Saison
dat2$Spielid <- paste0(dat2$saison,"_",as.character(dat2$Spieltag), "_", as.character(rep(1:9,nrow(dat2)/9)))
dat2$FTHG <- as.integer(dat2$FTHG)
dat2$FTAG <- as.integer(dat2$FTAG)
dat2$Result <- dat2$FTHG - dat2$FTAG
#dat2$Result[873] <- 0
dat3 <- dat2 %>% select(Spieltag, Spielid, HomeTeam, AwayTeam, FTR, Result, saison)
datb <- dat3
dat3 <- datb

#Platzierung pro Spieltag pro Team
dat3$Gewinner <- "X"
dat3$Gewinner[dat3$FTR == 'H' & !is.na(dat$FTR)] <- dat3$HomeTeam[dat3$FTR == 'H'& !is.na(dat$FTR)]
dat3$Gewinner[dat3$FTR == 'A'& !is.na(dat$FTR)] <- dat3$AwayTeam[dat3$FTR == 'A'& !is.na(dat$FTR)]

datp <- dat3 %>%
  gather(HeimAusw, Team, HomeTeam:AwayTeam) %>% select(-HeimAusw) %>%
  arrange(saison, Spieltag) %>%
  mutate(Punkte = ifelse(Team == Gewinner,3,ifelse(Gewinner == "X",1,0))) %>% select(-Gewinner, -FTR, -Spielid, -Result)

datp2 <- datp %>%
  group_by(saison, Team) %>% mutate(Punktzahl = cumsum(Punkte)) %>%
  ungroup() 

datp3 <- datp2 %>% mutate(Spieltag = Spieltag + 1) %>%
  group_by(Spieltag, saison) %>% mutate(Platzierung = rank(-Punktzahl, ties.method = "min")) %>% select(-Punktzahl, -Punkte)


#Platzierung pro Spieltag pro Team ENDE
datp3
TRUE




dat4 <- data.frame(spiel = 1:(nrow(dat2)))
dat4$S1 <- dat4$spiel - 1
dat4$S2 <- dat4$S1 -1
dat4$S3 <- dat4$S2 -1
dat4$S4 <- dat4$S3 -1
dat4$S5 <- dat4$S4 -1
dat5 <- dat3  %>%
  left_join(dat4, by = c("Spieltag" = "spiel")) %>% select(-Gewinner)
dat6 <- dat3 %>% select(Spieltag, Spielid, HomeTeam)

dat8 <- dat3 %>% select(-FTR, -Gewinner, -Result) %>%
  gather(Side, value, HomeTeam:AwayTeam) %>% select(-Side)


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
  select(-Spieltag,-(S1:S5), -saison, -FTR)

#dat7 <- na.omit(dat7)

dat2 <- dat2 %>% left_join(datp3, by = c("saison", "Spieltag", "HomeTeam" = "Team")) %>% left_join(datp3, by = c("saison","Spieltag","AwayTeam"= "Team"))

dat2_1 <- dat2 %>% select(-Div, -Date, -Spieltag, -saison)  %>%
  select(Spielid,HomeTeam, AwayTeam,Result,HS, AS, HST,AST, Platzierung.x, Platzierung.y)
#colnames(dat7) <- c("Spielid", "FTR",paste0(c("Spielid_","Side_"),rep(c("H","A"), times = ncol(dat7)-2, each = 2), "_", rep(1:((ncol(dat7)-2)/4), each = 4)))
colnames(dat7) <- c("Spielid","Hometeam","Awayteam", "FTR",paste0(c("Spielid_"),rep(c("H","A"), times = ncol(dat7)-2, each = 1), "_", rep(1:((ncol(dat7)-2)/2), each = 2)))

rm(dat, dat10,dat2021 ,dat11, dat12, dat2, dat3, dat4, dat5, dat6, dat8, datb, datp, datp2, datp3)

dat9 <- dat7 %>%
  filter(!is.na(dat7[,ncol(dat7)])) %>%
  left_join(dat2_1,by = c("Spielid_H_1" = "Spielid")) %>%
  left_join(dat2_1,by = c("Spielid_A_1" = "Spielid")) %>%
  left_join(dat2_1,by = c("Spielid_H_2" = "Spielid")) %>%
  left_join(dat2_1,by = c("Spielid_A_2" = "Spielid")) %>%
  left_join(dat2_1,by = c("Spielid_H_3" = "Spielid")) %>%
  left_join(dat2_1,by = c("Spielid_A_3" = "Spielid")) %>%
  left_join(dat2_1,by = c("Spielid_H_4" = "Spielid")) %>%
  left_join(dat2_1,by = c("Spielid_A_4" = "Spielid")) %>%
  left_join(dat2_1,by = c("Spielid_H_5" = "Spielid")) %>%
  left_join(dat2_1,by = c("Spielid_A_5" = "Spielid")) %>%
  select(-matches("Spielid_._."))




dat10 <- dat9 %>% separate(Spielid, remove = F ,into = c("Saison", "Spieltag", "Spielnummer"), sep = "_") %>% 
    mutate(Spieltag = as.integer(Spieltag)) %>%arrange(Saison, Spieltag) %>%
    select(-Saison, -Spieltag, -Spielnummer) %>% 
    select(-contains("Hometeam."), -contains("Awayteam."))

#dat10 <- dat10[!duplicated(dat10),]
#dat10 <- dat10[-(dim(dat10)[1]-(8:0)),]
dat10 <- dat10 %>% replace(is.na(.),0)
converter <- dat10 %>% select(-(Spielid:FTR)) %>% mutate_all(funs(as.integer(.)))
dat10 <- bind_cols(dat10 %>% select((Spielid:FTR)), converter)




#(dat10)
#colnames(dat10)
dat10 <- dat10 %>%
    group_by(Hometeam) %>%  
    mutate(across(4:73, ~ ifelse(is.na(.),median(.,na.rm=TRUE), .)))


prod <- dat10[is.na(dat9$FTR),]
prod
dat10 <- dat10 %>% filter(!is.na(FTR))




write_csv2(dat10, file = "data/loaded_data.csv")
write_csv2(prod, file = "data/prod_data.csv")
