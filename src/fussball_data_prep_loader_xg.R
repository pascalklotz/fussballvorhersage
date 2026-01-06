library(dplyr)
library(stringr)
library(readr)
library(tidyr)
library(readr)
#install.packages("readxl")
library("readxl")
#install.packages("zoo")
#rstudioapi::writeRStudioPreference("data_viewer_max_columns", 100L)

setwd("/home/pascal/fussballvorhersage")
getwd()


###### IMPORT #######

cols <- read_excel("fbref_scraping/colnames.xlsx")
cols <- cols[cols$import==1,]

dat2526 <- read_csv("fbref_scraping/hist_2526.csv")
dat2526$saison <- "2526"
dat2526 <- dat2526[, colnames(dat2526) %in% cols$colnames]
dat2425 <- read_csv("fbref_scraping/hist_2425.csv")
dat2425$saison <- "2425"
dat2425 <- dat2425[, colnames(dat2425) %in% cols$colnames]
dat2324 <- read_csv("fbref_scraping/hist_2324.csv")
dat2324$saison <- "2324"
dat2324 <- dat2324[, colnames(dat2324) %in% cols$colnames]
dat2223 <- read_csv("fbref_scraping/hist_2223.csv")
dat2223$saison <- "2223"
dat2223 <- dat2223[, colnames(dat2223) %in% cols$colnames]
dat2122 <- read_csv("fbref_scraping/hist_2122.csv")
dat2122$saison <- "2122"
dat2122 <- dat2122[, colnames(dat2122) %in% cols$colnames]
dat2021 <- read_csv("fbref_scraping/hist_2021.csv")
dat2021$saison <- "2021"
dat2021 <- dat2021[, colnames(dat2021) %in% cols$colnames]
dat1920 <- read_csv("fbref_scraping/hist_1920.csv")
dat1920$saison <- "1920"
dat1920 <- dat1920[, colnames(dat1920) %in% cols$colnames]

dat2526 <- dat2526[, colnames(dat2526) %in% colnames(dat2526)]
dat2425 <- dat2425[, colnames(dat2425) %in% colnames(dat2526)]
dat2324 <- dat2324[, colnames(dat2324) %in% colnames(dat2526)]
dat2223 <- dat2223[, colnames(dat2223) %in% colnames(dat2526)]
dat2122 <- dat2122[, colnames(dat2122) %in% colnames(dat2526)]
dat2021 <- dat2021[, colnames(dat2021) %in% colnames(dat2526)]
dat1920 <- dat1920[, colnames(dat1920) %in% colnames(dat2526)]

dat <- rbind(dat2526, dat2425, dat2324, dat2223, dat2122, dat2021, dat1920)

rollmean_num <- 4
if(rollmean_num > 0){
  dat <- dat %>%
    arrange(teamname, saison, spieltag)%>%
    group_by(teamname) %>%
    mutate(across(c(GF:GA,xG:`Won%`),
                  ~(ifelse(is.na(zoo::rollmean(.,rollmean_num,align = "right", fill = NA)),
                           .,
                           zoo::rollmean(.,rollmean_num,align = "right", fill = NA))),
                  .names = "avg_{.col}"))
}



dat$spieltag <- as.numeric(gsub("Matchweek ","", dat$Round))
dat <- dat[,-1]
dat$Result <- dat$GF - dat$GA
dat$Result_Goals <- dat$GF + dat$GA
dat$ResultxG <- dat$xG - dat$xGA
dat$Result_GoalsxG <- dat$xG + dat$xGA

dat <- dat %>% arrange(saison, spieltag)
dat <- dat %>% 
  arrange(saison, spieltag)%>% 
  mutate(Punkte_Home = ifelse(Result > 0, 3, ifelse(Result == 0, 1, 0))) %>%
  mutate(Punkte_Away = ifelse(Result < 0, 3, ifelse(Result == 0, 1, 0)))

dat <- dat %>%
  mutate(Venue = case_when(Venue == "Home" ~ 1,
                           Venue == "Away" ~ 0),
         D = case_when(D == "Mon" ~ 0,
                       D == "Tue" ~ 1,
                       D == "Wed" ~ 2,
                       D == "Thu" ~ 3,
                       D == "Fri" ~ 4,
                       D == "Sat" ~ 5,
                       D == "Sun" ~ 6))
df <- dat %>% filter(Venue == 1)
df <- df %>%
  group_by(saison,spieltag) %>%
  mutate(id = 1:9)


df <- df %>% 
  select(teamname, Opponent, Result, Result_Goals, ResultxG, Result_GoalsxG, saison,  spieltag, id)  %>% 
  mutate(Spielid = paste((saison), str_pad((spieltag),2, pad = "0"), str_pad((id),2, pad = "0"),sep = "_"))


df$S1 <- df$spieltag - 1
#df$S2 <- df$S1 -1
#df$S3 <- df$S2 -1
#df$S4 <- df$S3 -1
#df$S5 <- df$S4 -1

###### CREATE TABELLE #######

spielinfo <- dat  #%>% pivot_longer(cols = c(teamname, Opponent), names_to = "venue", values_to = "team")
spielinfo <- spielinfo %>% select(-Result, -Result_Goals, -ResultxG, -Result_GoalsxG, -Date, -Opponent) %>% 
  mutate(Punkte = case_when(Venue == 1 ~ Punkte_Home,
                            Venue == 0 ~ Punkte_Away)) %>%
  select(-Punkte_Home, -Punkte_Away)

spielinfo <- spielinfo %>%
  group_by(saison, teamname) %>% mutate(Points = cumsum(Punkte),tabelle = cumsum(Punkte) / spieltag) %>% ungroup()

test <- spielinfo %>% filter(teamname == 'Bayern Munich')
spielinfo <- spielinfo %>%
  select(-D)


spielinfo  <- spielinfo %>%
  mutate(xResult = xG - xGA
         , xGoals = xG + xGA
         , `Cmp%` = if_else(is.na(`Cmp%`),0,`Cmp%` )
         , `Def 3rd` = `Def 3rd` / (`Def 3rd` + `Mid 3rd` + `Att 3rd`)
         , `Mid 3rd` = `Mid 3rd` / (`Def 3rd` + `Mid 3rd` + `Att 3rd`)
         , `Att 3rd` = `Def 3rd` / (`Def 3rd` + `Mid 3rd` + `Att 3rd`)
  ) %>%
  select(-c(Punkte))

write_csv(spielinfo, file = "data/analyse_data.csv")

res <- df %>% #filter(!is.na(FTR)) %>% 
  left_join(spielinfo, by = c("teamname" = "teamname", "S1" = "spieltag", "saison")) %>%
  left_join(spielinfo, by = c("Opponent" = "teamname", "S1" = "spieltag", "saison"), suffix = c("","_1_away")) %>%
  #left_join(spielinfo, by = c("teamname" = "team", "S2" = "spieltag", "saison")) %>%
  #left_join(spielinfo, by = c("Opponent" = "team", "S2" = "spieltag", "saison")) %>%
  # left_join(spielinfo, by = c("teamname" = "team", "S3" = "spieltag", "saison")) %>%
  # left_join(spielinfo, by = c("Opponent" = "team", "S3" = "spieltag", "saison")) %>%
  # left_join(spielinfo, by = c("teamname" = "team", "S4" = "spieltag", "saison")) %>%
  # left_join(spielinfo, by = c("Opponent" = "team", "S4" = "spieltag", "saison")) %>%
  # left_join(spielinfo, by = c("teamname" = "team", "S5" = "spieltag", "saison")) %>%
  # left_join(spielinfo, by = c("Opponent" = "team", "S5" = "spieltag", "saison")) %>%
  select(-spieltag,-(S1:S1), -saison, -id) %>% ungroup()

res <- res %>%
  mutate(calc_result_home = GF - GA
         , calc_result_away = GF_1_away - GA_1_away
         , calc_poss = Poss - Poss_1_away
         , calc_poss_perc = (Poss - Poss_1_away) / Poss_1_away
      #   , calc_gls = Gls - Gls_1_away
         , calc_Sh = Sh - Sh_1_away
         , calc_SoT = SoT - SoT_1_away
         , `calc_SoT%` = `SoT%` - `SoT%_1_away`
         , `calc_G/Sh` = `G/Sh` - `G/Sh_1_away`
         , calc_SoTA = SoTA - SoTA_1_away
         , `calc_Save%` = `Save%` - `Save%_1_away`
         , calc_PG = PG - PG_1_away
         , `calc_Cmp%.1` = `Cmp%.1` - `Cmp%.1_1_away`
         , calc_PrgDist = PrgDist - PrgDist_1_away
         , calc_GCA = GCA - GCA_1_away
         , calc_Tkl = Tkl - Tkl_1_away
         , calc_Err = Err - Err_1_away
         , `calc_Att_3rd.1` = `Att 3rd.1` - `Att 3rd.1_1_away`
         , `calc_Won%` = `Won%`- `Won%_1_away`
         , calc_avg_result_home = avg_GF - avg_GA
         , calc_avg_result_away = avg_GF_1_away - avg_GA_1_away
         , calc_avg_poss = avg_Poss - avg_Poss_1_away
         , calc_avg_poss_perc = (avg_Poss - avg_Poss_1_away) / avg_Poss_1_away
      #   , calc_avg_gls = avg_Gls - avg_Gls_1_away
         , calc_avg_Sh = avg_Sh - avg_Sh_1_away
         , calc_avg_SoT = avg_SoT - avg_SoT_1_away
         , `calc_avg_SoT%` = `avg_SoT%` - `avg_SoT%_1_away`
         , `calc_avg_G/Sh` = `avg_G/Sh` - `avg_G/Sh_1_away`
         , calc_avg_SoTA = avg_SoTA - avg_SoTA_1_away
         , `calc_avg_Save%` = `avg_Save%` - `avg_Save%_1_away`
         , calc_avg_PG = avg_PG - avg_PG_1_away
         , `calc_avg_Cmp%.1` = `avg_Cmp%.1` - `avg_Cmp%.1_1_away`
         , calc_avg_PrgDist = avg_PrgDist - avg_PrgDist_1_away
         , calc_avg_GCA = avg_GCA - avg_GCA_1_away
         , calc_avg_Tkl = avg_Tkl - avg_Tkl_1_away
         , calc_avg_Err = avg_Err - avg_Err_1_away
         , `calc_avg_Att_3rd.1` = `avg_Att 3rd.1` - `avg_Att 3rd.1_1_away`
         , `calc_avg_Won%` = `avg_Won%`- `avg_Won%_1_away`
  )
test1 <- res %>% filter(teamname == 'Bayern Munich')

write_csv(res, file = "data/loaded_data.csv")



