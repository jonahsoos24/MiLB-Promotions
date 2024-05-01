library(tidyverse)
library(baseballr)

game_pks_nocancel <- read_csv("MiLB_Game_PKs.csv")
game_info <- read_csv("MiLB_Game_Info.csv")

MiLB_Season_Data <- game_pks_nocancel %>%
  left_join(game_info, by=c("game_pk" = "game_pk", "officialDate" = "game_date")) %>%
  unique() %>%
  select(1, 4, 5, 7, 9, 11, 16, 17, 20, 21, 35, 36, 42, 43, 45, 46, 52, 53, 55, 56, 74:79)
MiLB_Season_Data <- MiLB_Season_Data[, c(1:4, 13, 14, 11, 17, 18, 15, 12, 16, 5:10, 19:26)]

write_csv(MiLB_Season_Data, "MiLB_Season_Data.csv")

teams <- as.data.frame(mlb_team_affiliates(season = 2023))
teams <- teams %>%
  filter(sport_id == 11 |
         sport_id == 12 |
         sport_id == 13 |
         sport_id == 14
           ) %>%
  select(2, 16, 28, 35)

MiLB_Season_Data <- MiLB_Season_Data %>%
  left_join(teams, by=c("teams.home.team.id" = "team_id"))

MiLB_Project_Data <- MiLB_Season_Data %>%
  select(1, 4, 29, 28, 9, 6, 10, 7, 24) %>%
  unique()
colnames(MiLB_Project_Data) <- c("Game_PK", "Date", "Level", "League", "Home_Team", "Away_Team", "Home_Score", "Away_Score", "Attendence")

write_csv(MiLB_Project_Data, "MiLB_Project_Spreadsheet.csv")

dat_new <- read.csv("data_new.csv")
MiLB_Game_Info <- read.csv("MiLB_Game_Info.csv") %>% 
  select(2, 5:7, 9:10)

data_final <- dat_new %>%
  left_join(MiLB_Game_Info, by=c("Game_PK" = "game_pk"))

write_csv(data_final, "MiLB_Final_Data.csv")
