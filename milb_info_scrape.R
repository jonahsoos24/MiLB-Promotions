library(tidyverse)
library(baseballr)
library(lubridate)

# Getting a vector of possible week start dates
Dates <- seq.Date(
  from = as.Date("2023-03-31"),
  to = Sys.Date(),
  by = 1
)

# Cutting out dates outside of baseball season
Dates <- Dates[between(month(Dates), 3, 10)]

# Creating object for data to be binded into
game_pks <- NULL

for(StartDate in 1:length(Dates)){
  
  game_pks <- game_pks %>%
    rbind(
      suppressMessages(
        get_game_pks_mlb(
          date = Dates[StartDate],
          level_ids = c(11, 12, 13, 14)
        )
      ), use.names = TRUE, fill = TRUE
    )
  
  # printing loop progress
  print(StartDate/length(Dates))
}
game_pks <- game_pks %>%
  filter(!is.na(game_pk))

write_csv(game_pks, "MiLB_Game_PKs.csv")
read_csv("MiLB_Game_PKs.csv") %>%

game_pks_nocancel <- game_pks %>%
  filter(status.detailedState != "Cancelled" & status.detailedState != "Postponed")

game_info <- NULL
game_pks_nocancel$game_pk <- as.numeric(game_pks_nocancel$game_pk)

game_info <- map_df(.x = game_pks_nocancel$game_pk[1:8312], 
                    ~get_game_info_mlb(.x), .progress = TRUE)

write_csv(game_info, "MiLB_Game_Info.csv")