## MLB Injury Reporting Code ##

library(tidyverse)
library(readxl)
library(baseballr)

## Read in 2025 Data ##

mlb_2025 <- read_excel("RJ Analytics/MLB Injuries Raw Data.xlsx",
                       sheet="2025") |>
  distinct()

## Make sure we aren't duplicating players
## who have multiple IL stints ##

mlb_2025 <- mlb_2025 |>
  group_by(key_fangraphs) |>
  arrange(desc(`IL Retro Date`)) |>
  slice(1) |>
  ungroup()

## Identify Players who are yet to be activated (still on IL) ##

still_on_il <- mlb_2025 |>
  filter(Status != "Activated") |>
  select(Name,Team,Pos,`Injury / Surgery`,`IL Retro Date`,
         `Eligible to Return`,URL) |>
  arrange(`Eligible to Return`) |>
  rename(`Additional Information` = URL)

## Identify Players who have been activated ##

activated_players <- mlb_2025 |>
  filter(Status == "Activated") |>
  select(Name,Team,Pos,`Injury / Surgery`,`IL Retro Date`,
         `Return Date`,URL,key_fangraphs) |>
  arrange(`Return Date`) |>
  rename(`Additional Information` = URL)

## Subset to Pitchers & Batters ##

pitch <- activated_players |>
  mutate(Pos = ifelse(Pos == "SP" |
                        Pos == "RP" |
                        Pos == "SP/RP", "P",
                      ifelse(Pos == "DH", "DH", 
                             ifelse(Pos == "C" |
                                      Pos == "C/1B" |
                                      Pos == "C/OF", "C","Fielder")
                      )
                    )
  ) |>
  filter(Pos == "P")

## Calculate Mean/SD for WHIP for 2023 Regular Season
## for pitchers who had more than 15 IP ##

library(Lahman)

whip_df <- Pitching |>
  filter(yearID == 2023) |>
  group_by(playerID) |>
  summarise(whip = sum(BB + H) / sum(IPouts) * 3,
            IP = sum(IPouts) / 3) |>
  filter(IP >= 15) |>
  ungroup() |>
  summarise(mean_whip = mean(whip,na.rm=T),
            sd_whip = sd(whip,na.rm=T))

## Get Pitcher Performances since their return date ##

pitch_list <- vector('list',length=nrow(pitch))

for(i in 1:length(pitch_list)){
  
  pitch_list[[i]] <- fg_pitcher_game_logs(playerid = pitch$key_fangraphs[i],
                       year = 2025)
  
  pitch_list[[i]]$Date <- as.Date(pitch_list[[i]]$Date)
  
  pitch_list[[i]] <- pitch_list[[i]] |>
    filter(Date >= as.Date(pitch$`Return Date`[i]))
  
}

## Filter out pitch_list where we filter out pitchers who
## have 0 rows in their df ##

pitch_list2 <- pitch_list[!sapply(pitch_list, function(x) nrow(x) == 0)]

## Get Pitcher Names and key_fangraphs who have 0 rows in their df ##

have_not_returned_pitch <- pitch |>
  filter(!(key_fangraphs %in% unlist(lapply(pitch_list2, function(x) x$playerid)))) |>
  select(Name,Team,Pos,key_fangraphs)

## Loop through each player in pitchers_list ##

d <- 3

pitch_list3 <- vector('list', length = length(pitch_list2))

for (i in seq_along(pitch_list2)) {
  
  df <- pitch_list2[[i]] |>
    mutate(
      WHIP = if_else(IP == 0, NA_real_, (H + BB) / IP)
    ) |>
    select(PlayerName, playerid, Date, season, WHIP)
  
  # Compute USL and k values
  USL <- whip_df$mean_whip + d * whip_df$sd_whip
  k <- abs(USL - d) / 2
  
  # Compute W and sort by date
  player_logs <- df |>
    mutate(
      W = if_else(is.na(WHIP), 0, (USL - WHIP) / whip_df$sd_whip)
    ) |>
    arrange(Date)
  
  # Initialize CUSUM
  n <- nrow(player_logs)
  CUSUM <- numeric(n)
  
  if (n == 0) {
    pitch_list3[[i]] <- player_logs
    next
  }
  
  CUSUM[1] <- max(0, d - k - player_logs$W[1])
  
  if (n > 1) {
    for (j in 2:n) {
      CUSUM[j] <- max(0, CUSUM[j - 1] + d - k - player_logs$W[j])
    }
  }
  
  player_logs$CUSUM <- CUSUM
  pitch_list3[[i]] <- player_logs
}

## For each player in pitch_list3, if their max CUSUM
## is greater than 10, then they are considered "High Risk for Re-Injury"
## if their max CUSUM is between 6 and 10, then they are considered
## "Moderate Risk for Re-Injury" and if their max CUSUM is less than 6,
## then they are considered "Low Risk for Re-Injury" ##

pitch_list_df <- pitch_list3 |>
  bind_rows() |>
  group_by(playerid) |>
  filter(Date >= as.Date(Sys.Date() - 14)) |>
  summarise(PlayerName = first(PlayerName),
            Max_CUSUM = max(CUSUM,na.rm=T)) |>
  ungroup() |>
  mutate(Risk = case_when(
    Max_CUSUM >= 10 ~ "High Risk for Re-Injury",
    Max_CUSUM >= 6 & Max_CUSUM < 10 ~ "Moderate Risk for Re-Injury",
    TRUE ~ "Low Risk for Re-Injury"
  )) |>
  select(PlayerName,playerid,Max_CUSUM,Risk)

## Rowbind pitch_list_df with have_not_returned_pitch

pitch_list_final <- pitch_list_df |>
  bind_rows(have_not_returned_pitch |>
              rename(PlayerName = Name,
                     playerid = key_fangraphs) |>
              mutate(Max_CUSUM = NA,
                     Risk = "Has not appeared in MLB since return date",
                     playerid = as.integer(playerid)) |>
              select(PlayerName,playerid,Max_CUSUM,Risk))

## Join Risk Classification to pitch dataframe ##

pitch <- pitch |>
  mutate(key_fangraphs = as.integer(key_fangraphs)) |>
  left_join(pitch_list_final,
            by = c("key_fangraphs" = "playerid")) 


pitch <- pitch |>
  select(-key_fangraphs,-PlayerName,-Max_CUSUM)

pitch <- pitch |>
  group_by(Name) |>
  arrange(desc(`Return Date`)) |>
  slice_head(n=1) |>
  ungroup()

## For pitchers with NA Risk, set Risk to "Has not appeared in MLB since return date" ##

pitch <- pitch |>
  mutate(Risk = ifelse(is.na(Risk), "Has not appeared in MLB since return date", Risk))

## Great! Now for the batters ##

bat <- activated_players |>
  mutate(Pos = ifelse(Pos == "SP" |
                        Pos == "RP" |
                        Pos == "SP/RP", "P",
                      ifelse(Pos == "DH", "DH", 
                             ifelse(Pos == "C" |
                                      Pos == "C/1B" |
                                      Pos == "C/OF", "C","Fielder")
                      )
  )
  ) |>
  filter(Pos != "P")

## Get Batter Performances since their return date ##

bat_list <- vector('list',length=nrow(bat))

for(i in 1:length(bat_list)){
  
  bat_list[[i]] <- fg_batter_game_logs(playerid = bat$key_fangraphs[i],
                       year = 2025)
  
  bat_list[[i]]$Date <- as.Date(bat_list[[i]]$Date)
  
  bat_list[[i]] <- bat_list[[i]] |>
    filter(Date >= as.Date(bat$`Return Date`[i]))
  
}

## Filter out bat_list where we filter out batters who
## have 0 rows in their df ##

bat_list2 <- bat_list[!sapply(bat_list, function(x) nrow(x) == 0)]

## Get Batter Names and key_fangraphs who have 0 rows in their df ##

have_not_returned_bat <- bat |>
  filter(!(key_fangraphs %in% unlist(lapply(bat_list2, function(x) x$playerid)))) |>
  select(Name,Team,Pos,key_fangraphs)

## Filter out Dates from individual bat_list2 dataframes where the 
## player has 0 PA ##

bat_list2 <- lapply(bat_list2, function(x) {
  x |>
    filter(PA > 0)
})

## Filter out players who have 0 rows ##

bat_list2 <- lapply(bat_list2, function(x) {
  
  if(nrow(x) == 0){
    return(NULL)
  } else{
    return(x)
  }
  
})

## Remove NULL elements from bat_list2

bat_list2 <- bat_list2[!sapply(bat_list2, is.null)]

## Calculate CUSUM for wRC+ difference ##

bat_list3 <- vector('list', length = length(bat_list2))

for (i in seq_along(bat_list3)) {
  
  df <- bat_list2[[i]]
  
  df <- df |>
    mutate(wRC_plus_diff = `wRC+` - 75)
  
  # Initialize CUSUM
  CUSUM <- numeric(nrow(df))
  CUSUM[1] <- max(0, df$wRC_plus_diff[1])
  
  if (nrow(df) > 1) {
    for (j in 2:nrow(df)) {
      CUSUM[j] <- max(0, -df$wRC_plus_diff[j] + CUSUM[j - 1])
    }
  }
  
  df$CUSUM <- CUSUM
  bat_list3[[i]] <- df
}

## For each player in bat_list3, if their max CUSUM
## is greater than 120, then they are considered "High Risk for Re-Injury"
## if their max CUSUM is between 50 and 120, then they are considered
## "Moderate Risk for Re-Injury" and if their max CUSUM is less than 50,
## then they are considered "Low Risk for Re-Injury" ##

bat_list_df <- bat_list3 |>
  bind_rows() |>
  group_by(playerid) |>
  filter(Date >= as.Date(Sys.Date() - 7)) |>
  summarise(PlayerName = first(PlayerName),
            Max_CUSUM = max(CUSUM,na.rm=T)) |>
  ungroup() |>
  mutate(Risk = case_when(
    Max_CUSUM >= 120 ~ "High Risk for Re-Injury",
    Max_CUSUM >= 50 & Max_CUSUM < 120 ~ "Moderate Risk for Re-Injury",
    TRUE ~ "Low Risk for Re-Injury"
  )) |>
  select(PlayerName,playerid,Max_CUSUM,Risk)

## Rowbind bat_list_df with have_not_returned_bat

bat_list_final <- bat_list_df |>
  bind_rows(have_not_returned_bat |>
              rename(PlayerName = Name,
                     playerid = key_fangraphs) |>
              mutate(Max_CUSUM = NA,
                     Risk = "Has not appeared in MLB since return date",
                     playerid = as.integer(playerid)) |>
              select(PlayerName,playerid,Max_CUSUM,Risk)
            )

## Join Risk Classification to bat dataframe ##

bat <- bat |>
  mutate(key_fangraphs = as.integer(key_fangraphs)) |>
  left_join(bat_list_final,
            by = c("key_fangraphs" = "playerid"))

bat <- bat |>
  select(-key_fangraphs,-PlayerName,-Max_CUSUM)

## For batters with NA Risk, set Risk to "Has not appeared in MLB since return date" ##

bat <- bat |>
  mutate(Risk = ifelse(is.na(Risk), "Has not appeared in MLB since return date", Risk))

## Combine Pitchers and Batters ##

mlb_injury_risk <- bind_rows(pitch, bat) |>
  arrange(Team,Name)

## Create New Folder for this week's report ##

todays_date <- Sys.Date()

dir.create(paste0("RJ Analytics/MLB Weekly Injury Reports/Week of ",todays_date), showWarnings = FALSE)

## Write to Excel File ##

library(openxlsx)

todays_date <- Sys.Date()

write.xlsx(list(
  "Still on IL" = still_on_il,
  "Activated Pitchers" = pitch |> select(-Pos) |> arrange(Risk),
  "Activated Fielders" = bat |> select(-Pos) |> arrange(Risk)
), 
file = paste0("RJ Analytics/MLB Weekly Injury Reports/Week of ",todays_date, "/MLB Injury Risk Report ",todays_date,".xlsx")
)
