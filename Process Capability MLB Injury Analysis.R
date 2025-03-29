## New Process Capability Analysis for Injured MLB Pitchers ##

library(tidyverse)
library(baseballr)

## None of the below needs to be run again...just read in one_team_pitch
## on line 73 ##

## Loading Baseball Player ID Master File 
## From chadwich_player_lu ##

player_id <- chadwick_player_lu()

## Create Player Name Variable and remove blank spaces in first name
## if appropriate ##

player_id <- player_id |>
  mutate(name_first = gsub(" ","",name_first)) |>
  mutate(Name = paste(name_first,name_last,sep=" "))

## Reading in Andrew's Injury Data ##

## List files which start with One_Team ##

one_team_fn <- list.files(path = "MLB_Injury_Project/",pattern = "One_Team")

## List Files which start with more_teams ##

more_teams_fn <- list.files(path = "MLB_Injury_Project/",pattern = "more_teams")

## Read in the One_Team Files ##

one_team <- lapply(one_team_fn, FUN=function(x){
  
  fp <- paste("MLB_Injury_Project/",x,sep="")
  
  return(readxl::read_xlsx(fp))

})

## Read in the more_teams Files ##

more_teams <- lapply(more_teams_fn, FUN=function(x){
  
  fp <- paste("MLB_Injury_Project/",x,sep="")
  
  return(readxl::read_xlsx(fp))

})

## Concatenate One_Team Files ##

one_team <- bind_rows(one_team)

## Subset one_team to only include pitchers (RP, SP, SP/RP, RP/SP) ##

one_team_pitch <- one_team %>% 
  filter(Pos %in% c("RP","SP","SP/RP","RP/SP") &
           !is.na(Return_Date)) |>
  dplyr::select(!...13:...16) |>
  dplyr::select(Season,Name,Team,IL_Retro_Date,Return_Date)

## Merge one_team_pitch with player_id ##

one_team_pitch <- one_team_pitch |>
  left_join(player_id |>
              dplyr::select(Name,key_fangraphs),
            relationship = "one-to-many"
  )

## Read Back in one_team_pitch ##

one_team_pitch <- readxl::read_xlsx("one_team_pitch.xlsx")

## Determine if a player appears more than once in the data ##

multi_injuries <- one_team_pitch |>
  group_by(Name,Season) |>
  mutate(n = n()) |>
  ungroup() |>
  filter(n > 1 & n < 3) |>
  mutate(Multiple_Injury = "Yes")

## Reorder multi_injuries ##

multi_injuries2 <- multi_injuries |>
  group_by(Name,Season) |>
  arrange(IL_Retro_Date) |>
  mutate(Second_IL_Start_Date = max(IL_Retro_Date)) |>
  slice_head(n=1)

## Merge multi_injuries with one_team_pitch ##

one_team_pitch2 <- one_team_pitch |>
  left_join(multi_injuries2 |>
              dplyr::select(Name,Season,Multiple_Injury,Second_IL_Start_Date)
  ) |>
  mutate(Multiple_Injury = ifelse(is.na(Multiple_Injury),"No",Multiple_Injury)) |>
  distinct() |>
  group_by(Name,Season) |>
  arrange(IL_Retro_Date) |>
  slice_head(n=1)

## Let's just start with 2021 ##

## Specify Number of Standard Deviations used in 
## defining USL ##

d <- 3

one_team_pitch21 <- one_team_pitch2 |>
  filter(Season == 2021)

## Subset to just those players whose return date is before the end of august ##

one_team_pitch21 <- one_team_pitch21 |>
  filter(Return_Date <= "2021-08-31")

## Count Number of Appearances after return from injury ##

num_apps <- vector('double',length=nrow(one_team_pitch21))

for(i in 1:nrow(one_team_pitch21)){
  
  dt <- fg_pitcher_game_logs(one_team_pitch21$FanGraphs_ID[i],one_team_pitch21$Season[i])
  
  if(nrow(dt) == 0){
    
    num_apps[i] <- NA
    
  } else{
  
  num_apps[i] <- dt |>
    filter(Date >= one_team_pitch21$Return_Date[i]) |>
    nrow()
  }
}

## Append to one_team_pitch21 ##

one_team_pitch21$Num_Apps <- num_apps

## Remove NA and num_apps < 5 ##

one_team_pitch21 <- one_team_pitch21 |>
  filter(!is.na(Num_Apps) & Num_Apps >= 5)

## Now, we need to estimate mean and sd of WHIP for 2019 season
## We'd usually do last season but 2020 was weird. ##

whip_19 <- Lahman::Pitching |>
  filter(yearID == 2019 & 
           IPouts/3 >= 15) |>
  mutate(WHIP = (H+BB)/(IPouts/3)) |>
  summarize(Mean_WHIP = mean(WHIP,na.rm=TRUE),
            SD_WHIP = sd(WHIP,na.rm=TRUE))

Lahman::Pitching |>
  filter(yearID == 2019 & 
           IPouts/3 >= 15) |>
  mutate(WHIP = (H+BB)/(IPouts/3)) |>
  ggplot(aes(x=WHIP)) + geom_histogram(bins=15)

## Pretty normal ##

library(lubridate)

## Create a list to save individual player datasets ##

player_logs_list <- vector('list',length=nrow(one_team_pitch21))

## Loop through each player in one_team_pitch21 ##

for(i in 1:nrow(one_team_pitch21)){
  
  player <- one_team_pitch21[i,]
  
  player_logs <- fg_pitcher_game_logs(player$FanGraphs_ID,player$Season) |>
    filter(Date >= player$Return_Date) |>
    dplyr::select(PlayerName,Date,WHIP)
  
  USL <- whip_19$Mean_WHIP + d*whip_19$SD
  
  k <- abs(USL - d)/2
  
  player_logs |>
    mutate(W = (USL - WHIP)/whip_19$SD_WHIP) |>
    arrange(Date) -> player_logs

  CUSUM <- vector('double',length=nrow(player_logs))
  
  CUSUM[1] <- max(0,d-k-player_logs$W[1])
  
  for(j in 2:nrow(player_logs)){
    
    if(max(0,d - k - player_logs$W[j] + CUSUM[j-1]) == 0){
      
      CUSUM[j] <- 0
      
    } else{
      
      CUSUM[j] <- CUSUM[j-1] + d - k - player_logs$W[j]
      
    }
    
  }
  
  player_logs$CUSUM <- CUSUM
  
  player_logs_list[[i]] <- player_logs
  
}

## Add Multiple Injury Column to Each Dataframe in the List ##

for(i in 1:length(player_logs_list)){
  
  player_logs_list[[i]] <- player_logs_list[[i]] |>
    mutate(Multiple_Injury = one_team_pitch21$Multiple_Injury[i])
  
}

## Evaluate h thresholds of 1:20 ##

for(i in 1:length(player_logs_list)){
  
  player_logs_list[[i]] <- player_logs_list[[i]] |>
    mutate(CUSUM_1 = ifelse(CUSUM > 1,"Yes","No"),
           CUSUM_2 = ifelse(CUSUM > 2,"Yes","No"),
           CUSUM_3 = ifelse(CUSUM > 3,"Yes","No"),
           CUSUM_4 = ifelse(CUSUM > 4,"Yes","No"),
           CUSUM_5 = ifelse(CUSUM > 5,"Yes","No"),
           CUSUM_6 = ifelse(CUSUM > 6,"Yes","No"),
           CUSUM_7 = ifelse(CUSUM > 7,"Yes","No"),
           CUSUM_8 = ifelse(CUSUM > 8,"Yes","No"),
           CUSUM_9 = ifelse(CUSUM > 9,"Yes","No"),
           CUSUM_10 = ifelse(CUSUM > 10,"Yes","No"),
           CUSUM_11 = ifelse(CUSUM > 11,"Yes","No"),
           CUSUM_12 = ifelse(CUSUM > 12,"Yes","No"),
           CUSUM_13 = ifelse(CUSUM > 13,"Yes","No"),
           CUSUM_14 = ifelse(CUSUM > 14,"Yes","No"),
           CUSUM_15 = ifelse(CUSUM > 15,"Yes","No"),
           CUSUM_16 = ifelse(CUSUM > 16,"Yes","No"),
           CUSUM_17 = ifelse(CUSUM > 17,"Yes","No"),
           CUSUM_18 = ifelse(CUSUM > 18,"Yes","No"),
           CUSUM_19 = ifelse(CUSUM > 19,"Yes","No"),
           CUSUM_20 = ifelse(CUSUM > 20,"Yes","No"))
  
}

## Contingency Table to Determine if CUSUM predicts second injury ##

cusum_pred <- lapply(player_logs_list,FUN=function(x){
  
  return(c(x$Multiple_Injury[1],
           ifelse(sum(x$CUSUM_1 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_2 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_3 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_4 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_5 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_6 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_7 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_8 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_9 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_10 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_11 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_12 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_13 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_14 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_15 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_16 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_17 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_18 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_19 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_20 == "Yes") > 0,"Yes","No")
           
  ))
  
})

cusum_pred <- as.data.frame(do.call('rbind',cusum_pred))

names(cusum_pred) <- c("Multiple_Injury","CUSUM_1","CUSUM_2","CUSUM_3","CUSUM_4","CUSUM_5",
                       "CUSUM_6","CUSUM_7","CUSUM_8","CUSUM_9","CUSUM_10","CUSUM_11","CUSUM_12",
                       "CUSUM_13","CUSUM_14","CUSUM_15","CUSUM_16","CUSUM_17","CUSUM_18","CUSUM_19",
                       "CUSUM_20")

## Calculate Sensitivity and Specificity for CUSUM_1 ##

sensitivity_1 <- sum(cusum_pred$Multiple_Injury == "Yes" & cusum_pred$CUSUM_1 == "Yes")/
  sum(cusum_pred$Multiple_Injury == "Yes")

specificity_1 <- sum(cusum_pred$Multiple_Injury == "No" & cusum_pred$CUSUM_1 == "No")/
  sum(cusum_pred$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_2 ##

sensitivity_2 <- sum(cusum_pred$Multiple_Injury == "Yes" & cusum_pred$CUSUM_2 == "Yes")/
  sum(cusum_pred$Multiple_Injury == "Yes")

specificity_2 <- sum(cusum_pred$Multiple_Injury == "No" & cusum_pred$CUSUM_2 == "No")/
  sum(cusum_pred$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_3 ##

sensitivity_3 <- sum(cusum_pred$Multiple_Injury == "Yes" & cusum_pred$CUSUM_3 == "Yes")/
  sum(cusum_pred$Multiple_Injury == "Yes")

specificity_3 <- sum(cusum_pred$Multiple_Injury == "No" & cusum_pred$CUSUM_3 == "No")/
  sum(cusum_pred$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_4 ##

sensitivity_4 <- sum(cusum_pred$Multiple_Injury == "Yes" & cusum_pred$CUSUM_4 == "Yes")/
  sum(cusum_pred$Multiple_Injury == "Yes")

specificity_4 <- sum(cusum_pred$Multiple_Injury == "No" & cusum_pred$CUSUM_4 == "No")/
  sum(cusum_pred$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_5 ##

sensitivity_5 <- sum(cusum_pred$Multiple_Injury == "Yes" & cusum_pred$CUSUM_5 == "Yes")/
  sum(cusum_pred$Multiple_Injury == "Yes")

specificity_5 <- sum(cusum_pred$Multiple_Injury == "No" & cusum_pred$CUSUM_5 == "No")/
  sum(cusum_pred$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_6 ##

sensitivity_6 <- sum(cusum_pred$Multiple_Injury == "Yes" & cusum_pred$CUSUM_6 == "Yes")/
  sum(cusum_pred$Multiple_Injury == "Yes")

specificity_6 <- sum(cusum_pred$Multiple_Injury == "No" & cusum_pred$CUSUM_6 == "No")/
  sum(cusum_pred$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_7 ##

sensitivity_7 <- sum(cusum_pred$Multiple_Injury == "Yes" & cusum_pred$CUSUM_7 == "Yes")/
  sum(cusum_pred$Multiple_Injury == "Yes")

specificity_7 <- sum(cusum_pred$Multiple_Injury == "No" & cusum_pred$CUSUM_7 == "No")/
  sum(cusum_pred$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_8 ##

sensitivity_8 <- sum(cusum_pred$Multiple_Injury == "Yes" & cusum_pred$CUSUM_8 == "Yes")/
  sum(cusum_pred$Multiple_Injury == "Yes")

specificity_8 <- sum(cusum_pred$Multiple_Injury == "No" & cusum_pred$CUSUM_8 == "No")/
  sum(cusum_pred$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_9 ##

sensitivity_9 <- sum(cusum_pred$Multiple_Injury == "Yes" & cusum_pred$CUSUM_9 == "Yes")/
  sum(cusum_pred$Multiple_Injury == "Yes")

specificity_9 <- sum(cusum_pred$Multiple_Injury == "No" & cusum_pred$CUSUM_9 == "No")/
  sum(cusum_pred$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_10 ##

sensitivity_10 <- sum(cusum_pred$Multiple_Injury == "Yes" & cusum_pred$CUSUM_10 == "Yes")/
  sum(cusum_pred$Multiple_Injury == "Yes")

specificity_10 <- sum(cusum_pred$Multiple_Injury == "No" & cusum_pred$CUSUM_10 == "No")/
  sum(cusum_pred$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_11 ##

sensitivity_11 <- sum(cusum_pred$Multiple_Injury == "Yes" & cusum_pred$CUSUM_11 == "Yes")/
  sum(cusum_pred$Multiple_Injury == "Yes")

specificity_11 <- sum(cusum_pred$Multiple_Injury == "No" & cusum_pred$CUSUM_11 == "No")/
  sum(cusum_pred$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_12 ##

sensitivity_12 <- sum(cusum_pred$Multiple_Injury == "Yes" & cusum_pred$CUSUM_12 == "Yes")/
  sum(cusum_pred$Multiple_Injury == "Yes")

specificity_12 <- sum(cusum_pred$Multiple_Injury == "No" & cusum_pred$CUSUM_12 == "No")/
  sum(cusum_pred$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_13 ##

sensitivity_13 <- sum(cusum_pred$Multiple_Injury == "Yes" & cusum_pred$CUSUM_13 == "Yes")/
  sum(cusum_pred$Multiple_Injury == "Yes")

specificity_13 <- sum(cusum_pred$Multiple_Injury == "No" & cusum_pred$CUSUM_13 == "No")/
  sum(cusum_pred$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_14 ##

sensitivity_14 <- sum(cusum_pred$Multiple_Injury == "Yes" & cusum_pred$CUSUM_14 == "Yes")/
  sum(cusum_pred$Multiple_Injury == "Yes")

specificity_14 <- sum(cusum_pred$Multiple_Injury == "No" & cusum_pred$CUSUM_14 == "No")/
  sum(cusum_pred$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_15 ##

sensitivity_15 <- sum(cusum_pred$Multiple_Injury == "Yes" & cusum_pred$CUSUM_15 == "Yes")/
  sum(cusum_pred$Multiple_Injury == "Yes")

specificity_15 <- sum(cusum_pred$Multiple_Injury == "No" & cusum_pred$CUSUM_15 == "No")/
  sum(cusum_pred$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_16 ##

sensitivity_16 <- sum(cusum_pred$Multiple_Injury == "Yes" & cusum_pred$CUSUM_16 == "Yes")/
  sum(cusum_pred$Multiple_Injury == "Yes")

specificity_16 <- sum(cusum_pred$Multiple_Injury == "No" & cusum_pred$CUSUM_16 == "No")/
  sum(cusum_pred$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_17 ##

sensitivity_17 <- sum(cusum_pred$Multiple_Injury == "Yes" & cusum_pred$CUSUM_17 == "Yes")/
  sum(cusum_pred$Multiple_Injury == "Yes")

specificity_17 <- sum(cusum_pred$Multiple_Injury == "No" & cusum_pred$CUSUM_17 == "No")/
  sum(cusum_pred$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_18 ##

sensitivity_18 <- sum(cusum_pred$Multiple_Injury == "Yes" & cusum_pred$CUSUM_18 == "Yes")/
  sum(cusum_pred$Multiple_Injury == "Yes")

specificity_18 <- sum(cusum_pred$Multiple_Injury == "No" & cusum_pred$CUSUM_18 == "No")/
  sum(cusum_pred$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_19 ##

sensitivity_19 <- sum(cusum_pred$Multiple_Injury == "Yes" & cusum_pred$CUSUM_19 == "Yes")/
  sum(cusum_pred$Multiple_Injury == "Yes")

specificity_19 <- sum(cusum_pred$Multiple_Injury == "No" & cusum_pred$CUSUM_19 == "No")/
  sum(cusum_pred$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_20 ##

sensitivity_20 <- sum(cusum_pred$Multiple_Injury == "Yes" & cusum_pred$CUSUM_20 == "Yes")/
  sum(cusum_pred$Multiple_Injury == "Yes")

specificity_20 <- sum(cusum_pred$Multiple_Injury == "No" & cusum_pred$CUSUM_20 == "No")/
  sum(cusum_pred$Multiple_Injury == "No")

## Plot the relationship between sensitivity and specificity ##

sens_spec <- data.frame(CUSUM = 1:20,
                        Sensitivity = c(sensitivity_1,sensitivity_2,sensitivity_3,sensitivity_4,sensitivity_5,
                                        sensitivity_6,sensitivity_7,sensitivity_8,sensitivity_9,sensitivity_10,
                                        sensitivity_11,sensitivity_12,sensitivity_13,sensitivity_14,sensitivity_15,
                                        sensitivity_16,sensitivity_17,sensitivity_18,sensitivity_19,sensitivity_20),
                        Specificity = c(specificity_1,specificity_2,specificity_3,specificity_4,specificity_5,
                                        specificity_6,specificity_7,specificity_8,specificity_9,specificity_10,
                                        specificity_11,specificity_12,specificity_13,specificity_14,specificity_15,
                                        specificity_16,specificity_17,specificity_18,specificity_19,specificity_20)
                        
)

sens_spec |>
  ggplot(aes(x=1-Specificity,y=Sensitivity,label=CUSUM)) + 
  geom_point() + 
  geom_line() +
  geom_text(hjust=-0.1,vjust=0.5) +
  geom_abline(intercept = 0,slope = 1)

## Save the Sensitivity and Specificity Data for 2021 season ##

sens_spec |>
  writexl::write_xlsx("sens_spec_2021_d2.xlsx")

## Now let's do this exact same thing for 2022 and 2023 ##

## 2022 ##

one_team_pitch22 <- one_team_pitch2 |>
  filter(Season == 2022)

## Subset to just those players whose return date is before the end of august ##

one_team_pitch22 <- one_team_pitch22 |>
  filter(Return_Date <= "2022-08-31")

## Count Number of Appearances after return from injury ##

num_apps <- vector('double',length=nrow(one_team_pitch22))

for(i in 1:nrow(one_team_pitch22)){
  
  dt <- fg_pitcher_game_logs(one_team_pitch22$FanGraphs_ID[i],one_team_pitch22$Season[i])
  
  if(nrow(dt) == 0){
    
    num_apps[i] <- NA
    
  } else{
  
  num_apps[i] <- dt |>
    filter(Date >= one_team_pitch22$Return_Date[i]) |>
    nrow()
  }
}

## Append to one_team_pitch22 ##

one_team_pitch22$Num_Apps <- num_apps

## Remove NA and num_apps < 5 ##

one_team_pitch22 <- one_team_pitch22 |>
  filter(!is.na(Num_Apps) & Num_Apps >= 5)

## Now, we need to estimate mean and sd of WHIP for 2021 season

whip_21 <- Lahman::Pitching |>
  filter(yearID == 2021 & 
           IPouts/3 >= 15) |>
  mutate(WHIP = (H+BB)/(IPouts/3)) |>
  summarize(Mean_WHIP = mean(WHIP,na.rm=TRUE),
            SD_WHIP = sd(WHIP,na.rm=TRUE))

Lahman::Pitching |>
  filter(yearID == 2021 & 
           IPouts/3 >= 15) |>
  mutate(WHIP = (H+BB)/(IPouts/3)) |>
  ggplot(aes(x=WHIP)) + geom_histogram(bins=15)

## Pretty normal ##

## Loop through all players ##

player_logs_list22 <- vector('list',length=nrow(one_team_pitch22))

for(i in 1:nrow(one_team_pitch22)){
  
  player <- one_team_pitch22[i,]
  
  player_logs <- fg_pitcher_game_logs(player$FanGraphs_ID,player$Season) |>
    filter(Date >= player$Return_Date) |>
    dplyr::select(PlayerName,Date,WHIP)
  
  USL <- whip_21$Mean_WHIP + d*whip_21$SD
  
  k <- abs(USL - d)/2
  
  player_logs |>
    mutate(W = (USL - WHIP)/whip_21$SD_WHIP) |>
    arrange(Date) -> player_logs

  CUSUM <- vector('double',length=nrow(player_logs))
  
  CUSUM[1] <- max(0,d-k-player_logs$W[1])
  
  for(j in 2:nrow(player_logs)){
    
    if(max(0,d - k - player_logs$W[j] + CUSUM[j-1]) == 0){
      
      CUSUM[j] <- 0
      
    } else{
      
      CUSUM[j] <- CUSUM[j-1] + d - k - player_logs$W[j]
      
    }
    
  }
  
  player_logs$CUSUM <- CUSUM
  
  player_logs_list22[[i]] <- player_logs
  
}

## Add Multiple Injury Column to Each Dataframe in the List ##

for(i in 1:length(player_logs_list22)){
  
  player_logs_list22[[i]] <- player_logs_list22[[i]] |>
    mutate(Multiple_Injury = one_team_pitch22$Multiple_Injury[i])
  
}

## Evaluate h thresholds of 1:20 ##

for(i in 1:length(player_logs_list22)){
  
  player_logs_list22[[i]] <- player_logs_list22[[i]] |>
    mutate(CUSUM_1 = ifelse(CUSUM > 1,"Yes","No"),
           CUSUM_2 = ifelse(CUSUM > 2,"Yes","No"),
           CUSUM_3 = ifelse(CUSUM > 3,"Yes","No"),
           CUSUM_4 = ifelse(CUSUM > 4,"Yes","No"),
           CUSUM_5 = ifelse(CUSUM > 5,"Yes","No"),
           CUSUM_6 = ifelse(CUSUM > 6,"Yes","No"),
           CUSUM_7 = ifelse(CUSUM > 7,"Yes","No"),
           CUSUM_8 = ifelse(CUSUM > 8,"Yes","No"),
           CUSUM_9 = ifelse(CUSUM > 9,"Yes","No"),
           CUSUM_10 = ifelse(CUSUM > 10,"Yes","No"),
           CUSUM_11 = ifelse(CUSUM > 11,"Yes","No"),
           CUSUM_12 = ifelse(CUSUM > 12,"Yes","No"),
           CUSUM_13 = ifelse(CUSUM > 13,"Yes","No"),
           CUSUM_14 = ifelse(CUSUM > 14,"Yes","No"),
           CUSUM_15 = ifelse(CUSUM > 15,"Yes","No"),
           CUSUM_16 = ifelse(CUSUM > 16,"Yes","No"),
           CUSUM_17 = ifelse(CUSUM > 17,"Yes","No"),
           CUSUM_18 = ifelse(CUSUM > 18,"Yes","No"),
           CUSUM_19 = ifelse(CUSUM > 19,"Yes","No"),
           CUSUM_20 = ifelse(CUSUM > 20,"Yes","No"))
  
}

## Contingency Table to Determine if CUSUM predicts second injury ##

cusum_pred22 <- lapply(player_logs_list22,FUN=function(x){
  
  return(c(x$Multiple_Injury[1],
           ifelse(sum(x$CUSUM_1 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_2 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_3 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_4 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_5 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_6 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_7 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_8 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_9 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_10 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_11 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_12 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_13 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_14 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_15 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_16 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_17 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_18 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_19 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_20 == "Yes") > 0,"Yes","No")
           
  ))
  
})

cusum_pred22 <- as.data.frame(do.call('rbind',cusum_pred22))

names(cusum_pred22) <- c("Multiple_Injury","CUSUM_1","CUSUM_2","CUSUM_3","CUSUM_4","CUSUM_5",
                       "CUSUM_6","CUSUM_7","CUSUM_8","CUSUM_9","CUSUM_10","CUSUM_11","CUSUM_12",
                       "CUSUM_13","CUSUM_14","CUSUM_15","CUSUM_16","CUSUM_17","CUSUM_18","CUSUM_19",
                       "CUSUM_20")

## Calculate Sensitivity and Specificity for CUSUM_1 ##

sensitivity_1 <- sum(cusum_pred22$Multiple_Injury == "Yes" & cusum_pred22$CUSUM_1 == "Yes")/
  sum(cusum_pred22$Multiple_Injury == "Yes")

specificity_1 <- sum(cusum_pred22$Multiple_Injury == "No" & cusum_pred22$CUSUM_1 == "No")/
  sum(cusum_pred22$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_2 ##

sensitivity_2 <- sum(cusum_pred22$Multiple_Injury == "Yes" & cusum_pred22$CUSUM_2 == "Yes")/
  sum(cusum_pred22$Multiple_Injury == "Yes")

specificity_2 <- sum(cusum_pred22$Multiple_Injury == "No" & cusum_pred22$CUSUM_2 == "No")/
  sum(cusum_pred22$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_3 ##

sensitivity_3 <- sum(cusum_pred22$Multiple_Injury == "Yes" & cusum_pred22$CUSUM_3 == "Yes")/
  sum(cusum_pred22$Multiple_Injury == "Yes")

specificity_3 <- sum(cusum_pred22$Multiple_Injury == "No" & cusum_pred22$CUSUM_3 == "No")/
  sum(cusum_pred22$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_4 ##

sensitivity_4 <- sum(cusum_pred22$Multiple_Injury == "Yes" & cusum_pred22$CUSUM_4 == "Yes")/
  sum(cusum_pred22$Multiple_Injury == "Yes")

specificity_4 <- sum(cusum_pred22$Multiple_Injury == "No" & cusum_pred22$CUSUM_4 == "No")/
  sum(cusum_pred22$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_5 ##

sensitivity_5 <- sum(cusum_pred22$Multiple_Injury == "Yes" & cusum_pred22$CUSUM_5 == "Yes")/
  sum(cusum_pred22$Multiple_Injury == "Yes")

specificity_5 <- sum(cusum_pred22$Multiple_Injury == "No" & cusum_pred22$CUSUM_5 == "No")/
  sum(cusum_pred22$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_6 ##

sensitivity_6 <- sum(cusum_pred22$Multiple_Injury == "Yes" & cusum_pred22$CUSUM_6 == "Yes")/
  sum(cusum_pred22$Multiple_Injury == "Yes")

specificity_6 <- sum(cusum_pred22$Multiple_Injury == "No" & cusum_pred22$CUSUM_6 == "No")/
  sum(cusum_pred22$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_7 ##

sensitivity_7 <- sum(cusum_pred22$Multiple_Injury == "Yes" & cusum_pred22$CUSUM_7 == "Yes")/
  sum(cusum_pred22$Multiple_Injury == "Yes")

specificity_7 <- sum(cusum_pred22$Multiple_Injury == "No" & cusum_pred22$CUSUM_7 == "No")/
  sum(cusum_pred22$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_8 ##

sensitivity_8 <- sum(cusum_pred22$Multiple_Injury == "Yes" & cusum_pred22$CUSUM_8 == "Yes")/
  sum(cusum_pred22$Multiple_Injury == "Yes")

specificity_8 <- sum(cusum_pred22$Multiple_Injury == "No" & cusum_pred22$CUSUM_8 == "No")/
  sum(cusum_pred22$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_9 ##

sensitivity_9 <- sum(cusum_pred22$Multiple_Injury == "Yes" & cusum_pred22$CUSUM_9 == "Yes")/
  sum(cusum_pred22$Multiple_Injury == "Yes")

specificity_9 <- sum(cusum_pred22$Multiple_Injury == "No" & cusum_pred22$CUSUM_9 == "No")/
  sum(cusum_pred22$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_10 ##

sensitivity_10 <- sum(cusum_pred22$Multiple_Injury == "Yes" & cusum_pred22$CUSUM_10 == "Yes")/
  sum(cusum_pred22$Multiple_Injury == "Yes")

specificity_10 <- sum(cusum_pred22$Multiple_Injury == "No" & cusum_pred22$CUSUM_10 == "No")/
  sum(cusum_pred22$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_11 ##

sensitivity_11 <- sum(cusum_pred22$Multiple_Injury == "Yes" & cusum_pred22$CUSUM_11 == "Yes")/
  sum(cusum_pred22$Multiple_Injury == "Yes")

specificity_11 <- sum(cusum_pred22$Multiple_Injury == "No" & cusum_pred22$CUSUM_11 == "No")/
  sum(cusum_pred22$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_12 ##

sensitivity_12 <- sum(cusum_pred22$Multiple_Injury == "Yes" & cusum_pred22$CUSUM_12 == "Yes")/
  sum(cusum_pred22$Multiple_Injury == "Yes")

specificity_12 <- sum(cusum_pred22$Multiple_Injury == "No" & cusum_pred22$CUSUM_12 == "No")/
  sum(cusum_pred22$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_13 ##

sensitivity_13 <- sum(cusum_pred22$Multiple_Injury == "Yes" & cusum_pred22$CUSUM_13 == "Yes")/
  sum(cusum_pred22$Multiple_Injury == "Yes")

specificity_13 <- sum(cusum_pred22$Multiple_Injury == "No" & cusum_pred22$CUSUM_13 == "No")/
  sum(cusum_pred22$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_14 ##

sensitivity_14 <- sum(cusum_pred22$Multiple_Injury == "Yes" & cusum_pred22$CUSUM_14 == "Yes")/
  sum(cusum_pred22$Multiple_Injury == "Yes")

specificity_14 <- sum(cusum_pred22$Multiple_Injury == "No" & cusum_pred22$CUSUM_14 == "No")/
  sum(cusum_pred22$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_15 ##

sensitivity_15 <- sum(cusum_pred22$Multiple_Injury == "Yes" & cusum_pred22$CUSUM_15 == "Yes")/
  sum(cusum_pred22$Multiple_Injury == "Yes")

specificity_15 <- sum(cusum_pred22$Multiple_Injury == "No" & cusum_pred22$CUSUM_15 == "No")/
  sum(cusum_pred22$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_16 ##

sensitivity_16 <- sum(cusum_pred22$Multiple_Injury == "Yes" & cusum_pred22$CUSUM_16 == "Yes")/
  sum(cusum_pred22$Multiple_Injury == "Yes")

specificity_16 <- sum(cusum_pred22$Multiple_Injury == "No" & cusum_pred22$CUSUM_16 == "No")/
  sum(cusum_pred22$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_17 ##

sensitivity_17 <- sum(cusum_pred22$Multiple_Injury == "Yes" & cusum_pred22$CUSUM_17 == "Yes")/
  sum(cusum_pred22$Multiple_Injury == "Yes")

specificity_17 <- sum(cusum_pred22$Multiple_Injury == "No" & cusum_pred22$CUSUM_17 == "No")/
  sum(cusum_pred22$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_18 ##

sensitivity_18 <- sum(cusum_pred22$Multiple_Injury == "Yes" & cusum_pred22$CUSUM_18 == "Yes")/
  sum(cusum_pred22$Multiple_Injury == "Yes")

specificity_18 <- sum(cusum_pred22$Multiple_Injury == "No" & cusum_pred22$CUSUM_18 == "No")/
  sum(cusum_pred22$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_19 ##

sensitivity_19 <- sum(cusum_pred22$Multiple_Injury == "Yes" & cusum_pred22$CUSUM_19 == "Yes")/
  sum(cusum_pred22$Multiple_Injury == "Yes")

specificity_19 <- sum(cusum_pred22$Multiple_Injury == "No" & cusum_pred22$CUSUM_19 == "No")/
  sum(cusum_pred22$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_20 ##

sensitivity_20 <- sum(cusum_pred22$Multiple_Injury == "Yes" & cusum_pred22$CUSUM_20 == "Yes")/
  sum(cusum_pred22$Multiple_Injury == "Yes")

specificity_20 <- sum(cusum_pred22$Multiple_Injury == "No" & cusum_pred22$CUSUM_20 == "No")/
  sum(cusum_pred22$Multiple_Injury == "No")

## Plot the relationship between sensitivity and specificity ##

sens_spec_2022 <- data.frame(CUSUM = 1:20,
                        Sensitivity = c(sensitivity_1,sensitivity_2,sensitivity_3,sensitivity_4,sensitivity_5,
                                        sensitivity_6,sensitivity_7,sensitivity_8,sensitivity_9,sensitivity_10,
                                        sensitivity_11,sensitivity_12,sensitivity_13,sensitivity_14,sensitivity_15,
                                        sensitivity_16,sensitivity_17,sensitivity_18,sensitivity_19,sensitivity_20),
                        Specificity = c(specificity_1,specificity_2,specificity_3,specificity_4,specificity_5,
                                        specificity_6,specificity_7,specificity_8,specificity_9,specificity_10,
                                        specificity_11,specificity_12,specificity_13,specificity_14,specificity_15,
                                        specificity_16,specificity_17,specificity_18,specificity_19,specificity_20)
                        
)

sens_spec_2022 |>
  ggplot(aes(x=1-Specificity,y=Sensitivity,label=CUSUM)) + 
  geom_point() + 
  geom_line() +
  geom_text(hjust=-0.1,vjust=0.5) +
  geom_abline(intercept = 0,slope = 1)

## Save the Sensitivity and Specificity Data for 2022 season ##

sens_spec_2022 |>
  writexl::write_xlsx("sens_spec_2022_d2.xlsx")
  
## 2023 ##

one_team_pitch23 <- one_team_pitch2 |>
  filter(Season == 2023)

## Subset to just those players whose return date is before the end of august ##

one_team_pitch23 <- one_team_pitch23 |>
  filter(Return_Date <= "2023-08-31")

## Count Number of Appearances after return from injury ##

num_apps <- vector('double',length=nrow(one_team_pitch23))

for(i in 1:nrow(one_team_pitch23)){
  
  dt <- fg_pitcher_game_logs(one_team_pitch23$FanGraphs_ID[i],one_team_pitch23$Season[i])
  
  if(nrow(dt) == 0){
    
    num_apps[i] <- NA
    
  } else{
  
  num_apps[i] <- dt |>
    filter(Date >= one_team_pitch23$Return_Date[i]) |>
    nrow()
  }
}

## Append to one_team_pitch23 ##

one_team_pitch23$Num_Apps <- num_apps

## Remove NA and num_apps < 5 ##

one_team_pitch23 <- one_team_pitch23 |>
  filter(!is.na(Num_Apps) & Num_Apps >= 5)

## Now, we need to estimate mean and sd of WHIP for 2022 season

whip_22 <- Lahman::Pitching |>
  filter(yearID == 2022 & 
           IPouts/3 >= 15) |>
  mutate(WHIP = (H+BB)/(IPouts/3)) |>
  summarize(Mean_WHIP = mean(WHIP,na.rm=TRUE),
            SD_WHIP = sd(WHIP,na.rm=TRUE))

Lahman::Pitching |>
  filter(yearID == 2022 & 
           IPouts/3 >= 15) |>
  mutate(WHIP = (H+BB)/(IPouts/3)) |>
  ggplot(aes(x=WHIP)) + geom_histogram(bins=15)

## Pretty normal ##

## Loop through all players ##

player_logs_list23 <- vector('list',length=nrow(one_team_pitch23))

for(i in 1:nrow(one_team_pitch23)){
  
  player <- one_team_pitch23[i,]
  
  player_logs <- fg_pitcher_game_logs(player$FanGraphs_ID,player$Season) |>
    filter(Date >= player$Return_Date) |>
    dplyr::select(PlayerName,Date,WHIP)
  
  USL <- whip_22$Mean_WHIP + d*whip_22$SD
  
  k <- abs(USL - d)/2
  
  player_logs |>
    mutate(W = (USL - WHIP)/whip_22$SD_WHIP) |>
    arrange(Date) -> player_logs

  CUSUM <- vector('double',length=nrow(player_logs))
  
  CUSUM[1] <- max(0,d-k-player_logs$W[1])
  
  for(j in 2:nrow(player_logs)){
    
    if(max(0,d - k - player_logs$W[j] + CUSUM[j-1]) == 0){
      
      CUSUM[j] <- 0
      
    } else{
      
      CUSUM[j] <- CUSUM[j-1] + d - k - player_logs$W[j]
      
    }
    
  }
  
  player_logs$CUSUM <- CUSUM
  
  player_logs_list23[[i]] <- player_logs
  
}

## Add Multiple Injury Column to Each Dataframe in the List ##

for(i in 1:length(player_logs_list23)){
  
  player_logs_list23[[i]] <- player_logs_list23[[i]] |>
    mutate(Multiple_Injury = one_team_pitch23$Multiple_Injury[i])
  
}

## Evaluate h thresholds of 1:20 ##

for(i in 1:length(player_logs_list23)){
  
  player_logs_list23[[i]] <- player_logs_list23[[i]] |>
    mutate(CUSUM_1 = ifelse(CUSUM > 1,"Yes","No"),
           CUSUM_2 = ifelse(CUSUM > 2,"Yes","No"),
           CUSUM_3 = ifelse(CUSUM > 3,"Yes","No"),
           CUSUM_4 = ifelse(CUSUM > 4,"Yes","No"),
           CUSUM_5 = ifelse(CUSUM > 5,"Yes","No"),
           CUSUM_6 = ifelse(CUSUM > 6,"Yes","No"),
           CUSUM_7 = ifelse(CUSUM > 7,"Yes","No"),
           CUSUM_8 = ifelse(CUSUM > 8,"Yes","No"),
           CUSUM_9 = ifelse(CUSUM > 9,"Yes","No"),
           CUSUM_10 = ifelse(CUSUM > 10,"Yes","No"),
           CUSUM_11 = ifelse(CUSUM > 11,"Yes","No"),
           CUSUM_12 = ifelse(CUSUM > 12,"Yes","No"),
           CUSUM_13 = ifelse(CUSUM > 13,"Yes","No"),
           CUSUM_14 = ifelse(CUSUM > 14,"Yes","No"),
           CUSUM_15 = ifelse(CUSUM > 15,"Yes","No"),
           CUSUM_16 = ifelse(CUSUM > 16,"Yes","No"),
           CUSUM_17 = ifelse(CUSUM > 17,"Yes","No"),
           CUSUM_18 = ifelse(CUSUM > 18,"Yes","No"),
           CUSUM_19 = ifelse(CUSUM > 19,"Yes","No"),
           CUSUM_20 = ifelse(CUSUM > 20,"Yes","No"))
  
}

## Contingency Table to Determine if CUSUM predicts second injury ##

cusum_pred23 <- lapply(player_logs_list23,FUN=function(x){
  
  return(c(x$Multiple_Injury[1],
           ifelse(sum(x$CUSUM_1 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_2 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_3 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_4 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_5 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_6 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_7 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_8 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_9 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_10 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_11 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_12 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_13 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_14 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_15 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_16 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_17 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_18 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_19 == "Yes") > 0,"Yes","No"),
           ifelse(sum(x$CUSUM_20 == "Yes") > 0,"Yes","No")
           
  ))
  
})

cusum_pred23 <- as.data.frame(do.call('rbind',cusum_pred23))

names(cusum_pred23) <- c("Multiple_Injury","CUSUM_1","CUSUM_2","CUSUM_3","CUSUM_4","CUSUM_5",
                       "CUSUM_6","CUSUM_7","CUSUM_8","CUSUM_9","CUSUM_10","CUSUM_11","CUSUM_12",
                       "CUSUM_13","CUSUM_14","CUSUM_15","CUSUM_16","CUSUM_17","CUSUM_18","CUSUM_19",
                       "CUSUM_20")

## Calculate Sensitivity and Specificity for CUSUM_1 ##

sensitivity_1 <- sum(cusum_pred23$Multiple_Injury == "Yes" & cusum_pred23$CUSUM_1 == "Yes")/
  sum(cusum_pred23$Multiple_Injury == "Yes")

specificity_1 <- sum(cusum_pred23$Multiple_Injury == "No" & cusum_pred23$CUSUM_1 == "No")/
  sum(cusum_pred23$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_2 ##

sensitivity_2 <- sum(cusum_pred23$Multiple_Injury == "Yes" & cusum_pred23$CUSUM_2 == "Yes")/
  sum(cusum_pred23$Multiple_Injury == "Yes")

specificity_2 <- sum(cusum_pred23$Multiple_Injury == "No" & cusum_pred23$CUSUM_2 == "No")/
  sum(cusum_pred23$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_3 ##

sensitivity_3 <- sum(cusum_pred23$Multiple_Injury == "Yes" & cusum_pred23$CUSUM_3 == "Yes")/
  sum(cusum_pred23$Multiple_Injury == "Yes")

specificity_3 <- sum(cusum_pred23$Multiple_Injury == "No" & cusum_pred23$CUSUM_3 == "No")/
  sum(cusum_pred23$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_4 ##

sensitivity_4 <- sum(cusum_pred23$Multiple_Injury == "Yes" & cusum_pred23$CUSUM_4 == "Yes")/
  sum(cusum_pred23$Multiple_Injury == "Yes")

specificity_4 <- sum(cusum_pred23$Multiple_Injury == "No" & cusum_pred23$CUSUM_4 == "No")/
  sum(cusum_pred23$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_5 ##

sensitivity_5 <- sum(cusum_pred23$Multiple_Injury == "Yes" & cusum_pred23$CUSUM_5 == "Yes")/
  sum(cusum_pred23$Multiple_Injury == "Yes")

specificity_5 <- sum(cusum_pred23$Multiple_Injury == "No" & cusum_pred23$CUSUM_5 == "No")/
  sum(cusum_pred23$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_6 ##

sensitivity_6 <- sum(cusum_pred23$Multiple_Injury == "Yes" & cusum_pred23$CUSUM_6 == "Yes")/
  sum(cusum_pred23$Multiple_Injury == "Yes")

specificity_6 <- sum(cusum_pred23$Multiple_Injury == "No" & cusum_pred23$CUSUM_6 == "No")/
  sum(cusum_pred23$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_7 ##

sensitivity_7 <- sum(cusum_pred23$Multiple_Injury == "Yes" & cusum_pred23$CUSUM_7 == "Yes")/
  sum(cusum_pred23$Multiple_Injury == "Yes")

specificity_7 <- sum(cusum_pred23$Multiple_Injury == "No" & cusum_pred23$CUSUM_7 == "No")/
  sum(cusum_pred23$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_8 ##

sensitivity_8 <- sum(cusum_pred23$Multiple_Injury == "Yes" & cusum_pred23$CUSUM_8 == "Yes")/
  sum(cusum_pred23$Multiple_Injury == "Yes")

specificity_8 <- sum(cusum_pred23$Multiple_Injury == "No" & cusum_pred23$CUSUM_8 == "No")/
  sum(cusum_pred23$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_9 ##

sensitivity_9 <- sum(cusum_pred23$Multiple_Injury == "Yes" & cusum_pred23$CUSUM_9 == "Yes")/
  sum(cusum_pred23$Multiple_Injury == "Yes")

specificity_9 <- sum(cusum_pred23$Multiple_Injury == "No" & cusum_pred23$CUSUM_9 == "No")/
  sum(cusum_pred23$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_10 ##

sensitivity_10 <- sum(cusum_pred23$Multiple_Injury == "Yes" & cusum_pred23$CUSUM_10 == "Yes")/
  sum(cusum_pred23$Multiple_Injury == "Yes")

specificity_10 <- sum(cusum_pred23$Multiple_Injury == "No" & cusum_pred23$CUSUM_10 == "No")/
  sum(cusum_pred23$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_11 ##

sensitivity_11 <- sum(cusum_pred23$Multiple_Injury == "Yes" & cusum_pred23$CUSUM_11 == "Yes")/
  sum(cusum_pred23$Multiple_Injury == "Yes")

specificity_11 <- sum(cusum_pred23$Multiple_Injury == "No" & cusum_pred23$CUSUM_11 == "No")/
  sum(cusum_pred23$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_12 ##

sensitivity_12 <- sum(cusum_pred23$Multiple_Injury == "Yes" & cusum_pred23$CUSUM_12 == "Yes")/
  sum(cusum_pred23$Multiple_Injury == "Yes")

specificity_12 <- sum(cusum_pred23$Multiple_Injury == "No" & cusum_pred23$CUSUM_12 == "No")/
  sum(cusum_pred23$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_13 ##

sensitivity_13 <- sum(cusum_pred23$Multiple_Injury == "Yes" & cusum_pred23$CUSUM_13 == "Yes")/
  sum(cusum_pred23$Multiple_Injury == "Yes")

specificity_13 <- sum(cusum_pred23$Multiple_Injury == "No" & cusum_pred23$CUSUM_13 == "No")/
  sum(cusum_pred23$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_14 ##

sensitivity_14 <- sum(cusum_pred23$Multiple_Injury == "Yes" & cusum_pred23$CUSUM_14 == "Yes")/
  sum(cusum_pred23$Multiple_Injury == "Yes")

specificity_14 <- sum(cusum_pred23$Multiple_Injury == "No" & cusum_pred23$CUSUM_14 == "No")/
  sum(cusum_pred23$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_15 ##

sensitivity_15 <- sum(cusum_pred23$Multiple_Injury == "Yes" & cusum_pred23$CUSUM_15 == "Yes")/
  sum(cusum_pred23$Multiple_Injury == "Yes")

specificity_15 <- sum(cusum_pred23$Multiple_Injury == "No" & cusum_pred23$CUSUM_15 == "No")/
  sum(cusum_pred23$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_16 ##

sensitivity_16 <- sum(cusum_pred23$Multiple_Injury == "Yes" & cusum_pred23$CUSUM_16 == "Yes")/
  sum(cusum_pred23$Multiple_Injury == "Yes")

specificity_16 <- sum(cusum_pred23$Multiple_Injury == "No" & cusum_pred23$CUSUM_16 == "No")/
  sum(cusum_pred23$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_17 ##

sensitivity_17 <- sum(cusum_pred23$Multiple_Injury == "Yes" & cusum_pred23$CUSUM_17 == "Yes")/
  sum(cusum_pred23$Multiple_Injury == "Yes")

specificity_17 <- sum(cusum_pred23$Multiple_Injury == "No" & cusum_pred23$CUSUM_17 == "No")/
  sum(cusum_pred23$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_18 ##

sensitivity_18 <- sum(cusum_pred23$Multiple_Injury == "Yes" & cusum_pred23$CUSUM_18 == "Yes")/
  sum(cusum_pred23$Multiple_Injury == "Yes")

specificity_18 <- sum(cusum_pred23$Multiple_Injury == "No" & cusum_pred23$CUSUM_18 == "No")/
  sum(cusum_pred23$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_19 ##

sensitivity_19 <- sum(cusum_pred23$Multiple_Injury == "Yes" & cusum_pred23$CUSUM_19 == "Yes")/
  sum(cusum_pred23$Multiple_Injury == "Yes")

specificity_19 <- sum(cusum_pred23$Multiple_Injury == "No" & cusum_pred23$CUSUM_19 == "No")/
  sum(cusum_pred23$Multiple_Injury == "No")

## Calculate Sensitivity and Specificity for CUSUM_20 ##

sensitivity_20 <- sum(cusum_pred23$Multiple_Injury == "Yes" & cusum_pred23$CUSUM_20 == "Yes")/
  sum(cusum_pred23$Multiple_Injury == "Yes")

specificity_20 <- sum(cusum_pred23$Multiple_Injury == "No" & cusum_pred23$CUSUM_20 == "No")/
  sum(cusum_pred23$Multiple_Injury == "No")

## Plot the relationship between sensitivity and specificity ##

sens_spec_2023 <- data.frame(CUSUM = 1:20,
                        Sensitivity = c(sensitivity_1,sensitivity_2,sensitivity_3,sensitivity_4,sensitivity_5,
                                        sensitivity_6,sensitivity_7,sensitivity_8,sensitivity_9,sensitivity_10,
                                        sensitivity_11,sensitivity_12,sensitivity_13,sensitivity_14,sensitivity_15,
                                        sensitivity_16,sensitivity_17,sensitivity_18,sensitivity_19,sensitivity_20),
                        Specificity = c(specificity_1,specificity_2,specificity_3,specificity_4,specificity_5,
                                        specificity_6,specificity_7,specificity_8,specificity_9,specificity_10,
                                        specificity_11,specificity_12,specificity_13,specificity_14,specificity_15,
                                        specificity_16,specificity_17,specificity_18,specificity_19,specificity_20)
                        
)

sens_spec_2023 |>
  ggplot(aes(x=1-Specificity,y=Sensitivity,label=CUSUM)) + 
  geom_point() + 
  geom_line() +
  geom_text(hjust=-0.1,vjust=0.5) +
  geom_abline(intercept = 0,slope = 1)

## Save the Sensitivity and Specificity Data for 2023 season ##

sens_spec_2023 |>
  writexl::write_xlsx("sens_spec_2023_d2.xlsx")

## Could we count the number of appearances before the injury and 
## the number of appearances before C > h? ##

## Let's try for 2021 ##

## Go through player_logs_list and return 
## a list which only contains dataframes
## where Multiple_Injury == Yes ##

inj21 <- Filter(function(x) all(x$Multiple_Injury == "Yes"),player_logs_list)
inj22 <- Filter(function(x) all(x$Multiple_Injury == "Yes"),player_logs_list22)
inj23 <- Filter(function(x) all(x$Multiple_Injury == "Yes"),player_logs_list23)
  
## Add back the column for injury date ##

inj21 <- lapply(inj21,FUN=function(x){
  
  inj_date <- x |>
    left_join(one_team_pitch |>
                filter(Season == 2021),
              by=c("PlayerName"="Name")) |>
    arrange(desc(IL_Retro_Date)) |>
    slice_head(n=1) |>
    pull(IL_Retro_Date)
  
  x$Injury_Date <- inj_date
  
  return(x)
  
})

inj22 <- lapply(inj22,FUN=function(x){
  
  inj_date <- x |>
    left_join(one_team_pitch |>
                filter(Season == 2022),
              by=c("PlayerName"="Name")) |>
    arrange(desc(IL_Retro_Date)) |>
    slice_head(n=1) |>
    pull(IL_Retro_Date)
  
  x$Injury_Date <- inj_date
  
  return(x)
  
})

inj23 <- lapply(inj23,FUN=function(x){
  
  inj_date <- x |>
    left_join(one_team_pitch |>
                filter(Season == 2023),
              by=c("PlayerName"="Name")) |>
    arrange(desc(IL_Retro_Date)) |>
    slice_head(n=1) |>
    pull(IL_Retro_Date)
  
  x$Injury_Date <- inj_date
  
  return(x)
  
})

## Count up number of appearances before injury ##

num_apps_before_inj21 <- lapply(inj21,FUN=function(x){
  
  inj_date <- x$Injury_Date[1]
  
  num_apps <- x |>
    filter(Date < inj_date) |>
    nrow()
  
  dt <- tibble(PlayerName = x$PlayerName[1],
               Num_Apps_Before_Inj = num_apps)
  
  return(dt)
  
})

num_apps_before_inj22 <- lapply(inj22,FUN=function(x){
  
  inj_date <- x$Injury_Date[1]
  
  num_apps <- x |>
    filter(Date < inj_date) |>
    nrow()
  
  dt <- tibble(PlayerName = x$PlayerName[1],
               Num_Apps_Before_Inj = num_apps)
  
  return(dt)
  
})

num_apps_before_inj23 <- lapply(inj23,FUN=function(x){
  
  inj_date <- x$Injury_Date[1]
  
  num_apps <- x |>
    filter(Date < inj_date) |>
    nrow()
  
  dt <- tibble(PlayerName = x$PlayerName[1],
               Num_Apps_Before_Inj = num_apps)
  
  return(dt)
  
})

## Now for each CUSUM prefixed column, we want to count
## the number of No before the first Yes ##

cusum_counts21 <- vector('list',length=length(inj21))

for(i in 1:length(inj21)){
  
  ex <- inj21[[i]]
  
  cusum_counts21[[i]] <- as_tibble(
    
    ex |>
    select(starts_with("CUSUM_")) |>
    sapply(function(x){
      
      if(all(x == "No")){ 
        
        return(NA)
        
      }
      
      else{
      
      no_count <- 0
      
      for(val in x){
        
        if(val == "Yes") break
        if(val == "No") no_count <- no_count + 1
        
      }
      
      return(no_count)
      
      }
    })
  ) |>
    mutate(PlayerName = ex$PlayerName[1],.before=1) |>
    rename(Apps_Before_Signal = value) |>
    mutate(h = 1:20)
  
}
  
 
## Join this with the num_apps_before_inj21 list ##

for(i in 1:length(cusum_counts21)){
  
  cusum_counts21[[i]] <- cusum_counts21[[i]] |>
    left_join(num_apps_before_inj21[[i]],by="PlayerName") |>
    mutate(`Early Detect` = if_else(Apps_Before_Signal < Num_Apps_Before_Inj,"Yes","No"))
  
}


## Do the same process for 2022 ##

cusum_counts22 <- vector('list',length=length(inj22))

for(i in 1:length(inj22)){
  
  ex <- inj22[[i]]
  
  cusum_counts22[[i]] <- as_tibble(
    
    ex |>
      select(starts_with("CUSUM_")) |>
      sapply(function(x){
        
        if(all(x == "No")){ 
          
          return(NA)
          
        }
        
        else{
          
          no_count <- 0
          
          for(val in x){
            
            if(val == "Yes") break
            if(val == "No") no_count <- no_count + 1
            
          }
          
          return(no_count)
          
        }
      })
  ) |>
    mutate(PlayerName = ex$PlayerName[1],.before=1) |>
    rename(Apps_Before_Signal = value) |>
    mutate(h = 1:20)
  
}


## Join this with the num_apps_before_inj22 list ##

for(i in 1:length(cusum_counts22)){
  
  cusum_counts22[[i]] <- cusum_counts22[[i]] |>
    left_join(num_apps_before_inj22[[i]],by="PlayerName") |>
    mutate(`Early Detect` = if_else(Apps_Before_Signal < Num_Apps_Before_Inj,"Yes","No"))
  
}

## 2023 ##

cusum_counts23 <- vector('list',length=length(inj23))

for(i in 1:length(inj23)){
  
  ex <- inj23[[i]]
  
  cusum_counts23[[i]] <- as_tibble(
    
    ex |>
      select(starts_with("CUSUM_")) |>
      sapply(function(x){
        
        if(all(x == "No")){ 
          
          return(NA)
          
        }
        
        else{
          
          no_count <- 0
          
          for(val in x){
            
            if(val == "Yes") break
            if(val == "No") no_count <- no_count + 1
            
          }
          
          return(no_count)
          
        }
      })
  ) |>
    mutate(PlayerName = ex$PlayerName[1],.before=1) |>
    rename(Apps_Before_Signal = value) |>
    mutate(h = 1:20)
  
}

## Join this with the num_apps_before_inj23 list ##

for(i in 1:length(cusum_counts23)){
  
  cusum_counts23[[i]] <- cusum_counts23[[i]] |>
    left_join(num_apps_before_inj23[[i]],by="PlayerName") |>
    mutate(`Early Detect` = if_else(Apps_Before_Signal < Num_Apps_Before_Inj,"Yes","No"))
  
}

## Now we can calculate average number of appearances between injury and signal
## for each value of h ##

lapply(cusum_counts21,FUN=function(x){
  
  x |>
    #filter(`Early Detect` == "Yes") |>
    mutate(Apps_Between = Num_Apps_Before_Inj - Apps_Before_Signal)
  
}) |> do.call(what=rbind) |>
  group_by(h) |>
  summarize(Avg_Apps_Before_Signal = mean(Apps_Before_Signal,na.rm=TRUE),
            Avg_Apps_Before_Inj = mean(Num_Apps_Before_Inj,na.rm=TRUE)) -> arl_2021

## Calculate Marginal Mean Appearances Before Injury ##

lapply(cusum_counts21,FUN=function(x){
  
  x |>
    #filter(`Early Detect` == "Yes") |>
    mutate(Apps_Between = Num_Apps_Before_Inj - Apps_Before_Signal)
  
}) |> do.call(what=rbind) |>
  summarize(Avg_Apps_Before_Inj = mean(Num_Apps_Before_Inj,na.rm=TRUE)) -> marginal_mean21

## Can we find an example of a player whose Num_Apps_Before_Inj < Apps_Before_Signal ##

lapply(cusum_counts21,FUN=function(x){
  
  x |>
    filter(Num_Apps_Before_Inj < Apps_Before_Signal)
  
}) |> do.call(what=rbind) |> filter(PlayerName == "Antonio Senzatela")

## Count Number of Players who have a non-NA value for Apps_Before_Signal ##

lapply(cusum_counts21,FUN=function(x){
  
  x |>
    filter(!is.na(Apps_Before_Signal))
  
}) |> do.call(what=rbind) |>
  group_by(h) |> 
  count()

## ARL 2022 ##

lapply(cusum_counts22,FUN=function(x){
  
  x |>
    #filter(`Early Detect` == "Yes") |>
    mutate(Apps_Between = Num_Apps_Before_Inj - Apps_Before_Signal)
  
}) |> do.call(what=rbind) |>
  group_by(h) |>
  summarize(Avg_Apps_Before_Signal = mean(Apps_Before_Signal,na.rm=TRUE),
            Avg_Apps_Before_Inj = mean(Num_Apps_Before_Inj,na.rm=TRUE)) -> arl_2022

## Calculate Marginal Mean Appearances Before Injury ##

lapply(cusum_counts22,FUN=function(x){
  
  x |>
    #filter(`Early Detect` == "Yes") |>
    mutate(Apps_Between = Num_Apps_Before_Inj - Apps_Before_Signal)
  
}) |> do.call(what=rbind) |>
  summarize(Avg_Apps_Before_Inj = mean(Num_Apps_Before_Inj,na.rm=TRUE)) -> marginal_mean22

lapply(cusum_counts23,FUN=function(x){
  
  x |>
    #filter(`Early Detect` == "Yes") |>
    mutate(Apps_Between = Num_Apps_Before_Inj - Apps_Before_Signal)
  
}) |> do.call(what=rbind) |>
  group_by(h) |>
  summarize(Avg_Apps_Before_Signal = mean(Apps_Before_Signal,na.rm=TRUE),
            Avg_Apps_Before_Inj = mean(Num_Apps_Before_Inj,na.rm=TRUE)) -> arl_2023

## Calculate Marginal Mean Appearances Before Injury ##

lapply(cusum_counts23,FUN=function(x){
  
  x |>
    #filter(`Early Detect` == "Yes") |>
    mutate(Apps_Between = Num_Apps_Before_Inj - Apps_Before_Signal)
  
}) |> do.call(what=rbind) |>
  summarize(Avg_Apps_Before_Inj = mean(Num_Apps_Before_Inj,na.rm=TRUE)) -> marginal_mean23

print(marginal_mean23$Avg_Apps_Before_Inj)

## Write the ARL Data to File ##

writexl::write_xlsx(list(arl_2021,arl_2022,arl_2023),
                    "arl_data_final.xlsx")





##################################### creating Plot for Game Saved ####################################################

# Create the 'GameSaved' column for each year and combine data
arl_2021 <- arl_2021 %>%
  mutate(GameSaved = Avg_Apps_Before_Inj - Avg_Apps_Before_Signal, Year = "Year 2021")

arl_2022 <- arl_2022 %>%
  mutate(GameSaved = Avg_Apps_Before_Inj - Avg_Apps_Before_Signal, Year = "Year 2022")

arl_2023 <- arl_2023 %>%
  mutate(GameSaved = Avg_Apps_Before_Inj - Avg_Apps_Before_Signal, Year = "Year 2023")

# Combine all ARL data into one data frame
arl_combined <- bind_rows(arl_2021, arl_2022, arl_2023)

# Calculate the y-position for the label of the blue line (Avg_Apps_Before_Inj)
label_data <- arl_combined %>%
  group_by(Year) %>%
  summarise(label_y = mean(Avg_Apps_Before_Inj, na.rm = TRUE))

# Plot the data for Avg_Apps_Before_Inj, Avg_Apps_Before_Signal, and GameSaved
library(ggplot2)

ggplot(arl_combined, aes(x = h, group = Year)) +
  # Plot Avg_Apps_Before_Inj and Avg_Apps_Before_Signal
  geom_line(aes(y = Avg_Apps_Before_Inj, color = "Average Apps Before Injury"), size = 1) +
  geom_line(aes(y = Avg_Apps_Before_Signal, color = "Average Apps Before Signal"), size = 1) +
  
  # Plot GameSaved
  geom_line(aes(y = GameSaved, color = "Game Saved"), size = 1, linetype = "dashed") +
  
  # Add text label for the blue line (Avg_Apps_Before_Inj) only once
  geom_text(data = label_data, aes(x = max(arl_combined$h), y = label_y, label = round(label_y, 2)), 
            color = "blue", size = 4, hjust = 1.1, vjust = 1, fontface = "bold") +
  
  # Add title and labels
  labs(title = "Average Number of Appearances Before Injury and Signal for Each CUSUM Threshold(h)",
       x = "CUSUM Thresholds (h)", y = "Number of Appearances",
       color = "Legend") +
  
  # Customize the theme
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center title
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 12, face = "bold"), # Customize facet labels
    legend.position = "bottom",  # Place the legend at the bottom
    legend.box = "horizontal"   # Place the legend horizontally
  ) +
  scale_color_manual(values = c("Average Apps Before Injury" = "blue", 
                                "Average Apps Before Signal" = "red", 
                                "Game Saved" = "green", 
                                "Recommended Region h [7,12]" = "gray")) +
  
  # Add facet by Year with the new labels
  facet_wrap(~Year, scales = "free_y", labeller = label_value)  +  # Use 'label_value' for year labels
  
  # Add vertical lines at h = 7 and h = 12 (using geom_segment for legend inclusion)
  geom_vline(xintercept = c(7, 12), linetype = "dashed", color = "gray", size = 1) +
  # Add a dummy line to include "Recommended Region h [7,12]" in the legend
  geom_segment(aes(x = 7, y = 0, xend = 12, yend = 0), color = "gray", size = 1, linetype = "dashed", alpha = 0)  # Invisible but part of the legend


 ##################################### End of Plot Game Saved ####################################################

# Create the 'GameSaved' column for each year and combine data
arl_2021 <- arl_2021 %>%
  mutate(GameSaved = Avg_Apps_Before_Inj - Avg_Apps_Before_Signal, Year = "Year 2021")

arl_2022 <- arl_2022 %>%
  mutate(GameSaved = Avg_Apps_Before_Inj - Avg_Apps_Before_Signal, Year = "Year 2022")

arl_2023 <- arl_2023 %>%
  mutate(GameSaved = Avg_Apps_Before_Inj - Avg_Apps_Before_Signal, Year = "Year 2023")

# Combine all ARL data into one data frame
arl_combined <- bind_rows(arl_2021, arl_2022, arl_2023)

# Calculate the y-position for the label of the blue line (Avg_Apps_Before_Inj)
label_data <- arl_combined %>%
  group_by(Year) %>%
  summarise(label_y = mean(Avg_Apps_Before_Inj, na.rm = TRUE))

# Plot the data for Avg_Apps_Before_Inj, Avg_Apps_Before_Signal, and GameSaved
library(ggplot2)

ggplot(arl_combined, aes(x = h, group = Year)) +
  # Plot Avg_Apps_Before_Inj and Avg_Apps_Before_Signal
  geom_line(aes(y = Avg_Apps_Before_Inj, color = "Average Apps Before Injury"), size = 1) +
  geom_line(aes(y = Avg_Apps_Before_Signal, color = "Average Apps Before Signal"), size = 1) +
  
  # Plot GameSaved
  geom_line(aes(y = GameSaved, color = "Game Saved"), size = 1, linetype = "dashed") +
  
  # Add text label for the blue line (Avg_Apps_Before_Inj) only once
  geom_text(data = label_data, aes(x = max(arl_combined$h), y = label_y, label = round(label_y, 2)), 
            color = "blue", size = 4, hjust = 1.1, vjust = 1, fontface = "bold") +
  
  # Add title and labels
  labs(title = "Average Number of Appearances Before Injury and Signal for Each CUSUM Threshold(h)",
       x = "CUSUM Thresholds (h)", y = "Number of Appearances",
       color = "Legend") +
  
  # Customize the theme
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center title
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 12, face = "bold"), # Customize facet labels
    legend.position = "bottom",  # Place the legend at the bottom
    legend.box = "horizontal"   # Place the legend horizontally
  ) +
  scale_color_manual(values = c("Average Apps Before Injury" = "blue", 
                                "Average Apps Before Signal" = "red", 
                                "Game Saved" = "green")) +
  
  # Add facet by Year with the new labels
  facet_wrap(~Year, scales = "free_y", labeller = label_value)  # Use 'label_value' for year labels





## Plot the relationship between sensitivity and FPR ##

## Read back in sens_spec_2023 ##

sens_spec_2023 <- readxl::read_xlsx("sens_spec_2023.xlsx")

## Plot the relationship between sensitivity and FPR ##

library(viridis)

sens_spec_2023 |>
  mutate(CUSUM = factor(CUSUM,levels=c(5,10,15,20,25))) |>
  ggplot(aes(x=FPR,y=Sensitivity,color=CUSUM)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=0,y=0.5,xend=1,yend=0.5),linetype="dashed") +
  geom_segment(aes(x=0.5,y=0,xend=0.5,yend=1),linetype="dashed") +
  geom_abline(intercept = 0,slope = 1) +
  labs(x = "False Positive Rate",
       y = "Sensitivity",
       color = "CUSUM Threshold",
       title = "Classification Evaluation for CUSUM Multiple Injury Prediction",
       subtitle = "2021 - 2023 Regular Seasons") +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5)) +
  scale_color_viridis(discrete=TRUE,
                      option="H")

## Plot a CUSUM Chart for a Player ##
## Specify Month ##

adam_wainwright <- inj23[[1]]

adam_wainwright |>
  mutate(Date = as_date(Date),
         OOC = if_else(CUSUM < 10, "Out-of-Control","In-Control")) |>
  filter(Date <= "2023-07-31") |>
  ggplot(aes(x=Date,y=CUSUM)) + 
  geom_rect(aes(xmin=as_date("2023-06-11"),xmax=as_date("2023-07-05"),ymin=0,ymax=50),fill='grey',alpha=0.2) +
  geom_line() + 
  geom_point() +
  geom_hline(yintercept = 10,color='red') +
  geom_segment(aes(x=as_date("2023-07-05"),xend=as_date("2023-07-05"),y=0,yend=50),linetype="dashed") +
  geom_segment(aes(x=as_date("2023-06-11"),xend=as_date("2023-06-11"),y=0,yend=50),linetype="dashed") +
  labs(x = "Date",
       y = "CUSUM Statistic",
       title = "CUSUM Chart for Adam Wainwright",
       subtitle = "2023 Regular Season") +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5)) +
  scale_x_date(breaks = seq(min(as_date(adam_wainwright$Date)),
                            max(as_date("2023-08-15")),
                            by = "15 days"),
               date_labels = "%b %d")



## Generate Similar Chart for Adbert Alzolay 2021 h = 12 ##

adbert_alzolay <- inj21[[1]]

adbert_alzolay |>
  mutate(Date = as_date(Date),
         OOC = if_else(CUSUM < 12, "Out-of-Control","In-Control")) |>
  ggplot(aes(x=Date,y=CUSUM)) + 
  #geom_rect(aes(xmin=as_date("2021-08-01"),xmax=as_date("2021-08-31"),ymin=0,ymax=50),fill='grey',alpha=0.2) +
  geom_line() + 
  geom_point() +
  geom_hline(yintercept = 12,color='red') +
  geom_segment(aes(x=as_date("2021-08-14"),xend=as_date("2021-08-14"),y=0,yend=50),linetype="dashed") +
  #geom_segment(aes(x=as_date("2021-08-31"),xend=as_date("2021-08-31"),y=0,yend=50),linetype="dashed") +
  labs(x = "Date",
       y = "CUSUM Statistic",
       title = "CUSUM Chart for Adbert Alzolay",
       subtitle = "2021 Regular Season") +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5)) +
  scale_x_date(breaks = seq(min(as_date(adbert_alzolay$Date)),
                            max(as_date("2021-10-01")),
                            by = "15 days"),
               date_labels = "%b %d")

## Generate Similar Chart for Antonio_Senzatela 2021 h = 9 ##
Antonio_Senzatela <- inj21[[9]]
Antonio_Senzatela |>
  mutate(Date = as_date(Date),
         OOC = if_else(CUSUM < 10, "Out-of-Control","In-Control")) |>
  ggplot(aes(x=Date,y=CUSUM)) + 
  #geom_rect(aes(xmin=as_date("2021-08-01"),xmax=as_date("2021-08-31"),ymin=0,ymax=50),fill='grey',alpha=0.2) +
  geom_line() + 
  geom_point() +
  geom_hline(yintercept = 9,color='red') +
  geom_segment(aes(x=as_date("2021-08-14"),xend=as_date("2021-08-14"),y=0,yend=50),linetype="dashed") +
  #geom_segment(aes(x=as_date("2021-08-31"),xend=as_date("2021-08-31"),y=0,yend=50),linetype="dashed") +
  labs(x = "Date",
       y = "CUSUM Statistic",
       title = "CUSUM Chart for Antonio Senzatela",
       subtitle = "2021 Regular Season") +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5)) +
  scale_x_date(breaks = seq(min(as_date(adbert_alzolay$Date)),
                            max(as_date("2021-10-01")),
                            by = "15 days"),
               date_labels = "%b %d")


## Generate Similar Chart for Blake Snell 2021 h = 7 ##
Blake_Snell <- inj21[[12]]
Blake_Snell |>
  mutate(Date = as_date(Date),
         OOC = if_else(CUSUM < 7, "Out-of-Control","In-Control")) |>
  ggplot(aes(x=Date,y=CUSUM)) + 
  #geom_rect(aes(xmin=as_date("2021-08-01"),xmax=as_date("2021-08-31"),ymin=0,ymax=50),fill='grey',alpha=0.2) +
  geom_line() + 
  geom_point() +
  geom_hline(yintercept = 7,color='red') +
  geom_segment(aes(x=as_date("2021-08-14"),xend=as_date("2021-08-14"),y=0,yend=50),linetype="dashed") +
  #geom_segment(aes(x=as_date("2021-08-31"),xend=as_date("2021-08-31"),y=0,yend=50),linetype="dashed") +
  labs(x = "Date",
       y = "CUSUM Statistic",
       title = "CUSUM Chart for Blake Snell",
       subtitle = "2021 Regular Season") +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5)) +
  scale_x_date(breaks = seq(min(as_date(adbert_alzolay$Date)),
                            max(as_date("2021-10-01")),
                            by = "15 days"),
               date_labels = "%b %d")

