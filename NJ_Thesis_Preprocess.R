### Data Preprocessing 
### Niklas Jung
### Input: Read in Raw data
### Output: 2 Data frames for final analysis:
###   1. Tinit (all measurements with lagged variables, long format)
###   2 day_merged (aggregated Data for each day, long format)


library(tidyr)
library(dplyr)
library(ggplot2)

#Import Dataset for Innsbruck only (n=31)
#df <- read.csv("2025-05-05_EMATouchData_Innsbruck.csv")

#for all sites
#df <- read.csv("2025-05-05_EMATouchData_AllSites.csv")
df <- read.csv("2025-07-14_EMATouch_EMAData_AllSites.csv")

baseline <- read.csv("2025-08-21_EMATouch_BaselineData_AllSites.csv")


#Overview over Data
#summary(df)


#create Current Well Being Variable
df$WB <- (df$MDBF.1 + df$MDBF.2) /2

df$ProS <- (df$CurrentKindnessGeneral.1 + df$CurrentKindnessGeneral.2) /2
#General Kindness Scales correlate with .5957

## Kindness Day (aggregating score over 3 questions, for evening)
df$DayKindness.1 <- factor(df$DayKindness.1,
                           levels = c("never true","almost never true","occasionally true",
                             "sometimes true", "often true","almost always true","always true"),
                           ordered = TRUE)

df$DayKindness.2 <- factor(df$DayKindness.2,
                           levels = c("never true","almost never true","occasionally true",
                                      "sometimes true", "often true","almost always true","always true"),
                           ordered = TRUE)

df$DayKindness.3 <- factor(df$DayKindness.3,
                           levels = c("never true","almost never true","occasionally true",
                                      "sometimes true", "often true","almost always true","always true"),
                           ordered = TRUE)

#new variable
df$Total_DayKindness <- as.numeric(df$DayKindness.1) + as.numeric(df$DayKindness.2) + as.numeric(df$DayKindness.3)


# Convert to Date class
df$Date <- as.Date(df$Date)


# create new variables for touch with friends
df$Touch_Close <- factor(df$NrTouch.CloselyTrusted, levels = c("0", "1", "2-5", "11-20", ">20"), ordered = TRUE)
df$Touch_WellAcquainted <- factor(df$NrTouch.WellAcquainted, levels = c("0", "1", "2-5", "11-20", ">20"), ordered = TRUE)
df$Touch_DistantlyAcquainted <- factor(df$NrTouch.DistantlyAcquainted, levels = c("0", "1", "2-5", "11-20", ">20"), ordered = TRUE)
df$Touch_Unknown <-  factor(df$NrTouch.Unknown, levels = c("0", "1", "2-5", "11-20", ">20"), ordered = TRUE)
df$Touch_Pet <-  factor(df$NrTouch.Pet, levels = c("0", "1", "2-5", "11-20", ">20"), ordered = TRUE)

#For H1
#new data frame with touch, interaction-time and ProS aggregated over the day 
agg_day <- df %>%
  filter(NrPrompt %in% 1:5) %>%
  group_by(ID, NrDay) %>%
  summarize(
    NrT_Close = mean(as.numeric(Touch_Close) - 1, na.rm = TRUE),
    NrT_Well = mean(as.numeric(Touch_WellAcquainted) - 1, na.rm = TRUE),
    NrT_Dist = mean(as.numeric(Touch_DistantlyAcquainted) - 1, na.rm = TRUE),
    NrT_Unknown = mean(as.numeric(Touch_Unknown) - 1, na.rm = TRUE),
    
    NrT_Close_Sum = sum(as.numeric(Touch_Close) / 5 , na.rm = TRUE),
    NrT_Well_Sum = sum(as.numeric(Touch_WellAcquainted) / 5 , na.rm = TRUE),
    NrT_Dist_Sum = sum(as.numeric(Touch_DistantlyAcquainted) / 5 , na.rm = TRUE),
    NrT_Unknown_Sum = sum(as.numeric(Touch_Unknown) / 5 , na.rm = TRUE),
    
    
    InteractT_Close = mean(InteractionTime.CloselyTrusted, na.rm = TRUE),
    InteractT_Well = mean(InteractionTime.WellAcquainted, na.rm = TRUE),
    InteractT_Dist = mean(InteractionTime.DistantlyAcquainted, na.rm = TRUE),
    InteractT_Unknown = mean(InteractionTime.Unknown, na.rm = TRUE),
    .groups = "drop"
    
  )

#sum of touches (without pet)
agg_day$NrTouch <- agg_day$NrT_Close + agg_day$NrT_Well + agg_day$NrT_Dist + agg_day$NrT_Unknown

agg_day$Sum_Touch <-  agg_day$NrT_Close_Sum +  agg_day$NrT_Well_Sum +   agg_day$NrT_Dist_Sum +  agg_day$NrT_Unknown_Sum

agg_day$Interaction_Time <- agg_day$InteractT_Close + agg_day$InteractT_Well + agg_day$InteractT_Dist + agg_day$InteractT_Unknown

# evening measure for kindness vars
evening_prosociality <- df %>%
  filter(NrPrompt == 5) %>%
  dplyr::select(ID, NrDay ,"ProS_Eve" = ProS, Total_DayKindness, KindnessBehavior.Time, WB)

day_merged <- left_join(agg_day, evening_prosociality, by = c("NrDay", "ID"))

# Create a 0/1 variable: 1 = behavior occurred, 0 = zero
day_merged$BehaviorOccurred <- as.numeric(day_merged$KindnessBehavior.Time > 0)

## centering interaction time to make main effect for touch at average of interaction time
day_merged$Interaction_Time <- scale(day_merged$Interaction_Time, center = TRUE, scale = FALSE)


day_merged$InteractT_Close <- scale(day_merged$InteractT_Close, center = TRUE, scale = FALSE)
day_merged$InteractT_Well <- scale(day_merged$InteractT_Well, center = TRUE, scale = FALSE)
day_merged$InteractT_Dist <- scale(day_merged$InteractT_Dist, center = TRUE, scale = FALSE)
day_merged$InteractT_Unknown <- scale(day_merged$InteractT_Unknown, center = TRUE, scale = FALSE)


#-------------------------------------------------------------------------

#For H2

# Reshape NrTouch to long
long_touch_nr <- day_merged %>%
  select(ID, NrDay, ProS_Eve, Total_DayKindness , KindnessBehavior.Time,
         NrT_Close, NrT_Well, NrT_Dist, NrT_Unknown) %>%
  pivot_longer(cols = starts_with("NrT_"),
               names_to = "PartnerType",
               values_to = "NrTouch")

# Reshape InteractionTime to long
long_touch_time <- day_merged %>%
  select(ID, NrDay,
         InteractT_Close,
         InteractT_Well,
         InteractT_Dist,
         InteractT_Unknown) %>%
  pivot_longer(cols = starts_with("InteractT"),
               names_to = "PartnerType_Time",
               values_to = "InteractionTime")

# Clean PartnerType names so they match
long_touch_time <- long_touch_time %>%
  mutate(PartnerType = case_when(
    PartnerType_Time == "InteractT_Close" ~ "NrT_Close",
    PartnerType_Time == "InteractT_Well" ~ "NrT_Well",
    PartnerType_Time == "InteractT_Dist" ~ "NrT_Dist",
    PartnerType_Time == "InteractT_Unknown" ~ "NrT_Unknown"
  )) %>%
  select(-PartnerType_Time)

#Join both long tables
long_touch <- left_join(long_touch_nr, long_touch_time,
                        by = c("ID", "NrDay", "PartnerType"))
#Rename categories
long_touch$PartnerType <- as.factor(long_touch$PartnerType)

#remove interim dataframes
rm(long_touch_nr, long_touch_time, agg_day, evening_prosociality)

