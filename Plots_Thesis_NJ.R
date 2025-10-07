### Analyse Master
### Niklas Jung
### 08/08/2025
### Intent: Plotting results 


library(sjPlot)
library(sjmisc)
library(ggplot2)
library(patchwork)
library(survival)
library(survminer)


# TABLES
# Table for Descriptives 

# Select relevant variables
vars <- day_merged %>% select(ProS_Eve, Total_DayKindness, KindnessBehavior.Time)

# Function to calculate mean, SD, and range
get_descriptives <- function(x) {
  c(M = round(mean(x, na.rm = TRUE),2),
    SD = round(sd(x, na.rm = TRUE),2),
    Median = round(median(x, na.rm = TRUE), 2),
    IQR = round(IQR(x, na.rm = TRUE), 2),
    Range = paste0(round(min(x, na.rm = TRUE),2), "--", round(max(x, na.rm = TRUE),2)))
}

# Apply to all variables
desc_stats <- sapply(vars, get_descriptives)
desc_stats

# Table for touch distribution: 

# Function to calculate percent for a single column
percent_table <- function(x) {
  tbl <- table(x, useNA = "no")        # exclude NAs
  round(prop.table(tbl) * 100, 1)      # convert to percent
}

# Apply to all four touch variables
percent_close <- percent_table(df$Touch_Close)
percent_well  <- percent_table(df$Touch_WellAcquainted)
percent_dist  <- percent_table(df$Touch_DistantlyAcquainted)
percent_unknown <- percent_table(df$Touch_Unknown)

# Combine into a data frame for table display
touch_percent_df <- data.frame(
  Partner = c("Close", "Well-acquainted", "Distantly acquainted", "Unknown"),
  `0` = c(percent_close["0"], percent_well["0"], percent_dist["0"], percent_unknown["0"]),
  `1` = c(percent_close["1"], percent_well["1"], percent_dist["1"], percent_unknown["1"]),
  `2-5` = c(percent_close["2-5"], percent_well["2-5"], percent_dist["2-5"], percent_unknown["2-5"]),
  `11-20` = c(percent_close["11-20"], percent_well["11-20"], percent_dist["11-20"], percent_unknown["11-20"]),
  `>20` = c(percent_close[">20"], percent_well[">20"], percent_dist[">20"], percent_unknown[">20"])
)

touch_percent_df
df %>%
  select(Touch_Close, Touch_WellAcquainted, Touch_DistantlyAcquainted, Touch_Unknown) %>%
  summarise(across(everything(), ~ round(prop.table(table(.)) * 100, 1)))

#kindess behavior seperatly due to skewness


theme_set(theme_sjplot())

## Observed Values 
day_merged <- day_merged %>%
  mutate(IntTime_bin = cut(Interaction_Time,
                           breaks = quantile(Interaction_Time, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE),
                           labels = c("Low", "Medium", "High"),
                           include.lowest = TRUE))

ggplot(day_merged, aes(x = NrTouch, y = ProS_Eve, color = IntTime_bin)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +  # smooth trend lines
  labs(
    x = "Number of Touches",
    y = "Observed Evening Prosociality (ProS_Eve)",
    color = "Interaction Time"
  ) +
  coord_cartesian(xlim = c(0,7)) +
  theme_minimal()

## Predicted Values
#H1 models refit with lmer (to allow plot_model function to work)
interactplot_m1a <- lmer(ProS_Eve ~ NrTouch * Interaction_Time + (1 | ID ), data = day_merged)
interactplot_m1b <- lmer(Total_DayKindness ~ NrTouch * Interaction_Time + (1 | ID ), data = day_merged)
interactplot_m1c <- lmer(KindnessBehavior.Time ~ NrTouch * Interaction_Time + (1 | ID ), data = day_merged)

#Plot H1a
plot_model(interactplot_m1a, type = "pred", terms = c("NrTouch", "Interaction_Time"))

#Plot H1b
plot_model(interactplot_m1b, type = "pred", terms = c("NrTouch", "Interaction_Time"))

#Plot H1c
plot_model(interactplot_m1c, type = "pred", terms = c("NrTouch", "Interaction_Time"))

#Printing them next to each other with legend etc. 

p1 <- plot_model(interactplot_m1a, type = "pred", terms = c("NrTouch", "Interaction_Time")) +
  theme(legend.position = "none") +
  labs(
    title = "H1a: Evening Prosociality",
    x = "Number of Touches",
    y = "Predicted ProS_Eve"
  )

p2 <- plot_model(interactplot_m1b, type = "pred", terms = c("NrTouch", "Interaction_Time")) +
  theme(legend.position = "none") +
  labs(
    title = "H1b: Total Day Kindness",
    x = "Number of Touches",
    y = "Predicted Total_DayKindness"
  )

p3 <- plot_model(interactplot_m1c, type = "pred", terms = c("NrTouch", "Interaction_Time")) +
  labs(
    title = "H1c: Kindness Behavior Time",
    x = "Number of Touches",
    y = "Predicted KindnessBehavior.Time",
    color = "Interaction Time",      
    linetype = "Interaction Time"    
  ) +
  theme(legend.position = "right")  
#printing all plots next to each other
p1 + p2 + p3

#linear predictions for H1a is skewed (Pros > 100)

#-------------------------------------------------------------------------------

#Survival Curve for Behavior Kindness
surv_obj <- Surv(time = day_merged$KindnessBehavior.Time, event = rep(1, nrow(day_merged)))

fit <- survfit(surv_obj ~ 1, data = day_merged)

ggsurvplot(fit,
           conf.int = TRUE,
           xlab = "Time (seconds)",
           ylab = "Probability of continuing",
           title = "Survival curve of captcha persistence")

#-------------------------------------------------------------------------------

# Raw Data Touch & ProS Interaction for Partner Types
ggplot(long_touch, aes(x = NrTouch, y = ProS_Eve, color = PartnerType)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~PartnerType) +
  theme_minimal()

#-------------------------------------------------------------------------------

# Raw Data Interaction Time & ProS Interaction for Partner Types
ggplot(long_touch, aes(x = InteractionTime, y = ProS_Eve, color = PartnerType)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~PartnerType) +
  theme_minimal()

### H2b
ggplot(long_touch, aes(x = NrTouch, y = Total_DayKindness, color = PartnerType)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~PartnerType) +
  theme_minimal()



