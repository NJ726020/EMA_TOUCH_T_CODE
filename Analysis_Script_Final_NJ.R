### Analyse Master
### Niklas Jung
### 08/08/2025
### Intent: Final analysis 

library(lme4)
library(lmerTest)
library(performance)
library(afex)
library(emmeans)
library(ggplot2)
library(lattice)
library(glmmTMB)
library(effectsize)


# Data loaded in NJ_Thesis_Preprocess.R
source("NJ_Thesis_Preprocess.R")

#more readable digits and decimals in outputs
options(scipen = 999, digits = 6)  


#ICC für MLM 

# Random Intercept Model (Just ID as group/cluster)
m0 <- mixed(ProS_Eve ~ 1 + (1 | ID), data = day_merged, method = "S")
#m0 <- lmer(ProS_Eve ~ 1 + (1 | ID), data = day_merged)

#dotplot(ranef(m0, condVar=TRUE))

# ICC berechnen
summary(m0)
icc(m0)




#H1 Number of Touches and Kindness Evening (controlled for Interaction Time)
day_merged$Interaction_Time_z <- scale(day_merged$Interaction_Time, center = TRUE, scale = TRUE)
day_merged$NrTouch_z <- scale(day_merged$NrTouch, center = TRUE, scale = TRUE)

summary(day_merged[c("NrTouch_z", "Interaction_Time_z")])


#H1a
m1a <- mixed(ProS_Eve ~ NrTouch * Interaction_Time + (1 | ID ), method = "S", data = day_merged)
summary(m1a)

#get standazised coefs (ß)
standardize_parameters(m1a)

#H1a with scaled predictors
m1a_z <- mixed(ProS_Eve ~ NrTouch_z * Interaction_Time_z + (1 | ID ), method = "S", data = day_merged)
summary(m1a_z)

#get standazised coefs (ß)
standardize_parameters(m1a_z)

#H1b
m1b <- mixed(Total_DayKindness ~ NrTouch * Interaction_Time + (1 | ID ), method = "S", data = day_merged)

summary(m1b)

#get standazised coefs (ß)
standardize_parameters(m1b)

#h1b with scaled predictors
m1b_z <- mixed(Total_DayKindness ~ NrTouch_z * Interaction_Time_z + (1 | ID ), method = "S", data = day_merged)

summary(m1b_z)

standardize_parameters(m1b_z)

#H1c
# think about transformation or non-parametric

day_merged$BehaviorOccurred <- as.numeric(day_merged$KindnessBehavior.Time > 0)
# Hurdle part 1: zero vs non-zero (logistic) (standatized predictors)
m1c_hurdle_binary <- glmmTMB(
  BehaviorOccurred ~ NrTouch_z * Interaction_Time_z + (1 | ID),
  family = binomial(link = "logit"),
  data = day_merged
)
summary(m1c_hurdle_binary)

# get ORs
coefs <- summary(m1c_hurdle_binary)$coefficients$cond
ORs <- exp(coefs[, "Estimate"])
ORs

#and Confidence interval
exp(confint(m1c_hurdle_binary))


# Filter to positive durations only
day_positive <- subset(day_merged, KindnessBehavior.Time > 0)

# Hurdle part 2: duration given >0
m1c_hurdle_positive <- glmmTMB(
  KindnessBehavior.Time ~ NrTouch_z * Interaction_Time_z + (1 | ID),
  family = Gamma(link = "log"),
  data = day_positive
)

summary(m1c_hurdle_positive)
standardize_parameters(m1c_hurdle_positive)

#-------------------------------------------------------------------------------




## H2
long_touch$PartnerType <- factor(
  long_touch$PartnerType,
  levels = c("NrT_Close", "NrT_Well", "NrT_Dist", "NrT_Unknown")
)

#manually coded the dummy variables due to better readability than the mixed object
#long_touch$PT_Close   <- ifelse(long_touch$PartnerType == "NrT_Close", 1, 0)
#long_touch$PT_Well    <- ifelse(long_touch$PartnerType == "NrT_Well", 1, 0)
#long_touch$PT_Distant <- ifelse(long_touch$PartnerType == "NrT_Dist", 1, 0)
#long_touch$PT_Unknown <- ifelse(long_touch$PartnerType == "NrT_Unknown", 1, 0)

#m2a_dummy <- mixed(ProS_Eve ~ NrTouch * (PT_Well + PT_Distant + PT_Unknown) * InteractionTime + (1|ID), method = "S",  data = long_touch)
#m2a_dummy <- lmer(ProS_Eve ~ NrTouch * (PT_Well + PT_Distant + PT_Unknown) * InteractionTime + (1|ID),  data = long_touch)

#summary(m2a_dummy)
#standardize_parameters(m2a_dummy)

long_touch$InteractionTime_z <- scale(long_touch$InteractionTime, center = TRUE, scale = TRUE)
long_touch$NrTouch_z <- scale(long_touch$NrTouch, center = TRUE, scale = TRUE)


#Prefer lmer over mixed as category names stay coherent and more functions work with lmer objects
#m2a <- mixed(ProS_Eve ~ NrTouch_z * PartnerType * InteractionTime_z + (1 | ID), method = "S", data = long_touch)
m2a <- lmer(ProS_Eve ~ NrTouch_z * PartnerType * InteractionTime_z + (1 | ID), data = long_touch)

summary(m2a)



#get standazised coefs (ß)
standardize_parameters(m2a)



#H2b
#m2b <- mixed(Total_DayKindness ~ NrTouch * PartnerType * InteractionTime + (1 | ID), method = "S", data = long_touch)
m2b <- lmer(Total_DayKindness ~ NrTouch_z * PartnerType * InteractionTime_z + (1 | ID), data = long_touch)
summary(m2b)

standardize_parameters(m2b)




#H2c
#m2c <- lmer(KindnessBehavior.Time ~ NrTouch * PartnerType * InteractionTime + (1 | ID), data = long_touch)


#summary(m2c)

#H2c
# think about transformation or non-parametric

long_touch$BehaviorOccurred <- as.numeric(long_touch$KindnessBehavior.Time > 0)
# Hurdle part 1: zero vs non-zero (logistic)
m2c_hurdle_binary <- glmmTMB(
  BehaviorOccurred ~ NrTouch_z * PartnerType * InteractionTime_z + (1 | ID),
  family = binomial(link = "logit"),
  data = long_touch
)
summary(m2c_hurdle_binary)

# get ORs
coefs2 <- summary(m2c_hurdle_binary)$coefficients$cond
ORs2 <- exp(coefs2[, "Estimate"])
ORs2

#and Confidence interval
exp(confint(m2c_hurdle_binary))


# Filter to positive durations only
day_positive2 <- subset(long_touch, KindnessBehavior.Time > 0)

# Hurdle part 2: duration given >0
m2c_hurdle_positive <- glmmTMB(
  KindnessBehavior.Time ~ NrTouch_z * PartnerType * InteractionTime_z + (1 | ID),
  family = Gamma(link = "log"),
  data = day_positive2
)

summary(m2c_hurdle_positive)
standardize_parameters(m2c_hurdle_positive)


## Exploratory Analysis
##non linear interaction effect for H1a
#------------------------------------

# Baseline: linear interaction
m_lin <- lmer(
  ProS_Eve ~ NrTouch_z * Interaction_Time_z + (1 | ID),
  data = day_merged,
  REML = FALSE
)

# Quadratic moderation: linear + quadratic terms and their interactions with touch
m_quad <- lmer(
  ProS_Eve ~ NrTouch_z * poly(Interaction_Time_z, 2, raw = TRUE) + (1 | ID),
  data = day_merged,
  REML = FALSE
)

# Likelihood-ratio test
anova(m_lin, m_quad)



#bedenke, alles gescaled nun!
#Appendix descriptives komplett (Sex, Occupation)
# Push changes

