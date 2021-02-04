library(tidyverse)
library(MASS)



Spin <- read_csv("2019_Pitchers_Spin.csv")
Velocity <- read_csv("2019_Pitcher_Velocities.csv")
Statcast2019 <- read_csv("statcast2019.csv")


Statcast2019 <- Statcast2019 %>%
  mutate(Swing =  ifelse(description %in%
                           c("foul", "foul_bunt", "foul_pitchout",
                             "foul_top", "hit_into_play",
                             "hit_into_play_no_out",
                             "hit_into_play_score", "missed_bunt",
                             "swinging_pitchout",
                             "swinging_strike",
                             "swinging_strike_blocked"), 1, 0),
         Miss = ifelse(description %in%
                         c("swinging_pitchout",
                           "swinging_strike",
                           "swinging_strike_blocked"), 1, 0)) %>%
  rename(pitcher_id = pitcher) %>%
  group_by(pitcher_id, pitch_type) %>% 
  summarize(TotalPitches = n(),
            TotalSwings = sum(Swing),
            TotalMisses = sum(Miss),
            HorizontalLocationBall = abs(mean(plate_x, na.rm = TRUE)),
            VerticalLocationBall = abs(mean(plate_z, na.rm = TRUE)),
            AverageExitVelocity = mean(launch_speed, na.rm = TRUE),
            Barrels = sum(barrel, na.rm = TRUE),
            AverageLaunchAngle = mean(launch_angle, na.rm = TRUE),
            AverageHitDistance = mean(hit_distance_sc, na.rm = TRUE),
            AverageEstimatedBattingAverage = mean(estimated_ba_using_speedangle, na.rm = TRUE),
            AverageEstimatedWOBA = mean(estimated_woba_using_speedangle, na.rm = TRUE)) %>%
  filter(TotalSwings > 10) %>% 
  mutate(SwingPercentage = TotalSwings/TotalPitches,
         MissPercentage = TotalMisses/TotalPitches, 
         BarrelPercentage = Barrels/TotalPitches)

Spin <- gather(Spin, "PitchType", "SpinRate", -pitcher)

Velocity <- gather(Velocity, "PitchType", "Velocity", -pitcher) 

SpinVelocity <- merge(Spin, Velocity, by=c("pitcher","PitchType"))

SpinVelocity <- na.omit(SpinVelocity)


SpinVelocity <- SpinVelocity %>% 
  mutate(BauerIndex = SpinRate/Velocity)

SpinVelocity <- SpinVelocity %>% 
  rename(pitch_type = PitchType) %>% 
  rename(pitcher_id = pitcher)

complete <- merge(Statcast2019, SpinVelocity, by=c("pitcher_id","pitch_type"))

complete$pitch_type <-case_when(
  complete$pitch_type == "CH" ~ 1,
  complete$pitch_type == "CU" ~ 2,
  complete$pitch_type == "FC" ~ 3,
  complete$pitch_type == "FF" ~ 4,
  complete$pitch_type == "FS" ~ 5,
  complete$pitch_type == "SI" ~ 6,
  complete$pitch_type == "SL" ~ 7)

complete2 <- complete %>% 
  filter(TotalPitches >= 150) 

complete2 <- complete2[,-10]





###Initial Model (We Are Using)
fullmodel <- lm(AverageEstimatedWOBA ~ ., data = complete2)
summary(fullmodel)

#Then We Did A Hybrid Select On Model
hybridselect <- step(fullmodel,scope=list(lower=~1,upper=fullmodel),
                              direction="both",trace=FALSE, na.rm = T)
summary(hybridselecttest)
hybridselecttest <- lm(AverageEstimatedWOBA ~ HorizontalLocationBall + AverageExitVelocity +
                         AverageHitDistance + AverageEstimatedBattingAverage + SwingPercentage +
                         MissPercentage + BarrelPercentage + SpinRate + BauerIndex, data = complete2)
#Normality Tests On Initial Model
ols_test_normality(hybridselecttest)
ols_plot_resid_fit(hybridselecttest)
qqnorm(residuals(hybridselecttest))
qqline(residuals(hybridselect))
ols_plot_resid_hist(hybridselecttest)
boxcox(hybridselecttest, lambda = seq(-1, 2, .1))


vif(hybridselecttest)

## After Assessing BoxCox, Tried These Two (Worsened The Box-Cox)
complete3 <- sqrt(complete2)

fullmodel2 <- lm(AverageEstimatedWOBA ~ ., data = complete4)
summary(fullmodel2)

hybridselect2 <- step(newmodel2,scope=list(lower=~1,upper=newmodel2),
                      direction="both",trace=FALSE, na.rm = T)

vif(hybridselect2)

newmodel <- lm(AverageEstimatedWOBA ~ HorizontalLocationBall + AverageHitDistance +
               SwingPercentage + MissPercentage + AverageHitDistance + 
               AverageEstimatedBattingAverage + SpinRate + BauerIndex, data = complete4)

newmodel2 <- lm(AverageEstimatedWOBA ~ HorizontalLocationBall + AverageHitDistance +
                 SwingPercentage + MissPercentage + AverageHitDistance + 
                 AverageEstimatedBattingAverage + SpinRate + BauerIndex, data = complete4)

summary(newmodel)  
ols_plot_resid_fit(hybridselect2)
ols_plot_resid_hist()
qqnorm(residuals(newmodel))
qqline(residuals(newmodel))
shapiro.test(newmodel$residuals)

ols_plot_resid_fit(hybridselect2)
ols_plot_resid_hist(hybridselect2)
qqnorm(residuals(hybridselect2))
qqline(residuals(hybridselect2))
boxcox(hybridselect2, lambda = seq(-1, 2, .1))


#This Gives Us An Estimated Lambda Of 1
complete4 <- sqrt(complete3)




