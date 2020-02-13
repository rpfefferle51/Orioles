# Orioles
Baltimore Orioles Homerun Probabilities 
```{r}
library(dplyr)
library(ggplot2)
library(broom)
```

```{r}
#Importing CSV of Baseball Savant Data for ABs in 2019

PeteAlonso <- read.csv("PeteAlonso.csv", header = T)
TreyMancini <- read.csv("TreyMancini.csv", header = T)
RenatoNunez <- read.csv("RenatoNunez.csv", header = T)
AnthonySantander <- read.csv("AnthonySantander.csv", header = T)
```

```{r}
#Adding ESPN's Park Factor Value to the data based on stadium of at-bat
PeteAlonso$park_factor <- ifelse(PeteAlonso$home_team == "COL", 1.394, 
       ifelse(PeteAlonso$home_team == "TEX", 1.245,
              ifelse(PeteAlonso$home_team == "DET", 1.107,
                     ifelse(PeteAlonso$home_team == "WSH", 1.101,
                            ifelse(PeteAlonso$home_team == "BAL", 1.088,
                                   ifelse(PeteAlonso$home_team == "MIA", 1.087,
                                          ifelse(PeteAlonso$home_team == "HOU", 1.083,
        ifelse(PeteAlonso$home_team == "KC", 1.074,
               ifelse(PeteAlonso$home_team == "BOS", 1.063,
                      ifelse(PeteAlonso$home_team == "PHI", 1.047,
                             ifelse(PeteAlonso$home_team == "CIN", 1.038,
                                    ifelse(PeteAlonso$home_team == "TOR", 1.031,
                                           ifelse(PeteAlonso$home_team == "LAA", 1.018,
        ifelse(PeteAlonso$home_team == "PIT", 1.004,
               ifelse(PeteAlonso$home_team == "ATL", 1.003,
                   ifelse(PeteAlonso$home_team == "ARI", 0.977,  
                          ifelse(PeteAlonso$home_team == "MIL", 0.976,
                                 ifelse(PeteAlonso$home_team == "MIN", 0.975,
                                        ifelse(PeteAlonso$home_team == "CLE", 0.972,
        ifelse(PeteAlonso$home_team == "CWS", 0.966,
               ifelse(PeteAlonso$home_team == "SEA", 0.952,
                      ifelse(PeteAlonso$home_team == "CHC", 0.931,
                             ifelse(PeteAlonso$home_team == "STL", 0.917,
                                    ifelse(PeteAlonso$home_team == "LAD", 0.905,
                                           ifelse(PeteAlonso$home_team == "TB", 0.895,
        ifelse(PeteAlonso$home_team == "NYM", 0.891,
               ifelse(PeteAlonso$home_team == "OAK", 0.887,
                      ifelse(PeteAlonso$home_team == "SD", 0.860,
                             ifelse(PeteAlonso$home_team == "NYY", 0.816,
                                    ifelse(PeteAlonso$home_team == "SF", 0.798,
              0.00))))))))))))))))))))))))))))))

TreyMancini$park_factor <- ifelse(TreyMancini$home_team == "COL", 1.394, 
       ifelse(TreyMancini$home_team == "TEX", 1.245,
              ifelse(TreyMancini$home_team == "DET", 1.107,
                     ifelse(TreyMancini$home_team == "WSH", 1.101,
                            ifelse(TreyMancini$home_team == "BAL", 1.088,
                                   ifelse(TreyMancini$home_team == "MIA", 1.087,
                                          ifelse(TreyMancini$home_team == "HOU", 1.083,
        ifelse(TreyMancini$home_team == "KC", 1.074,
               ifelse(TreyMancini$home_team == "BOS", 1.063,
                      ifelse(TreyMancini$home_team == "PHI", 1.047,
                             ifelse(TreyMancini$home_team == "CIN", 1.038,
                                    ifelse(TreyMancini$home_team == "TOR", 1.031,
                                           ifelse(TreyMancini$home_team == "LAA", 1.018,
        ifelse(TreyMancini$home_team == "PIT", 1.004,
               ifelse(TreyMancini$home_team == "ATL", 1.003,
                   ifelse(TreyMancini$home_team == "ARI", 0.977,  
                          ifelse(TreyMancini$home_team == "MIL", 0.976,
                                 ifelse(TreyMancini$home_team == "MIN", 0.975,
                                        ifelse(TreyMancini$home_team == "CLE", 0.972,
        ifelse(TreyMancini$home_team == "CWS", 0.966,
               ifelse(TreyMancini$home_team == "SEA", 0.952,
                      ifelse(TreyMancini$home_team == "CHC", 0.931,
                             ifelse(TreyMancini$home_team == "STL", 0.917,
                                    ifelse(TreyMancini$home_team == "LAD", 0.905,
                                           ifelse(TreyMancini$home_team == "TB", 0.895,
        ifelse(TreyMancini$home_team == "NYM", 0.891,
               ifelse(TreyMancini$home_team == "OAK", 0.887,
                      ifelse(TreyMancini$home_team == "SD", 0.860,
                             ifelse(TreyMancini$home_team == "NYY", 0.816,
                                    ifelse(TreyMancini$home_team == "SF", 0.798,
              0.00))))))))))))))))))))))))))))))

RenatoNunez$park_factor <- ifelse(RenatoNunez$home_team == "COL", 1.394, 
       ifelse(RenatoNunez$home_team == "TEX", 1.245,
              ifelse(RenatoNunez$home_team == "DET", 1.107,
                     ifelse(RenatoNunez$home_team == "WSH", 1.101,
                            ifelse(RenatoNunez$home_team == "BAL", 1.088,
                                   ifelse(RenatoNunez$home_team == "MIA", 1.087,
                                      ifelse(RenatoNunez$home_team == "HOU", 1.083,
        ifelse(RenatoNunez$home_team == "KC", 1.074,
               ifelse(RenatoNunez$home_team == "BOS", 1.063,
                      ifelse(RenatoNunez$home_team == "PHI", 1.047,
                             ifelse(RenatoNunez$home_team == "CIN", 1.038,
                                    ifelse(RenatoNunez$home_team == "TOR", 1.031,
                                     ifelse(RenatoNunez$home_team == "LAA", 1.018,
        ifelse(RenatoNunez$home_team == "PIT", 1.004,
               ifelse(RenatoNunez$home_team == "ATL", 1.003,
                   ifelse(RenatoNunez$home_team == "ARI", 0.977,  
                          ifelse(RenatoNunez$home_team == "MIL", 0.976,
                                 ifelse(RenatoNunez$home_team == "MIN", 0.975,
                                      ifelse(RenatoNunez$home_team == "CLE", 0.972,
        ifelse(RenatoNunez$home_team == "CWS", 0.966,
               ifelse(RenatoNunez$home_team == "SEA", 0.952,
                      ifelse(RenatoNunez$home_team == "CHC", 0.931,
                             ifelse(RenatoNunez$home_team == "STL", 0.917,
                                    ifelse(RenatoNunez$home_team == "LAD", 0.905,
                                        ifelse(RenatoNunez$home_team == "TB", 0.895,
        ifelse(RenatoNunez$home_team == "NYM", 0.891,
               ifelse(RenatoNunez$home_team == "OAK", 0.887,
                      ifelse(RenatoNunez$home_team == "SD", 0.860,
                             ifelse(RenatoNunez$home_team == "NYY", 0.816,
                                    ifelse(RenatoNunez$home_team == "SF", 0.798,
              0.00))))))))))))))))))))))))))))))

AnthonySantander$park_factor <- ifelse(AnthonySantander$home_team == "COL", 1.394, 
       ifelse(AnthonySantander$home_team == "TEX", 1.245,
              ifelse(AnthonySantander$home_team == "DET", 1.107,
                     ifelse(AnthonySantander$home_team == "WSH", 1.101,
                            ifelse(AnthonySantander$home_team == "BAL", 1.088,
                                   ifelse(AnthonySantander$home_team == "MIA", 1.087,
                                      ifelse(AnthonySantander$home_team == "HOU", 1.083,
        ifelse(AnthonySantander$home_team == "KC", 1.074,
               ifelse(AnthonySantander$home_team == "BOS", 1.063,
                      ifelse(AnthonySantander$home_team == "PHI", 1.047,
                             ifelse(AnthonySantander$home_team == "CIN", 1.038,
                                    ifelse(AnthonySantander$home_team == "TOR", 1.031,
                                     ifelse(AnthonySantander$home_team == "LAA", 1.018,
        ifelse(AnthonySantander$home_team == "PIT", 1.004,
               ifelse(AnthonySantander$home_team == "ATL", 1.003,
                   ifelse(AnthonySantander$home_team == "ARI", 0.977,  
                          ifelse(AnthonySantander$home_team == "MIL", 0.976,
                                 ifelse(AnthonySantander$home_team == "MIN", 0.975,
                                      ifelse(AnthonySantander$home_team == "CLE", 0.972,
        ifelse(AnthonySantander$home_team == "CWS", 0.966,
               ifelse(AnthonySantander$home_team == "SEA", 0.952,
                      ifelse(AnthonySantander$home_team == "CHC", 0.931,
                             ifelse(AnthonySantander$home_team == "STL", 0.917,
                                    ifelse(AnthonySantander$home_team == "LAD", 0.905,
                                        ifelse(AnthonySantander$home_team == "TB", 0.895,
        ifelse(AnthonySantander$home_team == "NYM", 0.891,
               ifelse(AnthonySantander$home_team == "OAK", 0.887,
                      ifelse(AnthonySantander$home_team == "SD", 0.860,
                             ifelse(AnthonySantander$home_team == "NYY", 0.816,
                                    ifelse(AnthonySantander$home_team == "SF", 0.798,
              0.00))))))))))))))))))))))))))))))

#Creating Dummy Homerun Variable
PeteAlonso$homerun <- ifelse(PeteAlonso$events == "home_run", 1, 0)
TreyMancini$homerun <- ifelse(TreyMancini$events == "home_run", 1, 0)
RenatoNunez$homerun <- ifelse(RenatoNunez$events == "home_run", 1, 0)
AnthonySantander$homerun <- ifelse(AnthonySantander$events == "home_run", 1, 0)
```

```{r}
#Data formatting

#Replacing nulls with NA
PeteAlonso$launch_angle[PeteAlonso$launch_angle == "null"] <- NA
PeteAlonso$launch_speed[PeteAlonso$launch_speed == "null"] <- NA
PeteAlonso$effective_speed[PeteAlonso$effective_speed == "null"] <- NA
PeteAlonso$release_spin_rate[PeteAlonso$release_spin_rate == "null"] <- NA
PeteAlonso$hit_distance_sc[PeteAlonso$hit_distance_sc == "null"] <- NA

#changing structure for analysis
PeteAlonso$launch_angle <- as.numeric(as.character(PeteAlonso$launch_angle))
PeteAlonso$launch_speed <- as.numeric(as.character(PeteAlonso$launch_speed))
PeteAlonso$effective_speed <- as.numeric(as.character(PeteAlonso$effective_speed))
PeteAlonso$release_spin_rate <- as.numeric(as.character(PeteAlonso$release_spin_rate))
PeteAlonso$hit_distance_sc <- as.numeric(as.character(PeteAlonso$hit_distance_sc))
```

```{r}
#Logistic Regression models
#Standard Logistic Regression Model
glm_Alonso <- glm(homerun ~ launch_angle + launch_speed + effective_speed +   release_spin_rate + park_factor, data = PeteAlonso, family = binomial, na.action = na.exclude)
summary(glm_Alonso)

#Improved Model using polynomial fitting for launch angle
#Some regressors aren't statisticall significant but I chose to keep them anyways
glm_Alonso2 <- glm(homerun ~ poly(launch_angle, 2, raw=TRUE) + launch_speed + effective_speed:release_spin_rate + park_factor, data = PeteAlonso, family = binomial, na.action = na.exclude)

summary(glm_Alonso2)

#Same model but with distance
glm_Alonso3 <- lm(hit_distance_sc ~ poly(launch_angle,2,raw=TRUE) + launch_speed + effective_speed:release_spin_rate + park_factor, data = PeteAlonso, na.action = na.exclude)

summary(glm_Alonso3)
```

```{r}
#Filtering middle launch angle values
PeteAlonso_middle <- PeteAlonso %>%
  filter(launch_angle >= 0, launch_angle  <= 60)

# scatterplot with jitter
data_space <- ggplot(data = PeteAlonso_middle, aes(y = homerun, x = launch_angle)) + 
      geom_jitter(width = 0, height = 0.05, alpha = 0.5) + ggtitle("Pete Alonso") 
#smooth curve
 data_space +
  geom_smooth() + geom_vline(xintercept = 27.55)
#We see highest point on geom_smooth curve occurs about launch angle = 27.55 degrees
```

 
```{r}
# create new data frame with predictive data
#Used Aaron Nola's 2019 averages (4seam Fb) and Alsonso's average EV
#Saying it's at Citi Field for park factor
new_pitch_Nola <- data.frame(launch_angle = 27.55, launch_speed = 94.7, effective_speed = 89.7, release_spin_rate = 2171, park_factor = 0.891)

# make predictions on probability of a homerun on the given pitch at optimal launch #angle
augment(glm_Alonso2, newdata = new_pitch_Nola, type.predict = "response")
# 0.1426
# make predictions on expected distance on the given pitch at optimal launch #angle
augment(glm_Alonso3, newdata = new_pitch_Nola, type.predict = "response")
#260.76 feet
```


```{r}
#Confusion Matrix to see accuracy
tidy.Alonso <- augment(glm_Alonso2, type.predict = "response") %>%
    mutate(homerun.hat = round(.fitted))
tidy.Alonso %>%
  select(homerun, homerun.hat) %>%
  table()
```

```{r}
#Data formatting

#Replacing nulls with NA
TreyMancini$launch_angle[TreyMancini$launch_angle == "null"] <- NA
TreyMancini$launch_speed[TreyMancini$launch_speed == "null"] <- NA
TreyMancini$effective_speed[TreyMancini$effective_speed == "null"] <- NA
TreyMancini$release_spin_rate[TreyMancini$release_spin_rate == "null"] <- NA
TreyMancini$hit_distance_sc[TreyMancini$hit_distance_sc == "null"] <- NA

#changing structure for analysis
TreyMancini$launch_angle <- as.numeric(as.character(TreyMancini$launch_angle))
TreyMancini$launch_speed <- as.numeric(as.character(TreyMancini$launch_speed))
TreyMancini$effective_speed <- as.numeric(as.character(TreyMancini$effective_speed))
TreyMancini$release_spin_rate <- as.numeric(as.character(TreyMancini$release_spin_rate))
TreyMancini$hit_distance_sc <- as.numeric(as.character(TreyMancini$hit_distance_sc))
```

```{r}
#Standard Logistic model 
glm_Mancini <- glm(homerun ~ launch_angle + launch_speed + effective_speed + release_spin_rate + park_factor, data = TreyMancini, family = binomial, na.action = na.exclude)
summary(glm_Mancini)

#Improved Model using polynomial fitting for launch angle
#Some regressors were highly insignificant so I chose to remove them from model 
glm_Mancini2 <- glm(homerun ~ poly(launch_angle, 2, raw=TRUE) + launch_speed, data = TreyMancini, family = binomial, na.action = na.exclude)

summary(glm_Mancini2)

#Same model but with distance
glm_Mancini3 <- lm(hit_distance_sc ~ poly(launch_angle,2,raw=TRUE) + launch_speed, data = TreyMancini, na.action = na.exclude)

summary(glm_Mancini3)
```

```{r}
#Filtering middle launch angle values
TreyMancini_middle <- TreyMancini %>%
  filter(launch_angle >= 0, launch_angle  <= 60)

# scatterplot with jitter
data_space <- ggplot(data = TreyMancini_middle, aes(y = homerun, x = launch_angle)) + 
      geom_jitter(width = 0, height = 0.05, alpha = 0.5) + ggtitle("TreyMancini") 
#smooth curve
 data_space +
  geom_smooth() + geom_vline(xintercept = 25.7)
 #We see highest point on geom_smooth curve occurs about launch angle = 25.7 degrees
```

```{r}
# create new data frame with predictive data
#Used Eduardo Rodriguez's 2019 averages (4seam Fb) and Mancini's average EV
#Assume game played at Camden Yards
new_pitch_Rodriguez <- data.frame(launch_angle = 27.5, launch_speed = 93.6, effective_speed = 88, release_spin_rate = 2200, park_factor = 1.088)

# make predictions on probability of a homerun on the given pitch at optimal launch #angle
augment(glm_Mancini2, newdata = new_pitch_Rodriguez, type.predict = "response")
# 0.06814

# make predictions on expected distance on the given pitch at optimal launch #angle
augment(glm_Mancini3, newdata = new_pitch_Rodriguez, type.predict = "response")
#268.1 feet
```

```{r}
#Confusion Matrix
tidy.Mancini <- augment(glm_Mancini2, type.predict = "response") %>%
    mutate(homerun.hat = round(.fitted))
tidy.Mancini %>%
  select(homerun, homerun.hat) %>%
  table()
```

```{r}
#Data formatting

#Replacing nulls with NA
RenatoNunez$launch_angle[RenatoNunez$launch_angle == "null"] <- NA
RenatoNunez$launch_speed[RenatoNunez$launch_speed == "null"] <- NA
RenatoNunez$effective_speed[RenatoNunez$effective_speed == "null"] <- NA
RenatoNunez$release_spin_rate[RenatoNunez$release_spin_rate == "null"] <- NA
RenatoNunez$hit_distance_sc[RenatoNunez$hit_distance_sc == "null"] <- NA

#changing structure for analysis
RenatoNunez$launch_angle <- as.numeric(as.character(RenatoNunez$launch_angle))
RenatoNunez$launch_speed <- as.numeric(as.character(RenatoNunez$launch_speed))
RenatoNunez$effective_speed <- as.numeric(as.character(RenatoNunez$effective_speed))
RenatoNunez$release_spin_rate <- as.numeric(as.character(RenatoNunez$release_spin_rate))
RenatoNunez$hit_distance_sc <- as.numeric(as.character(RenatoNunez$hit_distance_sc))

```

```{r}
#Logistic model 
glm_Nunez <- glm(homerun ~ launch_angle + launch_speed + effective_speed + release_spin_rate + park_factor, data = RenatoNunez, family = binomial, na.action = na.exclude)

summary(glm_Nunez)

#Improved Model using polynomial fitting for launch angle
#All of the variables except park factor are significant, and park factor is pretty darn close
glm_Nunez2 <- glm(homerun ~ poly(launch_angle, 2, raw=TRUE) + launch_speed + effective_speed:release_spin_rate+ park_factor, data = RenatoNunez, family = binomial, na.action = na.exclude)

summary(glm_Nunez2)

#Same model but with distance
glm_Nunez3 <- lm(hit_distance_sc ~ poly(launch_angle,2,raw=TRUE) + launch_speed + effective_speed:release_spin_rate + park_factor, data = RenatoNunez, na.action = na.exclude)

summary(glm_Nunez3)
```

```{r}
#Filtering middle launch angle values
RenatoNunez_middle <- RenatoNunez %>%
  filter(launch_angle >= 0, launch_angle  <= 60)

# scatterplot with jitter
data_space <- ggplot(data = RenatoNunez_middle, aes(y = homerun, x = launch_angle)) + geom_jitter(width = 0, height = 0.05, alpha = 0.5) + 
  ggtitle("RenatoNunez")
#smooth curve
 data_space +
  geom_smooth() + geom_vline(xintercept = 25.6)
 
  #We see highest homerun probability at LA of about 25.6 degrees
```

```{r}
# create new data frame with predictive data
#Used Eduardo Rodriguez's 2019 averages (4seam Fb) and Nunez's average EV
#Assume game played at Camden Yards
new_pitch_Rodriguez <- data.frame(launch_angle = 25.6, launch_speed = 91.8, effective_speed = 88, release_spin_rate = 2200, park_factor = 1.088)

# make predictions on probability of a homerun on the given pitch at optimal launch #angle
augment(glm_Nunez2, newdata = new_pitch_Rodriguez, type.predict = "response")
# 0.00126

# make predictions on expected distance on the given pitch at optimal launch #angle
augment(glm_Nunez3, newdata = new_pitch_Rodriguez, type.predict = "response")
#238.88 feet
```

```{r}
#Confusion Matrix
tidy.Nunez <- augment(glm_Nunez2, type.predict = "response") %>%
    mutate(homerun.hat = round(.fitted))
tidy.Nunez %>%
  select(homerun, homerun.hat) %>%
  table()
```

```{r}
#Data formatting

#Replacing nulls with NA
AnthonySantander$launch_angle[AnthonySantander$launch_angle == "null"] <- NA
AnthonySantander$launch_speed[AnthonySantander$launch_speed == "null"] <- NA
AnthonySantander$effective_speed[AnthonySantander$effective_speed == "null"] <- NA
AnthonySantander$release_spin_rate[AnthonySantander$release_spin_rate == "null"] <- NA
AnthonySantander$hit_distance_sc[AnthonySantander$hit_distance_sc == "null"] <- NA

#changing structure for analysis
AnthonySantander$launch_angle <- as.numeric(as.character(AnthonySantander$launch_angle))
AnthonySantander$launch_speed <- as.numeric(as.character(AnthonySantander$launch_speed))
AnthonySantander$effective_speed <- as.numeric(as.character(AnthonySantander$effective_speed))
AnthonySantander$release_spin_rate <- as.numeric(as.character(AnthonySantander$release_spin_rate))
AnthonySantander$hit_distance_sc <- as.numeric(as.character(AnthonySantander$hit_distance_sc))
```

```{r}
#Standard Logistic model 
glm_Santander <- glm(homerun ~ launch_angle + launch_speed + effective_speed +   release_spin_rate + park_factor, data = AnthonySantander, family = binomial, na.action = na.exclude)

summary(glm_Santander)

#Improved Model using polynomial fitting for launch angle
#Some regressors aren't statisticall significant but I chose to keep them anyways
glm_Santander2 <- glm(homerun ~ poly(launch_angle, 2, raw=TRUE) + launch_speed, data = AnthonySantander, family = binomial, na.action = na.exclude)

summary(glm_Santander2)

#Same model but with distance
glm_Santander3 <- lm(hit_distance_sc ~ poly(launch_angle,2,raw=TRUE) + launch_speed, data = AnthonySantander, na.action = na.exclude)

summary(glm_Santander3)
```

```{r}
#Filtering middle launch angle values
AnthonySantander_middle <- AnthonySantander %>%
  filter(launch_angle >= 0, launch_angle  <= 60)

# scatterplot with jitter
data_space <- ggplot(data = AnthonySantander_middle, aes(y = homerun, x = launch_angle)) +
      geom_jitter(width = 0, height = 0.05, alpha = 0.5) + ggtitle("Anthony Santander") 
#smooth curve
 data_space +
  geom_smooth() + geom_vline(xintercept = 28)
 #We see highest point on geom_smooth curve occurs about launch angle = 28 degrees
```

```{r}
# create new data frame with predictive data
#Used Eduardo Rodriguez's 2019 averages (4seam Fb) and Nunez's average EV
#Assume game played at Camden Yards
new_pitch_Rodriguez <- data.frame(launch_angle = 28, launch_speed = 89.1, effective_speed = 88, release_spin_rate = 2200, park_factor = 1.088)

# make predictions on probability of a homerun on the given pitch at optimal launch #angle
augment(glm_Santander2, newdata = new_pitch_Rodriguez, type.predict = "response")
# 0.000462

# make predictions on expected distance on the given pitch at optimal launch #angle
augment(glm_Santander3, newdata = new_pitch_Rodriguez, type.predict = "response")
#247.1161 feet
```

```{r}
#Confusion Matrix
tidy.Santander <- augment(glm_Santander2, type.predict = "response") %>%
    mutate(homerun.hat = round(.fitted))
tidy.Santander %>%
  select(homerun, homerun.hat) %>%
  table()
```