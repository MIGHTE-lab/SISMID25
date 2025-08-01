library(lubridate)

setwd('/Users/fredlu/repos/SISMID23')


####### From previous exercise #######
################ (a) ################

df <- read.csv('./data/MX_Dengue_trends.csv')
df$Date <- as.Date(df$Date)
plot(df$Date, df$Dengue.CDC, type='l')

cor(df$Dengue.CDC, df[, 3:6])    # correlations

df_train <- df[1:36,]

mod <- lm(Dengue.CDC ~ dengue, data=df_train)
b <- mod$coefficients[1]    # intercept
m <- mod$coefficients[2]    # slope

df_valid <- df[37:nrow(df), ]
pred_static <- df_valid$dengue * m + b



################ Part 2 (a) ################


###### dynamic training ######

# empty array to store predictions
pred_dyn <- numeric(nrow(df_valid))

# sliding 36-month training window
for (month in 37:nrow(df)) {
  
  # define training set
  train_start <- month - 36
  train_end <- month - 1
  df_train <- df[train_start:train_end,]
  
  # fit model
  mod <- lm(Dengue.CDC ~ dengue, data=df_train)
  
  # add prediction using model
  pred_dyn[month - 36] <- predict(mod, df[month,])
}


################ (b) ################

plot(df_valid$Date, df_valid$Dengue.CDC, type='l', col='black')
lines(df_valid$Date, pred_static, type='l', col='red')
lines(df_valid$Date, pred_dyn, type='l', col='green')
legend(df_valid$Date[1], 25000,
       legend=c('Dengue CDC', 'Static', 'Dynamic'),
       col=c('black', 'red', 'green'), lty=1)


################ (c) ################

# how to evaluate our predictions?
rmse <- function(truth, pred) {
  sqrt(mean((truth - pred)^2))
}

rmse(df_valid$Dengue.CDC, pred_static)
rmse(df_valid$Dengue.CDC, pred_dyn)


################ (d) ################

###### dynamic training + more search terms ######

# empty array to store predictions
pred_dyn_all <- numeric(nrow(df_valid))

# sliding 36-month training window
for (month in 37:nrow(df)) {
  
  # define training set
  train_start <- month - 36
  train_end <- month - 1
  df_train <- df[train_start:train_end,]
  
  # fit model
  mod <- lm(Dengue.CDC ~ dengue + sintomas.de.dengue + mosquito + dengue.sintomas,
            data=df_train)
  
  # add prediction using model
  pred_dyn_all[month - 36] <- predict(mod, df[month,])
}

plot(df_valid$Date, df_valid$Dengue.CDC, type='l', col='black')
lines(df_valid$Date, pred_static, type='l', col='red')
lines(df_valid$Date, pred_dyn, type='l', col='green')
lines(df_valid$Date, pred_dyn_all, type='l', col='blue')
legend(df_valid$Date[1], 25000,
       legend=c('Dengue CDC', 'Static', 'Dynamic', 'Dynamic all vars'),
       col=c('black', 'red', 'green', 'blue'), lty=1)

rmse(df_valid$Dengue.CDC, pred_static)
rmse(df_valid$Dengue.CDC, pred_dyn)
rmse(df_valid$Dengue.CDC, pred_dyn_all)


################ (e) ################

###### add AR terms ######
library(dplyr)

add_ar <- function(df) {
  tmp = df %>% mutate(AR1=lag(Dengue.CDC, 1),
                      AR2=lag(Dengue.CDC, 2))
  
}

df_ar <- add_ar(df)


# empty array to store predictions
pred_dyn_ar <- numeric(nrow(df_valid))

# sliding 36-month training window
for (month in 37:nrow(df_ar)) {
  
  # define training set
  train_start <- max(month - 36, 3)    # first 2 rows of df have NAs because of AR terms
  train_end <- month - 1
  df_train <- df_ar[train_start:train_end,]
  
  
  # fit model
  mod <- lm(Dengue.CDC ~ dengue + sintomas.de.dengue + mosquito + dengue.sintomas + AR1 + AR2,
            data=df_train)
  
  # add prediction using model
  pred_dyn_ar[month - 36] <- predict(mod, df_ar[month,])
}


plot(df_valid$Date, df_valid$Dengue.CDC, type='l', col='black')
lines(df_valid$Date, pred_static, type='l', col='red')
lines(df_valid$Date, pred_dyn, type='l', col='green')
lines(df_valid$Date, pred_dyn_all, type='l', col='blue')
lines(df_valid$Date, pred_dyn_ar, type='l', col='purple')
legend(df_valid$Date[1], 25000,
       legend=c('Dengue CDC', 'Static', 'Dynamic', 'Dynamic all vars', 'ARGO'),
       col=c('black', 'red', 'green', 'blue', 'purple'), lty=1)

rmse(df_valid$Dengue.CDC, pred_static)
rmse(df_valid$Dengue.CDC, pred_dyn)
rmse(df_valid$Dengue.CDC, pred_dyn_all)
rmse(df_valid$Dengue.CDC, pred_dyn_ar)
