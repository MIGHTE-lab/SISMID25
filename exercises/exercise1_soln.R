library(lubridate)

setwd('/Users/fredlu/repos/SISMID23')



################ (a) ################

df <- read.csv('./data/MX_Dengue_trends.csv')
df$Date <- as.Date(df$Date)
plot(df$Date, df$Dengue.CDC, type='l')

cor(df$Dengue.CDC, df[, 3:6])    # correlations



################ (b) ################

df_train <- df[1:36,]

mod <- lm(Dengue.CDC ~ dengue, data=df_train)
b <- mod$coefficients[1]    # intercept
m <- mod$coefficients[2]    # slope



################ (c) ################

plot(df_train$dengue, df_train$Dengue.CDC)
abline(mod)



################ (d) ################

df_valid <- df[37:nrow(df), ]
pred_static <- df_valid$dengue * m + b

plot(df_valid$Date, df_valid$Dengue.CDC, type='l', col='black')
lines(df_valid$Date, pred_static, type='l', col='red')
legend(df_valid$Date[1], 25000,
       legend=c('Dengue CDC', 'Predicted'), col=c('black', 'red'), lty=1)

