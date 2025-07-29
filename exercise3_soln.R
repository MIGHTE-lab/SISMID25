# R version of Exercise 3. This is almost a complete solution, but the outbreak detection
# is only performed for the new_cases. To run the outbreak detection on the rest
# of the data traces, you can loop the code over the other columns, storing the results in a list.
library(lubridate)

setwd('/Users/fredlu/repos/SISMID23')


df <- read.csv('./data/covid_traces_WA.csv')
df$date <- mdy(df$date)

signals = c('new_cases', 'upToDate', 'cdc_ili',
            'Twitter_RelatedTweets', 'google_fever',
            'Kinsa_AnomalousFeverAbsolute')
par(mfrow=c(3, 2))
plot(df$date, df[, signals[1]], type='l', ylab=signals[1])
plot(df$date, df[, signals[2]], type='l', ylab=signals[2])
plot(df$date, df[, signals[3]], type='l', ylab=signals[3])
plot(df$date, df[, signals[4]], type='l', ylab=signals[4])
plot(df$date, df[, signals[5]], type='l', ylab=signals[5])
plot(df$date, df[, signals[6]], type='l', ylab=signals[6])


# (b)
alpha_arr <- numeric(nrow(df))
xarr <- df$new_cases
for (i in 11:nrow(df)) {
  before <- xarr[(i-10):(i-1)]
  after <- xarr[(i-9):i]
  mod <- lm(after ~ before +0)
  
  alpha <- mod$coefficients[1]
  if (is.na(alpha)) {
    alpha <- 0
  }
  alpha_arr[i] <- alpha
}

# (c)
par(mfrow=c(3, 1))
plot(df$date, df[, 'new_cases'], type='l')
plot(df$date, alpha_arr, type='l', col='blue')
abline(h=1, col='red')
plot(df$date, as.integer(alpha_arr > 1), type='l', col='green')


# (d)
out_arr <- numeric(nrow(df))
outbreak_already_active <- FALSE
for (i in 10:nrow(df)) {
  outbreak <- 0
  if (sum(as.integer(alpha_arr[(i-10):i] > 1)) == 10) {
    if (outbreak_already_active) {
      outbreak <- 0
    }
    else {
      outbreak_already_active <- TRUE
      outbreak <- 1
    }
  }
  else if (sum(as.integer(alpha_arr[(i-10):i] > 1)) == 0) {
    outbreak_already_active <- FALSE
  }
  out_arr[i] <- outbreak
}


# (e)
# indices where outbreaks occur
locs <- which(out_arr == 1)

par(mfrow=c(3, 1))
plot(df$date, df[, 'new_cases'], type='l')
points(df$date[locs], df[locs, 'new_cases'], col='red', pch=4)
plot(df$date, alpha_arr, type='l', col='blue')
abline(h=1, col='red')
plot(df$date, as.integer(alpha_arr > 1), type='l', col='green')

