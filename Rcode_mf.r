#################### Set up ####################

# Load libraries

library(readxl)
library(tidyverse)
library(zoo)

library(RJDemetra) # seasonal adjustment software
library(tseries) # adf test
library(multDM) # dm test

library(vars)
library(Metrics)
library(midasr)
library(forecast)

library(mfbvar)
library(Rmisc)

# Data import

hpi <- read.csv('data/HPI.csv', encoding='UTF-8', sep=';')
dax <- read.csv('data/DAX.csv', encoding='utf-8', sep=',')
unrate <- read.csv('data/UNRATE.csv', encoding='UTF-8', sep=';')
rbp <- read.csv('data/RBP.csv', encoding='UTF-8', sep=';')
cpi <- read.csv('data/CPI.csv', encoding='UTF-8', sep=';')
hl <- read_excel('data/HL.xlsx')
din <- read.csv('data/DIN.csv', encoding='UTF-8', sep=';')

variables <- c('dax','unrate', 'rbp', 'cpi', 'hl', 'din')

# Helper functions

quarter <- function(data) {
  data <- mutate(data,
                 Quarter = ifelse(Quarter == 'Quarter 1', '03-01',
                                  ifelse(Quarter == 'Quarter 2', '06-01',
                                         ifelse(Quarter == 'Quarter 3', '09-01',
                                                ifelse(Quarter == 'Quarter 4', '12-01', NA_character_)))))
  return(data)
}

newcol <- function(data, start, freq) {

  ts <- ts(data = data, start = start, frequency = freq)
  log <- log(ts)
  dese <- x13(log)$final$series[,2]

  result <- c(NA, diff(dese))

  return(result)
}

#################### Data preprocessing ####################

# hpi

hpi_qdf <- hpi[,1:3]
colnames(hpi_qdf) <- c('Year', 'Quarter', 'hpi')



hpi_qdf <- quarter(hpi_qdf)
hpi_qdf$Date  <-  paste(hpi_qdf$Year,hpi_qdf$Quarter)
hpi_qdf$Date <- as.Date(hpi_qdf$Date, format = '%Y %m-%d')
hpi_qdf$yearqtr <- as.yearqtr(hpi_qdf$Date)

colnames(dax)
dax_day <- dax[,c('Date', 'Close')]
dax_day$Date <- as.Date(dax_day$Date, format='%Y-%m-%d')

dax_day <- dax_day %>% filter(Close != 'null') # null in non-business day
dax_day$Close <- as.numeric(as.character(dax_day$Close))
dax_day$yearmon <- as.yearmon(dax_day$Date)

# dax


dax_df <- dax_day %>%
  group_by(yearmon) %>%
  dplyr::summarise(dax = mean(Close))


dax_qdf <- dax[,c('Date', 'Close')]
dax_qdf$Date <- as.Date(dax_qdf$Date, format='%Y-%m-%d')

dax_qdf <- dax_qdf %>% filter(Close != 'null')

dax_qdf$yearqtr <- as.yearqtr(dax_qdf$Date)
dax_qdf$Close <- as.numeric(as.character(dax_qdf$Close))

dax_qdf <- dax_qdf %>%
  group_by(yearqtr) %>%
  dplyr::summarise(dax = mean(Close))

# unrate 

unrate_df <- unrate[193:nrow(unrate),c(1:3)] # monthly data
colnames(unrate_df) <- c('Year', 'Month', 'unrate')
unrate_df$mon <- match(unrate_df$Month, month.name)
unrate_df$yearmon <- paste(unrate_df$Year, unrate_df$mon)
unrate_df$yearmon <- as.yearmon(unrate_df$yearmon, '%Y %m')

unrate_df$unrate <- gsub(',', '.', unrate_df$unrate)
unrate_df$unrate <- as.numeric(unrate_df$unrate)


unrate_qdf <- unrate_df
unrate_qdf$yearqtr <- as.yearqtr(unrate_qdf$yearmon)

unrate_qdf <- unrate_qdf %>%
  group_by(yearqtr) %>%
  dplyr::summarise(unrate = mean(unrate))

# rbp

rbp_df <- rbp
colnames(rbp_df) <- c('Year', 'Month','rbp')

rbp_df$mon <- match(rbp_df$Month, month.name)
rbp_df$yearmon <- paste(rbp_df$Year, rbp_df$mon)
rbp_df$yearmon <- as.yearmon(rbp_df$yearmon, '%Y %m')


rbp_qdf <- rbp_df
rbp_qdf$yearqtr <- as.yearqtr(rbp_qdf$yearmon)

rbp_qdf <- rbp_qdf %>%
  group_by(yearqtr) %>%
  dplyr::summarise(rbp = mean(rbp))

# cpi

cpi_df <- cpi[,1:3]
colnames(cpi_df) <- c('Year', 'Month', 'cpi')

cpi_df$mon <- match(cpi_df$Month, month.name)
cpi_df$yearmon <- paste(cpi_df$Year, cpi_df$mon)
cpi_df$yearmon <- as.yearmon(cpi_df$yearmon, '%Y %m')


cpi_qdf <- cpi_df
cpi_qdf$yearqtr <- as.yearqtr(cpi_qdf$yearmon)

cpi_qdf <- cpi_qdf %>%
  group_by(yearqtr) %>%
  dplyr::summarise(cpi = mean(cpi))

# hl

hl_df <- hl[order(as.Date(hl$Date, format = '%Y-%m-%d')),]
colnames(hl_df) <- c('Date', 'hl')
hl_df$yearmon <- as.yearmon(hl_df$Date)


hl_qdf <- hl_df
hl_qdf$yearqtr <- as.yearqtr(hl_df$Date)

hl_qdf <- hl_qdf %>%
  group_by(yearqtr) %>%
  dplyr::summarise(hl = mean(hl))

# din

din_qdf <- din[,c(1,2,4)]
colnames(din_qdf) <- c('Year', 'Quarter', 'din')


din_qdf <- quarter(din_qdf)
din_qdf$Date <- paste(din_qdf$Year, din_qdf$Quarter)
din_qdf$Date  <- as.Date(din_qdf$Date, format = '%Y %m-%d')
din_qdf$yearqtr <- as.yearqtr(din_qdf$Date)


Date <- seq(as.Date('1991-03-01'), as.Date('2023-12-01'), by = '1 month')
m_din <- data.frame(Date, rep(NA, length(Date)), rep(NA, length(Date)))
colnames(m_din) <- c('Date', 'din')

qq <- zoo(din_qdf$din, din_qdf$Date)
mm <- zoo(m_din$din, m_din$Date)
din_df <- merge(qq,mm)
din_df$din <- na.approx(din_df$qq)
din_df <- fortify.zoo(din_df)

din_df <- data.frame(yearmon = as.yearmon(din_df$Index),
                     din = din_df$din)


#################### Creating dataset ####################

# Dataset for monthly series

start_m <- 'Jan 2007' # start date
end_m <- 'Dec 2023' # end date

df <- data.frame(dax_df %>%
                   filter(yearmon >= start_m & yearmon <= end_m) %>%
                   dplyr::select(yearmon),
                 dax_df %>%
                   filter(yearmon >= start_m & yearmon <= end_m) %>%
                   dplyr::select(dax),
                 unrate_df %>%
                   filter(yearmon >= start_m & yearmon <= end_m) %>%
                   dplyr::select(unrate),
                 rbp_df %>%
                   filter(yearmon >= start_m & yearmon <= end_m) %>%
                   dplyr::select(rbp),
                 cpi_df %>%
                   filter(yearmon >= start_m & yearmon <= end_m) %>%
                   dplyr::select(cpi),
                 hl_df %>%
                   filter(yearmon >= start_m & yearmon <= end_m) %>%
                   dplyr::select(hl),
                 din_df %>%
                   filter(yearmon >= start_m & yearmon <= end_m) %>%
                   dplyr::select(din)
)

colnames(df) <- c('yearmon', 'dax', 'unrate', 'rbp', 'cpi',
                  'hl', 'din')

# Log-difference transformation for monthly seires

m_data <- data.frame(matrix(ncol = ncol(df), nrow = nrow(df))) # empty dataframe
colnames(m_data) <- names(df)
m_data[,1] <- df[,1] # yearmon

for (i in 3:ncol(m_data)) { # unrate, rbp, cpi, hl, din
  m_data[,i] <- newcol(df[,i],
                       start = min(df$yearmon),
                       freq = 12)
} # log-differencing


m_data$dax <- c(NA, diff(log(df$dax))) # Log-difference transformation for dax
m_data <- m_data[-c(1:3),] # remove observations before April 2007

# adf test for monthly series

ers.gnp <- ur.ers(m_data$dax, type="DF-GLS", model="const", lag.max=10)
summary(ers.gnp) # https://www.econometrics-with-r.org/16.2-ooiatdfglsurt.html

# Log-difference transformation for quarterly seires

start_q <- '2007 Q1' 
end_q <- '2023 Q4' 

qdf <- data.frame(hpi_qdf %>% # transform sereis into quarterly
                    filter(yearqtr >= start_q & yearqtr <= end_q) %>%
                    dplyr::select(yearqtr),
                  hpi_qdf %>%
                    filter(yearqtr >= start_q & yearqtr <= end_q) %>%
                    dplyr::select(hpi),
                  dax_qdf %>%
                    filter(yearqtr >= start_q & yearqtr <= end_q) %>%
                    dplyr::select(dax),
                  unrate_qdf %>%
                    filter(yearqtr >= start_q & yearqtr <= end_q) %>%
                    dplyr::select(unrate),
                  rbp_qdf %>%
                    filter(yearqtr >= start_q & yearqtr <= end_q) %>%
                    dplyr::select(rbp),
                  cpi_qdf %>%
                    filter(yearqtr >= start_q & yearqtr <= end_q) %>%
                    dplyr::select(cpi),
                  hl_qdf %>%
                    filter(yearqtr >= start_q & yearqtr <= end_q) %>%
                    dplyr::select(hl),
                  din_qdf %>%
                    filter(yearqtr >= start_q & yearqtr <= end_q) %>%
                    dplyr::select(din)
)

colnames(qdf) <- c('yearqtr', 'hpi', 'dax', 'unrate', 'rbp', 'cpi',
                   'hl', 'din')


q_data <- data.frame(matrix(ncol = ncol(qdf), nrow = nrow(qdf))) # empty dataframe
colnames(q_data) <- names(qdf)
q_data[,1] <- qdf[,1] # yearqtr

for (i in 2:ncol(q_data)) { # hpi, dax, unrate, rbp, cpi, hl, din
  q_data[,i] <- newcol(qdf[,i],
                       start = min(qdf$yearqtr),
                       freq = 4)
} # log-differencing

head(q_data)
q_data <- q_data[-1,]

# adf test for quarterly series


ers.gnp <- ur.ers(q_data$hl, type="DF-GLS", model="const", lag.max=4)
summary(ers.gnp)

#################### setting periods ####################

pub_lag <- data.frame(dax = 0,
                      unrate = 2,
                      rbp = 2,
                      cpi = 1,
                      hl = 2,
                      din = 3) # publication lag

window <- 43 # 2007 Q1 to 2017 Q4
out_size <- nrow(q_data) - window # out-of-sample size
in_per <- 1:window #
out_per <- (1:nrow(q_data))[-in_per]

recursive <- window:(nrow(q_data)-1) # recursive forecast period
horizon <- c(0, 1, 2, 3, 4, 8) # forecast horizon (quarters)

# evaluation points
eval_start <- 44
eval_end1 <- 52 # 2020 Q1
eval_end2 <- 67 # 2023 Q4

#################### Individual models ####################

# AR forecasting

ar_fore <- matrix(NA, nrow = 100, ncol = length(horizon))
ar_mse <- rep(NA, length(horizon))

for (i in recursive) {
  best_lag <- NULL
  min_bic <- Inf

  for (lag in 1:4) {
    ar_model <- arima(q_data$hpi[1:i], order = c(lag, 0, 0))
    current_bic <- BIC(ar_model)

    # Update minimum BIC and best lag if needed
    if (current_bic < min_bic) {
      min_bic <- current_bic
      best_lag <- lag
    }
  }


  ar_model <- arima(q_data$hpi[1:i], order = c(best_lag, 0, 0)) 

  for (j in seq_along(horizon)) {
    fore <- predict(ar_model, n.ahead = 9)$pred
    ar_fore[((i+1) + horizon[j]), j] <- fore[horizon[j]+1]
  }
}

# VAR forecasting

var_forecasts <- lapply(variables, function(var) matrix(NA, nrow = 100,
                                                           ncol = length(horizon)))
names(var_forecasts) <- variables

var_mse <- matrix(NA, nrow = length(horizon), ncol = length(variables))
colnames(var_mse) <- variables

for (var in variables) {
  for (i in recursive) {

    var_data <- q_data[1:i, c('hpi', var)]

    best_lag <- VARselect(var_data, lag.max = 4)$selection[3]
    var_model <- VAR(var_data, p = best_lag, type = 'const')

    for (j in seq_along(horizon)) {

      
      fore <- predict(var_model, n.ahead = 9)
      var_forecasts[[var]][((i+1)+horizon[j]), j] <- fore$fcst$hpi[horizon[j]+1]
  }
}}

# UMIDAS forecasting

umidas_forecasts <- lapply(variables, function(var) matrix(NA, nrow = 100,
                                                              ncol = length(horizon)))
names(umidas_forecasts) <- variables

umidas_mse <- matrix(NA, nrow = length(horizon), ncol = length(variables))
colnames(umidas_mse) <- variables

ar_lag <- seq(from = 1, to = 4, by = 1)


for (var in variables) {

  lag_seq <- seq(from = pub_lag[,var], to = mon_max, by = 1)
  lag_com <- expand.grid(ar_lag, lag_seq)

  for (i in recursive) {
    best_ar <- NULL
    best_lag <- NULL
    min_bic <- Inf

    for (idx in 1:nrow(lag_com)) {

      ar <- lag_com[idx,1]
      lag <- lag_com[idx,2]

      model <- midas_r(hpi ~ mls(hpi, k = 1:ar, m = 1) +
                         mls(variable, k = pub_lag[,var]:lag, m = mon_ratio),
                       start = NULL,
                     # if `start = NULL`, `lm()` is used for estimatingw the model 
                       data = list(hpi = q_data$hpi[1:i],
                                   variable = m_data[1:(mon_ratio*i),var]))
      current_bic <- BIC(model)

      if (current_bic < min_bic) {
        min_bic <- current_bic
        best_ar <- ar
        best_lag <- lag
      }
    }

    model <- midas_r(hpi ~ mls(hpi, k = 1:best_ar, m = 1) +
                       mls(variable, k = pub_lag[,var]:best_lag, m = mon_ratio),
                     start = NULL,
                     data = list(hpi = q_data$hpi[1:i],
                                 variable = m_data[1:(mon_ratio*i),var]))

    for (j in seq_along(horizon)) {
      fore <- average_forecast(list(model),
                               data = list(hpi = q_data[1:(i+9), 'hpi'],
                                           variable = m_data[1:((i+9)*mon_ratio), var]),
                               insample = 1:i, outsample = (i+1):(i+9),
                               type = 'fixed')
      umidas_forecasts[[var]][((i+1) + horizon[j]), j] <- fore$forecast[horizon[j]+1]

    }
  }
}

# MIDAS forecasting

## MIDAS set up

dax_dd <- dax_day
dax_dd$dax <- c(NA, diff(log(dax_dd$Close)))
dax_dd <- dax_dd %>%
  filter(yearmon >= 'Apr 2007'& yearmon <= 'Dec 2023')

mon_max <- 12
day_max <- 252

mon_ratio <- 3
day_ratio <- 63

## finding starting values

start1 <- c(-0.5,0,0.5)
start2 <- c(-0.01,-0.1,0,-0.5,-1)

start_set <- expand.grid(start1, start2)
midas_rss <- matrix(NA, nrow = nrow(start_set), ncol = length(variables))
colnames(midas_rss) <- variables


for (var in variables) {

  if (var == 'dax') {
    lag_max <- day_max
    ratio <- day_ratio
    data <- dax_dd

  } else {
    lag_max <- mon_max
    ratio <- mon_ratio
    data <- m_data
  }

  for (start in seq_len(nrow(start_set))) {

    best_model <- NULL
    best_lag <- NULL

    set <- expand_weights_lags(
      weights = c('nealmon'),
      from = pub_lag[,var], to = c(pub_lag[,var],lag_max),
      m = 1,
      start = list(nealmon = start_set[start,])
    )

    ic <- midas_r_ic_table(hpi ~ mls(variable, 0, ratio),
                           table = list(variable = set),
                           data = list(hpi = q_data[1:window, 'hpi'],
                                       variable = data[1:(window*ratio), var]))

    best_model <- modsel(ic, IC = 'BIC', type = 'restricted', print = FALSE)
    best_lag <- best_model$term_info$variable$lag_structure

    model <- midas_r(hpi ~ mls(variable, best_lag, ratio, nealmon),
                     start = list(variable = start_set[start,]),
                     data = list(hpi = q_data[1:window, 'hpi'],
                                 variable = data[1:(window*ratio), var]))

    midas_rss[start,var] <- sum((model$lhs-model$fitted.values)^2)


  }}


## forecasting and evaluation

midas_forecasts <- lapply(variables, function(var) matrix(NA, nrow = 100,
                                                          ncol = length(horizon)))
names(midas_forecasts) <- variables

midas_mse <- matrix(NA, nrow = length(horizon), ncol = length(variables))
colnames(midas_mse) <- variables

for (var in variables) {

  if (var == 'dax') {
    lag_max <- day_max
    ratio <- day_ratio
    data <- dax_dd
  } else {
    lag_max <- mon_max
    ratio <- mon_ratio
    data <- m_data
  }
  for (i in recursive) {

    starting_values <- start_set[which.min(midas_rss[,var]),]

    best_model <- NULL
    best_lag <- NULL

    set <- expand_weights_lags(
      weights = c('nealmon'),
      from = pub_lag[,var], to = c(pub_lag[,var], lag_max),
      m = 1, # by setting m = 1, the set of potential models is defined as all possible different combinations of functions and lag structures with a corresponding set of starting values.
      start = list(nealmon = starting_values)
    )

    ic <- midas_r_ic_table(hpi ~ mls(variable, 0, ratio),
                           table = list(variable = set),
                           data = list(hpi = q_data[1:i, 'hpi'],
                                       variable = data[1:(i*ratio), var]))


    best_model <- modsel(ic, IC = 'BIC', type = 'restricted', print = FALSE)
    best_lag <- best_model$term_info$variable$lag_structure

    model <- midas_r(hpi ~ mls(variable, best_lag, ratio, nealmon),
                     start = list(variable = starting_values),
                     # the number of parameters more than 2 -> opt problem
                     data = list(hpi = q_data[1:i, 'hpi'],
                                 variable = data[1:(i*ratio), var])
                     )

    for (j in seq_along(horizon)) {

      fore <- average_forecast(list(model),
                               data = list(hpi = q_data[1:(i+9), 'hpi'],
                                           variable = data[1:((i+9)*ratio), var]),
                               insample = 1:i, outsample = (i+1):(i+9),
                               type = 'fixed')

      midas_forecasts[[var]][((i+1) + horizon[j]), j] <- fore$forecast[horizon[j]+1]
      
    }
  }
}

# AR-MIDAS forecasting
## finding starting values

start1 <- c(-0.5,0,0.5)
start2 <- c(-0.01,-0.1,0,-0.5,-1)

start_set <- expand.grid(start1, start2)
ar_midas_rss <- matrix(NA, nrow = nrow(start_set), ncol = length(variables))
colnames(ar_midas_rss) <- variables


for (var in variables) {
  
  if (var == 'dax') {
    lag_max <- day_max
    ratio <- day_ratio
    data <- dax_dd
    
  } else {
    lag_max <- mon_max
    ratio <- mon_ratio
    data <- m_data
  }
  
  for (start in seq_len(nrow(start_set))) {
    
    best_model <- NULL
    best_lag <- NULL
    
    set <- expand_weights_lags(
      weights = c('nealmon'),
      from = pub_lag[,var], to = c(pub_lag[,var],lag_max),
      m = 1,
      start = list(nealmon = start_set[start,])
    )
    
    ic <- midas_r_ic_table(hpi ~ mls(hpi, 1:1, 1, '*') + 
                             mls(variable, 0, ratio),
                           table = list(variable = set),
                           data = list(hpi = q_data[1:window, 'hpi'],
                                       variable = data[1:(window*ratio), var]))
    
    best_model <- modsel(ic, IC = 'BIC', type = 'restricted', print = FALSE)
    best_lag <- best_model$term_info$variable$lag_structure
    
    model <- midas_r(hpi ~ mls(hpi, 1:1, 1, '*') + 
                       mls(variable, best_lag, ratio, nealmon),
                     start = list(variable = start_set[start,]),
                     data = list(hpi = q_data[1:window, 'hpi'],
                                 variable = data[1:(window*ratio), var]))
    
    ar_midas_rss[start,var] <- sum((model$lhs-model$fitted.values)^2)
    
    
  }}


## forecasting and evaluation

ar_midas_forecasts <- lapply(variables, function(var) matrix(NA, nrow = 100,
                                                             ncol = length(horizon)))
names(ar_midas_forecasts) <- variables

ar_midas_mse <- matrix(NA, nrow = length(horizon), ncol = length(variables))
colnames(ar_midas_mse) <- variables

starting_values <- start_set[which.min(ar_midas_rss[,var]),]

for (var in variables) {
  
  if (var == 'dax') {
    lag_max <- day_max
    ratio <- day_ratio
    data <- dax_dd
  } else {
    lag_max <- mon_max
    ratio <- mon_ratio
    data <- m_data
  }
  
  for (i in recursive) {
    
    
    best_model <- NULL
    best_lag <- NULL
    
    set <- expand_weights_lags(
      weights = c('nealmon'),
      from = pub_lag[,var], to = c(pub_lag[,var], lag_max),
      m = 1, # by setting m = 1, the set of potential models is defined as all possible different combinations of functions and lag structures with a corresponding set of starting values.
      start = list(nealmon = starting_values)
    )
    
    ic <- midas_r_ic_table(hpi ~ mls(hpi, 1:1, 1, '*') +
                             mls(variable, 0, ratio),
                           table = list(variable = set),
                           data = list(hpi = q_data[1:i, 'hpi'],
                                       variable = data[1:(i*ratio), var]))
    
    
    best_model <- modsel(ic, IC = 'BIC', type = 'restricted', print = FALSE)
    best_lag <- best_model$term_info$variable$lag_structure
    
    model <- midas_r(hpi ~ mls(hpi, 1:1, 1, '*') +
                       mls(variable, best_lag, ratio, nealmon),
                     start = list(variable = starting_values),
                     # the number of parameters more than 2 -> opt problem
                     data = list(hpi = q_data[1:i, 'hpi'],
                                 variable = data[1:(i*ratio), var])
    )
    
    for (j in seq_along(horizon)) {
      
      fore <- average_forecast(list(model),
                               data = list(hpi = q_data[1:(i+9), 'hpi'],
                                           variable = data[1:((i+9)*ratio), var]),
                               insample = 1:i, outsample = (i+1):(i+9),
                               type = 'fixed')
      
      ar_midas_forecasts[[var]][((i+1) + horizon[j]), j] <- fore$forecast[horizon[j]+1]
      
    }
  }
}



# MFBVAR forecasting

## MFBVAR set up

mfbvar_forecasts <- lapply(variables, function(var) matrix(NA, nrow = 100,
                                                           ncol = length(horizon)))
names(mfbvar_forecasts) <- variables

mfbvar_mse <- matrix(NA, nrow = length(horizon), ncol = length(variables))
colnames(mfbvar_mse) <- variables

lamb1 <- c(0.01, 0.1, 0.5, 0.9)
lamb2 <- c(0.1,0.5,1,2,4)
lamb_com <- expand.grid(lamb1, lamb2)




## forecasting and evaluation



for (var in variables) {
  
  
  
  full_list <- list()
  full_list[[var]] <- ts(m_data[,var],
                         start = c(2007, 6),
                         frequency = 12)
  full_list[['hpi']] <- ts(q_data[,'hpi'],
                           start = c(2007,2),
                           frequency = 4)
  
  c_interval <- t(sapply(full_list, CI, ci=0.95))
  prior_intervals <- c_interval[,c('upper','lower')]
  moments <- interval_to_moments(prior_intervals)
  
  for (i in recursive) {
    
    min_mse <- Inf
    best_lambda <- NULL
    
    mflist <- list()
    mflist[[var]] <- ts(m_data[1:((i+1)*3-pub_lag[,var]),var],
                        start = c(2007, 6),
                        frequency = 12)
    mflist[['hpi']] <- ts(q_data[1:i,'hpi'],
                          start = c(2007,2),
                          frequency = 4)
    
    for (lamb in 1:nrow(lamb_com)) {
      
      prior <- set_prior(Y = mflist, n_lags = 4, n_reps = 15000,
                         lambda1 = lamb_com[lamb,1],
                         lambda3 = lamb_com[lamb,2],
                         # the aggregation scheme, I use the intra-quarterly average here
                         aggregation = "average",
                         d = 'intercept',
                         prior_psi_mean = moments$prior_psi_mean,
                         prior_psi_Omega = moments$prior_psi_Omega,
                         n_fcst = 27,
                         prior_Pi_AR1 = 0.9) 
      
      
      model <- estimate_mfbvar(prior, prior = 'ss', variance = 'iw')
      
      fore <- predict(model, pred_bands = NULL) %>%
        filter(variable == 'hpi') %>%
        group_by(fcst_date) %>%
        dplyr::summarise(fore_mean = mean(fcst))
      
      current_mse <- mse(q_data$hpi[i+1], fore$fore_mean[1])
      
      if (current_mse < min_mse) {
        min_mse <- current_mse
        best_lambda <- lamb
      }
    }
    
    final_prior <- update_prior(prior,
                                lambda1 = lamb_com[best_lambda, 1],
                                lambda2 = lamb_com[best_lambda, 2])
    final_model <- estimate_mfbvar(final_prior, prior = 'ss', variance = 'iw')
    fore <- predict(final_model, pred_bands = NULL) %>%
      filter(variable == 'hpi') %>%
      group_by(fcst_date) %>%
      dplyr::summarise(fore_mean = mean(fcst))
    
    for (j in seq_along(horizon)) {
      
      mfbvar_forecasts[[var]][((i+1) + horizon[j]), j] <- fore$fore_mean[horizon[j]+1]
      
    }
  }
}

# Results for individual models

h <- c(0,1,2,3,4,8)
nam <- c(rep('var',6),rep('mfbvar',6),rep('umidas',6),
         rep('midas',6),rep('ar-midas',6))


before_list <- lapply(variables, function(var) {
  mat <- data.frame(h = h, model = nam, msfe = rep(NA, 30))
}) # list of the relative MSFEs for the first evaluation sample
after_list <- lapply(variables, function(var) {
  mat <- data.frame(h = h, model = nam, msfe = rep(NA, 30))
}) # list of the relative MSFEs for the second evaluation sample
names(before_list) <- variables 
names(after_list) <- variables 


var_mse <- matrix(NA, nrow = length(horizon), ncol = length(variables))
umidas_mse <- matrix(NA, nrow = length(horizon), ncol = length(variables))
midas_mse <- matrix(NA, nrow = length(horizon), ncol = length(variables))
ar_midas_mse <- matrix(NA, nrow = length(horizon), ncol = length(variables))
mfbvar_mse <- matrix(NA, nrow = length(horizon), ncol = length(variables))
colnames(var_mse) <- variables
colnames(umidas_mse) <- variables
colnames(midas_mse) <- variables
colnames(ar_midas_mse) <- variables
colnames(mfbvar_mse) <- variables

end_point <- c(52,67)
end <- 1
  
for (var in variables) {
    
    for (j in seq_along(horizon)) {
      ar_mse[j] <- mse(q_data$hpi[(eval_start + horizon[j]):end_point[end]],
                       ar_fore[(eval_start + horizon[j]):end_point[end], j])
      var_mse[j, var] <- mse(q_data$hpi[(eval_start + horizon[j]):end_point[end]],
                             var_forecasts[[var]][(eval_start + horizon[j]):end_point[end], j])
      umidas_mse[j, var] <- mse(q_data$hpi[(eval_start + horizon[j]):end_point[end]],
                                umidas_forecasts[[var]][(eval_start + horizon[j]):end_point[end], j])
      midas_mse[j, var] <- mse(q_data$hpi[(eval_start + horizon[j]):end_point[end]],
                               midas_forecasts[[var]][(eval_start + horizon[j]):end_point[end], j])
      ar_midas_mse[j, var] <- mse(q_data$hpi[(eval_start + horizon[j]):end_point[end]],
                                  ar_midas_forecasts[[var]][(eval_start + horizon[j]):end_point[end], j])
      mfbvar_mse[j, var] <- mse(q_data$hpi[(eval_start + horizon[j]):end_point[end]],
                                mfbvar_forecasts[[var]][(eval_start + horizon[j]):end_point[end], j])
    }
    before_list[[var]][,'msfe'] <- c(round(var_mse[,var]/ar_mse,2), 
                                round(mfbvar_mse[,var]/ar_mse,2),
                                round(umidas_mse[,var]/ar_mse,2),
                                round(midas_mse[,var]/ar_mse,2),
                                round(ar_midas_mse[,var]/ar_mse,2))
  }
end <- 2

for (var in variables) {
  
  for (j in seq_along(horizon)) {
    ar_mse[j] <- mse(q_data$hpi[(eval_start + horizon[j]):end_point[end]],
                     ar_fore[(eval_start + horizon[j]):end_point[end], j])
    var_mse[j, var] <- mse(q_data$hpi[(eval_start + horizon[j]):end_point[end]],
                           var_forecasts[[var]][(eval_start + horizon[j]):end_point[end], j])
    umidas_mse[j, var] <- mse(q_data$hpi[(eval_start + horizon[j]):end_point[end]],
                              umidas_forecasts[[var]][(eval_start + horizon[j]):end_point[end], j])
    midas_mse[j, var] <- mse(q_data$hpi[(eval_start + horizon[j]):end_point[end]],
                             midas_forecasts[[var]][(eval_start + horizon[j]):end_point[end], j])
    ar_midas_mse[j, var] <- mse(q_data$hpi[(eval_start + horizon[j]):end_point[end]],
                                ar_midas_forecasts[[var]][(eval_start + horizon[j]):end_point[end], j])
    mfbvar_mse[j, var] <- mse(q_data$hpi[(eval_start + horizon[j]):end_point[end]],
                              mfbvar_forecasts[[var]][(eval_start + horizon[j]):end_point[end], j])
  }
  after_list[[var]][,'msfe'] <- c(round(var_mse[,var]/ar_mse,2), 
                                   round(mfbvar_mse[,var]/ar_mse,2),
                                   round(umidas_mse[,var]/ar_mse,2),
                                   round(midas_mse[,var]/ar_mse,2),
                                   round(ar_midas_mse[,var]/ar_mse,2))
}

# dm test

dm <- matrix(NA, nrow = 6, ncol = 6) 
colnames(dm) <- variables

end <- eval_end2

for (j in 1:6) {
  for (var in variables) {
    
    test <- dm.test(q_data$hpi[(44+horizon[j]):end]-
                                umidas_forecasts[[var]][(44+horizon[j]):end,j],
                              q_data$hpi[(44+horizon[j]):end]-
                                ar_midas_forecasts[[var]][(44+horizon[j]):end,j],
                              h = horizon[j]+1,
                              alternative = 'two.sided',power = 2)
    
    dm[j,var] <- test$p.value
    
    
    
  }
}

t(round(dm,3))

#################### combined predictor models ####################

# VAR forecasting

var_all_forecasts <- matrix(NA, nrow = 100, ncol = length(horizon))



for (i in recursive) {

    var_data <- q_data[1:i, -1]

    best_lag <- VARselect(var_data, lag.max = 4)$selection[3]
    var_model <- VAR(var_data, p = best_lag, type = 'const')


    for (j in seq_along(horizon)) {
      fore <- predict(var_model, n.ahead = 9)
      var_all_forecasts[((i+1)+horizon[j]), j] <- fore$fcst$hpi[horizon[j]+1]

    }
    
}

# UMIDAS forecasting

## if to > 5, Error in midas_r.fit(prepmd) : 
## Not possible to estimate MIDAS model (U-MIDAS), more parameters than observations
lag_com <- expand.grid(seq(from = 1, to = 4, by = 1),
                       seq(from = pub_lag[,1], to = 5, by = 1),
                       seq(from = pub_lag[,2], to = 5, by = 1),
                       seq(from = pub_lag[,3], to = 5, by = 1),
                       seq(from = pub_lag[,4], to = 5, by = 1),
                       seq(from = pub_lag[,5], to = 5, by = 1),
                       seq(from = pub_lag[,6], to = 5, by = 1))

umidas_all_forecasts <- matrix(NA, nrow = 100, ncol = length(horizon))
names(umidas_all_forecasts) <- variables


  
  for (i in recursive) {
    
    best_lag <- NULL
    min_bic <- Inf
    
    for (lag in 1:nrow(lag_com)) {
      
      model <- midas_r(hpi ~ mls(hpi, k = 1:lag_com[lag,1], m = 1) +
                         mls(dax, k = pub_lag[,'dax']:lag_com[lag,2], m = 3) +
                         mls(unrate, k = pub_lag[,'unrate']:lag_com[lag,3], m = 3) +
                         mls(rbp, k = pub_lag[,'rbp']:lag_com[lag,4], m = 3) +
                         mls(cpi, k = pub_lag[,'cpi']:lag_com[lag,5], m = 3) +
                         mls(hl, k = pub_lag[,'hl']:lag_com[lag,6], m = 3) +
                         mls(din, k = pub_lag[,'din']:lag_com[lag,7], m = 3),
                       start = NULL,
                      # if `start = NULL`, `lm()` is used for estimating the model 
                       data = list(hpi = q_data$hpi[1:i],
                                   dax = m_data[1:(mon_ratio*i),'dax'],
                                   unrate = m_data[1:(mon_ratio*i),'unrate'],
                                   rbp = m_data[1:(mon_ratio*i),'rbp'],
                                   cpi = m_data[1:(mon_ratio*i),'cpi'],
                                   hl = m_data[1:(mon_ratio*i),'hl'],
                                   din = m_data[1:(mon_ratio*i),'din']))
      current_bic <- BIC(model)
      
      if (current_bic < min_bic) {
        min_bic <- current_bic
        best_lag <- lag 
      }
    }
    
    
    
    model <- midas_r(hpi ~ mls(hpi, k = 1:lag_com[best_lag, 1], m = 1) +
                       mls(dax, k = 0:lag_com[best_lag,2], m = 3) +
                       mls(unrate, k = 2:lag_com[best_lag,3], m = 3) +
                       mls(rbp, k = 2:lag_com[best_lag,4], m = 3) +
                       mls(cpi, k = 1:lag_com[best_lag,5], m = 3) +
                       mls(hl, k = 2:lag_com[best_lag,6], m = 3) +
                       mls(din, k = 3:lag_com[best_lag,7], m = 3),
                     start = NULL,
                     data = list(hpi = q_data$hpi[1:i],
                                 dax = m_data[1:(mon_ratio*i),'dax'],
                                 unrate = m_data[1:(mon_ratio*i),'unrate'],
                                 rbp = m_data[1:(mon_ratio*i),'rbp'],
                                 cpi = m_data[1:(mon_ratio*i),'cpi'],
                                 hl = m_data[1:(mon_ratio*i),'hl'],
                                 din = m_data[1:(mon_ratio*i),'din']))
    
    for (j in seq_along(horizon)) {
      
      fore <- average_forecast(list(model),
                               data = list(hpi = q_data[1:(i+9), 'hpi'],
                                           dax = m_data[1:((i+9)*mon_ratio), 'dax'],
                                           unrate = m_data[1:((i+9)*mon_ratio), 'unrate'],
                                           rbp = m_data[1:((i+9)*mon_ratio), 'rbp'],
                                           cpi = m_data[1:((i+9)*mon_ratio), 'cpi'],
                                           hl = m_data[1:((i+9)*mon_ratio), 'hl'],
                                           din = m_data[1:((i+9)*mon_ratio), 'din']),
                               insample = 1:i, outsample = (i+1):(i+9),
                               type = 'fixed')
      
      umidas_all_forecasts[((i+1) + horizon[j]), j] <- fore$forecast[horizon[j]+1]
   
    }
  }

# MIDAS forecasting


## finding starting values


day_set <- c(105, 168, 231)
mon_set <- c(5,8,11)

lag_com1 <-  expand.grid(day_set,mon_set)
lag_com2 <- expand.grid(day_set,mon_set,mon_set,mon_set,mon_set,mon_set)



start1 <- 0
start2 <- c(-0.01,0,-0.5)
start_set <- expand.grid(start1, start2,start1, start2, start1, start2, 
                         start1, start2,start1, start2, start1, start2)


midas_all_rss <- matrix(NA, nrow = nrow(lag_com), ncol = nrow(start_set))

for (lag in seq_len(nrow(lag_com1))) {
  for (start in seq_len(nrow(start_set))) {
    model <- midas_r(hpi ~ mls(dax, 0:lag_com1[lag,1], day_ratio, nealmon) +
                       mls(unrate, 2:lag_com1[lag,2], mon_ratio, nealmon) +
                       mls(rbp, 2:lag_com1[lag,2], mon_ratio, nealmon) +
                       mls(cpi, 1:lag_com1[lag,2], mon_ratio, nealmon) +
                       mls(hl, 2:lag_com1[lag,2], mon_ratio, nealmon) +
                       mls(din, 3:lag_com1[lag,2], mon_ratio, nealmon),
                     start = list(dax = start_set[start,c(1,2)],
                                  unrate = start_set[start,c(3,4)],
                                  rbp = start_set[start,c(5,6)],
                                  cpi = start_set[start,c(7,8)],
                                  hl = start_set[start,c(9,10)],
                                  din = start_set[start,c(11,12)]),
                     data = list(hpi = q_data[1:window, 'hpi'],
                                 dax = dax_dd[1:(window*day_ratio), 'dax'],
                                 unrate = m_data[1:(window*mon_ratio), 'unrate'],
                                 rbp = m_data[1:(window*mon_ratio), 'rbp'],
                                 cpi = m_data[1:(window*mon_ratio), 'cpi'],
                                 hl = m_data[1:(window*mon_ratio), 'hl'],
                                 din = m_data[1:(window*mon_ratio), 'din']))
    
    midas_all_rss[lag,start] <- sum((model$lhs-model$fitted.values)^2)
  }
}
  

min_rss <- which(midas_all_rss[seq_len(nrow(lag_com1)),] == min(midas_all_rss[seq_len(nrow(lag_com1)),]), arr.ind = TRUE)


start_dax <- start_set[min_rss[,2],c(1,2)]
start_unrate <-start_set[min_rss[,2],c(3,4)]
start_rbp <- start_set[min_rss[,2],c(5,6)]
start_cpi <- start_set[min_rss[,2],c(7,8)]
start_hl <- start_set[min_rss[,2],c(9,10)]
start_din <- start_set[min_rss[,2],c(11,12)]


## forecasting and evaluation

midas_all_fore <- matrix(NA, nrow = 100,
                         ncol = length(horizon))

for (i in recursive) {

  best_lag <- NULL
  min_bic <- Inf
  
  for (lag in 1:nrow(lag_com2)) {

    model <- midas_r(hpi ~ mls(dax, 0:lag_com2[lag,1], day_ratio, nealmon) +
                       mls(unrate, 2:lag_com2[lag,2], mon_ratio, nealmon) +
                       mls(rbp, 2:lag_com2[lag,3], mon_ratio, nealmon) +
                       mls(cpi, 1:lag_com2[lag,4], mon_ratio, nealmon) +
                       mls(hl, 2:lag_com2[lag,5], mon_ratio, nealmon) +
                       mls(din, 3:lag_com2[lag,6], mon_ratio, nealmon),
                     start = list(dax = start_dax,
                                  unrate = start_unrate,
                                  rbp = start_rbp,
                                  cpi = start_cpi,
                                  hl = start_hl,
                                  din = start_din),
                     data = list(hpi = q_data[1:i, 'hpi'],
                                 dax = dax_dd[1:(i*day_ratio), 'dax'],
                                 unrate = m_data[1:(i*mon_ratio), 'unrate'],
                                 rbp = m_data[1:(i*mon_ratio), 'rbp'],
                                 cpi = m_data[1:(i*mon_ratio), 'cpi'],
                                 hl = m_data[1:(i*mon_ratio), 'hl'],
                                 din = m_data[1:(i*mon_ratio), 'din']))
    
    current_bic <- BIC(model)
    
    if (current_bic < min_bic) {
      min_bic <- current_bic
      best_lag <- lag
    }
  }
  
  l_dax <- lag_com2[best_lag,1]
  l_unrate <- lag_com2[best_lag,2]
  l_rbp <- lag_com2[best_lag,3]
  l_cpi <- lag_com2[best_lag,4]
  l_hl<- lag_com2[best_lag,5]
  l_din <- lag_com2[best_lag,6]
  


  model <- midas_r(hpi ~ mls(dax, 0:l_dax, day_ratio, nealmon) +
                     mls(unrate, 2:l_unrate, mon_ratio, nealmon) +
                     mls(rbp, 2:l_rbp, mon_ratio, nealmon) +
                     mls(cpi, 1:l_cpi, mon_ratio, nealmon) +
                     mls(hl, 2:l_hl, mon_ratio, nealmon) +
                     mls(din, 3:l_din, mon_ratio, nealmon),
                   start = list(dax = start_dax,
                                unrate = start_unrate,
                                rbp = start_rbp,
                                cpi = start_cpi,
                                hl = start_hl,
                                din = start_din),
                   data = list(hpi = q_data[1:i, 'hpi'],
                               dax = dax_dd[1:(i*day_ratio), 'dax'],
                               unrate = m_data[1:(i*mon_ratio), 'unrate'],
                               rbp = m_data[1:(i*mon_ratio), 'rbp'],
                               cpi = m_data[1:(i*mon_ratio), 'cpi'],
                               hl = m_data[1:(i*mon_ratio), 'hl'],
                               din = m_data[1:(i*mon_ratio), 'din']))

  for (j in seq_along(horizon)) {

    fore <- average_forecast(list(model),
                             data = list(hpi = q_data[1:(i+9), 'hpi'],
                                         dax = dax_dd[1:((i+9)*day_ratio), 'dax'],
                                         unrate = m_data[1:((i+9)*mon_ratio), 'unrate'],
                                         rbp = m_data[1:((i+9)*mon_ratio), 'rbp'],
                                         cpi = m_data[1:((i+9)*mon_ratio), 'cpi'],
                                         hl = m_data[1:((i+9)*mon_ratio), 'hl'],
                                         din = m_data[1:((i+9)*mon_ratio), 'din']),

                             insample = 1:i, outsample = (i+1):(i+9),
                             type = 'fixed')

    midas_all_fore[((i+1) + horizon[j]), j] <- fore$forecast[horizon[j]+1]
    
  }}


# AR-MIDAS forecasting

## finding starting values



ar_midas_all_rss <- matrix(NA, nrow = nrow(lag_com1), ncol = nrow(start_set))

for (lag in seq_len(nrow(lag_com1))) {
  for (start in seq_len(nrow(start_set))) {
    
    model <- midas_r(hpi ~ mls(hpi, 1:1, 1, '*') +
                       mls(dax, 0:lag_com1[lag,1], day_ratio, nealmon) +
                       mls(unrate, 2:lag_com1[lag,2], mon_ratio, nealmon) +
                       mls(rbp, 2:lag_com1[lag,2], mon_ratio, nealmon) +
                       mls(cpi, 1:lag_com1[lag,2], mon_ratio, nealmon) +
                       mls(hl, 2:lag_com1[lag,2], mon_ratio, nealmon) +
                       mls(din, 3:lag_com1[lag,2], mon_ratio, nealmon),
                     start = list(dax = start_set[start,c(1,2)],
                                  unrate = start_set[start,c(3,4)],
                                  rbp = start_set[start,c(5,6)],
                                  cpi = start_set[start,c(7,8)],
                                  hl = start_set[start,c(9,10)],
                                  din = start_set[start,c(11,12)]),
                     data = list(hpi = q_data[1:window, 'hpi'],
                                 dax = dax_dd[1:(window*day_ratio), 'dax'],
                                 unrate = m_data[1:(window*mon_ratio), 'unrate'],
                                 rbp = m_data[1:(window*mon_ratio), 'rbp'],
                                 cpi = m_data[1:(window*mon_ratio), 'cpi'],
                                 hl = m_data[1:(window*mon_ratio), 'hl'],
                                 din = m_data[1:(window*mon_ratio), 'din']))
  
    
    ar_midas_all_rss[lag,start] <- sum((model$lhs-model$fitted.values)^2)
  }
}

ar_midas_min_rss <- which(ar_midas_all_rss[seq_len(nrow(lag_com1)),] == min(ar_midas_all_rss[seq_len(nrow(lag_com1)),]), arr.ind = TRUE)




start_dax <- start_set[ar_midas_min_rss[,2],c(1,2)]
start_unrate <- start_set[ar_midas_min_rss[,2],c(3,4)]
start_rbp <- start_set[ar_midas_min_rss[,2],c(5,6)]
start_cpi <- start_set[ar_midas_min_rss[,2],c(7,8)]
start_hl <- start_set[ar_midas_min_rss[,2],c(9,10)]
start_din <- start_set[ar_midas_min_rss[,2],c(11,12)]


## forecasting and evaluation

ar_midas_all_fore <- matrix(NA, nrow = 100,
                            ncol = length(horizon))

for (i in recursive) {
  
  best_lag <- NULL
  min_bic <- Inf
  
  for (lag in 1:nrow(lag_com2)) {
    
    model <- midas_r(hpi ~ mls(hpi, 1:1, 1, '*') +
                mls(dax, 0:lag_com2[lag,1], day_ratio, nealmon) +
                mls(unrate, 2:lag_com2[lag,2], mon_ratio, nealmon) +
                mls(rbp, 2:lag_com2[lag,3], mon_ratio, nealmon) +
                mls(cpi, 1:lag_com2[lag,4], mon_ratio, nealmon) +
                mls(hl, 2:lag_com2[lag,5], mon_ratio, nealmon) +
                mls(din, 3:lag_com2[lag,6], mon_ratio, nealmon),
              start = list(dax = start_dax,
                           unrate = start_unrate,
                           rbp = start_rbp,
                           cpi = start_cpi,
                           hl = start_hl,
                           din = start_din),
              data = list(hpi = q_data[1:i, 'hpi'],
                          dax = dax_dd[1:(i*day_ratio), 'dax'],
                          unrate = m_data[1:(i*mon_ratio), 'unrate'],
                          rbp = m_data[1:(i*mon_ratio), 'rbp'],
                          cpi = m_data[1:(i*mon_ratio), 'cpi'],
                          hl = m_data[1:(i*mon_ratio), 'hl'],
                          din = m_data[1:(i*mon_ratio), 'din']))
    
    current_bic <- BIC(model)
    
    if (current_bic < min_bic) {
      min_bic <- current_bic
      best_lag <- lag
    }
  }
  
  l_dax <- lag_com2[best_lag,1]
  l_unrate <- lag_com2[best_lag,2]
  l_rbp <- lag_com2[best_lag,3]
  l_cpi <- lag_com2[best_lag,4]
  l_hl<- lag_com2[best_lag,5]
  l_din <- lag_com2[best_lag,6]
  
  
  
  model <- midas_r(hpi ~ mls(hpi, 1:1, 1, '*') +
                     mls(dax, 0:l_dax, day_ratio, nealmon) +
                     mls(unrate, 2:l_unrate, mon_ratio, nealmon) +
                     mls(rbp, 2:l_rbp, mon_ratio, nealmon) +
                     mls(cpi, 1:l_cpi, mon_ratio, nealmon) +
                     mls(hl, 2:l_hl, mon_ratio, nealmon) +
                     mls(din, 3:l_din, mon_ratio, nealmon),
                   start = list(dax = start_dax,
                                unrate = start_unrate,
                                rbp = start_rbp,
                                cpi = start_cpi,
                                hl = start_hl,
                                din = start_din),
                   data = list(hpi = q_data[1:i, 'hpi'],
                               dax = dax_dd[1:(i*day_ratio), 'dax'],
                               unrate = m_data[1:(i*mon_ratio), 'unrate'],
                               rbp = m_data[1:(i*mon_ratio), 'rbp'],
                               cpi = m_data[1:(i*mon_ratio), 'cpi'],
                               hl = m_data[1:(i*mon_ratio), 'hl'],
                               din = m_data[1:(i*mon_ratio), 'din']))
  
  for (j in seq_along(horizon)) {
    
    fore <- average_forecast(list(model),
                             data = list(hpi = q_data[1:(i+9), 'hpi'],
                                         dax = dax_dd[1:((i+9)*day_ratio), 'dax'],
                                         unrate = m_data[1:((i+9)*mon_ratio), 'unrate'],
                                         rbp = m_data[1:((i+9)*mon_ratio), 'rbp'],
                                         cpi = m_data[1:((i+9)*mon_ratio), 'cpi'],
                                         hl = m_data[1:((i+9)*mon_ratio), 'hl'],
                                         din = m_data[1:((i+9)*mon_ratio), 'din']),
                             
                             insample = 1:i, outsample = (i+1):(i+9),
                             type = 'fixed')
    
    ar_midas_all_fore[((i+1) + horizon[j]), j] <- fore$forecast[horizon[j]+1]
    
  }}

# MFBVAR forecasting



mfbvar_all_forecasts <- matrix(NA, nrow = 100,ncol = length(horizon))


lamb1 <- c(0.01, 0.1, 0.5, 0.9)
lamb2 <- c(0.1,0.5,1,2,4)
lamb_com <- expand.grid(lamb1, lamb2)
lamb_vec <- rep(NA)


full_list <- list()
for (var in variables) {
  full_list[[var]] <- ts(m_data[,var],
                         start = c(2007, 6),
                         frequency = 12)
}
full_list[['hpi']] <- ts(q_data[,'hpi'],
                         start = c(2007,2),
                         frequency = 4)

c_interval <- t(sapply(full_list, CI, ci=0.95))
prior_intervals <- c_interval[,c('upper','lower')]
moments <- interval_to_moments(prior_intervals)


# forecasting and evaluation

for (i in recursive) {
  
  min_mse <- Inf
  best_lambda <- NULL
  
  mflist <- list()
  
  for (var in variables) {
    mflist[[var]] <- ts(m_data[1:((i+1)*3-pub_lag[,var]),var],
                        start = c(2007, 4),
                        frequency = 12)
  }
  mflist[['hpi']] <- ts(q_data[1:i,'hpi'],
                        start = c(2007,2),
                        frequency = 4)
  
  for (lamb in 1:nrow(lamb_com)) {
    
    prior <- set_prior(Y = mflist, n_lags = 4, n_reps = 15000,
                       lambda1 = lamb_com[lamb,1],
                       lambda3 = lamb_com[lamb,2],
                       aggregation = "average",
                       d = 'intercept',
                       prior_psi_mean = moments$prior_psi_mean,
                       prior_psi_Omega = moments$prior_psi_Omega,
                       n_fcst = 27,
                       prior_Pi_AR1 = 0.9) 
    
    
    model <- estimate_mfbvar(prior, prior = 'ss', variance = 'iw')
    
    fore <- predict(model, pred_bands = NULL) %>%
      filter(variable == 'hpi') %>%
      group_by(fcst_date) %>%
      dplyr::summarise(fore_mean = mean(fcst))
    
    current_mse <- mse(q_data$hpi[i+1], fore$fore_mean[1])
    
    if (current_mse < min_mse) {
      min_mse <- current_mse
      best_lambda <- lamb
    }
  }
  
  
  final_prior <- update_prior(prior,
                              lambda1 = lamb_com[best_lambda, 1],
                              lambda2 = lamb_com[best_lambda, 2])
  final_model <- estimate_mfbvar(final_prior, prior = 'ss', variance = 'iw')
  fore <- predict(final_model, pred_bands = NULL) %>%
    filter(variable == 'hpi') %>%
    group_by(fcst_date) %>%
    dplyr::summarise(fore_mean = mean(fcst))
  
  for (j in seq_along(horizon)) {
    
    mfbvar_all_forecasts[((i+1) + horizon[j]), j] <- fore$fore_mean[horizon[j]+1]
    
  }
}

# Results for pooled models


models <- c('var','umidas',
            'midas','ar-midas','mfvar')


before_mse <- matrix(NA,6,5)
after_mse <- matrix(NA,6,5)
colnames(before_mse) <- models
colnames(after_mse) <- models

eval_end <- eval_end1

for (j in seq_along(horizon)) {
 
    before_mse[j, 'var'] <- mse(q_data$hpi[(eval_start + horizon[j]):eval_end],
                                var_all_forecasts[(eval_start + horizon[j]):eval_end, j])
    before_mse[j, 'mfvar'] <- mse(q_data$hpi[(eval_start + horizon[j]):eval_end],
                                  mfbvar_all_forecasts[(eval_start + horizon[j]):eval_end, j])
    before_mse[j, 'umidas'] <- mse(q_data$hpi[(eval_start + horizon[j]):eval_end],
                                   umidas_all_forecasts[(eval_start + horizon[j]):eval_end, j])
    before_mse[j, 'midas'] <- mse(q_data$hpi[(eval_start + horizon[j]):eval_end],
                                  midas_all_fore[(eval_start + horizon[j]):eval_end, j])
    before_mse[j, 'ar-midas'] <- mse(q_data$hpi[(eval_start + horizon[j]):eval_end],
                                     ar_midas_all_fore[(eval_start + horizon[j]):eval_end, j])
}
eval_end <- eval_end2

for (j in seq_along(horizon)) {

    after_mse[j, 'var'] <- mse(q_data$hpi[(eval_start + horizon[j]):eval_end],
                               var_all_forecasts[(eval_start + horizon[j]):eval_end, j])
    after_mse[j, 'mfvar'] <- mse(q_data$hpi[(eval_start + horizon[j]):eval_end],
                                 mfbvar_all_forecasts[(eval_start + horizon[j]):eval_end, j])
    after_mse[j, 'umidas'] <- mse(q_data$hpi[(eval_start + horizon[j]):eval_end],
                                  umidas_all_forecasts[(eval_start + horizon[j]):eval_end, j])
    after_mse[j, 'midas'] <- mse(q_data$hpi[(eval_start + horizon[j]):eval_end],
                                 midas_all_fore[(eval_start + horizon[j]):eval_end, j])
    after_mse[j, 'ar-midas'] <- mse(q_data$hpi[(eval_start + horizon[j]):eval_end],
                                    ar_midas_all_fore[(eval_start + horizon[j]):eval_end, j])
  }


before_ar <- rep(NA)
after_ar <- rep(NA)
for (j in seq_along(horizon)) {
  before_ar[j] <- mse(q_data$hpi[(eval_start + horizon[j]):eval_end1],
                   ar_fore[(eval_start + horizon[j]):eval_end1, j])
  after_ar[j] <- mse(q_data$hpi[(eval_start + horizon[j]):eval_end2],
                   ar_fore[(eval_start + horizon[j]):eval_end2, j])
}

pooled_before <- before_mse/before_ar
pooled_after <- after_mse/after_ar


rel_var <- var_mse/ar_mse
rel_mfbvar <- mfbvar_mse/ar_mse
rel_umidas <- umidas_mse/ar_mse
rel_midas <- midas_mse/ar_mse
rel_armidas <- ar_midas_mse/ar_mse

pooled_mse <- pooled_before

quant_df <- data.frame(
  var = sapply(1:6, function(i) {
    ecdf(rel_var[i,])(pooled_mse[i, "var"])
  }),
  mfbvar = sapply(1:6, function(i) {
    ecdf(rel_mfbvar[i,])(pooled_mse[i, "mfvar"])
  }),
  umidas = sapply(1:6, function(i) {
    ecdf(rel_umidas[i,])(pooled_mse[i, "umidas"])
  }),
  midas = sapply(1:6, function(i) {
    ecdf(rel_midas[i,])(pooled_mse[i, "midas"])
  }),
  armidas = sapply(1:6, function(i) {
    ecdf(rel_armidas[i,])(pooled_mse[i, "ar-midas"])
  })
)


#################### Figures ####################


# Figure 1

df$yearmon_date <- as.Date(df$yearmon)
hpi_qdf$yearqtr_date <- as.Date(hpi_qdf$yearqtr)
dax_dd$Date <- as.Date(dax_dd$Date)

ggplot(dax_dd, aes(x = Date, y = Close)) +
  geom_line(color = "black") +
  coord_cartesian(xlim = range(dax_dd$Date[131:4053])) + 
  labs(title = " ", x = "Date", y = "EUR") +
  theme_classic()+
  scale_x_date(date_labels = "%Y", date_breaks = "3 year")+
  theme(axis.title.x = element_text(size = 28), 
        axis.title.y = element_text(size = 28),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26))+
  geom_vline(xintercept = as.numeric(dax_dd$Date[3279]), 
             linetype = "dashed", color = "red", size = 1)

ggplot(df, aes(x = yearmon_date, y = unrate)) +
  geom_line(color = "black") +
  coord_cartesian(xlim = range(df$yearmon_date[10:192])) + 
  labs(title = " ", x = "Date", y = "Percent") +
  theme_classic()+
  scale_x_date(date_labels = "%Y", date_breaks = "3 year")+
  theme(axis.title.x = element_text(size = 28), 
        axis.title.y = element_text(size = 28),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26))+
  geom_vline(xintercept = as.numeric(df$yearmon_date[159]), 
             linetype = "dashed", color = "red", size = 1)

ggplot(df, aes(x = yearmon_date, y = rbp)) +
  geom_line(color = "black") +
  coord_cartesian(xlim = range(df$yearmon_date[10:192])) + 
  labs(title = " ", x = "Date", y = "Number") +
  theme_classic()+
  scale_x_date(date_labels = "%Y", date_breaks = "3 year")+
  theme(axis.title.x = element_text(size = 28), 
        axis.title.y = element_text(size = 28),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26))+
  geom_vline(xintercept = as.numeric(df$yearmon_date[159]), 
             linetype = "dashed", color = "red", size = 1)

ggplot(df, aes(x = yearmon_date, y = cpi)) +
  geom_line(color = "black") +
  coord_cartesian(xlim = range(df$yearmon_date[10:192])) + 
  labs(title = " ", x = "Date", y = "Index (2020=100)") +
  theme_classic()+
  scale_x_date(date_labels = "%Y", date_breaks = "3 year")+
  theme(axis.title.x = element_text(size = 28), 
        axis.title.y = element_text(size = 28),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26))+
  geom_vline(xintercept = as.numeric(df$yearmon_date[159]), 
             linetype = "dashed", color = "red", size = 1)

ggplot(df, aes(x = yearmon_date, y = hl)) +
  geom_line(color = "black") +
  coord_cartesian(xlim = range(df$yearmon_date[10:192])) + 
  labs(title = " ", x = "Date", y = "Percent") +
  theme_classic()+
  scale_x_date(date_labels = "%Y", date_breaks = "3 year")+
  theme(axis.title.x = element_text(size = 28), 
        axis.title.y = element_text(size = 28),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26))+
  geom_vline(xintercept = as.numeric(df$yearmon_date[159]), 
             linetype = "dashed", color = "red", size = 1)

ggplot(df, aes(x = yearmon_date, y = din)) +
  geom_line(color = "black") +
  coord_cartesian(xlim = range(df$yearmon_date[10:192])) + 
  labs(title = " ", x = "Date", y = "EUR (billion)
") +
  theme_classic()+
  scale_x_date(date_labels = "%Y", date_breaks = "3 year")+
  theme(axis.title.x = element_text(size = 28), 
        axis.title.y = element_text(size = 28),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26))+
  geom_vline(xintercept = as.numeric(df$yearmon_date[159]), 
             linetype = "dashed", color = "red", size = 1)

ggplot(hpi_qdf, aes(x = yearqtr_date, y = hpi)) +
  geom_line(color = "black") +
  coord_cartesian(xlim = range(hpi_qdf$yearqtr_date[29:92])) + 
  labs(title = " ", x = "Date", y = "Index (2015=100)") +
  theme_classic()+
  scale_x_date(date_labels = "%Y", date_breaks = "3 year")+
  theme(axis.title.x = element_text(size = 28), 
        axis.title.y = element_text(size = 28),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26))+
  geom_vline(xintercept = as.numeric(hpi_qdf$yearqtr_date[84]), 
             linetype = "dashed", color = "red", size = 1)


# Figure 2


m_data$yearmon_date <- as.Date(m_data$yearmon)
q_data$yearqtr_date <- as.Date(q_data$yearqtr)

ggplot(m_data, aes(x = yearmon_date, y = dax)) +
  geom_line(color = "black") +
  coord_cartesian(xlim = range(m_data$yearmon_date[10:192])) + 
  geom_ribbon(aes(ymin = c_interval['dax','lower'], 
                  ymax = c_interval['dax','upper']), 
              fill = "green", alpha = 0.5) +
  labs(title = " ", x = "Date", y = "Growth") +
  geom_hline(yintercept = c_interval['dax','mean'], 
             linetype = "dashed", 
             color = "red", size =1) +
  theme_classic()+
  scale_x_date(date_labels = "%Y", date_breaks = "3 year")+
  theme(axis.title.x = element_text(size = 28), 
        axis.title.y = element_text(size = 28),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26))


ggplot(m_data, aes(x = yearmon_date, y = unrate)) +
  geom_line(color = "black") +
  coord_cartesian(xlim = range(m_data$yearmon_date[10:192])) + 
  geom_ribbon(aes(ymin = c_interval['unrate','lower'], 
                  ymax = c_interval['unrate','upper']), 
              fill = "green", alpha = 0.5) +
  labs(title = " ", x = "Date", y = "Growth") +
  geom_hline(yintercept = c_interval['unrate','mean'], 
             linetype = "dashed", color = "red", size =1) +
  theme_classic()+
  scale_x_date(date_labels = "%Y", date_breaks = "3 year")+
  theme(axis.title.x = element_text(size = 28), 
        axis.title.y = element_text(size = 28),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26))

ggplot(m_data, aes(x = yearmon_date, y = rbp)) +
  geom_line(color = "black") +
  coord_cartesian(xlim = range(m_data$yearmon_date[10:192])) + 
  geom_ribbon(aes(ymin = c_interval['rbp','lower'], 
                  ymax = c_interval['rbp','upper']), 
              fill = "green", alpha = 0.5) +
  labs(title = " ", x = "Date", y = "Growth") +
  geom_hline(yintercept = c_interval['rbp','mean'], linetype = "dashed", 
             color = "red", size =1) +
  theme_classic()+
  scale_x_date(date_labels = "%Y", date_breaks = "3 year")+
  theme(axis.title.x = element_text(size = 28), 
        axis.title.y = element_text(size = 28),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26))

ggplot(m_data, aes(x = yearmon_date, y = cpi)) +
  geom_line(color = "black") +
  coord_cartesian(xlim = range(m_data$yearmon_date[10:192])) + 
  geom_ribbon(aes(ymin = c_interval['cpi','lower'], 
                  ymax = c_interval['cpi','upper']), 
              fill = "green", alpha = 0.5) +
  labs(title = " ", x = "Date", y = "Growth") +
  geom_hline(yintercept = c_interval['cpi','mean'], 
             linetype = "dashed", color = "red", size =1) +
  theme_classic()+
  scale_x_date(date_labels = "%Y", date_breaks = "3 year")+
  theme(axis.title.x = element_text(size = 28), 
        axis.title.y = element_text(size = 28),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26))

ggplot(m_data, aes(x = yearmon_date, y = hl)) +
  geom_line(color = "black") +
  coord_cartesian(xlim = range(m_data$yearmon_date[10:192])) + 
  geom_ribbon(aes(ymin = c_interval['hl','lower'], 
                  ymax = c_interval['hl','upper']), 
              fill = "green", alpha = 0.5) +
  labs(title = " ", x = "Date", y = "Growth") +
  geom_hline(yintercept = c_interval['hl','mean'], 
             linetype = "dashed", color = "red", size =1) +
  theme_classic()+
  scale_x_date(date_labels = "%Y", date_breaks = "3 year")+
  theme(axis.title.x = element_text(size = 28), 
        axis.title.y = element_text(size = 28),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26))

ggplot(m_data, aes(x = yearmon_date, y = din)) +
  geom_line(color = "black") +
  coord_cartesian(xlim = range(m_data$yearmon_date[10:192])) + 
  geom_ribbon(aes(ymin = c_interval['din','lower'], 
                  ymax = c_interval['din','upper']), 
              fill = "green", alpha = 0.5) +
  labs(title = " ", x = "Date", y = "Growth") +
  geom_hline(yintercept = c_interval['din','mean'], linetype = "dashed", color = "red", size =1) +
  theme_classic()+
  scale_x_date(date_labels = "%Y", date_breaks = "3 year")+
  theme(axis.title.x = element_text(size = 28), 
        axis.title.y = element_text(size = 28),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26))

ggplot(q_data, aes(x = yearqtr_date, y = hpi)) +
  geom_line(color = "black") +
  coord_cartesian(xlim = range(q_data$yearqtr_date[4:64])) + 
  geom_ribbon(aes(ymin = c_interval['hpi','lower'], 
                  ymax = c_interval['hpi','upper']), 
              fill = "green", alpha = 0.5) +
  labs(title = " ", x = "Date", y = "Growth") +
  geom_hline(yintercept = c_interval['hpi','mean'], linetype = "dashed", color = "red", size =1) +
  theme_classic()+
  scale_x_date(date_labels = "%Y", date_breaks = "3 year")+
  theme(axis.title.x = element_text(size = 28), 
        axis.title.y = element_text(size = 28),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26))

# Figure 3


be_umi <- matrix(NA, nrow = length(horizon), ncol = length(variables))
af_umi <- matrix(NA, nrow = length(horizon), ncol = length(variables))
be_armi <- matrix(NA, nrow = length(horizon), ncol = length(variables))
af_armi <- matrix(NA, nrow = length(horizon), ncol = length(variables))
colnames(be_umi) <- variables
colnames(af_umi) <- variables
colnames(be_armi) <- variables
colnames(af_armi) <- variables

for (var in variables) {
  for (j in seq_along(horizon)) {
    
    be_umi[j, var] <- mse(q_data$hpi[(eval_start + horizon[j]):eval_end1],
                          umidas_forecasts[[var]][(eval_start + horizon[j]):eval_end1, j])
    af_umi[j, var] <- mse(q_data$hpi[(eval_start + horizon[j]):eval_end2],
                              umidas_forecasts[[var]][(eval_start + horizon[j]):eval_end2, j])
    be_armi[j, var] <- mse(q_data$hpi[(eval_start + horizon[j]):eval_end1],
                           ar_midas_forecasts[[var]][(eval_start + horizon[j]):eval_end1, j])
    af_armi[j, var] <- mse(q_data$hpi[(eval_start + horizon[j]):eval_end2],
                                ar_midas_forecasts[[var]][(eval_start + horizon[j]):eval_end2, j])
    
  }
}

var <- 'din'

  abs_df <- data.frame(h = c(0,1,2,3,4,8),
                       before = be_umi[,var]/be_armi[,var],
                       after = af_umi[,var]/af_armi[,var])
  colnames(abs_df) <- c('h','Before', 'After')
  
  abs_df_long <- reshape2::melt(abs_df, id = "h", 
                                variable.name = "sample", 
                                value.name = "ratio")
  
  
ggplot(abs_df_long, aes(x = h, y = ratio, color = sample,
                                  shape = sample)) +
    geom_line(linewidth = 1) +
    scale_color_discrete(name="COVID-19") +
    scale_shape_discrete(name='COVID-19') +
    geom_point(size = 3) +
    theme_classic()+
    theme(axis.title.x = element_text(size = 28), 
          axis.title.y = element_text(size = 28),
          axis.text.x = element_text(size = 26),
          axis.text.y = element_text(size = 26),
          legend.text = element_text(size = 26),
          legend.title = element_text(size = 26)) +
  labs(title = " ", x = "Horizon", y = "Ratio") +
    geom_hline(yintercept = 1, 
               linetype = "dashed", color = "black", size =1)
  

abs_df <- data.frame(h = c(0,1,2,3,4,8),
                     before = before_mse[,'umidas']/before_mse[,'ar-midas'],
                     after = after_mse[,'umidas']/after_mse[,'ar-midas'])
colnames(abs_df) <- c('h','Before', 'After')

abs_df_long <- reshape2::melt(abs_df, id = "h", 
                              variable.name = "sample", 
                              value.name = "ratio")


ggplot(abs_df_long, aes(x = h, y = ratio, color = sample,
                        shape = sample)) +
  geom_line(linewidth = 1) +
  scale_color_discrete(name="COVID-19") +
  scale_shape_discrete(name='COVID-19') +
  geom_point(size = 3) +
  theme_classic()+
  theme(axis.title.x = element_text(size = 28), 
        axis.title.y = element_text(size = 28),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26),
        legend.text = element_text(size = 26),
        legend.title = element_text(size = 26)) +
  labs(title = " ", x = "Horizon", y = "Ratio") +
  geom_hline(yintercept = 1, 
             linetype = "dashed", color = "black", size =1)