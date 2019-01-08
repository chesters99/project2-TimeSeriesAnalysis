# Project 2 for Peter Tsapatsaris
# CS 598:  Practical Statistical Learnings

################ Load Environment ##################
# clean workspace
rm(list = ls())

# load necessary packages
mypackages = c( "lubridate","forecast", "tidyverse")   
tmp = setdiff(mypackages, rownames(installed.packages())) 
if (length(tmp) > 0) install.packages(tmp)

library(lubridate)
library(forecast)
library(tidyverse)

# converts a Date x num_store forecast to a dataframe
# with Date, Store, value = Weekly_Price columns
flatten_forecast <- function(f_model) {
    f_model %>%
        gather(Store, value, -Date, convert = TRUE)
}

# Adds forecasts to the testing dataframe
update_forecast <- function(test_month, dept_preds, dept, num_model) {
    dept_preds <- flatten_forecast(dept_preds)
    
    pred.d <- test_month %>%
        filter(Dept == dept) %>%
        select('Store', 'Date') %>%
        left_join(dept_preds, by = c('Store', 'Date'))
    
    pred.d.idx <- test_month$Dept == dept
    pred.d <- test_month[pred.d.idx, c('Store', 'Date')] %>%
        left_join(dept_preds, by = c('Store', 'Date'))
    
    if (num_model == 1) {
        test_month$Weekly_Pred1[pred.d.idx] <- pred.d$value
    } else if(num_model == 2) {
        test_month$Weekly_Pred2[pred.d.idx] <- pred.d$value
    } else {
        test_month$Weekly_Pred3[pred.d.idx] <- pred.d$value
    }
    
    test_month
}

# update forecasts in the global test dataframe
update_test <- function(test_month) {
    test <<- test %>%
        dplyr::left_join(test_month,
                         by = c('Date', 'Store', 'Dept', 'IsHoliday')) %>%
        mutate(Weekly_Pred1 = coalesce(Weekly_Pred1.y, Weekly_Pred1.x)) %>%
        mutate(Weekly_Pred2 = coalesce(Weekly_Pred2.y, Weekly_Pred2.x)) %>%
        mutate(Weekly_Pred3 = coalesce(Weekly_Pred3.y, Weekly_Pred3.x)) %>%
        select(-Weekly_Pred1.x, -Weekly_Pred1.y,
               -Weekly_Pred2.x, -Weekly_Pred2.y,
               -Weekly_Pred3.x, -Weekly_Pred3.y)
}


##### Model Building Functions #####

# Forecasts out the last observation in the training data.  Unused in this program. 
naive_model<- function(train_ts, test_ts){
    num_forecasts <- nrow(test_ts)
    train_ts[is.na(train_ts)] <- 0
    
    # naive forecast per store
    for(j in 2:ncol(train_ts)){
        store_ts <- ts(train_ts[, j], frequency=52)
        test_ts[, j] <- naive(store_ts, num_forecasts)$mean
    }
    test_ts
}

# Model that takes median of three weeks surrounding prior year.  Unused in this program.
MySimpleModel = function(train_ts, test_ts){
  
  train.wk = train_ts$Date
  train.wk = train.wk - train.wk[1]  
  train.wk = train.wk/7 + 5  
  train.wk = as.numeric(train.wk) %% 52 
  train_ts$Wk = train.wk
  
  test.wk = test_ts$Date
  test.wk = test.wk - train_ts$Date[1]
  test.wk = test.wk/7 + 5
  test.wk = as.numeric(test.wk) %% 52
  test_ts$Wk = test.wk
  
  train_ts$Yr = year(train_ts$Date)
  test_ts$Yr = year(test_ts$Date)
  
  for(s in 2:ncol(train_ts)){
    
    for (i in 1:nrow(test_ts)){
      test_week = test_ts$Wk[i]
      test_year = test_ts$Yr[i]
      
      price_1 = train_ts[which(train_ts$Wk == test_week & train_ts$Yr == test_year-1) , s]
      # price_2 = train_ts[which(train_ts$Wk == test_week - 1 & train_ts$Yr == test_year-1) , s]
      # price_3 = train_ts[which(train_ts$Wk == test_week + 1 & train_ts$Yr == test_year-1) , s]
      # 
      # combined = c(price_1, price_2, price_3) 
      # 
      # if (length(combined) == 0){
      #    test_ts[i,s] = 0
      #   }else{
      #    test_ts[i,s] = median(combined)
      #   }
      test_ts[i,s] = price_1
      }
    }
  
  test_ts[is.na(test_ts)] = 0
  test_ts$Wk = NULL
  test_ts$Yr = NULL
  return(test_ts)
}

# Seasonal naive model. 
s_naive_model<- function(train_ts, test_ts){
  num_forecasts <- nrow(test_ts)
  train_ts[is.na(train_ts)] <- 0
  
  # naive forecast per store
  for(j in 2:ncol(train_ts)){
    store_ts <- ts(train_ts[, j], frequency=52)
    test_ts[, j] <- snaive(store_ts, num_forecasts)$mean
  }
  test_ts
}

# Linear model using trend and season. 
s_and_t_model = function(train_ts, test_ts){
  num_forecasts <- nrow(test_ts)
  train_ts[is.na(train_ts)] <- 0
  
  for(j in 2:ncol(train_ts)){
    store_ts <- ts(train_ts[, j], frequency=52)
    fit = tslm(store_ts ~ trend + season)
    fc = forecast(fit, h=num_forecasts)
    test_ts[, j] <- fc$mean
  }
  
  # for(i in 1:num_forecasts){
  #   if(test_ts$Date[i] == "2011-11-04"){
  #     test_ts[i,2:ncol(test_ts)] = train_ts[which(train_ts$Date == "2010-11-5"), 2:ncol(train_ts)]
  #   } 
  #   if(test_ts$Date[i] == "2011-11-11"){
  #     test_ts[i,2:ncol(test_ts)] = train_ts[which(train_ts$Date == "2010-11-12"), 2:ncol(train_ts)]
  #   } 
  #   if(test_ts$Date[i] == "2011-11-18"){
  #     test_ts[i,2:ncol(test_ts)] = train_ts[which(train_ts$Date == "2010-11-19"), 2:ncol(train_ts)]
  #   } 
  #   if(test_ts$Date[i] == "2011-11-25"){
  #     test_ts[i,2:ncol(test_ts)] = train_ts[which(train_ts$Date == "2010-11-26"), 2:ncol(train_ts)]
  #   } 
  #   if(test_ts$Date[i] == "2011-12-02"){
  #     test_ts[i,2:ncol(test_ts)] = train_ts[which(train_ts$Date == "2010-12-03"), 2:ncol(train_ts)]
  #   }
  #   if(test_ts$Date[i] == "2011-12-09"){
  #     test_ts[i,2:ncol(test_ts)] = train_ts[which(train_ts$Date == "2010-12-10"), 2:ncol(train_ts)]
  #   }
  #   if(test_ts$Date[i] == "2011-12-16"){
  #     test_ts[i,2:ncol(test_ts)] = train_ts[which(train_ts$Date == "2010-12-17"), 2:ncol(train_ts)]
  #   }
  #   if(test_ts$Date[i] == "2011-12-23"){
  #     test_ts[i,2:ncol(test_ts)] = train_ts[which(train_ts$Date == "2010-12-24"), 2:ncol(train_ts)]
  #   }
  #   if(test_ts$Date[i] == "2011-12-30"){
  #     test_ts[i,2:ncol(test_ts)] = train_ts[which(train_ts$Date == "2010-12-31"), 2:ncol(train_ts)]
  #   }
  # }
  test_ts
}

# Model that uses trend and season linear model until fold 7, and then seasonal decomposition using stlf.
stlf_model = function(train_ts, test_ts){
  num_forecasts <- nrow(test_ts)
  train_ts[is.na(train_ts)] <- 0
  
  if(t < 7){
    for(j in 2:ncol(train_ts)){
      store_ts <- ts(train_ts[, j], frequency=52)
      fit = tslm(store_ts ~ trend + season)
      fc = forecast(fit, h=num_forecasts)
      test_ts[, j] <- fc$mean
    }
  } else{
    for(j in 2:ncol(train_ts)){
      store_ts <- ts(train_ts[, j], frequency=52)
      fc = stlf(store_ts, h=num_forecasts, t.window=13, s.window=7, method = "arima", ic = "bic")
      pred = as.numeric(fc$mean)
      test_ts[, j] <- pred
    }
  }
  test_ts
}

##### Prediction Loop #####

mypredict <- function() {
    ###### Create train and test time-series #######
    if (t > 1) {
        # append the previous periods test data to the current training data
        train <<- rbind(train, new_test)
    }
    
    # filter test data.frame for the month that needs predictions
    # backtesting starts during March 2011
    start_date <- ymd("2011-03-01") %m+% months(2 * (t - 1))
    end_date <- ymd("2011-05-01") %m+% months(2 * (t - 1))
    test_month <- test %>%
        filter(Date >= start_date & Date < end_date)
    
    # Dates are not the same across months!
    test_dates <- unique(test_month$Date)
    num_test_dates <- length(test_dates)
    
    # Not all stores may need predictions either
    all_stores <- unique(test_month$Store)
    num_stores <- length(all_stores)
    
    # Most importantly not all departments need predictions
    test_depts <- unique(test_month$Dept)
    
    # Dateframe with (num_test_dates x num_stores) rows
    test_frame <- data.frame(
        Date=rep(test_dates, num_stores),
        Store=rep(all_stores, each=num_test_dates)
    )
    
    # Create the same dataframe for the training data
    # (num_train_dates x num_stores)
    train_dates <- unique(train$Date)
    num_train_dates <- length(train_dates)
    train_frame <- data.frame(
        Date=rep(train_dates, num_stores),
        Store=rep(all_stores, each=num_train_dates)
    )
    
    #### Perform a individual forecasts for each department
    for (dept in test_depts) {
        # filter for the particular department in the training data
        train_dept_ts <- train %>%
            filter(Dept == dept) %>%
            select(Store, Date, Weekly_Sales)
        
        # Reformat so that each column is a weekly time-series for that
        # store's department.
        # The dataframe has a shape (num_train_dates, num_stores)
        train_dept_ts <- train_frame %>%
            left_join(train_dept_ts, by = c('Date', 'Store')) %>%
            spread(Store, Weekly_Sales)
        
        # We create a similar dataframe to hold the forecasts on
        # the dates in the testing window
        test_dept_ts <- test_frame %>%
            mutate(Weekly_Sales = 0) %>%
            spread(Store, Weekly_Sales)
        
        ###### Model Fitting / Forecasting ######
        
        # Model 1:  Seasonal Naive
        f_simple = s_naive_model(train_dept_ts, test_dept_ts)
        test_month <- update_forecast(test_month,f_simple, dept, 1)
        
        # Model 2:  Trend and Seasonal Linear
        l_mod = s_and_t_model (train_dept_ts, test_dept_ts)
        test_month <- update_forecast(test_month,l_mod, dept, 2)
        
        # Model 3:  Blended Linear and STLF
        stlf_lm =  stlf_model(train_dept_ts, test_dept_ts)
        test_month <- update_forecast(test_month,stlf_lm, dept, 3)
    }
    
    # update global test dataframe
    update_test(test_month)
}
