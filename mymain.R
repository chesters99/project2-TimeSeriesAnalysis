mypackages = c("tidyverse", "lubridate")   
missing = setdiff(mypackages, rownames(installed.packages())) 
if (length(missing) > 0) install.packages(missing)

library(tidyverse)
library(lubridate)
options(readr.num_columns=0, scipen=999)

# main prediction function
mypredict = function() {
  
  # function to calculate weighted average of two closest historical sales weeks one year ago
  blender = function(test_date) {
    train_date = test_date - years(1)
    days_to_friday = (13 - wday(train_date)) %% 7
    next_friday = train_date + days_to_friday
    proportion = days_to_friday / 7 # fraction of week to reallocate
    
    df_next = subset(train,    Date == next_friday)      # get train data from this week a year ago
    df_prev = subset(train,    Date == next_friday - 7)  # get train data from previos week a year ago
    df_test = subset(testfold, Date == test_date)[, c("Store", "Dept")] # get test data
    
    df_merged = merge(df_next, df_prev,   by=c("Store", "Dept"), all   = TRUE) # merge the two historical weeks
    df_merged = merge(df_test, df_merged, by=c("Store", "Dept"), all.x = TRUE) # merge with the test data fold
    
    df_merged$Weekly_Sales = with(df_merged, (1-proportion) * ifelse(is.na(Weekly_Sales.x),0,Weekly_Sales.x) +  # calc blended weeks
                                                proportion  * ifelse(is.na(Weekly_Sales.y),0,Weekly_Sales.y))
    df_merged$Date = test_date
    return(df_merged[,c("Store", "Dept", "Date", "Weekly_Sales")]) # return blended sales date
  }
  
  # function to apply a trend factor to a store or a dept sales
  trender = function(sales, store_depts, dates, store_or_dept_num, trend) {
    index = which(store_depts == store_or_dept_num)
    weeks = as.integer( (dates[index] - min(dates[index]))/7 )
    sales[index] = sales[index] * trend^(1/52 * (52 - weeks)) # adjust sales based on trend factor and period
    return(sales)
  }
  
  find_dept_trends = function (train, dept_list=NULL) {
    if (is.null(dept_list)) {
      dept_list = unique(train$Dept)
      dept_list = c(92,87,6,5,55,59,90,72,82,93,16,58,21,85,30,11,26,54,96,17)
    }
    dept_list = dept_list[!dept_list %in% c(18,47,51,78,39,77,78,43,99)] # remove bad departments
    for (dept in dept_list) { #
      depts = train[train$Dept==dept,]
      last_date = as.Date("2011-02-25")
      sales = aggregate(x=depts$Weekly_Sales, by=list(Group.Date=depts$Date), FUN=sum)
      model = lm(x ~ poly(Group.Date,2), data=sales)
      pred_start = as.numeric( predict(model, newdata=data.frame(Group.Date=min(train$Date))) )
      pred_end   = as.numeric( predict(model, newdata=data.frame(Group.Date=last_date)) )
      duration = as.numeric(last_date - min(train$Date) )
      trend = (pred_end - pred_start) / pred_start * 365 / duration + 1
      if (trend>0.995 & trend<1.005 ) next
      cat(paste0('c(',dept,',',round(trend,2),'), '))
    }
  }
  
  # **********  START mypredict  **********
  # if first run then add fields to train, otherwise append new_test with new date fields to train
  if (t==1) { 
    train <<- mutate(train, Date=ymd(Date), Year=year(Date), Week=week(Date))
  } else {
    train <<- rbind(train, mutate(new_test, Date=ymd(Date), Year=year(Date), Week=week(Date)))
  }
  
  # find_dept_trends(train, NULL); return('dept trends');
  
  # create a fold of data from test.csv for testing, then add new date fields added
  predict_start = floor_date(max(train$Date) + 7, "month") 
  predict_end   = predict_start %m+% months(2) - 1
  ids = which(test$Date >= predict_start & test$Date <= predict_end)
  testfold = mutate(test[ids,],  Date = ymd(Date), Year=year(Date), Week = week(Date), Prev_Year = Year-1)
  
  # move test period weeks to align with train period weeks for thanksgiving
  testfold$Week[testfold$Week==48] = 49
  testfold$Week[testfold$Week==47] = 48
  
  # predict week sales based on previous years week sales (seasonal naive)
  df_preds = merge(testfold, train, by.x = c("Store","Dept","Prev_Year","Week"), by.y = c("Store","Dept","Year","Week"), all.x = TRUE)
  df_preds$Weekly_Sales[is.na(df_preds$Weekly_Sales)] = 0
  df_preds = df_preds[, c("Store", "Dept", "Date.x", "Weekly_Sales")]
  colnames(df_preds)[3] = "Date"
  
  # dates not to be blended are thanksgiving and early April (tuned)
  test_dates = as.character(unique(testfold$Date))
  to_be_blended_dates = test_dates[!test_dates %in% c("2011-03-25","2011-04-01", "2011-11-25","2011-12-02", "2012-04-06","2012-04-13")]
  
  df_preds_unblended = subset(df_preds, !((as.character(Date)) %in% to_be_blended_dates))  # remove dates not to be blended
  df_preds_blended = bind_rows(lapply(ymd(to_be_blended_dates), blender))                  # blend two prior weeks proportionally
  df_preds_blended = rbind(df_preds_blended, df_preds_unblended)                           # add blended to unblended weeks
  
  # apply store trends from linear quadratic model
  df_preds_trended = df_preds_blended
  for (store_num in unique(train$Store)) {
    if (store_num %in% c(16,10,28,28,22,45)) next
    stores = train[train$Store==store_num,]
    sales = aggregate(x=stores$Weekly_Sales, by=list(Group.Date=stores$Date), FUN=sum)
    model = lm(x ~ poly(Group.Date,2), data=sales)
    pred_start = as.numeric( predict(model, newdata=data.frame(Group.Date=min(train$Date))) )
    pred_end   = as.numeric( predict(model, newdata=data.frame(Group.Date=max(train$Date))) )
    duration = as.numeric(max(train$Date) - min(train$Date) )
    trend = (pred_end - pred_start) / pred_start * 365*0.75 / duration + 1
    df_preds_trended$Weekly_Sales = 
        trender(df_preds_trended$Weekly_Sales, df_preds_trended$Store, df_preds_trended$Date, store_num, trend) 
  }

  # use department trends from linear quadratic model
  df_preds_trended2 = df_preds_trended
  for (dept_num in unique(train$Dept)) {
    if (!dept_num %in% c(92,87,6,93,58,21,40,54,90,74,13,85,19,65,2,52,30,37,4,26,51,50,45)) next
    depts = train[train$Dept==dept_num,]
    sales = aggregate(x=depts$Weekly_Sales, by=list(Group.Date=depts$Date), FUN=sum)
    model = lm(x ~ poly(Group.Date,2), data=sales)
    pred_start = as.numeric( predict(model, newdata=data.frame(Group.Date=min(train$Date))) )
    pred_end   = as.numeric( predict(model, newdata=data.frame(Group.Date=max(train$Date))) )
    duration = as.numeric(max(train$Date) - min(train$Date) )
    trend = (pred_end - pred_start) / pred_start * 365*0.75 / duration  + 1
    df_preds_trended2$Weekly_Sales = 
      trender(df_preds_trended2$Weekly_Sales, df_preds_trended2$Dept, df_preds_trended2$Date, dept_num, trend) 
  }
  
  # use dept trends for high-trend departments (pre-calculated from train.csv in find_dept_trends function above)
  dept_trends = list(c(92,1.05), c(87,1.17), c(6,0.83), c(5,0.93), c(55,0.88), c(59,0.73), c(90,1.04), c(72,0.95), c(82,1.09), c(93,1.04),
                   c(16,0.93), c(58,0.96), c(21,0.93), c(85,0.93), c(30,0.92), c(11,0.99), c(26,0.96), c(54,0.59), c(96,1.09), c(17,0.96))
  df_preds_trended3 = df_preds_trended
  for(d in dept_trends) {
    df_preds_trended3$Weekly_Sales = trender(df_preds_trended3$Weekly_Sales, df_preds_trended3$Dept, df_preds_trended3$Date, d[1], d[2])
  }

  # sort by Store Dept, Date to align with test.csv 
  df_preds_trended  = df_preds_trended[ order(df_preds_trended$Store,  df_preds_trended$Dept,  df_preds_trended$Date),]
  df_preds_trended2 = df_preds_trended2[order(df_preds_trended2$Store, df_preds_trended2$Dept, df_preds_trended2$Date),]
  df_preds_trended3 = df_preds_trended3[order(df_preds_trended3$Store, df_preds_trended3$Dept, df_preds_trended3$Date),]
  
  # save 3 model predictions (round to zero places as has no impact, also no need to correct negative or zero predictions)
  test[ids, "Weekly_Pred1"] <<- round(df_preds_trended$Weekly_Sales, 0)  # blended weeks seasonal model with store trend
  test[ids, "Weekly_Pred2"] <<- round(df_preds_trended2$Weekly_Sales,0)  # blended weeks seasonal model with store trend and dept trend
  test[ids, "Weekly_Pred3"] <<- round(df_preds_trended3$Weekly_Sales,0)  # blended weeks seasonal model with store trend, pre-calc dept trend
}

# mypredict()

