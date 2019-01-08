rm(list=ls())
library(tidyverse)
options(warn=0)
source("mymain.R")

start_time <- Sys.time()
# train <- readr::read_csv('train.csv')
# for (bamm in seq(0.5, 1.1, by=0.05)) {
  
# read in train / test dataframes
train <- readr::read_csv('train.csv')
test <- readr::read_csv('test.csv', col_types = list(
  Weekly_Pred1 = col_double(),
  Weekly_Pred2 = col_double(),
  Weekly_Pred3 = col_double()
))

# save weighted mean absolute error WMAE
num_folds <- 10
wae <- tibble(
  model_one = rep(0, num_folds), 
  model_two = rep(0, num_folds), 
  model_three = rep(0, num_folds)
)

# time-series CV
for (t in 1:num_folds) {
  # *** THIS IS YOUR PREDICTION FUNCTION ***
  mypredict()
  
  # Load fold file 
  # You should add this to your training data in the next call 
  # to mypredict()
  fold_file <- paste0('fold_', t, '.csv')
  new_test <- readr::read_csv(fold_file)
  
  # extract predictions matching up to the current fold
  scoring_tbl <- new_test %>% 
    left_join(test, by = c('Date', 'Store', 'Dept'))
  
  # compute WMAE
  actuals <- scoring_tbl$Weekly_Sales
  preds <- select(scoring_tbl, contains('Weekly_Pred'))
  weights <- if_else(scoring_tbl$IsHoliday.x, 5, 1)
  wae[t, ] <- colSums(weights * abs(actuals - preds)) / sum(weights)
}

# save results to a file for grading
readr::write_csv(wae, 'Error.csv')

print(as.data.frame(wae))
print(colMeans(wae))
print(Sys.time()-start_time)
# }


# Graphs for report
plotter = function () {
  pdf("sales.pdf",10, 5) 
  par(mfrow=c(1,2))  
  par(mar=c(2.1,2.1,1.5,0.8))
  sales = aggregate(x=train$Weekly_Sales, by=list(Group.Date=train$Date), FUN=sum)
  Avg_Weekly_Sales = sales$x/length(unique(train$Store))
  plot(x=sales$Group.Date, y=Avg_Weekly_Sales, ylim=c(0,3800000), pch=20, 
       main="Sales by Store", xlab='Date', cex.axis=0.7, cex.lab=0.7)
  axis(4,labels=FALSE)
  for (store_num in unique(train$Store)) {
    train1 = train[train$Store == store_num,]
    sales = aggregate(x=train1$Weekly_Sales, by=list(Group.Date=train1$Date), FUN=sum)
    lines(sales, col=store_num)
    abline(lm(sales$x ~ sales$Group.Date), lwd=0.65, lty=2)
  }
  points(x=sales$Group.Date, y=Avg_Weekly_Sales, ylim=c(0,3800000), pch=20)
  abline(lm(Avg_Weekly_Sales ~ sales$Group.Date), col="red", lwd=1)
  
  sales = aggregate(x=train$Weekly_Sales, by=list(Group.Date=train$Date), FUN=sum)
  Avg_Weekly_Sales = sales$x/length(unique(train$Dept))
  plot(x=sales$Group.Date, y=Avg_Weekly_Sales, ylim=c(0,5500000), pch=20, 
       main="Sales by Dept", xlab='Date',  cex.axis=0.7, cex.lab=0.7)
  axis(4,labels=FALSE)
  for (store_num in unique(train$Dept)) {
    train1 = train[train$Dept == store_num,]
    sales = aggregate(x=train1$Weekly_Sales, by=list(Group.Date=train1$Date), FUN=sum)
    lines(sales, col=store_num)
    abline(lm(sales$x ~ sales$Group.Date), lwd=0.65, lty=2)
  }
  points(x=sales$Group.Date, y=Avg_Weekly_Sales, ylim=c(0,3800000), pch=20)
  dev.off()
}
plotter()

