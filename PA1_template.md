---
title: "Reproducible Research: Peer Assessment 1"
author: "David R. Cote"
date: "6/23/2020"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data

Loading Libraries:

```r
library(data.table)
library(ggplot2)
library(knitr)
```

Loading Data:

```r
fileUrl <-  "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,destfile=paste0(getwd(),'/repdata%2Fdata%2Factivity.zip'), method = "curl")
unzip("repdata%2Fdata%2Factivity.zip")
```

Create Data Table:

```r
ActivityData <- data.table::fread(input = "activity.csv")
```

Format Dates in Table:

```r
ActivityData[, date := as.POSIXct(date, format = "%Y-%m-%d")]
```

## What is mean total number of steps taken per day?

### 1. Calculate the total number of steps taken per day  


```r
Daily_Steps <- ActivityData[, c(lapply(.SD, sum, na.rm = FALSE)), .SDcols = c("steps"), by = .(date)] 
barplot(Daily_Steps$steps,xlab="Date",ylab="Steps")  
```

![](PA1_template_files/figure-html/dailysteps-1.png)<!-- -->

### 2. Make a histogram of the total number of steps taken each day  


```r
hist(Daily_Steps$steps,xlab="Steps",breaks=5,main="Histogram of Steps per Day")
```

![](PA1_template_files/dailystepshist-1.png)<!-- -->
  
### 3. Calculate and report the mean and median of the total number of steps taken per day  

```r
Daily_Steps[, .(Mean_Steps = mean(steps, na.rm = TRUE), Median_Steps = median(steps, na.rm = TRUE))]
```

```
##    Mean_Steps Median_Steps
## 1:   10766.19        10765
```

## What is the average daily activity pattern?

### 1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)  

```r
Interval_Steps <- ActivityData[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval)]
plot(Interval_Steps$interval,Interval_Steps$steps, type="l",xlab="Time",ylab="Average Steps")
```

![](PA1_template_files/figure-html/intervalsteps-1.png)<!-- -->
  
### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
Interval_Steps[steps == max(steps), .(max_interval = interval)]
```

```
##    max_interval
## 1:          835
```

## Imputing missing values

### 1. Calculate and report the total number of missing values in the dataset 


```r
ActivityData[is.na(steps), .N ]
```

```
## [1] 2304
```

### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

#### Fill null step entries with median steps for the interval

### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
ActivityDataNoNull <- ActivityData
ActivityDataNoNull[is.na(steps), "steps"] <- ActivityDataNoNull[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
Daily_Steps_No_Null <- ActivityDataNoNull[, c(lapply(.SD, sum, na.rm = FALSE)), .SDcols = c("steps"), by = .(date)] 
Interval_Steps_No_Null <- ActivityDataNoNull[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval)]
barplot(Daily_Steps_No_Null$steps,xlab="Date",ylab="Steps")  
```

![](PA1_template_files/figure-html/newsetnonulls-1.png)<!-- -->

### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?  


```r
Daily_Steps_No_Null <- ActivityDataNoNull[, c(lapply(.SD, sum, na.rm = FALSE)), .SDcols = c("steps"), by = .(date)] 
hist(Daily_Steps_No_Null$steps,xlab="Steps",breaks=5,main="Histogram of Steps per Day")
```

![](PA1_template_files/figure-html/dailystepshistnonull-1.png)<!-- -->

```r
Daily_Steps_No_Null[, .(Mean_Steps_No_Null = mean(steps, na.rm = TRUE), Median_Steps_No_Null = median(steps, na.rm = TRUE))]
```

```
##    Mean_Steps_No_Null Median_Steps_No_Null
## 1:            9354.23                10395
```
  
#### Imputing null values with median interval values appears to primarily fill out the lower end of the histogram, and lowers the mean and median of the step counts.  

## Are there differences in activity patterns between weekdays and weekends?

### 1. Create a new factor variable in the dataset with two levels Weekday and Weekend indicating whether a given date is a weekday or on the weekend.


```r
ActivityData[, `Weekday`:= weekdays(x = date)]
ActivityData[grepl(pattern = "Monday|Tuesday|Wednesday|Thursday|Friday", x = `Weekday`), "Weekdayend"] <- "Weekday"
ActivityData[grepl(pattern = "Saturday|Sunday", x = `Weekday`), "Weekdayend"] <- "Weekend"
ActivityData[, `Weekdayend` := as.factor(`Weekdayend`)]
ActivityDataNoNull[, `Weekday`:= weekdays(x = date)]
ActivityDataNoNull[grepl(pattern = "Monday|Tuesday|Wednesday|Thursday|Friday", x = `Weekday`), "Weekdayend"] <- "Weekday"
ActivityDataNoNull[grepl(pattern = "Saturday|Sunday", x = `Weekday`), "Weekdayend"] <- "Weekend"
ActivityDataNoNull[, `Weekdayend` := as.factor(`Weekdayend`)]
Daily_Steps_No_Null <- ActivityDataNoNull[, c(lapply(.SD, sum, na.rm = FALSE)), .SDcols = c("steps"), by = .(date)] 
```

### Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
Interval_Steps_No_Null <- ActivityDataNoNull[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval, `Weekdayend`)] 
ggplot(Interval_Steps_No_Null , aes(x = interval , y = steps, color=`Weekdayend`)) + geom_line() + labs(title = "Weekday vs. Weekend", x = "Interval", y = "Steps") + facet_wrap(~`Weekdayend` , ncol = 1, nrow=2)
```

![](PA1_template_files/figure-html/weekdayendplot-1.png)<!-- -->
  
#### Weekday activity seems dominated by a burst of early morning activity followed by lesser activity for the duration of the day. Weekends have a more moderate level of activity throughout the waking hours.
