---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and Preprocessing data :


```r
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
fileName <- "./data/activity.csv"

if (!file.exists("data")) {
  dir.create("data")
}
if(!file.exists(fileName)) {
  download.file(url, destfile = "repdata-data-activity.zip")
  unzip("repdata-data-activity.zip", exdir = "./data")
  file.remove("repdata-data-activity.zip")
} 

if(!exists("Activity")) {
  Activity <- read.csv(fileName)
}
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day

```r
totalSteps <- aggregate(steps ~ date, Activity, sum)
```

2. Make a histogram of the total number of steps taken each day

```r
library(ggplot2)

ggplot(totalSteps, aes(x = steps)) + geom_histogram(binwidth = 500) + 
  labs(x = "Total no of steps each day", 
       y="Freqyuency", 
       title = "Total no of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->



3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean(totalSteps$steps)
```

```
## [1] 10766.19
```

```r
median(totalSteps$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e.type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
avgSteps <- aggregate(steps ~ interval, Activity, mean)

ggplot(avgSteps, aes(x=interval,y=steps)) + geom_line() + 
  labs(x = "Interval", 
       y = "Averaged steps across all days", 
       title="Time series plot of the 5 minute interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->



2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
avgSteps$interval[which.max(avgSteps$steps)]
```

```
## [1] 835
```


## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(Activity$steps))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
meanValues <- aggregate(steps ~ date, Activity, mean, na.action = na.pass)
minOfMeanValues <- min(meanValues$steps, na.rm = TRUE)
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
ActivityNewDT <- Activity
ActivityNewDT$steps[is.na(ActivityNewDT$steps)] <- minOfMeanValues
```

4. Make a histogram of the total number of steps taken each day

```r
totalStepsForNewDT <- aggregate(steps ~ date, ActivityNewDT, sum)

ggplot(totalStepsForNewDT, aes(steps)) + geom_histogram(binwidth = 500) + 
  labs(x = "Total no of steps each day", 
       title = "Total no of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->



5. Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
library(knitr)
```

```
## Warning: package 'knitr' was built under R version 3.4.3
```

```r
dtf <- data.frame("type" = c("Before Imputting Missing Values", "After Imputting Missing Values"),
                  "Mean Steps"= c(mean(totalSteps$steps), mean(totalStepsForNewDT$steps)),
                  "Median Steps" = c(median(totalSteps$steps), median(totalStepsForNewDT$steps)))
kable(dtf)
```



type                               Mean.Steps   Median.Steps
--------------------------------  -----------  -------------
Before Imputting Missing Values     10766.189          10765
After Imputting Missing Values       9359.607          10395

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
ActivityNewDT$type <- with(ActivityNewDT, ifelse(weekdays(as.Date(date), abbr = TRUE) %in% c("Sat", "Sun"), "weekend", "weekday"))
```

2. Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
avgStepsNewDT <- aggregate(steps ~ type+interval, ActivityNewDT, sum)

ggplot(avgStepsNewDT, aes(x=interval,y=steps, color=type)) + geom_line() + 
  labs(x = "Interval", y = "Number of steps", 
       title="Activity patterns between weekdays and weekends") + facet_grid(type~.)
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->



