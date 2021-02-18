---
title: "Reproducible Research: Peer Assessment 1"
author: "Lee Saxton"
date: "February 18, 2021"
output: 
  html_document:
     keep_md: true
---

## Loading and preprocessing the data
Load libraries  
Set some options for displaying numbers  
Set working directory, check for data directory and create if necessary, then unzip data file  

```r
library(ggplot2)
library(png)
library(knitr)
options(digits=5, scipen=10000)
if (!file.exists("data")) {
  dir.create("data")
}
unzip("./activity.zip", exdir="./data")
activity <- read.csv("./data/activity.csv")
```
Display some basic information about the dataset

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
tail(activity)
```

```
##       steps       date interval
## 17563    NA 2012-11-30     2330
## 17564    NA 2012-11-30     2335
## 17565    NA 2012-11-30     2340
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
```
## What is mean total number of steps taken per day?
Calculate mean steps per day using aggregate function  
First need to convert date to POSIXct format

```r
activity$date <- as.POSIXct(activity$date, "%Y-%m-%d")
stepsperday <- aggregate(steps ~ date, activity, FUN=sum, na.rm=TRUE)
```
Create a histogram of the total number of steps in a day

```r
png("./figures/figure1.png", width=720, height=720)
hist(stepsperday$steps, xlab="Total Steps Taken In a Day", main="Histogram of Total Steps Taken In A Day", breaks=10, col="red", cex.lab=1.5, cex.axis=1.5, cex.main=2 )
dev.off()
```

```r
include_graphics("./figures/figure1.png")
```

<img src="./figures/figure1.png" width="720" />
  
Report the mean number of steps in a day  

```r
mean_orig <- mean(stepsperday$steps)
mean_orig
```

```
## [1] 10766
```
Report the median steps in a day  

```r
median_orig <- as.numeric(median(stepsperday$steps))
median_orig
```

```
## [1] 10765
```
## What is the average daily activity pattern?
Calculate the average daily activity pattern, again using aggregate

```r
stepsperinterval <- aggregate(steps ~ interval, activity, FUN=mean, na.rm=TRUE)
```
Create a line plot of the average number of steps per 5 minute interval

```r
png("./figures/figure2.png", width=720, height=720)
with(stepsperinterval,plot(steps ~ interval, type="l", col="red", lwd=2, xlab="Interval", ylab="Steps", main="Average Steps Taken Per Interval", cex.lab=1.5, cex.axis=1.5, cex.main=2))
dev.off()
```

```r
include_graphics("./figures/figure2.png")
```

<img src="./figures/figure2.png" width="720" />
  
Show the 5 minute interval across all days that has the highest number of steps  

```r
max_steps <- stepsperinterval[which.max(stepsperinterval$steps),]
max_steps
```

```
##     interval  steps
## 104      835 206.17
```
## Imputing missing values
There are a number of intervals where no measurments are available, indicated indicated by NA in the steps column.  
We will fill these values with the average number of steps in that interval for the whole dataset, as calculated above  
  
First calculate total number of missing values

```r
tot_val <-nrow(activity)
tot_missing <- sum(is.na(activity$steps))
pcnt_missing <- (tot_missing / tot_val)*100
```
The total number of measurements is 17568  
The number of missing measurement is 2304  
The percentage of missing measurements is 13.11475%  
  
We now fill in the missing measurements with the mean for that interval calculated for the entire dataset  

```r
activity_fill <- transform(activity, steps=ifelse(is.na(activity$steps), yes=stepsperinterval$steps, no=activity$steps))
```
Check that we have no missing values

```r
sum(is.na(activity_fill$steps))
```

```
## [1] 0
```
Now we create a histogram of the total steps per day after imputing the missing values  

```r
stepsperdayfill <- aggregate(steps ~ date, activity_fill, FUN=sum, na.rm=TRUE)
png("./figures/figure3.png", width=720, height=720)
hist(stepsperdayfill$steps, xlab="Total Steps Taken In a Day", main="Histogram of Total Steps Taken In A Day - Post Infill", breaks=10, col="red", cex.lab=1.5, cex.axis=1.5, cex.main=2)
dev.off()
```

```r
include_graphics("./figures/figure3.png")
```

<img src="./figures/figure3.png" width="720" />
  
We now re-calculate the mean and median values of steps per day and compare to original values  

```r
mean_fill <- mean(stepsperdayfill$steps)
median_fill <- median(stepsperdayfill$steps)
```

The original (pre-imputing) mean steps per day was 10766.18868  
The post imputing mean steps per day is 10766.18868   
The original (pre-imputing) median steps per day is 10765  
The post imputing median steps per day is 10766.18868  

## Are there differences in activity patterns between weekdays and weekends?
Calculate weekday and then add a daytype column to activity dataframe to identify days belonging to weekday or weekend  

```r
activity$weekday <- weekdays(activity$date)
activity$daytype <- factor(ifelse(activity$weekday %in% c("Saturday", "Sunday"), "weekend", "weekday"))
head(activity)
```

```
##   steps       date interval weekday daytype
## 1    NA 2012-10-01        0  Monday weekday
## 2    NA 2012-10-01        5  Monday weekday
## 3    NA 2012-10-01       10  Monday weekday
## 4    NA 2012-10-01       15  Monday weekday
## 5    NA 2012-10-01       20  Monday weekday
## 6    NA 2012-10-01       25  Monday weekday
```

```r
tail(activity)
```

```
##       steps       date interval weekday daytype
## 17563    NA 2012-11-30     2330  Friday weekday
## 17564    NA 2012-11-30     2335  Friday weekday
## 17565    NA 2012-11-30     2340  Friday weekday
## 17566    NA 2012-11-30     2345  Friday weekday
## 17567    NA 2012-11-30     2350  Friday weekday
## 17568    NA 2012-11-30     2355  Friday weekday
```
Calculate average steps taken in each 5 mintue interval for weekend days and week days  

```r
stepsperintervalpertype <- aggregate(steps ~ interval + daytype, activity, FUN=mean, na.rm=TRUE)
```
Now create the panel plot using facets on the newly created daytype factor to differentiate between panels  

```r
ggplot(stepsperintervalpertype, aes(x=interval, y=steps)) +
geom_line(colour="lightblue") +
labs(x="Interval", y="Number of steps") +
facet_wrap(~daytype , ncol=1, nrow=2) +
theme_bw ()
```

![](PA1_template_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

```r
ggsave("./figures/figure4.png")
```

```
## Saving 7 x 5 in image
```
**This concludes the assignment**




