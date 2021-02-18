## Load ggplot2 library
library(ggplot2)
## Set working directory
setwd("/Users/lee_saxton/Documents/Data Science Specialisation/05 Reproducible Research/Week 2/Week 2 Assignment/RepData_Week2_Project/")
## Check for data directory and create if necessary
if (!file.exists("data")) {
  dir.create("data")
}
## Unzip data
unzip("./activity.zip", exdir="./data")
## Read csv file
activity <- read.csv("./data/activity.csv")
## Get structure of activity
str(activity)
## Convert date to POSIXct format
activity$date <- as.POSIXct(activity$date, "%Y-%m-%d")
## Create "weekday" column in data frame
activity$weekday <- weekdays(activity$date)
## Calculate total number of steps per day using aggregate function using ~
## notation to indicate as a function of, and set na.rm=TRUE to ignore NA values
stepsperday <- aggregate(steps ~ date, activity, FUN=sum, na.rm=TRUE)
## Create histogram of total number of steps taken per day
png("./figures/figure1.png", width=480, height=480)
hist(stepsperday$steps, xlab="Total Steps Taken In a Day", main="Histogram of Total Steps Taken In A Day", breaks=10, col="red" )
dev.off()
## Calculate mean steps per day
mean(stepsperday$step)
## Calculate median steps per day
median(stepsperday$steps)
## Calculate the average daily activity pattern, again using aggregate
stepsperinterval <- aggregate(steps ~ interval, activity, FUN=mean, na.rm=TRUE)
## Create plot line plot of average steps per interval
png("./figures/figure2.png", width=480, height=480)
with(stepsperinterval,plot(steps ~ interval, type="l", col="red", lwd=2, xlab="Interval", ylab="Steps", main="Average Steps Taken Per Interval"))
dev.off()
## Calculate and display interval with the average maximum number of steps
stepsperinterval[which.max(stepsperinterval$steps),]
## Calculate and report the total number of missing values in the dataset
missing <- sum(is.na(activity$steps))
missing
## Filling missing interval values with the mean value for that interval, previously calculate
activity_fill <- transform(activity, steps=ifelse(is.na(activity$steps), yes=stepsperinterval$steps, no=activity$steps))
## Re-create histogram of total number of steps taken per day and re-calculate 
## mean and median
stepsperdayfill <- aggregate(steps ~ date, activity_fill, FUN=sum, na.rm=TRUE)
png("./figures/figure3.png", width=480, height=480)
hist(stepsperdayfill$steps, xlab="Total Steps Taken In a Day", main="Histogram of Total Steps Taken In A Day - Post Infill", breaks=10, col="red" )
dev.off()
## Calculate mean steps per day post infill
mean(stepsperdayfill$step)
## Calculate median steps per day post infill
median(stepsperdayfill$steps)
## Create interval average steps for weekend days and weekdays
## Create third variable called type for weekday or weekend
activity$daytype <- factor(ifelse(activity$weekday %in% c("Saturday", "Sunday"), "weekend", "weekday"))
## Now create weekend or weekday interval averages
stepsperintervalpertype <- aggregate(steps ~ interval + daytype, activity, FUN=mean, na.rm=TRUE)
## Now create panel plot
ggplot(stepsperintervalpertype, aes(x=interval, y=steps))+
geom_line(colour="lightblue") +
labs(x="Interval", y="Number of steps") +
facet_wrap(~daytype , ncol=1, nrow=2) +
theme_bw ()
ggsave("./figures/figure4.png")