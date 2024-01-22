---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
unzip("activity.zip")
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date)
```

## What is mean total number of steps taken per day?

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
steps.per.day <- activity %>% group_by(date) %>% summarise(sum.steps = sum(steps))

hist(steps.per.day$sum.steps, main = "Histogram of total numbers of steps taken each day", xlab = "Steps", ylim = c(0, 30), col = "purple")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
mean(steps.per.day$sum.steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(steps.per.day$sum.steps, na.rm = TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
steps.per.interval <- activity %>% group_by(interval) %>% summarise(mean.steps = mean(steps, na.rm = TRUE))

ggplot(steps.per.interval, aes(x = interval, y = mean.steps)) + geom_line(col = "darkblue") + xlab("5-minute interval") + ylab("Average number of daily steps") + ggtitle("Average daily activity pattern per 5-minute interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
steps.per.interval[which.max(steps.per.interval$mean.steps),]
```

```
## # A tibble: 1 × 2
##   interval mean.steps
##      <int>      <dbl>
## 1      835       206.
```

## Imputing missing values
Calculate and report the total number of missing values in the data set

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```
The strategy for filling in all the missing values is replacing the missing values for the mean of its 5-minute interval and create a new data set

```r
activity.non.NA <- activity
  for (i in 1:17568) {
    if (is.na(activity$steps[i])) {
      activity.non.NA$steps[i] <- steps.per.interval$mean.steps[activity.non.NA$interval[i] == steps.per.interval$interval]
    }
  }
```

```r
steps.per.day <- activity.non.NA %>% group_by(date) %>% summarise(sum.steps = sum(steps))

hist(steps.per.day$sum.steps, main = "Histogram of total numbers of steps taken each day", xlab = "Steps", ylim = c(0, 40), col = "pink")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
mean(steps.per.day$sum.steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(steps.per.day$sum.steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?
First, we create a new factor variable in the data set with two levels - “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
activity.non.NA$date <- as.Date(strptime(activity.non.NA$date, format = "%Y-%m-%d"))

activity.non.NA$type_of_day <- sapply(activity.non.NA$date, function(x) {
  if (weekdays(x) == "zaterdag" | weekdays(x) == "zondag")
    (y <- "Weekend")
  else (y <- "Weekday")
})
```
Now we make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

```r
activity.by.day <- aggregate(steps ~ interval + type_of_day, activity.non.NA, mean, na.rm = TRUE)

ggplot(activity.by.day, aes(x = interval, y = steps, col = type_of_day)) + geom_line() + xlab("5-minute interval") + ylab("Average number of steps") + ggtitle("Average daily steps: weekdays versus weekend") + facet_wrap(~type_of_day, nrow = 2, ncol = 1) + scale_color_discrete(name = "Type of day")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
