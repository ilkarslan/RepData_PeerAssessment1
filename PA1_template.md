# Reproducible Research: Peer Assignment 1


## Loading and preprocessing the data

Load the data:


```r
activity <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

Make a histogram of the total number of steps taken each day:


```r
library(ggplot2)
steps <- tapply(activity$steps, activity$date, FUN = sum, na.rm = TRUE)
qplot(steps, binwidth = 1000, xlab = "total number of steps taken each day")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 

Calculate and report the mean and median total number of steps taken per day:



```r
mean(steps, na.rm = TRUE)
```

```
## [1] 9354
```

```r
median(steps, na.rm=TRUE)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis):


```r
library(ggplot2)
avg <- aggregate(x = list(steps=activity$steps), by = list(interval=activity$interval),FUN = mean, na.rm =TRUE)
ggplot(data = avg, aes(x=interval, y = steps)) +
     geom_line() +
    xlab("5 min interval") +
    ylab("average number of steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
avg[which.max(avg$steps),]
```

```
##     interval steps
## 104      835 206.2
```


## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
miss <- is.na(activity$steps)
table(miss)
```

```
## miss
## FALSE  TRUE 
## 15264  2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

All of the missing values are filled in with mean value for that 5-minute
interval.


```r
fill <- function(steps, interval){
      filled <- NA
      if(!is.na(steps))
            filled <- c(steps)
      else
            filled <- (avg[avg$interval == interval, "steps"])
      return(filled)
}
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
filled.data <- activity
filled.data$steps <- mapply(fill, filled.data$steps, filled.data$interval)
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(total.steps, binwidth= 1000, xlab = "total number of steps taken each daya")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

```r
mean(total.steps)
```

```
## [1] 10766
```

```r
median(total.steps)
```

```
## [1] 10766
```

Mean and median values are higher after imputing missing data. The reason is that in the original data, there are some days with `steps` values `NA` for any `interval`. The total number of steps taken in such days are set to 0s by default. However, after replacing missing `steps` values with the mean `steps`
of associated `interval` value, these 0 values are removed from the histogram of total number of steps taken each day.


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=weekday.or.weekend)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:


```r
averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
    xlab("5-minute interval") + ylab("Number of steps")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

## 
