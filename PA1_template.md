# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
activity <- read.csv(file = unz("activity.zip", "activity.csv"), header = TRUE, sep = ",", quote = "\"")
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



## What is mean total number of steps taken per day?

```r
activity2 <- subset(activity, !is.na(activity$steps))
steps <- tapply(activity2$steps, activity2$date, sum)
hist(steps)
abline(v = mean(steps, na.rm = TRUE), col = "blue", lwd = 2)
abline(v = median(steps, na.rm = TRUE), col = "green", lwd = 2)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->



## What is the average daily activity pattern?

```r
steps_interval <- tapply(activity2$steps, activity2$interval, mean)
plot(unique(activity2$interval), steps_interval, type="l", ylab = "Average number of steps", xlab = "5 min intervals", main= "Average daily activity pattern")
abline(v = 835, col = "blue", lwd = 2)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
The interval number 104 (containing the 835th minute) is the one contaning the maximum number of steps.


## Imputing missing values

```r
missing_values <- subset(activity, is.na(steps))
nrow(missing_values)
```

```
## [1] 2304
```

```r
activity2 <- activity
activity2$steps[is.na(activity2$steps)] <- steps_interval[as.character(activity2$interval[is.na(activity2$steps)])]

steps <- tapply(activity2$steps, activity2$date, sum)
hist(steps)
abline(v = mean(steps, na.rm = TRUE), col = "blue", lwd = 2)
abline(v = median(steps, na.rm = TRUE), col = "green", lwd = 2)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
There are 2304 missing values.
We fill them with the mean for that 5-minute interval.     
The values we get does not differ from those of the first part of the assignment.     
There is no impact on the estimates of the total daily number of steps.

## Are there differences in activity patterns between weekdays and weekends?

```r
activity2$weekend <- weekdays(as.Date(activity2$date))
activity2$weekend[activity2$weekend %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")] <- "weekday"
activity2$weekend[activity2$weekend %in% c("Saturday", "Sunday")] <- "weekend"
activity2$weekend <- as.factor(activity2$weekend)

steps_interval_weekday <- tapply(activity2$steps[activity2$weekend == "weekday"], activity2$interval[activity2$weekend == "weekday"], mean)
steps_interval_weekend <- tapply(activity2$steps[activity2$weekend == "weekend"], activity2$interval[activity2$weekend == "weekend"], mean)

par(mfrow=c(2,1))
plot(unique(activity2$interval[activity2$weekend == "weekend"]), steps_interval_weekend, type="l", ylab = "Average number of steps", xlab = "5 min intervals", main= "weekend")
plot(unique(activity2$interval[activity2$weekend == "weekday"]), steps_interval_weekday, type="l", ylab = "Average number of steps", xlab = "5 min intervals", main= "weekday")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

