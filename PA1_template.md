# Reproducible Research: Peer Assessment 1
Author: Alexander Tsvetkov

## Loading and preprocessing the data

```r
file <- unzip("activity.zip")
data <- read.csv(file, colClasses = c(steps = "numeric", date = "Date", interval = "integer"))
```



## What is mean total number of steps taken per day?

Here is a histogram of total number of steps per day

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 


The average and the median are the following:

```r
mean(data$steps, na.rm = T)
```

```
## [1] 37.38
```

```r
median(data$steps, na.rm = T)
```

```
## [1] 0
```


## What is the average daily activity pattern?

The following plot shows the number of steps averaged across the dates for each interval:

```r
means <- aggregate(steps ~ interval, data = data, FUN = mean)
plot(means)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


Let's find the interval with the highest number of steps:

```r
res <- means[means$steps == max(means$steps), ]
res
```

```
##     interval steps
## 104      835 206.2
```


So the interval that contains the maximum number of steps is 835

## Imputing missing values

The total number of missing values in the dataset:

```r
nrow(data[is.na(data$steps), ])
```

```
## [1] 2304
```


We will use the average value for that interval to fill the missing values. Let's create a new column with those values filled:

```r
data$steps_filled <- apply(data, 1, function(row) means[means$interval == as.integer(row["interval"]), 
    ][["steps"]])
```


Now let's recalculate the mean/median and plot the histogram for the dataset with those filled values:

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 



```r
mean(data$steps_filled, na.rm = T)
```

```
## [1] 37.38
```

```r
median(data$steps_filled, na.rm = T)
```

```
## [1] 34.11
```


As we see, assuming the missing values does not change the average, but does affect the median.

## Are there differences in activity patterns between weekdays and weekends?

Let's create a two-level factor variable to distinguish weekdays and weekends:

```r
daytype <- factor(c("weekday", "weekend"))
```


Then create a function to classify the dates into those categories:

```r
classify <- function(x) if (weekdays(x) == "Saturday" | weekdays(x) == "Sunday") daytype[2] else daytype[1]
```


Now let's add a new column indicating the type of the day:

```r
data$daytype <- apply(data, 1, function(row) classify(as.Date(row[["date"]])))
```


Here are the patterns of steps against interval distribution for weekdays and weekends separately:


```r
data_weekday <- data[data$daytype == daytype[1], ]
data_weekend <- data[data$daytype == daytype[2], ]

par(mfrow = c(2, 1))
plot(data_weekday$interval, data_weekday$steps, type = "l", main = "weekday", 
    xlab = "Interval", ylab = "Number of steps")
plot(data_weekend$interval, data_weekend$steps, type = "l", main = "weekend", 
    xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 

