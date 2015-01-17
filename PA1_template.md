

Peer assessment 1
==============================

## Loading and preprocessing the data

```r
data <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

```r
sum_per_day <- aggregate(data$steps, by=list(data$date), FUN=sum, na.rm=T)
hist(sum_per_day$x, main="Total steps per day", xlab="Total steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 


```r
mean_total <- mean(sum_per_day$x)
median_total <- median(sum_per_day$x)
```
The mean of total steps per day is 9354.2295082.
The median of total steps per day is 10395.


## What is the average daily activity pattern?

```r
mean_per_interval <- aggregate(data$steps, by=list(data$interval), FUN=mean, na.rm=T)
plot(mean_per_interval, type="l", xlab="Interval", ylab="Steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
max_interval <- mean_per_interval[which.max(mean_per_interval$x), 1]
```
The interval which contains the maximum number of steps across all the days on average is 835.


## Imputing missing values

```r
total_na <- sum(is.na(data))

data2 <- data.frame(data)
for(i in 1:nrow(data)){
    if (is.na(data[i,])[1]){
        int <- data[i,3]
        data2[i,1] = mean_per_interval[mean_per_interval[,1] == int, 2]
    }
}

sum_per_day2 <- aggregate(data2$steps, by=list(data2$date), FUN=sum, na.rm=T)
hist(sum_per_day2$x, main="Total steps per day", xlab="Total steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

Total NA lines: 2304. 

After the substitution, the distribution became "more normal". That is not surprising because we have changed the data in the most expected way for each missed value (we used the mean values for the corresponding intervals instead NA).

## Are there differences in activity patterns between weekdays and weekends?
Adding the column to data2:

```r
Sys.setlocale("LC_TIME","en_US")
x <- rep("weekday", nrow(data))
x[which(weekdays(as.Date(data2$date)) %in% c("Saturday", "Sunday"))] = "weekend"
data2 <- cbind(data2, as.factor(x))
colnames(data2)[4] <- "weekend"
```

Adding a temporary data frame `mean_per_interval` with aggregated average values

```r
mean_per_interval <- aggregate(data2$steps, by=list(data2$interval, data2$weekend), FUN=mean, na.rm=T)
colnames(mean_per_interval) <- c("interval", "weekend", "steps")
```

Plotting the results.

```r
library(lattice)
xyplot(steps ~ interval | weekend, data=mean_per_interval, type="l", layout=c(1,2))
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 
