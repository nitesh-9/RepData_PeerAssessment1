---
title: "Reproducible Research: Peer Assessment 1"
output: 
    html_document:
        keep_md: true
---



```r
knitr::opts_chunk$set(echo = TRUE, warning = FALSE)
```


## Loading and preprocessing the data

```r
# read data
if (!file.exists("activity.csv")) {
  unzip("activity.zip")
}


data<- read.csv("activity.csv")

summary(data)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

```r
#Remove any NA's within the Data
data2 <- na.omit(data)

#Change date column into date format
data2$date <- as.Date(data2$date)


summary(data2)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-02   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-29   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-30   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-16   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-29   Max.   :2355.0
```


## What is mean total number of steps taken per day?


```r
data3 <- aggregate(steps ~ date, data2, sum)
hist(data3$steps
     ,breaks = "Sturges"
     , xlab = "Number of Steps"
     , main = "Histogram of Daily Number of Steps"
     ,col = "light blue")
```

![](PA1_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
mean(data3$steps)
```

```
## [1] 10766.19
```

```r
median(data3$steps)
```

```
## [1] 10765
```

The mean number of steps per day = 1.0766\times 10^{4}

The median number of steps per day = 10765


## What is the average daily activity pattern?


```r
#The average number of steps per interval
stepsbyint <- aggregate(steps ~ interval, data2, mean)

plot(stepsbyint$interval
     , stepsbyint$steps
     , type ="l"
     , col = "dark blue"
     , ylab = "Number of Steps"
     , xlab = "Interval"
     , main = "Average Number of Steps by Interval")
```

![](PA1_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
stepsbyint$interval[which.max(stepsbyint$steps)]
```

```
## [1] 835
```

```r
round(stepsbyint$steps[which.max(stepsbyint$steps)])
```

```
## [1] 206
```
The interval which contains the max number of steps = 835 with 206 steps

## Imputing missing values


```r
sum(is.na(data))
```

```
## [1] 2304
```
The number of NA's in the original data set = 2304

Replacing the NA's with the mean number of steps


```r
data4 <- data
data4$date <- as.Date(data4$date)
#Replace the NA's with mean number of steps
data4$steps[is.na(data4$steps)] <- mean(stepsbyint$steps)

summary(data4)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 37.38   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0
```

```r
summary(data)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

Comparing the summaries, it can be seen that the NA's have been removed

Histogram with imputed values:


```r
data5 <- aggregate(steps ~ date, data4, sum)
hist(data5$steps
     ,breaks = "Sturges"
     , xlab = "Number of Steps"
     , main = "Histogram of Daily Number of Steps"
     ,col = "light blue")
```

![](PA1_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?

Separating the data into weekends and weekdays:


```r
day <- weekdays(data4$date)
daytype <- ifelse(day == "Saturday" | day == "Sunday", "Weekend", "Weekday")
data6 <- cbind(daytype, data4)

#Plot to show weekday vs weekend steps

stepsbydaytype <- aggregate(steps ~ interval + daytype, data6, mean)

library(ggplot2)

ggplot(stepsbydaytype, aes(interval, steps)) +
    geom_line(stat = "identity", aes(colour = daytype)) +
    facet_grid(daytype ~ ., scales="fixed", space="fixed")
```

![](PA1_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
