
---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
library(readr)
## auto unzip and read csv
activity <- read_csv("activity.zip")
```

```
## Rows: 17568 Columns: 3
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## dbl  (2): steps, interval
## date (1): date
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```


## What is mean total number of steps taken per day?

```r
stepsPerDay <- aggregate(steps~date, 
                         data = activity, 
                         FUN = sum, 
                         na.rm = TRUE
                         )
hist(stepsPerDay$steps,breaks = 10, main = "Histogram of total steps per day", xlab = "steps per day")
```

![](tmp_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
mn <- mean(stepsPerDay$steps)
md <- median(stepsPerDay$steps)
mn
```

```
## [1] 10766.19
```

```r
md
```

```
## [1] 10765
```

```r
ans <- paste("The mean total number of steps is",mn,"steps.\nThe median total number of steps is",md,"steps.")
print(ans)
```

```
## [1] "The mean total number of steps is 10766.1886792453 steps.\nThe median total number of steps is 10765 steps."
```
The mean total number of steps is $1.0766189\times 10^{4}$ steps.
The median total number of steps is $1.0765\times 10^{4}$ steps.

## What is the average daily activity pattern?
see plot...

```r
meanStepsPerIntv <- aggregate(steps~interval, 
                              data = activity, 
                              FUN = mean, 
                              na.rm =TRUE
                              ) 
plot(steps~interval, data = meanStepsPerIntv, type = 'l', main = "average step across interval")
```

![](tmp_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
## time of day people walk a lot?
walkalot <- meanStepsPerIntv$interval[which.max(meanStepsPerIntv$steps)]
hh <- round(walkalot/60,0)
walkalot
```

```
## [1] 835
```

```r
ans <- paste("people walk the most steps at interval",walkalot,". This is about",hh,": 00 Time.")
print(ans)
```

```
## [1] "people walk the most steps at interval 835 . This is about 14 : 00 Time."
```
people walk a lot at Interval 835. This is roughly 14:00 Time.

## Imputing missing values
### impute strategy:
First let's decide how to impute the data.

1. find which index need to be imputed
2. find interval of that index
3. get the mean of step on that interval across every day
4. impute the steps in that index

```r
## will impute with mean of each interval, mean taken across all days
NAs <- !complete.cases(activity) 
NAindices <- which(NAs)

imputedActivity <- activity
for (i in NAindices){
    missingInterval <- activity$interval[i]
    imputedActivity$steps[i] <- meanStepsPerIntv$steps[meanStepsPerIntv$interval == missingInterval]
}
## count errors before and after
errorCount1 <- sum(NAs)
errorCount2 <- sum(!complete.cases(imputedActivity))
ans <- paste("There was",errorCount1,"errors before imputing and",errorCount2,"error after imputing.")
print(ans)
```

```
## [1] "There was 2304 errors before imputing and 0 error after imputing."
```
there was 2304 errors before imputing. After impute there is 0 error.

## Are there differences in activity patterns between weekdays and weekends?

```r
isweekday <- function(dte){
    ifelse(
        weekdays(dte) %in% c("Sunday", "Saturday"),
        FALSE,
        TRUE)
    }
daytype <- as.factor(
    ifelse(
        isweekday(imputedActivity$date),
        "weekday",
        "weekend"
        )
    )
imputedActivity <- cbind(imputedActivity, daytype)

agg<-aggregate(steps~interval+daytype,data = imputedActivity, FUN = sum )
par(mfrow=c(2,1),mar=c(4,2,1,1))
plot(steps~interval, 
            data = subset(agg,daytype == "weekday"), 
            type='l',
            main = "weekday"
            )
plot(steps~interval,
            data = subset(agg,daytype == "weekend"), 
            type='l',
            main = "weekend")
```

![](tmp_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
There are differences in number of steps and whether it is weekday or weekend. people walk more on weekday than on weekend (notice the different y-scale). Also, number of steps shoot up on earlier interval on weekday than on weekend. This tells how people getting to work on weekday and rest on weekend affect our data.  
