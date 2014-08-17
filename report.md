Reproducible Research Report
============================================

## Peer Assignement Week 1
This ist the report of my approach how I processed and analyzed the data for the first peer assignment of COursera course 'Reproducible Research'.

## Loading and processing the data 

The used is available under the following [link] (https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip). After the data is loaded you need to unzip the archive file and load the contained csv-file 'activity.csv'


```r
act<-read.csv("activity.csv")
dim(act)
```

```
## [1] 17568     3
```

```r
summary(act)
```

```
##      steps               date          interval   
##  Min.   :  0.0   2012-10-01:  288   Min.   :   0  
##  1st Qu.:  0.0   2012-10-02:  288   1st Qu.: 589  
##  Median :  0.0   2012-10-03:  288   Median :1178  
##  Mean   : 37.4   2012-10-04:  288   Mean   :1178  
##  3rd Qu.: 12.0   2012-10-05:  288   3rd Qu.:1766  
##  Max.   :806.0   2012-10-06:  288   Max.   :2355  
##  NA's   :2304    (Other)   :15840
```

```r
head(act)
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

1 Make a histogram of the total number of steps taken each day

```r
t_st<-tapply(act$steps,act$date,FUN=function(x) sum(x,na.rm=TRUE))
plot(t_st,col="red",type="S",xaxt="n",bg="blue",main="Total number of steps",
                xlab="Date",ylab="Totals")
axis(1, at=seq(1, 61, by=1),labels=names(t_st))
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
day_totals<-data.frame(day=names(t_st),total_steps=t_st)
```

2 Calculate and report the mean and median total number of steps taken per day

```r
mean_steps<-mean(day_totals$total_steps,na.rm=TRUE)
median_steps<-median(day_totals$total_steps,na.rm=TRUE)
```

The mean total number of steps taken per day is 9354.2295and the median is equal to 10395.
