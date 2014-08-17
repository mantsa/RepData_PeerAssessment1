Reproducible Research Report
============================================

# Reproducible Research: Peer Assessment 1
This ist the report of my approach how I processed and analyzed the data for the first peer assignment of COursera course 'Reproducible Research'.

## Loading and processing the data 

The used is available under the following [link] (https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip). After the data is loaded you need to unzip the archive file and load the contained csv-file 'activity.csv'


```r
library(data.table)
act<-fread("activity.csv")
dim(act)
```

```
## [1] 17568     3
```

```r
summary(act)
```

```
##      steps           date              interval   
##  Min.   :  0.0   Length:17568       Min.   :   0  
##  1st Qu.:  0.0   Class :character   1st Qu.: 589  
##  Median :  0.0   Mode  :character   Median :1178  
##  Mean   : 37.4                      Mean   :1178  
##  3rd Qu.: 12.0                      3rd Qu.:1766  
##  Max.   :806.0                      Max.   :2355  
##  NA's   :2304
```

```r
head(act)
```

```
##    steps       date interval
## 1:    NA 2012-10-01        0
## 2:    NA 2012-10-01        5
## 3:    NA 2012-10-01       10
## 4:    NA 2012-10-01       15
## 5:    NA 2012-10-01       20
## 6:    NA 2012-10-01       25
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

The mean total number of steps taken per day is 9354.2295 and the median is equal to 10395.


## What is the average daily activity pattern?

1 Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
m_st_int<-act[,mean(steps,na.rm=TRUE),by=act$interval]
plot(m_st_int$act,m_st_int$V1,main="Average number of steps taken per interval",xlab="Intervals",ylab="Mean values per interval",type="n")
lines(m_st_int$act,m_st_int$V1,col="blue")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


2    Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
m_max_st<-m_st_int[max(m_st_int$V1),"act",with=FALSE]
```
Interval 1705 contains the maximum number of steps

## Imputing missing values

1 Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
##get the vector for complete.cases
cc<-complete.cases(act$steps,act$date,act$interval)
##number of rows with missing values
nr_cc<-length(cc[!cc])
```

Total number of rows with missing values is 2304

2 Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Let't take the mean value for the same 5-minute interval and replace the missing values with it:

```r
act_na<-act[!cc]
setkey(act_na,interval)
setkey(m_st_int,act)
setnames(m_st_int,"act","interval")
act_na<-merge(act_na,m_st_int)
act_na<-subset(act_na,select=c("V1","date","interval"))
setnames(act_na,"V1","steps")
```


3 Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
act_clean<-rbind(act_na,act[cc])
dim(act_clean)
```

```
## [1] 17568     3
```

```r
ccc<-complete.cases(act_clean$steps,act_clean$date,act_clean$interval)
##number of rows with missing values. If result = 0 => Okay
length(ccc[!ccc])
```

```
## [1] 0
```

4 Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
t_st_clean<-tapply(act_clean$steps,act_clean$date,FUN=function(x) sum(x,na.rm=TRUE))
plot(t_st_clean,col="red",type="S",xaxt="n",bg="blue",main="Total number of steps",
                xlab="Date",ylab="Totals")
axis(1, at=seq(1, 61, by=1),labels=names(t_st))
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

```r
#calculate totals
day_totals_clean<-data.frame(day=names(t_st_clean),total_steps=t_st_clean)
#calculate the clean mean and median values
mean_steps_clean<-mean(day_totals_clean$total_steps,na.rm=TRUE)
median_steps_clean<-median(day_totals_clean$total_steps,na.rm=TRUE)
```
The historgram of the new cleaned data shows some new peaks for dates, for which we do not have any data before.

The clean mean total number of steps taken per day is 1.0766 &times; 10<sup>4</sup> and the clean median is equal to 1.0766 &times; 10<sup>4</sup>. There are differences between the previous values and the "clean" ones. It seems that the approach to take subsitute the mean values with the avarages for the corresponding inteval has an impact higher impact on the clean mean than on the clean median etc...


##Are there differences in activity patterns between weekdays and weekends?


1 Create a new factor variable in the dataset with two levels  weekday and weekend indicating whether a given date is a weekday or weekend day.

```r
act$weekday<-weekdays(as.Date(act$date))
act$isWeekday<-ifelse(act$weekday=="Sonntag" | act$weekday=="Samstag","weekend","weekday")
```


2 Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:


```r
library(lattice)
#calculate the mean value
act[,wd_mean:=mean(steps,na.rm=TRUE),by=c("isWeekday","interval")]
```

```
##        steps       date interval weekday isWeekday wd_mean
##     1:    NA 2012-10-01        0  Montag   weekday  2.3333
##     2:    NA 2012-10-01        5  Montag   weekday  0.4615
##     3:    NA 2012-10-01       10  Montag   weekday  0.1795
##     4:    NA 2012-10-01       15  Montag   weekday  0.2051
##     5:    NA 2012-10-01       20  Montag   weekday  0.1026
##    ---                                                    
## 17564:    NA 2012-11-30     2335 Freitag   weekday  1.8718
## 17565:    NA 2012-11-30     2340 Freitag   weekday  2.0769
## 17566:    NA 2012-11-30     2345 Freitag   weekday  0.2051
## 17567:    NA 2012-11-30     2350 Freitag   weekday  0.3077
## 17568:    NA 2012-11-30     2355 Freitag   weekday  1.4615
```

```r
xyplot(act$wd_mean ~ act$interval | act$isWeekday, layout = c(1, 2), type="l")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 
