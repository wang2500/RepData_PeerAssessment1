Reproducible Research: Peer Assessment 1
---------------------------------------------------------

1.load the data

```r
project<-read.csv("activity.csv")
str(project)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
unique(project$date) 
```

```
##  [1] 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06
##  [7] 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12
## [13] 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18
## [19] 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24
## [25] 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30
## [31] 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05
## [37] 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11
## [43] 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17
## [49] 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23
## [55] 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29
## [61] 2012-11-30
## 61 Levels: 2012-10-01 2012-10-02 2012-10-03 2012-10-04 ... 2012-11-30
```
####there are 61 days
2.exclude NA values

```r
newdata<-na.omit(project)
```
3.transform the activity data

```r
activity <- transform(newdata, 
                      datetime = strptime( paste(date,formatC(interval,width=4,flag="0")), "%Y-%m-%d %H%M")) 
```

## What is mean total number of steps taken per day?

1.calculate the total number of steps taken per day

```r
sumstep<-aggregate(newdata[,1],list(newdata$date),sum)
```
2.calculate the mean per date

```r
meanstep<-sum(sumstep$x)/53
meanstep
```

```
## [1] 10766.19
```

```r
hist(sumstep[,2],col="red",xlab="total steps numbers",main="total number of steps taken by each day",breaks=61)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 
3.calcualte the mean and median of the total number of steps taken per day

```r
meanstep<-sum(sumstep$x)/53
meanstep
```

```
## [1] 10766.19
```

```r
medianstep<-median(sumstep$x)
medianstep
```

```
## [1] 10765
```
####Mean: 10765   
####Median: 10776.19 
## What is the average daily activity pattern?
1.make time series plot

```r
intervalstep<-aggregate(newdata[,1],list(newdata$interval),mean)
timeseriesdata<-ggplot(intervalstep,aes(intervalstep[,1],intervalstep[,2]))+
  geom_line(color='red')+
  labs(title="the time series data of interval vs. steps")+
  xlab("5-min interval")+ylab("steps taken")
timeseriesdata
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 
2.calculate which 5-minute interval has the maximum number of steps

```r
df3<-data.frame(x=intervalstep[,2])
m<-max(df3$x)
which(df3$x==m)
```

```
## [1] 104
```
####Therefore the 104th interval has the largest imputing missing values

1.total number of missing values in the dataset

```r
df4<-data.frame(project[,1],project[,2],project[,3])
sum(is.na(df4[,1]))
```

```
## [1] 2304
```
####Therefore, there are 2304 rows of missing values
2.devise the filling strategy for missing values

####Through calculation, I found only 8 days out of 61 days have NA values.I will just calculate the mean step of those 8 days and replace the NA values with the mean step of that day


3. Create a new dataset that is equal to the original dataset

```r
missing<-is.na(df4[,1])
na<-split(project[,1][missing],project[,2])
```

```
## Warning in split.default(project[, 1][missing], project[, 2]): data length
## is not a multiple of split variable
```
####found only 8 dates have NA values and all the values are NAs

```r
s<-split(project$steps,project$date)
#replace NA values with 0
s[1]<-list(rep(0,288))
s[8]<-list(rep(0,288))
s[32]<-list(rep(0,288))
s[35]<-list(rep(0,288))
s[40]<-list(rep(0,288))
s[41]<-list(rep(0,288))
s[45]<-list(rep(0,288))
s[61]<-list(rep(0,288))

newdf<-do.call(rbind, lapply(s,data.frame,stringsAsFactors=FALSE))
newdf$date<-project$date
newdf$interval<-project$interval
```
4. make a histogram of the total number of steps taken each day

```r
sumstep1<-aggregate(newdf[,1],list(newdf$date),sum)

newhist<-hist(sumstep1[,2],col="red",xlab="total steps numbers",main="total number of steps taken by each day",breaks=61)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 
5. Calculate and report the mean and median

```r
meanstep1<-sum(sumstep1[,2])/53
meanstep1
```

```
## [1] 10766.19
```

```r
medianstep1<-median(sumstep1[,2])
medianstep1
```

```
## [1] 10395
```
####Yes, the mean are still the same because we found all the NA values concentrated at each days, the original mean was  zero after we replace the original values with the mean, all the NA values become 0. Therefore, the mean are still the same. However, since there are more values in the dataset, the median does change to 10395 instead.



## Are there differences in activity patterns between weekdays and weekends?
Yes. There are differences.

1.transform factor into date format

```r
time<-as.Date(newdf[,2],format="%Y-%m-%d")
```
2.translate date into weekdays and add into newdf

```r
newdf$weekday<-weekdays(time)

newdf$weekday[newdf$weekday=="Sunday"| newdf$weekday=="Saturday"]<-"Weekend"
newdf$weekday[newdf$weekday=="Monday"| newdf$weekday=="Tuesday"| newdf$weekday=="Wednesday"| newdf$weekday=="Thursday"| newdf$weekday=="Friday"]<-"Weekdays"
newdf$weekday<-newdf$weekday
```
3.make the time series plot

```r
ninterval<-as.numeric(newdf$interval)
df1<-newdf[which(newdf$weekday=="Weekend"),]
df2<-newdf[which(newdf$weekday=="Weekdays"),]
timeseriesdata1<-ggplot(df1,aes(df1[,3],df1[,1]))+
  geom_line(color='red')+
  labs(title="the time series data of interval vs. steps")+
  xlab("5-min interval")+ylab("steps taken")

timeseriesdata2<-ggplot(df2,aes(df2[,3],df2[,1]))+
  geom_line(color='red')+
  labs(title="the time series data of interval vs. steps")+
  xlab("5-min interval")+ylab("steps taken")
timeseriesdata1
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png) 

```r
timeseriesdata2
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-2.png) 
