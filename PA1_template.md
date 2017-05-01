
# Reading File and Cleaning File

```r
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
origdata <- read.csv(unz(temp, "activity.csv"),header = TRUE, sep = ",",colClasses=c("numeric", "character", "numeric"))
nonadata<-na.omit(origdata)
```

# What is mean total number of steps taken per day?
## Ignoring the missing values


```r
print("Histogram of mean number of steps taken per day")
```

```
## [1] "Histogram of mean number of steps taken per day"
```

```r
library(ggplot2)
total.steps <- tapply(nonadata$steps, nonadata$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
```

![plot of chunk mean total number of steps](figure/mean total number of steps-1.png)

```r
print("The mean and median are:")
```

```
## [1] "The mean and median are:"
```

```r
mean(total.steps)
```

```
## [1] 10766.19
```

```r
median(total.steps)
```

```
## [1] 10765
```

#What is the average daily activity pattern?


```r
avgpattern <- aggregate(nonadata[c("steps")], list(interval = nonadata$interval), mean)
amax<-max(avgpattern[,2])
newdata <- subset(avgpattern,avgpattern[,2]==amax)
newdata<-newdata[order("interval"),]
print(paste0("The interval ",newdata[1,]$interval," has maximum number of steps of ",amax))
```

```
## [1] "The interval 835 has maximum number of steps of 206.169811320755"
```

```r
ggplot(data=avgpattern, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken")
```

![plot of chunk average daily activity pattern](figure/average daily activity pattern-1.png)

#Imputing missing values
##Calculting the total number of missing values in the dataset by computing difference in the number of rows between the original data frame and data frame where NA values were removed

```r
diff<-nrow(origdata)-nrow(nonadata)
print(paste0("The number of missing values is ",diff))
```

```
## [1] "The number of missing values is 2304"
```

##Filling in missing values using mean of that interval and created new data set

```r
library(sqldf)
```

```
## Loading required package: gsubfn
```

```
## Loading required package: proto
```

```
## Loading required package: RSQLite
```

```r
tmp2 <- sqldf("SELECT avgpattern.steps,origdata.date,origdata.interval fROM origdata,avgpattern where origdata.steps is NULL and origdata.interval=avgpattern.interval")
```

```
## Loading required package: tcltk
```

```
## Warning: Quoted identifiers should have class SQL, use DBI::SQL() if the
## caller performs the quoting.
```

```r
imputedata<-rbind(nonadata,tmp2)
```

##Make a histogram of the total number of steps taken each day along with mean and median
###Plotting a histogram of the daily total number of steps taken, plotted with a bin interval of 1000 steps, after filling missing values.

```r
library(ggplot2)
total1.steps <- tapply(imputedata$steps, imputedata$date, FUN=sum)
qplot(total1.steps, binwidth=1000, xlab="total number of steps taken each day")
```

![plot of chunk new mean total number of steps with new data](figure/new mean total number of steps with new data-1.png)

```r
mean(total1.steps)
```

```
## [1] 10766.19
```

```r
median(total1.steps)
```

```
## [1] 10766.19
```
## Calculating difference between imputed and non imputed data

```r
meandiff<-mean(total.steps)-mean(total1.steps)
mediandiff<-median(total.steps)-median(total1.steps)
```

#Are there differences in activity patterns between weekdays and weekends?

```r
library(chron)
imputedata$weektype<-chron::is.weekend(imputedata$date)

aadi <- aggregate(steps ~ interval + weektype, data=imputedata, mean)
ggplot(aadi[aadi$weektype=="FALSE",], aes(interval, steps)) + 
    geom_line() + 
    xlab("5-minute interval") + 
    ylab("avarage number of steps") + ggtitle("weekday")
```

![plot of chunk weekday weekend pattern differences](figure/weekday weekend pattern differences-1.png)

```r
ggplot(aadi[aadi$weektype=="TRUE",], aes(interval, steps)) + 
    geom_line() + 
    xlab("5-minute interval") + 
    ylab("avarage number of steps") + ggtitle("weekend")
```

![plot of chunk weekday weekend pattern differences](figure/weekday weekend pattern differences-2.png)

```r
total2.steps <- tapply(aadi$steps, aadi$weektype, FUN=sum)
if (total2.steps[1]>total2.steps[2]) {print("Weekday mean is higher")} else {"Weekend mean is higher"}
```

```
## [1] "Weekend mean is higher"
```
