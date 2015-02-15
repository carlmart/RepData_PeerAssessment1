---
title: "Project 1"
author: "---------"
date: "2/10/2015"
input:  .rmd
output: .md  .html 
download date: "02/10/2015"
---

## Reproducible Research Peer Assessment 1
## Downloaded Data Date "Feb. 3, 2015" 
## Data

We can now download large amounts of data about personal movement
using activity monitoring devices. Data remains under-utilized due to
many two factors - data is hard to obtain and lack of statistical methods
to process and interpret the data.


The data for this assignment was  downloaded from the course web site:
on date : "02/10/2015"
Dataset: Activity monitoring data [52K]
Variables for this dataset : steps , date, interval(identifier for 5 min. interval)
Required libraries  :


```r
library(reshape2)
library(ggplot2)   
```

```
## Loading required package: methods
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lubridate)
```

Downloading the data , using read.csv, and setting up directories:

```r
# url from class link
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
if (!file.exists("data")) 
    dir.create("data")

if (!file.exists("data/data.zip")) 
    download.file(url, destfile = "data/data.zip", mode="wb")     

if (!file.exists("data/activity.csv"))   # found out later we could have just cloned it ;)			 
    unzip("data/data.zip", exdir="data")

df <- read.csv("data/activity.csv",header=TRUE)
```



```r
nrow(df)			# to get total number of rows  [1] 17568
```

```
## [1] 17568
```

```r
names(df)			# to get column names [1] "steps"    "date"     "interval"
```

```
## [1] "steps"    "date"     "interval"
```

```r
dim(df)				# [1] 17568     3
```

```
## [1] 17568     3
```


Now for some quick analysis to see if it matches what we are suppose to get.
Preprocessing the  data, and converting to long format.

```r
mdf <- melt(df, id.vars = c("date","interval") )	# melt data into usable format 
str(mdf)				# 'data.frame':   17568 obs. of  4 variables:
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ variable: Factor w/ 1 level "steps": 1 1 1 1 1 1 1 1 1 1 ...
##  $ value   : int  NA NA NA NA NA NA NA NA NA NA ...
```


Now that we have the data downloaded to R , we can format the data
into an easier format to work with and group by day.


```r
mdftbl <- tbl_df(mdf)      				# for selection
mdftbl$date <-   as.Date(mdftbl$date)  			# convert to date format
```

<!--   head(mdftbl)     --> 
<!--   date            interval variable value     --> 
<!--    2012-10-01        0     steps     NA       -->


The mean total number of steps per day ,or daily mean, can be calculated with the following R code:

```r
bydate <- mdftbl %>% 
       		group_by(date) %>%
       		summarise(dailymean = mean(value,na.rm=TRUE))
```


1. Mean Total Number of steps taken per day :
<!--  37.3826  removes all NaN's  -->

```r
mean(bydate$dailymean,na.rm=TRUE)     
```

```
## [1] 37.3826
```

2. Histogram of total (maximum) number of steps taken per day.


```r
maxbydate <- mdftbl %>%
                group_by(date) %>%
                summarise(dailymax = max(value,na.rm=TRUE))

par(mar=c(4.2, 4.2, 4.0, 4.2))

m <- barplot(  maxbydate$dailymax , main = "Maximum Steps Per Day " ,
            col="lightblue",
            xlab="Days",
            ylab="Max Steps"
        )
axis(1, at=m,labels=1:61)
```

![plot of chunk chunk8](figure/chunk8-1.png) 

3. Calculate mean and median of total number of steps taken per day.

```r
mean(maxbydate$dailymax,na.rm=T) 
```

```
## [1] 600.8868
```

```r
median(maxbydate$dailymax,na.rm=T)
```

```
## [1] 555
```

What is the Average Daily Activity pattern?
1. Make a time series plot of the 5 minute interval (x axis), and average number of steps taken, 
averaged across all days (y axis).

```r
par(mar=c(4.2, 4.2, 4.0, 3.0),bg="grey",adj=1)  # default adj = 0,center, 1 = right
plot(bydate$dailymean,
    type="o",
    col="blue",
    tck=1,
    xaxt="n",
    ylim=range(0:50),
    ann=FALSE)

box()
#lines(bydate$dailymean, type="o", pch=22, lty=2, col="red")
title(main="Avg Daily Steps", col.main="black", font.main=4)
# Label the x and y axes with dark green text
title(xlab="Days", col.lab=rgb(0,0,0))
title(ylab="Frequency", col.lab=rgb(0,0,0))
abline(mean(bydate$dailymean,na.rm=TRUE),0,col="red",lty=2 )
axis(1, at= seq(1,80,by=20), lab=c("2012-10-01","2012-10-14","2012-11-01","2012-11-30"))
```

![plot of chunk chunk10](figure/chunk10-1.png) 

Which 5 minute interval, on average across all days in dataset, 
contains the maximum number of steps?

```r
msteps <-  max(df$steps,na.rm =T)    # [1] 806
df[df$steps %in% msteps,]            # yea!
```

```
##       steps       date interval
## 16492   806 2012-11-27      615
```


Inputing missing values
1. calculate and report the total number of missing values in the data set.

```r
sum( is.na( df$steps ) )   
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. 
The strategy does not need to be sophisticated. For example, you could use the 
mean/median for that day, or the mean for that 5-minute interval, etc.
<!-- bydate <- mdftbl %>%                       -->
<!--           group_by(date) %>%               -->
<!--           summarise(dailymean = mean(value,na.rm=TRUE))  -->



```r
bydate2 <- mdftbl %>%
	group_by(date) %>%
        mutate(steps = ifelse(is.na(steps),as.integer( mean(steps, na.rm = T)),steps))
```

```
## Error in ifelse(is.na(steps), as.integer(mean(steps, na.rm = TRUE)), steps): object 'steps' not found
```

```r
head(bydate2)
```

```
## Error in head(bydate2): object 'bydate2' not found
```

```r
tail(bydate2)
```

```
## Error in tail(bydate2): object 'bydate2' not found
```

```r
dim(bydate2)
```

```
## Error in eval(expr, envir, enclos): object 'bydate2' not found
```


3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
   Original file has length(df$steps) = 17568 with names (steps , date ,interval)

```r
bydate2 <- mdftbl %>%
                group_by(date) %>%
		mutate(steps = ifelse(is.na(steps),as.integer( mean(steps, na.rm = T)),steps))
```

```
## Error in ifelse(is.na(steps), as.integer(mean(steps, na.rm = TRUE)), steps): object 'steps' not found
```

```r
df2 <- dcast(bydate2,date + var1 ~ steps )
```

```
## Error in match(x, table, nomatch = 0L): object 'bydate2' not found
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1)  Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

2)  Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.





