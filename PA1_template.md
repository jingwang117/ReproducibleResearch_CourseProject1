---
title: "Reproducible Research Course Assignment 1"
author: "Jing Wang"
date: "5/24/2019"
output: html_document
---

```{r library}
library(ggplot2)
library(scales)
library(Hmisc)
```

## Loading and preprocessing the data

```{r load data}

unzip('activity.zip')

activity <- read.csv('activity.csv')
```

## What is mean total number of steps taken per day?
```{r}
totalstepsdaily <- tapply(activity$steps, activity$date, sum, na.rm=TRUE)
qplot(totalstepsdaily, xlab='Total steps taken per day', ylab='Frequency using binwith 500', binwidth=500)
```

##### Calculate and report the mean and median total number of steps taken per day
```{r}
print(totalstepsMean <- mean(totalstepsdaily))
print(totalstepMedian <- median(totalstepsdaily))
```

## What is the average daily activity pattern?
```{r}
averageactivitydaily <- aggregate(x=list(meanSteps=activity$steps), by=list(interval=activity$interval), FUN=mean, na.rm=TRUE)
```

##### 1. Make a time series plot
```{r}
ggplot(data=averageactivitydaily, aes(x=interval, y=meanSteps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken") 
```

##### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
print(maxsteps <- which.max(averageactivitydaily$meanSteps))
print(intervalmaxsteps <- gsub("([0-9]{1,2})([0-9](2))", "\\1:\\2", averageactivitydaily[maxsteps, 'interval']))
```


## Imputing missing values
##### 1. total number of missing values in the dataset 
```{r}
MissingData <- is.na(activity$steps)
totalNA <-  sum(MissingData)
percentageNA <- mean(MissingData)
print(totalNA)
print(paste(sprintf("%.2f", percentageNA * 100), "%", sep=""))
```

##### 2.Apply the mean value for the 5-minute interval to the missing values
```{r}
activityImputed <- activity
activityImputed$steps <- impute(activity$steps, fun=mean)
```


##### 3. Make a histogram of the total number of steps taken each day 
```{r}
stepsImputed <- tapply(activityImputed$steps, activityImputed$date, sum)
qplot(stepsImputed, xlab='Total steps per day (Imputed)', ylab='Frequency using binwith 500', binwidth=500)
```

##### 4. Calculate and report the mean and median total number of steps taken per day. 
```{r}
print(stepsImputedMean <- mean(stepsImputed))
print(stepsImputedMedian <- median(stepsImputed))
```

## Are there differences in activity patterns between weekdays and weekends?

```{r}
activityImputed$dateType <-  ifelse(as.POSIXlt(activityImputed$date)$wday %in% c(0,6), 'weekend', 'weekday')

average <- aggregate(steps ~ interval + dateType, data=activityImputed, mean)
ggplot(average, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(dateType ~ .) +
    xlab("5-minute interval") + 
    ylab("Number of steps taken")
```


