## Loading and preprocessing the data

# Load libraries, read in data, and ensure data are in the right format
library(ggplot2)
activityDat <- read.csv("activity.csv")
activityDat$date <- as.Date(activityDat$date, format = "%Y-%m-%d")

## What is mean total number of steps taken per day?

# Calculate the total number of steps taken per day
totSteps <- setNames(aggregate(activityDat$steps, 
          list(activityDat$date), sum), c("Date", "Step_Sum"))

# Histogram of sum of steps
ggplot(totSteps, aes(x=Step_Sum)) + 
  geom_histogram(binwidth=1000, fill="white", color="black") +
  ylim(0, 20)

# Mean and Median of Step_Sum 
mean(totSteps$Step_Sum, na.rm=TRUE)
median(totSteps$Step_Sum, na.rm=TRUE)

## What is the average daily activity pattern?

# Calculate the average number of steps taken per time interval
avgSteps <- setNames(aggregate(activityDat$steps, list(activityDat$interval),
                               mean, na.rm=TRUE), c("Interval", "Step_Avg"))

# Timeseries of average steps per time interval
ggplot(avgSteps, aes(x=Interval, y=Step_Avg)) + geom_line()

# The maximum number of steps across the averaged data
avgSteps[avgSteps$Step_Avg == max(avgSteps$Step_Avg),]

## Imputing missing values

# Number of missing observations
sum(is.na(activityDat$steps))

# I will use the average of that 5-min interval wherever a missing value 
# appears to create a new column
activityDat$missingSteps <- avgSteps$Step_Avg
activityDat$missingSteps <- ifelse(is.na(activityDat$steps), 
        activityDat$missingSteps, 
        activityDat$steps)

# Calculate the total number of steps taken per day
totStepsMissing <- setNames(aggregate(activityDat$missingSteps, 
                               list(activityDat$date), sum), c("Date", "Step_Sum_Missing"))

# Histogram of sum of steps using imputed missing observations
ggplot(totStepsMissing, aes(x=Step_Sum_Missing)) + 
  geom_histogram(binwidth=1000, fill="white", color="black") +
  ylim(0, 20)

# Mean and Median of Step_Sum_Missing
mean(totStepsMissing$Step_Sum_Missing)
median(totStepsMissing$Step_Sum_Missing)

## Are there differences in activity patterns between weekdays and weekends?

# Creating a weekday/weekend factor variable
activityDat$day <- as.factor(ifelse(weekdays(activityDat$date) == "Sunday" | 
                            weekdays(activityDat$date) == "Saturday", 
                                   "Weekend", "Weekday"))

# Calculate the average number of steps taken per time interval
moreAvgSteps <- setNames(aggregate(activityDat$missingSteps, 
                  by=list(activityDat$interval,activityDat$day),mean),
                  c("Interval", "Day", "Step_Avg_Missing"))

# Timeseries of average steps per time interval
ggplot(moreAvgSteps, aes(x=Interval, y=Step_Avg_Missing)) + geom_line() + facet_grid(Day ~ .)

