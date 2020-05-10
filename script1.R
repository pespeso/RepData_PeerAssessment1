###### Reproducible Research
### Week 2 / Course Project 1

library(lattice)

### 1. Loading and preprocessing the data

# 1.1. Load data
if (!file.exists("activity.csv")) {
  if (!file.exists("activity.zip")) {
    dlurl <- 'http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'  
    download.file(dlurl,destfile='activity.zip',mode='wb')
  }
  unzip('activity.zip')
}
data <- read.csv("activity.csv")  

# 1.2. Process/transform the data (if necessary) into a format suitable for your analysis
# No need to process/transform the data.

### 2. What is mean total number of steps taken per day?

# 2.1. Calculate the total number of steps taken per day
steps_by_day <- aggregate(steps ~ date, data, sum)

# 2.2. If you do not understand the difference between a histogram and a barplot,
# research the difference between them. Make a histogram of the total number of steps taken each day
hist(steps_by_day$steps, main = paste("Total Steps Each Day"),xlab="# of steps")

# 2.3. Calculate and report the mean and median of the total number of steps taken per day
steps_mean <- mean(steps_by_day$steps)
steps_median <- median(steps_by_day$steps)

### 3. What is the average daily activity pattern?

# 3.1. Make a time series plot (i.e. type = "l"\color{red}{\verb|type = "l"|}type="l")
# of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
steps_by_interval <- aggregate(steps ~ interval, data, mean)
plot(steps_by_interval$interval,
     steps_by_interval$steps,
     type="l",
     xlab="Interval",
     ylab="# of steps",
     main="Avg(# of steps) for each day by interval")

# 3.2. Which 5-minute interval, on average across all the days in the dataset,
# contains the maximum number of steps?
max_interval <- steps_by_interval[which.max(steps_by_interval$steps),1]

### 4. Imputing missing values
# Note that there are a number of days/intervals where there are missing values (coded as NA).
# The presence of missing days may introduce bias into some calculations or summaries of the data.

# 4.1. Calculate and report the total number of missing values in the dataset
# (i.e. the total number of rows with NAs)
NATotal <- sum(!complete.cases(data))

# 4.2. Devise a strategy for filling in all of the missing values in the dataset.
# The strategy does not need to be sophisticated. For example, you could use
# the mean/median for that day, or the mean for that 5-minute interval, etc.
avg_steps <- aggregate(steps ~ interval, data = data, FUN = mean)
fillNA <- numeric()
for (i in 1:nrow(data)) {
  obs <- data[i, ]
  if (is.na(obs$steps)) {
    steps <- subset(avg_steps, interval == obs$interval)$steps
  } else {
    steps <- obs$steps
  }
  fillNA <- c(fillNA, steps)
}

# 4.3. Create a new dataset that is equal to the original dataset but with
# the missing data filled in.
new_activity <- data
new_activity$steps <- fillNA

# 4.4. Make a histogram of the total number of steps taken each day and
# Calculate and report the mean and median total number of steps taken per day. 
# Do these values differ from the estimates from the first part of the assignment?
# What is the impact of imputing missing data on the estimates of the total daily number of steps?
StepsTotalUnion <- aggregate(steps ~ date, data = new_activity, sum, na.rm = TRUE)

hist(StepsTotalUnion$steps,
     main = paste("Total Steps Each Day"),
     col="gray80",
     xlab="Number of Steps")
#Create Histogram to show difference.
hist(steps_by_day$steps,
     main = paste("Total Steps Each Day"),
     col="gray50",
     xlab="Number of Steps",
     add=T)
legend("topright",
       c("Imputed", "Non-imputed"),
       col=c("gray80", "gray50"),
       lwd=3)

steps_meantotal <- mean(StepsTotalUnion$steps)
steps_mediantotal <- median(StepsTotalUnion$steps)
steps_mediandiff <- steps_mediantotal - steps_median
steps_meandiff <- steps_meantotal - steps_mean

### 5. Are there differences in activity patterns between weekdays and weekends?
# For this part the weekdays() function may be of some help here.
# Use the dataset with the filled-in missing values for this part.

# 5.1. Create a new factor variable in the dataset with two levels – “weekday”
# and “weekend” indicating whether a given date is a weekday or weekend day.
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
new_activity$dow = as.factor(ifelse(is.element(weekdays(as.Date(new_activity$date)),weekdays), "Weekday", "Weekend"))
StepsTotalUnion <- aggregate(steps ~ interval + dow, new_activity, mean)
xyplot(StepsTotalUnion$steps ~ StepsTotalUnion$interval | StepsTotalUnion$dow,
       main="Average Steps per Day by Interval",
       xlab="Interval",
       ylab="Steps",
       layout=c(1,2),
       type="l")

# 5.2 Make a panel plot containing a time series plot (i.e. type = "l") of
# the 5-minute interval (x-axis) and the average number of steps taken,
# averaged across all weekday days or weekend days (y-axis).
# See the README file in the GitHub repository to see an example of what
# this plot should look like using simulated data.
xyplot(StepsTotalUnion$steps ~ StepsTotalUnion$interval|StepsTotalUnion$dow,
       main="Average Steps per Day by Interval",
       xlab="Interval",
       ylab="Steps",
       layout=c(1,2),
       type="l")





