**1. Loading and preprocessing the data**
-----------------------------------------

#### 1.1 Load the data

    setwd("~/Desktop/Reproducible Research Project 1")
    activity <- read.csv("activity.csv")

#### 1.2 Process/transform the data

    activity <- transform(activity, date = as.Date(date))

**2. What is mean total number of steps taken per day?**
--------------------------------------------------------

#### 2.1 Calculate the total number of steps taken per day

    StepsPerDay <- aggregate(steps ~ date, activity, sum, na.rm = TRUE)

#### 2.2 Make a histogram of the total number of steps taken each day

    hist(StepsPerDay$steps, xlab = "Steps", main = "Total Number of Steps taken each day", ylim = c(0, 35), labels = TRUE)

![](PA1_template_files/figure-markdown_strict/histogram%20total%20of%20steps%20taken%20each%20day-1.png)

    # Save plot to png file
    dev.copy(png, file = "Histogram StepsPerDay.png")

    ## quartz_off_screen 
    ##                 3

    dev.off()

    ## quartz_off_screen 
    ##                 2

#### 2.3 Calculate and report the mean and median of the total number of steps taken per day

    # Mean:
    mean(StepsPerDay$steps)

    ## [1] 10766.19

    # Median:
    median(StepsPerDay$steps)

    ## [1] 10765

**3. What is the average daily activity pattern?**
--------------------------------------------------

#### 3.1 Make a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

    mean_Avg_Daily <- aggregate(steps ~ interval, activity, mean, na.rm = TRUE)
    plot(mean_Avg_Daily$interval, mean_Avg_Daily$steps, type = "l", main = "Average Steps of the 5-minute Interval", xlab = "5-min Interval", ylab = "Average Steps")

![](PA1_template_files/figure-markdown_strict/Time%20Series%20Plot-1.png)

    # Save plot to png file
    dev.copy(png, file = "Time Series Plot.png")

    ## quartz_off_screen 
    ##                 3

    dev.off()

    ## quartz_off_screen 
    ##                 2

#### 3.2 Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

    max_interval <- mean_Avg_Daily$interval[which(mean_Avg_Daily$steps == max(mean_Avg_Daily$steps))]
    max_interval

    ## [1] 835

**4. Imputing missing values**
------------------------------

#### 4.1 Calculate and report the total number of missing values in the dataset

    sum(is.na(activity))

    ## [1] 2304

#### 4.2 & 4.3 Devise a strategy for filling in all of the missing values in the dataset.

    fillIn_MV <- activity 
    for (i in 1:length(fillIn_MV$steps)) {
      if (is.na(fillIn_MV$steps[i])) {
        fillIn_MV$steps[i] <- mean_Avg_Daily$steps[mean_Avg_Daily$interval == fillIn_MV$interval[i]]
      }
    }
    # Aggregate filled data + verify if any NA
    filled_MisValues <- aggregate(steps ~ date, fillIn_MV, sum, na.rm = TRUE)
    sum(is.na(filled_MisValues$steps))

    ## [1] 0

#### 4.4 Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

    hist(filled_MisValues$steps, main = "Total Steps taken each day", xlab = "Steps", ylim = c(0, 40), labels = TRUE)

![](PA1_template_files/figure-markdown_strict/Histogram%20Total%20Steps%20taken%20daily-1.png)

    # Save plot to png file
    dev.copy(png, file = "Histogram Total Daily Steps.png")

    ## quartz_off_screen 
    ##                 3

    dev.off()

    ## quartz_off_screen 
    ##                 2

    # Mean = total steps taken per day
    mean(filled_MisValues$steps)

    ## [1] 10766.19

    # Median = total steps taken per day
    median(filled_MisValues$steps)

    ## [1] 10766.19

**5. Are there differences in activity patterns between weekdays and weekends?**
--------------------------------------------------------------------------------

#### 5.1 Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

    fillIn_MV$date <- as.Date(filled_MisValues$date)
    fillIn_MV$wkdy <- "weekday"
    fillIn_MV$wkdy[weekdays(fillIn_MV$date) == "Saturday" | weekdays(fillIn_MV$date) == "Sunday"] <- "weekend"
    fillIn_MV$wkdy <- as.factor(fillIn_MV$wkdy)
    fillIn_MV_Interval <- aggregate(steps ~ interval + wkdy, fillIn_MV, mean, na.rm = TRUE)

#### 5.2 Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

    library(ggplot2)
    x <- ggplot(fillIn_MV_Interval, aes(interval, steps))
    x + facet_grid(wkdy ~ .) + geom_line() + ggtitle("Average Steps taken Weekday and Weekend")

![](PA1_template_files/figure-markdown_strict/Panel%20Plot%20(Time%20Series%20Plot)-1.png)

    # Save plot to png file
    dev.copy(png, file = "Panel Plot - Time Series.png")

    ## quartz_off_screen 
    ##                 3

    dev.off()

    ## quartz_off_screen 
    ##                 2
