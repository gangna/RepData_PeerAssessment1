## Loading and preprocessing the data
activity=read.csv("activity.csv")


## What is mean total number of steps taken per day?
totalstepsPD <- tapply(activity$steps, activity$date,sum)
hist(totalstepsPD,xlab="Steps per Day", main="Total Number of Steps Per Day")

mean(totalstepsPD,na.rm=TRUE)
median(totalstepsPD,na.rm=TRUE)


## What is the average daily activity pattern?
steps_interval<-aggregate(steps~interval,data=activity,mean,na.rm=TRUE)
plot(steps_interval,
     xlab="5-minute Time Interval",
     ylab="Mean number of steps",
     main="Average Steps at 5-minute Intervals",
     type="l")

steps_interval[which.max(steps_interval$steps),]$interval


## Imputing missing values
sum(is.na(activity$steps))
# use the mean interval to substitute NAs
fillinNA<-function(x){
  steps_interval[steps_interval$interval==x,]$steps
}
# create new dataset
activity_new <- activity
# fill in the NAs in new dataset
for(i in 1:nrow(activity_new)){
  if(is.na(activity_new[i,]$steps)){
    activity_new[i,]$steps<-fillinNA(activity_new[i,]$interval)
  }
}
# check if NAs in new dataset equals 0.
sum(is.na(activity_new))

totalstepsPD_new <- tapply(activity_new$steps, activity_new$date,sum)
hist(totalstepsPD_new,
     xlab="Steps per Day", 
     main="Imputed Total Number of Steps Per Day")
mean(totalstepsPD_new,na.rm=TRUE)
median(totalstepsPD_new,na.rm=TRUE)

## Are there differences in activity patterns between weekdays and weekends?
Sys.setlocale("LC_TIME", "English")
activity_new$date <- as.Date(activity_new$date,"%Y-%m-%d")
day <- weekdays(activity_new$date)
activity_new$day_type <- ifelse(day == "Saturday" | day == "Sunday",
                                "Weekend", "Weekday")
mean_new <- aggregate(activity_new$steps,
                      by=list(activity_new$interval,
                              activity_new$day_type),
                      mean)
names(mean_new) <- c("interval","day_type","steps")

# make panel plot
library("lattice")
panel <- xyplot(steps~interval | day_type, 
                data=mean_new,
                layout=c(1,2),
                xlab="Interval",
                ylab = "Number of steps",
                type="l")

# show and save plot
show(panel)
png(filename = 'panel_plot.png', width=480, height=480)
plot(panel)
dev.off()