setwd("~/RepData_PeerAssessment1")
unz("activity", "activity.csv")
activity <- read.csv("activity.csv")

activity$date <- as.Date(activity$date, "%Y-%m-%d")
str(activity)

stepsperday<-tapply(activity$steps, activity$date, FUN=sum)

hist(stepsperday,  main = "Frequency of the Total number of stepsa day", xlab = "Steps")
stepsmean <- mean(stepsperday, na.rm=TRUE)
stepsmedian <- median(stepsperday, na.rm=TRUE)



series <- tapply(activity$steps, activity$interval, FUN=mean, na.rm = TRUE)
plot(series, type = "l", xlab = "5-minutes interval",  ylab = "Average number of steps taken")
maxiinterval<-names(series)[which.max(apply(series, MARGIN=1,max))]

missing<-sum(!complete.cases(activity$steps))
missing


missingdata<- which(is.na(activity$steps))
medians <- rep(median(activity$steps, na.rm=TRUE), times=length(missingdata))

activity[missingdata, "steps"] <- medians

stepsperdaymediansNA<-tapply(activity$steps, activity$date, FUN=sum)
hist(stepsperdaymediansNA,  main = "Frequency of the Total number of stepsa day", xlab = "Steps")
stepsmean1 <- mean(stepsperdaymediansNA, na.rm=TRUE)
stepsmedian1 <- median(stepsperdaymediansNA, na.rm=TRUE)

activity1 <- data.frame(activity, weekday=weekdays(activity$date))
activity2 <- cbind(activity1, day = ifelse(activity1$weekday == "Saturday" | 
              activity1$weekday == "Sunday", "weekend", "weekday"))

meanday <- aggregate(steps ~ interval + day, data = activity2, mean)

library(lattice)
xyplot(steps ~ interval | day,meanday, type="l", 
       xlab="Interval", 
       ylab="Number of steps", 
       layout=c(1,2))

                     
