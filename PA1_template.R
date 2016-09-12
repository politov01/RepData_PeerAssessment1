getwd()
setwd("C:\\HTML_DOC\\coursera.org\\05_Reproducible Research\\Github\\CourseProject1\\RepData_PeerAssessment1")
getwd()



# Check if the data is downloaded and download when applicable
if(!file.exists("activity.zip")) { 
    download.file(url="https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", 
                  destfile="activity.zip", mode = "wb") 
  } 

if(!file.exists("activity.csv")) { 
   unzip(zipfile="activity.zip") 
  } 

## ----loaddata------------------------------------------------------------ 
data <- read.csv("activity.csv", head=TRUE, na.strings="NA")
data$date <- as.Date(data$date)

steps_per_day <- aggregate(steps ~ date, data=data, sum, na.rm=TRUE)
steps_per_day
#         date steps
#1  2012-10-02   126
#2  2012-10-03 11352
#3  2012-10-04 12116
#....
#50 2012-11-26 11162
#51 2012-11-27 13646
#52 2012-11-28 10183
#53 2012-11-29  7047

hist(steps_per_day$steps, 
     breaks=25,
     xlab="Daily total steps", 
     ylab="Frequency", 
     col="red",
     main="Steps per day (missing data ignored)")

cat("Mean:", mean(steps_per_day$steps))
cat("Median:", median(steps_per_day$steps))


# Compute the average number of steps
average_steps <- aggregate(steps ~ interval, data=data, mean, na.rm=TRUE)

# Time series plot of the average number of steps taken
plot(average_steps$interval, 
     average_steps$steps, 
     type="l", 
     col="blue", 
     xlab="5-minute Time Interval", 
     ylab="Average Steps in the interval across all days", 
     main="Time series plot of steps taken")

# Locate when the maximum steps happened in the time series
max_steps <- which(average_steps$steps == max(average_steps$steps))
cat("Interval with maximum steps: ", average_steps[max_steps,1])


#Calculating total number of missing values in the dataset
sum(is.na(data$steps))


# Find the missing indices
index_missing <- is.na(data$steps)
str(index_missing)
# logi [1:17568] TRUE TRUE TRUE TRUE TRUE TRUE ...
#[17269] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#[17281]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# ...
#[17557]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE

# dataset without NA
data_clear <- subset(data, !is.na(data$steps)) #clear data
str(data_clear)
# count mean
data_mean <- tapply(data_clear$steps, data_clear$interval, mean, na.rm=TRUE, simplify=T)
str(data_mean)
# num [1:288(1d)] 1.717 0.3396 0.1321 0.1509 0.0755 ...
# - attr(*, "dimnames")=List of 1
#  ..$ : chr [1:288] "0" "5" "10" "15" ...
str(names(data_mean))
# chr [1:288] "0" "5" "10" "15" "20" "25" "30" "35" "40" ...
names(data_mean)
#  [1] "0"    "5"    "10"   "15"   "20"   "25"   "30"   "35"   "40"   "45"  
#  
#[271] "2230" "2235" "2240" "2245" "2250" "2255" "2300" "2305" "2310" "2315"
#[281] "2320" "2325" "2330" "2335" "2340" "2345" "2350" "2355"
data_mean["0":"15"]
#        0         5        10        15        20        25        30        35 
#1.7169811 0.3396226 0.1320755 0.1509434 0.0754717 2.0943396 0.5283019 0.8679245 
#       40        45        50        55       100       105       110 
#0.0000000 1.4716981 0.3018868 0.1320755 0.3207547 0.6792453 0.1509434 


# Create a copy of the original dataset
data_imputed <- data


# Replace data=NA in dataset appropriate value in 5-time intervals
data_imputed$steps[index_missing] <- data_mean[as.character(data_imputed$interval[index_missing])]
table(data_imputed$steps)
#                 0 0.0754716981132075  0.113207547169811  0.132075471698113 
#             11166                 16                  8                 24 
# 0.150943396226415  0.169811320754717  0.207547169811321  0.226415094339623 
#                16                 16                  8                 16 
# .......
#               777                781                783                785 
#                 1                  1                  1                  3 
#               786                789                794                802 
#                 1                  1                  1                  1 
#               806 
#                 1 

data_sumary_by_date <- tapply(data_imputed$steps, data_imputed$date, sum, na.rm=TRUE, simplify=T)

hist(x=data_sumary_by_date,
     col="green",
     breaks=20,
     xlab="daily steps",
     ylab="Frequency",
     main="The distribution of daily total (After imputation)")

mean(data_sumary_by_date)
#[1] 10766.19
median(data_sumary_by_date)
#[1] 10766.19


# Add character column for day of the week
data_imputed <- data.frame(steps=data_imputed$steps, 
                           date=data_imputed$date, 
                           dayofweek=tolower(weekdays(data_imputed$date)), 
                           interval=data_imputed$interval)
# Append a daytype column to distinguish between weekday and weekend
data_imputed <- cbind(data_imputed, daytype=ifelse(data_imputed$dayofweek %in% c("saturday", "sunday"), "weekend", "weekday"))


# Compute the time series
data_type <- aggregate(data_imputed$steps,
                       by=list(data_imputed$daytype, data_imputed$dayofweek, data_imputed$interval),
                       mean)
str(data_type)
#'data.frame':   2016 obs. of  4 variables:
# $ Group.1: Factor w/ 2 levels "weekday","weekend": 1 1 2 2 1 1 1 1 1 2 ...
# $ Group.2: Factor w/ 7 levels "friday","monday",..: 1 2 3 4 5 6 7 1 2 3 ...
# $ Group.3: int  0 0 0 0 0 0 0 5 5 5 ...
# $ x      : num  0.382 1.493 0.215 0.215 5.413 ...
data_type
#     Group.1   Group.2 Group.3            x
#1    weekday    friday       0 3.815514e-01
#2    weekday    monday       0 1.492662e+00
#3    weekend  saturday       0 2.146226e-01
#4    weekend    sunday       0 2.146226e-01
#5    weekday  thursday       0 5.412998e+00
#6    weekday   tuesday       0 0.000000e+00
#7    weekday wednesday       0 3.968553e+00
#8    weekday    friday       5 7.547170e-02
#9    weekday    monday       5 7.547170e-02
#10   weekend  saturday       5 4.245283e-02
#11   weekend    sunday       5 4.245283e-02
#12   weekday  thursday       5 3.773585e-02
#13   weekday   tuesday       5 0.000000e+00
#14   weekday wednesday       5 2.037736e+00
#15   weekday    friday      10 2.935010e-02
#16   weekday    monday      10 2.935010e-02
#
#
#2001 weekday   tuesday    2345 0.000000e+00
#2002 weekday wednesday    2345 9.601677e-01
#2003 weekday    friday    2350 5.031447e-02
#2004 weekday    monday    2350 5.031447e-02
#2005 weekend  saturday    2350 2.830189e-02
#2006 weekend    sunday    2350 2.830189e-02
#2007 weekday  thursday    2350 4.696017e-01
#2008 weekday   tuesday    2350 0.000000e+00
#2009 weekday wednesday    2350 9.140461e-01
#2010 weekday    friday    2355 1.127883e+00
#2011 weekday    monday    2355 2.389937e-01
#2012 weekend  saturday    2355 1.344340e-01
#2013 weekend    sunday    2355 1.344340e-01
#2014 weekday  thursday    2355 1.194969e-01
#2015 weekday   tuesday    2355 2.222222e+00
#2016 weekday wednesday    2355 3.341719e+00

names(data_type)
#[1] "Group.1" "Group.2" "Group.3" "x" 

# Rename the attributes
names(data_type) <- c("daytype", "weekday", "interval", "mean")

# Plot the data
library(lattice)
xyplot(mean ~ interval | factor(daytype), 
       data=data_type, 
       type="l", 
       lwd=1, 
       xlab="5-minute Interval", 
       ylab="Number of steps", 
       layout=c(1,2))


library(psych)
by(data_type,
   data_type$daytype,
   FUN=describe)

library(ggplot2)
ggplot(data_type, aes(x=interval, y=mean, color=daytype)) +
       geom_line() +
       theme(legend.position="bottom")


