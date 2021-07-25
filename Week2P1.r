library( datasets )
library( dplyr )

# Load Data
#===========
dataset <- read.csv( "activity.csv" )
str( dataset )
cat("\n")
dim( dataset )
cat("\n")
summary( dataset )
cat("\n")
head( dataset )
cat("\n")
tail( dataset )
cat("\n")

#data.frame':   17568 obs. of  3 variables:
# $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
# $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
# $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

#Question 1
#===========
DayStepsDF <- dataset %>% group_by( date ) %>% summarize( DaySteps = sum( steps , na.rm = TRUE ) )
str( DayStepsDF )
cat("\n")
dim( DayStepsDF )
cat("\n")
summary( DayStepsDF )
cat("\n")

png( "graphQ1.png", width = 700, height = 700, unit = "px" )
mygraph <-  hist( DayStepsDF$DaySteps, xlab = "Total Steps in a day (count)", 
                    ylab = "Frequency", 
                    main = "Histogram\nTotal Steps in one day", 
                    ylim = c(0, 20), 
                    xlim = c(0, 25000), 
                    col = "blue", 
                    density = 5,
                    angle = 60,
                    breaks = c(0, 2500, 5000, 7500, 10000, 12500, 15000, 17500, 20000, 22500), #seq( 0, max(DayStepsDF$DaySteps), length.out = 10 ),
                    border = "darkmagenta",
                    labels = TRUE )
            abline( v = mean( DayStepsDF$DaySteps), col = "darkblue", lty = 2, lwd = 4  )
            abline( v = median( DayStepsDF$DaySteps), col = "orange", lty = 2, lwd = 4  )
            legend("topright", c("Mean", "Median"), fill = c( "darkblue", "orange" ) )

print( mygraph )

dev.off()

#     date              DaySteps
# Length:61          Min.   :    0
# Class :character   1st Qu.: 6778
# Mode  :character   Median :10395
#                    Mean   : 9354
#                    3rd Qu.:12811
#                    Max.   :21194

#Question 2
#==========
#What is the average daily activity pattern?

#   Make a time series plot (i.e. type = "l"\color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) 
#   and the average number of steps taken, averaged across all days (y-axis)
#   Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

IntervalDF <- dataset %>% group_by( interval ) %>% summarize( DaySteps = mean( steps , na.rm = TRUE ) )
cat("\n")
str( IntervalDF )
cat("\n")
dim( IntervalDF )
cat("\n")
summary( IntervalDF )
cat("\n")
head( IntervalDF )
cat("\n")
tail( IntervalDF )
cat("\n")
png( "graphQ2.png", width = 700, height = 700, unit = "px" )
mygraph <-  with( IntervalDF, 
                    plot( x = interval, y = DaySteps, type = "l" ,
                        xlab = "Time of the day ( Interval )",
                        ylab = "Number of steps", 
                        main = "Mean Steps registered vs time of the day (measured over 61 days).", 
                        ylim = c(0, 220), 
                        #xlim = c(0, 25000),
                        col = "blue",
                        lwd = 4 )
                    )
            legend("topright", c("Mean steps for each time interval across all days."), fill = c( "darkblue" ) )
            axis(1, at = seq( 0, 2400, 200 ) )
print( mygraph )
maxSteps <- max( IntervalDF$DaySteps )
maxSteps # 206 steps is the highest average steps registered over 61 days at time interval.....
cat("\n")
maxInterval <- IntervalDF[ IntervalDF$DaySteps == maxSteps, 1]
maxInterval # .....8.35 am 
cat("\n")
abline( v = maxInterval, col = "magenta", lty = 2, lwd = 4  )
abline( h = maxSteps, col = "orange", lty = 2, lwd = 4  )
dev.off()
cat("\n")

# Question 3:
#============
# Imputing missing values
# Note that there are a number of days/intervals where there are missing values (coded as NA\color{red}{\verb|NA|}NA). 
# The presence of missing days may introduce bias into some calculations or summaries of the data.

#    3.1 Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA\color{red}{\verb|NA|}NAs)
datasetNA <- which( is.na( dataset$steps ) )# 2304 rows are "NA" $steps+
str( datasetNA )
cat("\n")
mean( is.na( dataset$steps ) ) * 100 # 13% of steps data is missing.
cat("\n")

#    3.2 Devise a strategy for filling in all of the missing values in the dataset. 
#       The strategy does not need to be sophisticated. For example, you could 	use the mean/median for that day, 
#       or the mean for that 5-minute interval, etc.

# Explore the NA values.
# Total number of steps per day:
# Total number of intervals wich register a value, including zer0 ( no NA )
DayMeanStepsValues <- dataset %>% group_by( date ) %>% summarize( NotNAs = sum( !is.na( steps ) ) )
DayMeanStepsValues <- as.data.frame( DayMeanStepsValues )
DayMeanStepsValues # The count of intervals with registered steps over 61 days is either 0, or 288. 
# The NA values belong to full days.
cat("\n")

#       Strategy: replace missing values with the prior or next day's value.
#       =========
#    3.3 Create a new dataset that is equal to the original dataset but with the missing data filled in.
datasetFullNoNA <- dataset
for( row in datasetNA  ){
        Refrow <- 0
        if( row <= 288 ){
                Refrow <- row + 288 # 288 intervals in 1 day. Adding 288 points to the same interval of the next day.
        } else {
                Refrow <- row - 288 # same as above prior day.
        } 
        datasetFullNoNA[ row, 1 ] <- datasetFullNoNA[ Refrow, 1 ]
}
str( datasetFullNoNA )
cat("\n")
mean( is.na( datasetFullNoNA ) ) * 100 # 0% no missing steps data reported as "NA".

DayMeanStepsDF <- datasetFullNoNA  %>% group_by( date ) %>% summarize( DaySteps = sum( steps , na.rm = TRUE ) )
str( DayMeanStepsDF )
cat("\n")
summary( DayMeanStepsDF )
cat("\n")
DayMeanStepsDF <- as.data.frame( DayMeanStepsDF )
DayMeanStepsDF # every day of the 61 measured has a mean now.

DayStepsNoNADF <- datasetFullNoNA %>% group_by( date ) %>% summarize( DaySteps = sum( steps , na.rm = TRUE ) )
str( DayStepsNoNADF )
cat("\n")
dim( DayStepsNoNADF )
cat("\n")
summary( DayStepsNoNADF )
cat("\n")

#    3.4 Make a histogram of the total number of steps taken each day and 
#    3.5 Calculate and report the mean and median total number of steps taken per day. 
png( "graphQ3.png", width = 700, height = 700, unit = "px" )
mygraph <-  hist( DayStepsNoNADF$DaySteps, xlab = "Total Steps in a day (count)", 
                    ylab = "Frequency", 
                    main = "Question 3.4 ~ 3.5 - Histogram\nTotal Steps in one day", 
                    ylim = c(0, 20), 
                    xlim = c(0, 25000), 
                    col = "blue", 
                    density = 5,
                    angle = 60,
                    breaks = c(0, 2500, 5000, 7500, 10000, 12500, 15000, 17500, 20000, 22500), #seq( 0, max(DayStepsDF$DaySteps), length.out = 10 ),
                    border = "darkmagenta",
                    labels = TRUE )
            abline( v = mean( DayStepsNoNADF$DaySteps), col = "darkblue", lty = 2, lwd = 4  )
            abline( v = median( DayStepsNoNADF$DaySteps), col = "orange", lty = 2, lwd = 4  )
            abline( v = mean( DayStepsDF$DaySteps), col = "magenta", lty = 1, lwd = 4  )
            abline( v = median( DayStepsDF$DaySteps), col = "green", lty = 1, lwd = 4  )
            legend("topright", c("Mean (No NAs Q3)", "Median (No NAs Q3)", "Mean (with NAs Q1)", "Median (with NAs Q1)" ), fill = c( "darkblue", "orange", "magenta", "green" ) )

print( mygraph )
dev.off()
 # date              DaySteps
 # Length:61          Min.   :   41
 # Class :character   1st Qu.: 8334
 # Mode  :character   Median :10571
 #                   Mean   :10304
 #                   3rd Qu.:12883
 #                   Max.   :21194
#	3.6 Do these values differ from the estimates from the first part of the assignment? 
        # the values differ from those calculated in Q1. Both Mean and Median have increased ( moved to the right on the bar graph )

#	3.7 What is the impact of imputing missing data on the estimates of the total daily number of steps?
                # Since the strategy is set to add the prevoius or next day step count to the dataset, the total number of 
                # steps for all days goes up from 570 to 629 thousand steps. 
CompareNADF1 <- datasetFullNoNA %>% group_by( date ) %>% summarize( DayStepsNoNA = sum( steps , na.rm = FALSE ) )
TotalSteps1 <- sum( CompareNADF1$DayStepsNoNA )
# A total of 628,555 steps after replacing "NAs" with the previous or next day activity.
TotalSteps1
CompareNADF2 <- dataset %>% group_by( date ) %>% summarize( DayStepsNA = sum( steps , na.rm = FALSE ) )
TotalSteps2 <- sum( CompareNADF2$DayStepsNA, na.rm = TRUE )
# A total of 570,608 eliminting "NAs" ( same as considering "NAs" = 0 )
TotalSteps2
CompareNADF <- cbind( CompareNADF1, CompareNADF2$DayStepsNA ) 
names( CompareNADF ) <- c( "Date", "DayStepsNoNA", "DayStepsNA" )
str( CompareNADF )
cat("\n")
dim( CompareNADF )
cat("\n")
summary( CompareNADF )
cat("\n")
CompareNADF
library( ggplot2 )
#coeff <- 0.05 #( adjustment coefficient for the y secondary axis.)
png( "graphQ37.png", width = 700, height = 700, unit = "px" )
# base plot:
mygraph <- ggplot( CompareNADF ) + 
geom_point( aes( x = Date, y = DayStepsNoNA, color = "Steps (no NAs)" ),  shape = 1, size = 5, alpha = 2 )  + 
geom_point( aes( x = Date, y = ( DayStepsNA ), color = "Steps (with NAs)" ),  shape = 2, size = 2, alpha = 0.5 )  + 
#scale_y_continuous( sec.axis = sec_axis( ~./coeff ) ) + # scale_y_continuous( sec.axis = ~ .^2)
labs( title = "QUESTION 3.7 - Total count of steps for each day\n (Including NAs and not including NAs)" )  + 
ylab( "Total count of steps" )
print( mygraph )
dev.off()

#Question 4
#===========
# Are there differences in activity patterns between weekdays and weekends?
# For this part the weekdays function may be of some help here. Use the dataset with the filled-in missing values for this part.

# 4.1 Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday 
# or weekend day.
library( chron )
cat( "\n")
datasetFullNoNA$date <- as.Date( datasetFullNoNA$date )
DayType <- is.weekend( datasetFullNoNA$date )
datasetFullNoNA <- cbind( datasetFullNoNA, DayType )
datasetFullNoNA$DayType <- as.factor( datasetFullNoNA$DayType )
levels( datasetFullNoNA$DayType )[ levels( datasetFullNoNA$DayType ) == "TRUE" ] <- "Weekend"
levels( datasetFullNoNA$DayType )[ levels( datasetFullNoNA$DayType ) == "FALSE" ] <- "Weekday"
str( datasetFullNoNA )
cat( "\n")

# 4.2 Make a panel plot containing a time series plot (i.e. type = "l"\color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval 
# (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
# See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
#par( mfcol = c( 1, 1) )

str( datasetFullNoNA )
datasetFullNoNA$DayType <- as.character( datasetFullNoNA$DayType )
MeanDayTypeDFWeekday <- datasetFullNoNA[ datasetFullNoNA$DayType == "Weekday", ]  
MeanDayTypeDFWeekend <- datasetFullNoNA[ datasetFullNoNA$DayType == "Weekend", ]  

WeekDayMean <- MeanDayTypeDFWeekday %>% group_by( interval ) %>% summarize( StepsWeekDay = mean( steps , na.rm = FALSE ) )
cat("\n")
str( WeekDayMean )
cat("\n")
WeekEndMean <- MeanDayTypeDFWeekend %>% group_by( interval ) %>% summarize( StepsWeekEnd = mean( steps , na.rm = FALSE ) )
cat("\n")
str( WeekEndMean )
cat("\n")

par( mfcol = c( 1, 2) )

# The interval with the highest average number of steps is 835 on weekdays, with 203 steps on average accross all weekdays.
maxStepsWeek <- max( WeekDayMean$StepsWeekDay )
maxStepsWeek # 
cat("\n")
maxIntervalWeek <- WeekDayMean[ WeekDayMean$StepsWeekDay == maxStepsWeek, 1]
maxIntervalWeek # 
cat("\n")

# The interval with the highest average number of steps is 915 on weekends, with 184 steps on average accross all weekends.
maxStepsEnd <- max( WeekEndMean$StepsWeekEnd )
maxStepsEnd 
cat("\n")
maxIntervalEnd <- WeekEndMean[ WeekEndMean$StepsWeekEnd == maxStepsEnd, 1]
maxIntervalEnd # 
cat("\n")

png( "graphQ42.png", width = 700, height = 700, unit = "px" )
par( mfrow = c( 2, 1) )
mygraph1 <- with( WeekDayMean,
                        plot( x = interval, y = StepsWeekDay, col = "green", type = "l" , ylim = c(0,220), xlim = c(0, 2400), 
                                main = "QUESTION 4 - Average count of steps for each interval across Weekdays\n(NAs replaced with prior/next day activity)" )
                )
print( mygraph1 )       
abline( v = maxIntervalWeek, col = "magenta", lty = 2, lwd = 4  )
abline( h = maxStepsWeek, col = "orange", lty = 2, lwd = 4  )

mygraph2 <- with( WeekEndMean,
                        plot( x = interval, y = StepsWeekEnd, col = "blue", type = "l" , ylim = c(0,220), xlim = c(0, 2400), 
                                main = "QUESTION 4 - Average count of steps for each interval across Weekends\n(NAs replaced with prior/next day activity)" ) 
                )             
print( mygraph2 )
abline( v = maxIntervalEnd, col = "magenta", lty = 2, lwd = 4  )
abline( h = maxStepsEnd, col = "orange", lty = 2, lwd = 4  )

dev.off()

