setwd("~/Yandex.Disk.localized/projects/Course5/week2")
library("tidyr")
library("dplyr")
library("ggplot2")
library("scales")
library("lubridate")


read_df <- function() {
        df1 <-tbl_df(read.csv2("activity.csv", na.strings = NA))
        names <- names(df1)
        names <- strsplit(names, split = "[.]")
        df1 <<- separate(df1, col = 1 ,sep = ",", into = names[[1]])        
}
change_classes <- function() {
        df1 %>% mutate(steps = as.numeric(steps)) %>%
        mutate(interval = as.numeric(interval)) %>%
        mutate(date = as.POSIXct(date, format = "%Y-%m-%d")) ->> df1 
}



## plot sum of steps for each day
df1 %>%
        ggplot(aes(x = date,y =  steps)) + geom_bar(stat = "identity") +
        scale_x_datetime(date_breaks = "1 day", date_label = "%d-%m") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
        
## calculate mean and median of steps for each day        
df1 %>%
        summarise(mean = mean(steps,na.rm = TRUE), median = median(steps, TRUE))
        
        
## calculate mean per interval across all days
df1 %>%
        group_by(interval) %>%
        summarise(mean = mean(steps, na.rm = TRUE))  %>%
        ggplot(aes(x = interval, y = mean)) + geom_bar(stat = "identity")+
        scale_x_continuous(breaks = seq(0, 2500, by = 150))



## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
df1 %>%
        group_by(interval) %>%
        summarise(mean = mean(steps, na.rm = TRUE)) %>%
        filter(mean ==  max(mean))
        
        


## biggest interval per all averages
df1 %>%
        group_by(interval) %>%
        summarise(mean = mean(steps, na.rm = TRUE)) %>%
        filter(mean == max(mean))

## making df3 with average by interval
df1 %>%
        group_by(interval) %>%
        summarise(mean = mean(steps, na.rm = TRUE)) ->> df3

df2 <- df1

## total number of NA

sum(!complete.cases(df1))


## input missing values
for (i in 1:length(df2$steps)) {
        if (is.na(df2$steps[i]) == TRUE) {
                
                df2$steps[i] <- df3$mean[match(df2$interval[i], df3$interval)]  
        }
}


## make new histogramm wi NAs
df2 %>%
        ggplot(aes(x = date,y =  steps)) + geom_bar(stat = "identity") +
        scale_x_datetime(date_breaks = "1 day", date_label = "%d-%m") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

## calculate mean and median of steps for each day        
df2 %>%
        summarise(mean = mean(steps,na.rm = TRUE), median = median(steps, TRUE))




## make plots devided by weekdays
df2 <- mutate(df2, weekday = weekdays(df2$date)) 

        for (i in 1:length(df2$weekday)) {
                if (df2$weekday[i] == "Saturday" || df2$weekday[i] == "Sunday" ) {
                        df2$day_type[i] <- "Weekend"
                } else {
                        df2$day_type[i] <- "Weekday"
                }
        } 
df2$day_type <- as.factor(df2$day_type)


df2 %>% 
        ggplot(aes(x = interval, y = steps)) + geom_smooth() +
        theme_linedraw() + facet_grid(day_type ~ .)
        
        

