# setwd("~/Documents/github_repositories/reproducible_research/project_1")
data <- read.csv('activity.csv')
library('lubridate')
library('ggplot2')
library('dplyr')
library('knitr')

## part 1: plot steps per day
data$date <- as.POSIXct(data$date)
plot1 <- ggplot(data, aes(x = date, y = steps)) +
    geom_histogram(stat = 'identity') + 
    labs(title = "Steps Per Day", x = 'Date', y = 'Number of Steps') +  
    scale_x_datetime()
plot1    

## rearrange data for calculations
tot.steps.day <- data %>%
    group_by(date) %>%
    summarise(tot.steps = sum(steps))

## mean calculation
mean.steps.day <- mean(tot.steps.day$tot.steps, na.rm = TRUE)

## median calculation
median.steps.day <- median(tot.steps.day$tot.steps, na.rm = TRUE)

int.mean <- data %>%
    group_by(interval) %>%
    summarise(mean.steps = mean(steps, na.rm = TRUE))

plot2 <- ggplot(int.mean, aes(x = interval, y = mean.steps)) +
    theme(legend.position = 'none') + 
    geom_line(aes(color = mean.steps), size = 1.4) + 
    geom_hline(aes(yintercept = mean(int.mean$mean.steps))) + 
    annotate('text', x = 4, y = 45, label = 'mean') +
    labs(title = 'Mean Steps Per Interval') + 

plot2

## interval with max mean steps per day
max.int.steps <-filter(int.mean, mean.steps == max(mean.steps))

## part 3
inc.cases <- sum(!complete.cases(data))

data3 <- data
data3 <- left_join(data3, int.mean, by = 'interval')
data3$steps[which(is.na(data3$steps))] <- data3$mean.steps[is.na(data3$steps)]

plot4 <- ggplot(data3, aes(x = date, y = steps)) +
    geom_histogram(stat = 'identity') + 
    labs(title = "Steps Per Day", x = 'Date', y = 'Number of Steps') + 
    scale_x_datetime()
plot4

tot.steps.day.data3 <- data3 %>%
    group_by(date) %>%
    summarise(tot.steps = sum(steps))

imp.mean <- mean(tot.steps.day.data3$tot.steps)
imp.median <-median(tot.steps.day.data3$tot.steps)

data.steps.sum <- sum(data$steps, na.rm = TRUE)
data3.steps.sum <- sum(data3$steps)

## Minimal change after imputing NA values. This could be a result of my imputation method.

## part 4
data4 <- data3
data4$wday <- wday(data4$date)
data4$week.cat <- NA
data4$week.cat[which(data4$wday > 5)] <- 'weekend'
data4$week.cat[is.na(data4$week.cat)] <- 'weekday'
data4$week.cat <- as.factor(data4$week.cat)

int.data4 <- data4 %>%
    group_by(week.cat, interval) %>%
    summarise(mean.steps = mean(steps))

plot5 <- ggplot(int.data4, aes(x = interval, y = mean.steps, group = week.cat)) +
    geom_line() + facet_wrap(~week.cat, ncol = 1) + 
    labs(title = 'Weekday vs weekend: mean steps per internval', x = 'Interval', y = 'Mean steps')

