##Load Essential Libraries
library(tidyverse)
library(ggplot2)
library(gsubfn)
library(mosaic)
library(lubridate)
library(dplyr)
library(knitr)
library(caret)
library(gmodels)
library(lattice)
library(ggplot2)
library(gridExtra)
library(Kmisc)
library(ROCR)
library(corrplot)
library(forecast)
library(h2o)
library(RCurl)
library(bitops)
library(RColorBrewer)
library(boot)
##Set drive which files will be pulled from
setwd("C:\\Users\\Grant\\Desktop\\STATS\\Computing With R")
##Load Data taken from open meteo historical API website
gambier70.75 <- as_tibble(read.csv("Gambier.Weather.1970-1975.csv"))
gambier16.21 <- as_tibble(read.csv("Gambier.Weather.2016-2021.csv"))
gambier71.21 <- as_tibble(read.csv("Gambier.Weather.1971-2021.csv"))
hourlyGambier16.21 <- as_tibble(read.csv("Hourly.Gambier.Weather.2016-2021.csv"))
hourlyGambier70.75 <- as_tibble(read.csv("Hourly.Gambier.Weather.1970-1975.csv"))
##Had to edit these files in notes to remove rows that broke it, but eventually they worked
##Renaming the data with original column titles was difficult so I changed them manually using wordpad
##Now it's time to format the data, want to add day and month, adjust some strings as well
gambier70.75 <- gambier70.75 %>%
  add_column("Day" = lubridate::day(gambier70.75$time)) %>%
  add_column("Month" = lubridate::month(gambier70.75$time , label = TRUE)) %>%
  add_column("Year" = lubridate::year(gambier70.75$time)) %>%
  mutate(sunrise.iso8601. = gsub(".*T", "", gambier70.75$sunrise.iso8601.)) %>%
  mutate(sunset.iso8601. = gsub(".*T", "", gambier70.75$sunset.iso8601.))

sum(is.na(gambier70.75))

gambier16.21 <- gambier16.21 %>%
  add_column("Day" = lubridate::day(gambier16.21$time)) %>%
  add_column("Month" = lubridate::month(gambier16.21$time , label = TRUE)) %>%
  add_column("Year" = lubridate::year(gambier16.21$time)) %>%
  mutate(sunrise.iso8601. = gsub(".*T", "", gambier16.21$sunrise.iso8601.)) %>%
  mutate(sunset.iso8601. = gsub(".*T", "", gambier16.21$sunset.iso8601.)) %>%
  rename("ActualTempMin" = "ActualTempmMin")

gambier71.21 <- gambier71.21 %>%
  add_column("Day" = lubridate::day(gambier71.21$time)) %>%
  add_column("Month" = lubridate::month(gambier71.21$time , label = TRUE)) %>%
  add_column("Year" = lubridate::year(gambier71.21$time)) %>%
  mutate(sunrise.iso8601. = gsub(".*T", "", gambier71.21$sunrise.iso8601.)) %>%
  mutate(sunset.iso8601. = gsub(".*T", "", gambier71.21$sunset.iso8601.))

hourlyGambier16.21 <- hourlyGambier16.21 %>%
  add_column("Month" = lubridate::month(hourlyGambier16.21$time , label = TRUE)) %>%
  add_column("Year" = lubridate::year(hourlyGambier16.21$time)) %>%
  add_column("Day" = lubridate::day(hourlyGambier16.21$time))

##Fix some weird stuff in the data
for(i in 1:24){
  hourlyGambier70.75 <- hourlyGambier70.75[-1,]
}

hourlyGambier70.75 <- hourlyGambier70.75 %>%
  add_column("Month" = lubridate::month(hourlyGambier70.75$time , label = TRUE)) %>%
  add_column("Year" = lubridate::year(hourlyGambier70.75$time)) %>%
  add_column("Day" = lubridate::day(hourlyGambier70.75$time))



sum(is.na(gambier16.21))
str(gambier70.75)
##Datasets look good and there are no missing values(NA)
##Put together the dataset for a corn season
cornSeason16.21 <- gambier16.21 %>%
  filter(Month == "Apr" | Month == "May" | Month == "Jun" | Month == "Jul" | Month == "Aug") %>%
  mutate(Day = ifelse(Month == "Apr" & Day < 20 , NA , Day)) %>%
  na.omit()

cornSeason70.75 <- gambier70.75 %>%
  filter(Month == "Apr" | Month == "May" | Month == "Jun" | Month == "Jul" | Month == "Aug") %>%
  mutate(Day = ifelse(Month == "Apr" & Day < 20 , NA , Day)) %>%
  na.omit()

cornHourly16.21 <- hourlyGambier16.21 %>%
  filter(Month == "Apr" | Month == "May" | Month == "Jun" | Month == "Jul" | Month == "Aug") %>%
  mutate(Day = ifelse(Month == "Apr" & Day < 20 , NA , Day)) %>%
  na.omit()

cornHourly70.75 <- hourlyGambier70.75 %>%
  filter(Month == "Apr" | Month == "May" | Month == "Jun" | Month == "Jul" | Month == "Aug") %>%
  mutate(Day = ifelse(Month == "Apr" & Day < 20 , NA , Day)) %>%
  na.omit()

##Look at some aspects of the dataset
mean(cornSeason16.21$ActualTempMax)
#Max Temp
dev.off()
plot(cornSeason16.21$ActualTempMax, pch=20, xlab="Day", ylab="Max Temp (F°)" , col = "red")
histogram(cornSeason16.21$ActualTempMax, pch=20, xlab="Max Temp (F°)", col = "red")
histogram(cornSeason70.75$ActualTempMax, col = "blue" , add = TRUE)

hist(cornSeason70.75$ActualTempMax, pch=20, xlab="Max Temp (F°)", col = "blue" , main ="Corn Seasons")
hist(cornSeason16.21$ActualTempMax, pch=20, xlab="Max Temp (F°)", col = "red" , add = TRUE)
legend(40 , 200 , legend=c("70-75" , "16-21") , fill=c("blue" , "red"))


plot(cornSeason70.75$ActualTempMax, pch=20, xlab="Max Temp (F°)", col = "red")
plot(cornSeason16.21$ActualTempMax, pch=20, xlab="Max Temp (F°)", col = "blue" , add= TRUE)


#Min Temp
plot(cornSeason16.21$ActualTempMin, pch=20, xlab="Day", ylab="Min Temp (F°)" , col = "lightblue")
histogram(cornSeason16.21$ActualTempMin, pch=20, xlab="Min Temp (F°)", col = "lightblue")

#Rain Fall
plot(cornSeason16.21$rain_sum.inch., pch=20, xlab="Day", ylab="Rain (Inches)" , col = "blue")
histogram(cornSeason16.21$rain_sum.inch., pch=20, xlab="Rain (Inches)", col = "blue")

##Build the Model for Rain forecasting:

simulateRain <- function(inData , iterations){
  sumPrecipitation = NULL
  for(i in 1:iterations){
    rainFallen = sample(inData$rain_sum.inch. , 800/5 , replace = TRUE)
    sumPrecipitation[i]= sum(rainFallen)
  }
  return(sumPrecipitation)
}

sum(cornSeason16.21$rain_sum.inch.) / 5
rainResults <- simulateRain(cornSeason16.21 , 100000)
histogram(rainResults, pch=20, xlab="Rain (Inches)", col = "blue")
##Results look like they are fairly decent compared to average across the 5 years here

##Build the Model for Max Temp Forecasting
simulateMaxTemp <- function(inData , iterations){
  meanMax = NULL
  for(i in 1:iterations){
    daysPassed = sample(inData$ActualTempMax , 800/5 , replace = TRUE)
    meanMax[i]= mean(daysPassed)
  }
  return(meanMax)
}
x <- as.data.frame(simulateMaxTemp(cornSeason16.21 , 100000))
ggplot(x , aes(x = x$`simulateMaxTemp(cornSeason16.21, 1e+05)` )) +
  geom_histogram(aes(fill = "red")) +
  ggtitle("simulateMaxTemp") +
  xlab("Mean Daily Max Temp")

maxResults <- simulateMaxTemp(cornSeason16.21 , 100000)
plot(maxResults, pch=20, xlab="Growing Season", ylab="Max Temp (F°)" , col = "red")
histogram(maxResults, pch=20, xlab="Max Temp (F°)", col = "red")

##Build the Model for Min Temp Forecasting
simulateMinTemp <- function(inData , iterations){
  meanMin = NULL
  for(i in 1:iterations){
    daysPassed = sample(inData$ActualTempMin , 800/5 , replace = TRUE)
    meanMin[i]= mean(daysPassed)
  }
  return(meanMin)
}
minResults <- simulateMinTemp(cornSeason16.21 , 100000)

plot(minResults, pch=20, xlab="Day", ylab="Min Temp (F°)" , col = "lightblue")
histogram(minResults, pch=20, xlab="Min Temp (F°)", col = "lightblue")

##Build the Model for if there is frost
simulateFrost <- function(inData , iterations){
  frostDaysSpread = NULL
  for(i in 1:iterations){
    frostDays = sample(inData$ActualTempMin , 800/5 , replace = TRUE)
    frostDays = frostDays[frostDays <= 32]
    frostDays = length(frostDays) 
    frostDaysSpread[i] = frostDays
  }
  return(frostDaysSpread)
}
frostResults <- simulateFrost(cornSeason16.21 , 100000)
mean(frostResults)
histogram(frostResults, pch=20, xlab="Days with Frost", col = "snow2")
##Compare against actual
frosty <- cornSeason16.21$ActualTempMin
frosty<- frosty[frosty <= 32]
length(frosty)
##Build a Model for Flooding:
simulateFlooding <- function(inData , iterations){
  floodingDaysSpread = NULL
  for(i in 1:iterations){
    dayRainInches = sample(inData$precipitation..inch. , 3859 , replace = TRUE)
    rainRate = dayRainInches 
    rainRate = rainRate[rainRate != Inf]
    rainRate = rainRate %>% na.omit()
    floodDays = rainRate[rainRate >= .3]
    floodDays = length(floodDays) 
    floodingDaysSpread[i] = floodDays
  }
  return(floodingDaysSpread)
}
floodingResults <- simulateFlooding(cornHourly16.21 , 100000)
histogram(floodingResults, pch=20, xlab="Days with Heavy Rain", col = "steelblue")
mean(floodingResults)
##Compare against actual:
rainRate <- cornHourly16.21$precipitation..inch.
floodDays = rainRate[rainRate >= .3]
floodDays = length(floodDays)
floodDays
max(hourlyGambier16.21$precipitation..inch.)
##Not too bad at all

##Time to Bring all of these Functions Together:
simulateTheHarvest <- function(inData , seasons){
  heavyRainDays <- simulateFlooding(cornHourly16.21 , seasons)
  print(1)
  frostDays <- simulateFrost(inData , seasons)
  print(2)
  avgMaxTemp <- simulateMaxTemp(inData , seasons)
  print(3)
  avgMinTemp <- simulateMinTemp(inData , seasons)
  print(4)
  rainSum <- simulateRain(inData , seasons)
  print(5)
  seasonNum <- seq(seasons)
  print(6)
  returnFrame <- data.frame(seasonNum,
                            avgMaxTemp,
                            avgMinTemp,
                            rainSum,
                            frostDays,
                            heavyRainDays)
  return(returnFrame)
}

harvestResults <- simulateTheHarvest(cornSeason16.21 , 1000000)

##Visualize the Future Harvests
grid.table(head(harvestResults))

par( mfrow= c(1,3) )
hist(harvestResults$rainSum,xlab="Season Rain Total", col = "steelblue" , main = "Rain Sum")
hist(harvestResults$avgMinTemp,xlab="Season Avg Min Temp", col = "lightblue" , main="Avg Min Temp")
hist(harvestResults$avgMinTemp,xlab="Season Frost Days", col = "snow2" , main="Days With Frost")






##Write a function to break down the harvest results
breakDownTheHarvest <- function(inData){
  
  perfectSeason <- inData %>%
    filter(avgMaxTemp >= 75 & avgMaxTemp <= 86 & frostDays == 0 & heavyRainDays == 0 & rainSum >= 15) %>%
    nrow()
  idealOutcome <- perfectSeason / nrow(inData)
  
  worstSeason <- inData %>%
    filter(avgMaxTemp < 75 & frostDays >= 1 & heavyRainDays >= 1 & rainSum < 15) %>%
    nrow()
  worstSeason <- worstSeason / nrow(inData)
  
  okaySeason <- inData %>%
    filter(avgMaxTemp >= 75 & avgMaxTemp <= 86 & frostDays > 1 & heavyRainDays >= 2) %>%
    nrow()
  okaySeason <- okaySeason / nrow(inData)
  
  goodSeason <- inData %>%
    filter(avgMaxTemp >= 75 & avgMaxTemp <= 86 & frostDays <= 1 & heavyRainDays <= 1) %>%
    nrow()
  goodSeason <- goodSeason / nrow(inData)
  
  tooCold <- inData %>%
    filter(avgMaxTemp < 75) %>%
    nrow()
  tooCold <- tooCold / nrow(inData)
  
  tooHot <- inData %>%
    filter(avgMaxTemp >= 86 ) %>%
    nrow()
  tooHot <- tooHot / nrow(inData)
  
  notEnoughRain <- inData %>%
    filter(rainSum < 15 ) %>%
    nrow()
  notEnoughRain <- notEnoughRain / nrow(inData)
  
  returnFrame = data.frame(idealOutcome , okaySeason , goodSeason, worstSeason , notEnoughRain , tooCold , tooHot)
  return(returnFrame)
}

breakDownResults <- breakDownTheHarvest(harvestResults)
grid.table(breakDownResults)


##Confidence Interval setup for idealOutcome

confidence = NULL
for(i in 1:100){
  x <- simulateTheHarvest(cornSeason16.21 , 10000)
  x <- breakDownTheHarvest(x)
  confidence[i] = x$idealOutcome[1]
}
##Make the confidence interval
mean_value <- mean(confidence)
n <- length(confidence)
standard_deviation <- sd(confidence)
standard_error <- standard_deviation / sqrt(n)
t_score <- qt(p = .025 , df = n - 1 , lower.tail = F)
margin_error <- t_score * standard_error
lowBound <- mean_value - margin_error
upBound <- mean_value + margin_error
print(c(lowBound, upBound))
hist(confidence , xlab = "idealOutcome%" , main="idealOutcome Spread" , col = "green")

##Let's Adjust for Global Warming
##Check for normal distributions
histogram(harvestResults$avgMaxTemp)
histogram(harvestResults$avgMinTemp)
histogram(harvestResults$rainSum)
histogram(gambier16.21$ActualTempMax)

histogram(cornSeason16.21$ActualTempMin)
histogram(cornHourly16.21$rain..inch.)
##Build the Function that accounts for global warming
##Function to Simulate heavy rain days in the future, accounting for global warming
harvest70.75 <- simulateTheHarvest(cornSeason70.75 , 1000000)
histogram(harvestResults$heavyRainDays , xlab = "Heavy Rain Days" , main = "simulateTheHarvest()" ,col = "steelblue")
simulateFloodingFuture <- function(inData , years , iterations){
  
  rainRate <- cornHourly16.21$precipitation..inch.
  rainRate = rainRate[rainRate >= .3]
  rainRate = length(rainRate)
  rainRate = rainRate/5
  
  rainRate2 <- cornHourly70.75$precipitation..inch.
  rainRate2 = rainRate2[rainRate2 >= .3]
  rainRate2 = length(rainRate2)
  rainRate2 = rainRate2 / 5
  

  rainMeanChange <- (rainRate - rainRate2) / 50
  rainMeanChange <- rainRate + (rainMeanChange * years)
  floodDays = NULL
  for(i in 1:iterations){
    heavyRainSpread <- rexp(1 , rate = (1/rainMeanChange))
    heavyRainSpread = heavyRainSpread + .5
    heavyRainSpread <- floor(heavyRainSpread)
    if(is.na(heavyRainSpread[1])){
      heavyRainSpread[1] = 0
      floodDays[i] = heavyRainSpread 
    }
    floodDays[i] = heavyRainSpread  
  }
  return(floodDays)
}
test <- simulateFloodingFuture(harvestResults , 1 , 100000)
mean(test)

##Make function to simulate frost days in the future accounting for global warming
simulateFrost <- function(inData , iterations){
  frostDaysSpread = NULL
  for(i in 1:iterations){
    frostDays = sample(inData$ActualTempMin , 800/5 , replace = TRUE)
    frostDays = frostDays[frostDays <= 32]
    frostDays = length(frostDays) 
    frostDaysSpread[i] = frostDays
  }
  return(frostDaysSpread)
}
simulateFrostFuture <- function(inData , years, iterations){
  minTempMean <- mean(harvestResults$frostDays)
  minTempRateChange <- mean(harvestResults$frostDays) - mean(harvest70.75$frostDays)
  minTempRateChange <- minTempRateChange / 50
  minTempRateChange <- minTempMean + (minTempRateChange * years)
  frostDays = NULL
  for(i in 1:iterations){
    frostSpreads <- rexp(1 , rate = (1/minTempRateChange))
    frostSpreads = frostSpreads + .5
    frostSpreads <- floor(frostSpreads)
    if(is.na(frostSpreads[1])){
      frostSpreads[1] = 0
      floodDays[i] = heavyRainSpread 
    }
    frostDays[i] = frostSpreads
  }
  return(frostDays)
}

frostTest <- simulateFrostFuture(harvestResults , 1 , 1000)

histogram(harvestResults$avgMaxTemp , xlab="Mean Max Temp" , main = "simulateTheHarvest()" , col = "red")


##Make Function to Simulate future harvests with global warming conditions
simulateFutureHarvest <- function(inData ,yearsInFuture , iterations){
  
  maxTempMean <- mean(inData$avgMaxTemp)
  maxTempSD <- sd(inData$avgMaxTemp)
  maxTempRateChange <- mean(cornSeason16.21$ActualTempMax) - mean(cornSeason70.75$ActualTempMax)
  maxTempRateChange <- maxTempRateChange / 50
  maxTempRateSD <- sd(cornSeason16.21$ActualTempMax) - sd(cornSeason70.75$ActualTempMax)
  maxTempRateSD <- maxTempRateSD / 50
  
  minTempMean <- mean(inData$avgMinTemp)
  minTempSD <- sd(inData$avgMinTemp)
  minTempRateChange <- mean(cornSeason16.21$ActualTempMin) - mean(cornSeason70.75$ActualTempMin)
  minTempRateChange <- minTempRateChange / 50
  minTempRateSD <- sd(cornSeason16.21$ActualTempMin) - sd(cornSeason70.75$ActualTempMin)
  minTempRateSD <- minTempRateSD / 50
  
  precipRateMean <- mean(inData$rainSum)
  precipRateSD <- sd(inData$rainSum)
  precipRateChange <- mean(cornSeason16.21$rain_sum.inch.) - mean(cornSeason70.75$rain_sum.inch.)
  precipRateChange <- precipRateChange / 50
  precipRateSD <- sd(cornSeason16.21$rain_sum.inch.) - sd(cornSeason70.75$rain_sum.inch.)
  precipRateChangeSD <- precipRateSD / 50
  
  seasonNum <- seq(iterations)
  print(1)
  avgMaxTemp <- rnorm(iterations , mean = maxTempMean + (maxTempRateChange * yearsInFuture),maxTempSD + (maxTempRateSD * yearsInFuture))
  print(2)
  avgMinTemp <- rnorm(iterations , mean = minTempMean + (minTempRateChange * yearsInFuture),minTempSD + (minTempRateSD * yearsInFuture))
  print(3)
  rainSum <- rnorm(iterations , mean = precipRateMean + (precipRateChange * yearsInFuture),abs(precipRateSD + (precipRateChangeSD * yearsInFuture)))
  print(4)
  frostDays <- simulateFrostFuture(inData , yearsInFuture , iterations)
  print(5)
  heavyRainDays <- simulateFloodingFuture(inData , yearsInFuture , iterations)
  print(6)
  returnFrame <- data.frame(seasonNum , 
                            avgMaxTemp , 
                            avgMinTemp ,
                            rainSum,
                            frostDays ,
                            heavyRainDays)
  return(returnFrame)
}
test <- simulateFutureHarvest(harvestResults , 1 , 1000)
test <- breakDownTheHarvest(test)

futureHarvestResults <- simulateFutureHarvest(harvestResults , 100 , 1000000)
futureHarvestResultsBreakdown <- breakDownTheHarvest(futureHarvestResults)

##Visualize/Breakdown the results
histogram(futureHarvestResults$avgMaxTemp , xlab="Mean Max Temp F° (100 years in future)" , main = "simulateFutureHarvest()" , col = "red")
grid.table(head(futureHarvestResults))
grid.table(futureHarvestResultsBreakdown)


futureHarvestResults100 <- simulateFutureHarvest(harvestResults , 100 , 100000)
futureHarvestResults50 <- simulateFutureHarvest(harvestResults , 50 , 100000)

years100inFuture <- breakDownTheHarvest(futureHarvestResults100)
years50inFuture <- breakDownTheHarvest(futureHarvestResults50)


##Make function to simulate x years into future and get data for each
simulateManyYears <- function(inData , years , iterations){
  yearInFuture <- seq(years)
  returnFrame <- data.frame(yearInFuture = yearInFuture,
                            idealOutcome = "" , 
                            okaySeason = "" ,
                            goodSeason = "",
                            worstSeason = "" ,
                            notEnoughRain = "",
                            tooCold = "",
                            tooHot = "") 
  for(i in 1:years){
    print(i)
    harvest <- simulateFutureHarvest(inData , i , iterations)
    breakDown <- breakDownTheHarvest(harvest)
    returnFrame$idealOutcome[i] = breakDown$idealOutcome[1]
    returnFrame$okaySeason[i] = breakDown$okaySeason[1]
    returnFrame$goodSeason[i] = breakDown$goodSeason[1]
    returnFrame$worstSeason[i] = breakDown$worstSeason[1]
    returnFrame$notEnoughRain[i] = breakDown$notEnoughRain[1]
    returnFrame$tooCold[i] = breakDown$tooCold[1]
    returnFrame$tooHot[i] = breakDown$tooHot[1]
  }
  return(returnFrame)
}
yearsSimulated <- simulateManyYears(harvestResults , 500 , 10000)
grid.table(head(yearsSimulated))

longYearsSimulated <- yearsSimulated %>%
  pivot_longer(-yearInFuture, names_to = "variable", values_to = "value")

ggplot(longYearsSimulated , aes(x = yearInFuture , y = as.numeric(value) , color = variable)) +
  ylim(0 , 1)+
  ylab("Projected Outcome %")+
  xlab("Year in Future") +
  geom_line(size = 1)


str(longYearsSimulated)
