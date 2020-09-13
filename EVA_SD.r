# Extreme Value Analysis for San Diego daily rainfall
# Wilder & Lamb
# September 2020

# install R-packages for script
rm(list=ls())
install.packages("extRemes")
install.packages("in2extRemes")
install.packages("ggplot2")
library(extRemes)
library(ggplot2)
library(dplyr)
install.packages("TSstudio")
library(TSstudio)
install.packages(c("leaflet", "sp"))
library(sp)
library(leaflet)

# Plot rain gauge location
df <- data.frame(longitude = runif(1, -117.16667, -117.16667), 
                 latitude = runif(1, 32.73333, 32.73333))

coordinates(df) <- ~longitude+latitude
leaflet(df) %>% addMarkers() %>% addTiles()

# Import the data and look at the first six rows
daily <- read.csv(file = 'DailyData.csv', header = TRUE) 
print(tail(daily))

# We can also plot this as a timeseries
datez <- seq(from = as.Date("1966-10-01"), to = as.Date("2019-09-30"), by = 1)
daily$DATE <- as.POSIXct(daily$DATE,format = "%m/%d/%y")
#plot the data
pltdaily <- ggplot(data=daily,  # the data frame
      aes(datez, PrecipMM)) +   # the variables of interest
      geom_bar(stat="identity") +   # create a bar graph
      xlab("Date") + ylab("Precipitation (mm)") +  # label the x & y axes
      ggtitle("Daily Precipitation for San Diego")  # add a title

print(pltdaily)

# histogram of the daily rainfall appear skewed to the left
RainDailyMM<- daily$PrecipMM
hist(RainDailyMM)

# So, using MATLAB we found the peak 24-hour storm event for each WY from 1967-2019
max24hr <- read.csv(file = 'Max24hrStorm.csv', header = TRUE)
print(tail(max24hr))

# histogram of the annual maximum appear normally distributed
Rain24hrmaxMM <- max24hr$PrecipMM
hist(Rain24hrmaxMM)


# To conduct an extreme value analysis, we can do this quite easily in R by using the GEV fit in extRemes library
fit = fevd(x=PrecipMM,data=max24hr, location.fun= ~1, scale.fun=~1,
            shape.fun=~1, use.phi=FALSE, type="GEV",units="mm",na.action=na.fail)

# Let's now plot our results
plot(fit)

# For our study we are interested in understanding what the 2-year, 10-year, 25-year,50-year, and 100-year storm events.
# Data are assumed to be  stationary.
print(return.level(fit,return.period = c(2,10, 25, 50,100)))

### In summary, it would seem that the max 24-hour storm event under investigation (WY 2017 = 59.4mm) did not exceeded the 50-year, 24-hour storm event according to the Generalized extreme value distribution at the San Diego Airport rain gauge. 

### It is worth noting that the rain gauge is approximately 5-miles west of the investigation site, and therefore, does introduce spatial and temporal variability. Also, this statistical method does not include the impact of climate change and treats the dataset as being stationary.

### References:
### https://cran.r-project.org/web/packages/extRemes/extRemes.pdf
### https://www.sandiego.gov/sites/default/files/drainage_design_manual_jan2017.pdf
### https://science.sciencemag.org/content/319/5863/573