library(readr)
library(ggplot2)
library(ggfortify)
library(zoo)
library(forecast)

tute1 <- read_csv("http://otexts.com/fpp2/extrafiles/tute1.csv")

View(tute1)

# The [, -1] removes the first column which contains the quarters 
mytimeseries <- ts(tute1[, -1], start=1981, frequency = 4)

autoplot(mytimeseries, facets =T)
getwd()

# the seconf argument is required because the excel sheet has two header rows
retaildata <- readxl::read_excel("retail.xlsx", skip=1)

myts <- ts(retaildata[, "A3349873A"], frequency = 12, start=c(1984,4))

autoplot(myts)

ggseasonplot(myts)

ggsubseriesplot(myts)

gglagplot(myts)

ggAcf(myts)


