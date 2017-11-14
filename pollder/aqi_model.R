library(data.table)
library(ggplot2)
source('calc_aqi_pas.R')

url <- 'http://www.stateair.net/web/assets/historical/'
bei <- c('Beijing_2017_HourlyPM25_created20170803.csv', 'Beijing_2016_HourlyPM25_created20170201.csv',
         'Beijing_2015_HourlyPM25_created20160201.csv', 'Beijing_2014_HourlyPM25_created20150203.csv',
         'Beijing_2013_HourlyPM2.5_created20140325.csv', 'Beijing_2012_HourlyPM2.5_created20140325.csv',
         'Beijing_2011_HourlyPM25_created20140709.csv', 'Beijing_2010_HourlyPM25_created20140709.csv',
         'Beijing_2009_HourlyPM25_created20140709.csv', 'Beijing_2008_HourlyPM2.5_created20140325.csv')
beijing <- rbindlist(lapply(paste0(url, '1/', bei[3:6]), fread, skip = 3))

# quick and dirty drop rows
beijing <- subset(beijing, Value !=-999)

# fix date and subset
beijing$Date <- as.Date(paste(beijing$Year, beijing$Month, beijing$Day, sep = "-"))
beijing <- beijing[, c('Date', 'Year', 'Month', 'Day', 'Hour', 'Value'), with=F]

# daily data
bd <- beijing[, .(Value=mean(Value), Year=mean(Year)), by = Date]
bd$AQI <- calc_aqi(bd$Value)$AQI
bd$Month <- month(bd$Date)

# melt and plot
bm <- melt(bd, id.vars = c('Date', 'Year'))
ggplot(bd, aes(x = Date, y = Value)) + geom_line()  #+ facet_grid(variable~.)
ggplot(bd, aes(x = Date, y = AQI)) + geom_line()  #

# first basic trend model
bd$d <- seq_along(bd$Date)
season <- lm(bd$Value ~ cos( 2*pi*bd$d / 365 ))
bd$season <- predict(season, data.frame(bd$d))
bd$ValSeas <- bd$Value - bd$season

# aqi trend model
aqi_season <- lm(bd$AQI ~ cos( 2*pi*bd$d / 365 ))
bd$aqi_season <- predict(aqi_season, data.frame(bd$d))

# plot with trend function
ggplot(bd, aes(x = Date)) + 
  geom_line(aes(y = Value)) + 
  geom_line(aes(y = season, colour = 'season')) + xlab("") + ylab("PM2.5 micrograms/m3") +
  theme(legend.position="none") + ggtitle("Beijing Particulate Matter PM2.5") +
  geom_hline(yintercept = 150, colour = "red4") +
  geom_hline(yintercept = 250, colour = "violetred4")

# plot detrended PM2.5 = PM2.5 - season
ggplot(bd, aes(x = Date)) + 
  geom_line(aes(y = ValSeas)) + 
  xlab("") + ylab("PM2.5 micrograms/m3") +
  theme(legend.position="none") + ggtitle("Beijing Particulate Matter PM2.5 - deseasonalized")

# plot with trend function
ggplot(bd, aes(x = Date)) + 
  geom_line(aes(y = AQI)) + 
  geom_line(aes(y = aqi_season, colour = 'aqi_season')) + xlab("") + ylab("US EPA AQI") +
  theme(legend.position="none") + ggtitle("Beijing Particulate Matter PM2.5 AQI") +
  geom_hline(yintercept = 200, colour = "red4") +
  geom_hline(yintercept = 300, colour = "violetred4")

# burn analysis for monthly contracts
burn <- setDT(bd)[, .(PAS = calc_pas(AQI)$PAS), by = .(Year, Month)]
burn <- reshape(burn, idvar = "Month", timevar = "Year", direction = "wide")

