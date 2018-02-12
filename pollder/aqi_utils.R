# function library
###############################################################################
# 1. get_stateair_data
# 2. Calc_aqi
# 3. Calc_pas
###############################################################################


# function for downloading and aggregating air quality data from stateair.net
get_stateair_data <- function(files_vec){
  
  # Imports files in files_vec form stateair.net, imputes NAs, returns hourly and daily values
  #
  # Args:
  #   files_vec : Vector with character strings containing file names from stateair.net
  #
  # Returns:
  #   List with two data frames (data.table)
  #   1. Data frame with hourly obs: DateTime, Date, Year, Value (PM2.5 concenttration), ValueImp (NAs imputed with EMA)
  #   2. Same as nr 1, aggregated to daily: Value_NA = mean of value disregarding NAs, Value_HL = (MaxVal+MinVAL)/2
  
  # required packages
  require(data.table)
  require(imputeTS)
  require(lubridate)
  
  # the US Embassy data url
  url <- 'http://www.stateair.net/web/assets/historical/'
  
  # import files in files_vec
  dat <- rbindlist(lapply(paste0(url, '1/', files_vec), fread, skip = 3))
  
  # create datetime, replace -999 and negative values with NA and impute missing values
  dat$DateTime <- ymd_h(paste(dat$Year, dat$Month, dat$Day, dat$Hour, sep = '-'))
  dat$Value <- ifelse(dat$Value == -999, NA, dat$Value)
  dat$ValueImp <- ifelse(dat$Value == -999, NA, dat$Value)
  dat$ValueImp <- ifelse(dat$Value < 0, NA, dat$Value)
  dat$ValueImp <- na.ma(dat$ValueImp, k = 4, weighting = 'exponential')
  
  # create date and subset
  dat$Date <- as.Date(paste(dat$Year, dat$Month, dat$Day, sep = "-"))
  dat <- dat[, c('DateTime', 'Date', 'Year', 'Month', 'Value', 'ValueImp', 'QC Name'), with = F]
  
  # daily values
  dat_d <- dat[, .(Value=mean(ValueImp), Value_NA=mean(Value, na.rm = T), MaxVal=max(ValueImp), 
                   MinVal=min(ValueImp), Year=mean(Year), Month = mean(Month)), by = Date]
  dat_d$Value_HL <- (dat_d$MaxVal + dat_d$MinVal)/2
  
  return(list(dat, dat_d))
}


# function for calculating air quality index (AQI) from concentration measurements
calc_aqi <- function(con, 
                     # pm.2.5 concentration limits (default)
                     c_lo = c(0, 12.1, 35.5, 55.5, 150.5, 250.5, 350.5), 
                     c_up = c(12, 35.4, 55.4, 150.4, 250.4, 350.4, 500.4)
                     ) {
  # Computes the air quality index value for a pollutant.
  # https://en.wikipedia.org/wiki/Air_quality_index#Computing_the_AQI
  #
  # Args:
  #   con : Vector with concentration values
  #   c_lo: 7 element vector with lower intervals for pollutant aqi calculation
  #         PM2.5 is used as default, user can input other pollutants/ levels
  #   c_up: 7 element vector with upper intervals for pollutant aqi calculation
  #         PM2.5 is used as default, user can input other pollutants/ levels
  #
  # Returns:
  #   Data frame with original concentration and calculated aqi
  
  # the aqi index scale
  aqi_lo <- c(0, 51, 101, 151, 201, 301, 401)
  aqi_up <- c(50, 100, 150, 200, 300, 400, 500)
  
  # calculate the aqi
  aqi <- as.numeric()
  for (i in seq_along(con)){
    # find the level k (1-7) for aqi calculation
    if (con[i] >= c_lo[7]){
      k <- 7
    } else {
      k <- min(which((c_lo <= con[i] & con[i] <= c_up) == TRUE))
    }
    aqi[i] <- (aqi_up[k]-aqi_lo[k])*(con[i]-c_lo[k])/(c_up[k]-c_lo[k]) + aqi_lo[k]
  }
  # round the aqi to whole number and return df
  return(data.frame(Concentration = con, AQI = round(aqi)))
}


# function for calculating pollution alert score (PAS) from aqi time series
calc_pas <- function(aqi, limit = 300) {
  # Calculates the cumulative sum over an aqi limit value (PAS)
  #
  # Args:
  #   aqi   : Vector with aqi values
  #   limit : AQI limit for raising a pollution alert
  #
  # Returns:
  #   Data frame with total nr of obs, nr of obs over limit, Limit and PAS
  
  # the pas calculation
  pas <- sum(aqi[aqi > limit]-limit)
  npas <- length(aqi[aqi > limit])

  return(data.frame(TotObs = length(aqi), PasObs = npas, PAS = pas))
}

# # melt and plot
# dat_dm <- melt(datl[[2]][,!c('Year', 'MinVal', 'MaxVal'), with=F], id.vars = c('Date'))
# ggplot(dat_dm, aes(x = Date, y = value)) + geom_line() + facet_wrap( ~ variable)
# 
# # save to .csv
# write.csv2(datl[[2]], paste0('shenyang', '.csv'), row.names = F)
