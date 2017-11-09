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