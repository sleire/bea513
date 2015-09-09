# Various option pricing models

BScall <- function(S,K,t,r,v){
  # option premium for B&S european call
  #
  # Args:
  # S - spot price
  # K - strike 
  # t - time to expiry in years
  # r - risk free interest rate
  # v - volatility
  #
  # Returns:
  # Option premium
  #
  d1 <- (log(S/K)+(r+0.5*v^2)*t)/(v*sqrt(t))
  d2 <- d1-v*sqrt(t)
  S*pnorm(d1)-K*exp(-r*t)*pnorm(d2)
}

BSput  <- function(S,K,t,r,v){
  # option premium for B&S european put
  #
  # Args:
  # S - spot price
  # K - strike 
  # t - time to expiry in years
  # r - risk free interest rate
  # v - volatility
  #
  # Returns:
  # Option premium
  #
  d1 <- (log(S/K)+(r+0.5*v^2)*t)/(v*sqrt(t))
  d2 <- d1-v*sqrt(t)
  K*exp(-r*t)*pnorm(-d2)-S*pnorm(-d1)
}

B76call <- function(F,K,t,r,v){
  # option premium for Black76 european call
  #
  # Args:
  # F - futures price
  # K - strike 
  # t - time to expiry in years
  # r - risk free interest rate
  # v - volatility
  #
  # Returns:
  # Option premium
  #
  d1 <- (log(F/K)+(0.5*v^2)*t)/(v*sqrt(t))
  d2 <- d1-v*sqrt(t)
  exp(-r*t)*(F*pnorm(d1)-K*pnorm(d2))
}

B76put  <- function(F,K,t,r,v){
  # option premium for Black76 european call
  #
  # Args:
  # F - futures price
  # K - strike 
  # t - time to expiry in years
  # r - risk free interest rate
  # v - volatility
  #
  # Returns:
  # Option premium
  #
  d1 <- (log(F/K)+(0.5*v^2)*t)/(v*sqrt(t))
  d2 <- d1-v*sqrt(t)
  exp(-r*t)*(K*pnorm(-d2)-F*pnorm(-d1))
}