# load R-packages and source functions in aqi_utils.R
###############################################################################
library(yuima)
#library(yuimaGUI)
library(data.table)
library(ggplot2)
library(gridExtra)
library(latex2exp)
#library(sde)
source('aqi_utils.R')
###############################################################################

# import and prep data
###############################################################################
# bei <- c('Beijing_2017_HourlyPM25_created20170803.csv', 'Beijing_2016_HourlyPM25_created20170201.csv',
#          'Beijing_2015_HourlyPM25_created20160201.csv', 'Beijing_2014_HourlyPM25_created20150203.csv',
#          'Beijing_2013_HourlyPM2.5_created20140325.csv', 'Beijing_2012_HourlyPM2.5_created20140325.csv',
#          'Beijing_2011_HourlyPM25_created20140709.csv', 'Beijing_2010_HourlyPM25_created20140709.csv',
#          'Beijing_2009_HourlyPM25_created20140709.csv', 'Beijing_2008_HourlyPM2.5_created20140325.csv')
# 
# # select 2009-2017 and order by year
# bei <- bei[1:9]
# bei <- sort(bei)
# 
# # import beijing data for 2010-2017 with the sourced function get_stateair_data
# dat <- get_stateair_data(files_vec = bei)[[2]]
# 
# dat <- fread('aqi.csv')
# 
# # add day number (d) and subset to remove missing values in Q1 2009
# dat$d <- seq_along(dat$Date)
# dat <- subset(dat, Date >= as.Date('2009-03-01'))
# 
# # concentration values always positive -> model log value
# dat$LogValue <- log(dat$Value)
###############################################################################

# alternative data, http://young-0.com
###############################################################################
dat <- fread('aqi.csv', drop = 'city')
dat$day <- as.Date(dat$day)

# daily values
dat <- dat[, .(avg=round(mean(avg)), conc=round(mean(conc, na.rm = T))), by = day]

#dat$conc <- ifelse(dat$conc == 0,2,dat$conc)

# add day number (d) and subset to remove missing values in Q1 2009
dat$d <- seq_along(dat$day)
#dat <- subset(dat, day >= as.Date('2009-03-01'))

# concentration values always positive -> model log value
dat$LogValue <- log(dat$con)
dat$dLogValue <- c(0,diff(dat$LogValue))
###############################################################################

# model trend function and deseasonalize
###############################################################################
# fit sinusoidal trend function for logValue
Logseason <- lm(dat$LogValue ~ dat$d + sin( 2*pi*dat$d / 365 ) + cos( 2*pi*dat$d / 365 ))
summary(Logseason)

# calulate coefficients in the seasonal function Logvalue
A <- Logseason$coefficients[1]
B <- Logseason$coefficients[2]
C <- sqrt(Logseason$coefficients[3]**2 + Logseason$coefficients[4]**2)
D <- atan(Logseason$coefficients[4]/Logseason$coefficients[3])-pi

# print coefficients
coef <- data.frame(A, B, C, D)
rownames(coef) <- 'PM2.5 trend'
coef

# add the estimated trend function to the data frame and subtract trend to deseasonalize
dat$Logseason <- predict(Logseason, data.frame(dat$d))
dat$LogValSeas <- dat$LogValue - dat$Logseason # deseasonalized value

# kernel dansity for log diff
ggplot(dat, aes(x = dLogValue)) + geom_density()

# experiment impute 95% CI
#q <- quantile(dat$LogValSeas,c(0.2,0.8))
#dat$impLogValSeas <- ifelse(dat$LogValSeas < q[1],q[1],
#                            ifelse(dat$LogValSeas > q[2],q[2],dat$LogValSeas))
#dat$LogValSeas <- dat$impLogValSeas
#dat$impLogValSeas <- ifelse(dat$LogValSeas < q[1],q[1],dat$LogValSeas)
#dat$impLogValSeas <- ifelse(dat$LogValSeas > q[2],q[2],dat$LogValSeas)

###############################################################################

# stochastic models
###############################################################################

# approach
# A. Suggest competing models
# B. Model selection using AIC


# A1. a simple ou-model
###############################################################################
ou <- setYuima(data=setData(dat$LogValSeas, delta = 1/365), 
                    model = setModel(drift = c("-theta*x"), 
                                     diffusion = "sigma",
                                     solve.variable = "x"))

# lasso & qmle estimates of ou parameters
ou_par <- lasso(ou, 
                     start = list(theta = 0, sigma = 0.1), 
                     method = 'L-BFGS-B' 
                     #lower=list(theta=0,sigma=0)
                     #upper=list(theta=100,sigma=40)
)

# qmle, alternatively use ou_jump_par$lasso
sigma_est <- ou_par$mle[1]
theta_est <- ou_par$mle[2]
#sigma_est <- ou_par$mle[1]
#theta_est <- ou_par$mle[2]


# simulate with estimated parameters
ou_sim <- simulate(ou@model, 
                        true.parameter = list(theta = theta_est, sigma = sigma_est),
                        sampling = setSampling(delta = 1/365, n = dim(dat)[1]-1),
                        xinit = last(dat$LogValSeas))

#dat$p_est <- exp(ou_sim@data@original.data)
#dat$p_est <- exp(dat$Logseason+ou_sim@data@original.data)
dat$p_est <- exp(dat$Logseason)
###############################################################################

# A2. a simple ou-model with gaussian jumps
###############################################################################
ou_jump <- setYuima(data=setData(dat$LogValSeas, delta = 1/365), 
                    model = setModel(drift = c("-theta*x"), 
                                     diffusion = "sigma",
                                     jump.coeff = "1", 
                                     measure=list(intensity = "10", df = list("dnorm(z, 0, 0.2)")),
                                     measure.type = "CP", 
                                     solve.variable = "x"))

# lasso & qmle estimates of ou_jump parameters
ou_jump_par <- lasso(ou_jump, 
                    start = list(theta = 0, sigma = 0.1), 
                    method = 'L-BFGS-B' 
                    #lower=list(theta=0,sigma=0)
                    #upper=list(theta=100,sigma=40)
                    )

# qmle, alternatively use ou_jump_par$lasso
sigma_est <- ou_jump_par$mle[1]
theta_est <- ou_jump_par$mle[2]


# simulate with estimated parameters
ou_jump_sim <- simulate(ou_jump@model, 
                        true.parameter = list(theta = theta_est, sigma = sigma_est),
                        sampling = setSampling(delta = 1/365, n = dim(dat)[1]-1),
                        xinit = last(dat$LogValSeas))
###############################################################################

#dat$p_est <- exp(dat$Logseason+ou_jump_sim@data@original.data)


###############################################################################

# plots
###############################################################################

# PM2.5 plot with trend function and PM2.5 limits equivalent to 200 and 300 AQI
ggplot(dat, aes(x = day)) + 
  geom_line(aes(y = avg)) + theme_bw() +
#  geom_line(aes(y = Logseason, colour = 'season')) + xlab("") + ylab(TeX("PM2.5 $\\mu/m^3$")) +
  theme(legend.position="none") + ggtitle("Beijing Particulate Matter PM2.5") +
  geom_hline(yintercept = 200, colour = "red4") +
  geom_hline(yintercept = 300, colour = "violetred4") +
  theme(text = element_text(size=8))

# PM2.5 plot with trend function and PM2.5 limits equivalent to 200 and 300 AQI
ggplot(dat, aes(x = day)) + 
  geom_line(aes(y = p_est)) + theme_bw() +
  #geom_line(aes(y = season, colour = 'season')) + xlab("") + ylab(TeX("PM2.5 $\\mu/m^3$")) +
  theme(legend.position="none") + ggtitle("Beijing Particulate Matter PM2.5") +
  geom_hline(yintercept = 200, colour = "red4") +
  geom_hline(yintercept = 300, colour = "violetred4") +
  theme(text = element_text(size=8))
###############################################################################

# jump experiments
#lambda <- c(rep(0.99,20),rep(0.01, 20), rep(0.99,20), rep(0.05,40))
day <- seq(as.Date('2017-01-01'),as.Date('2017-12-31'), by = 1)
d <- seq_along(day)
lambda <- 0.5 + (cos( 2*pi*d / 365 )) * 0.3
jump <- rbinom(n = 365,size = 1,prob = lambda)
size <- rnorm(n = 365, mean = 0, sd = 0.5)
x <- jump*size
plot(x, type="h")
