# Pull in data
file_path = "https://raw.githubusercontent.com/aabromowitz/TimeSeriersProject/refs/heads/main/MSPUS.csv"
mhp <- read.csv(file_path, header = TRUE)
library(tswge)
mhp <- mhp$MSPUS
log.mhp = log(mhp)

# Parmeters
# roll.win.rmse.wge(series, horizon = 2, s = 0, d = 0, phi = 0, theta = 0)
# fore.sigplusnoise.wge(x,linear=TRUE,method="mle",freq=0,max.p=5,n.ahead=10,lastn=FALSE,plot=TRUE,alpha=.05,limits=TRUE)
series = log.mhp
horizon = 12
linear = TRUE
method = "mle"
freq=0
max.p=5

# Determine number of RMSEs to calculate
l = length(series)
minSizeForSigPlusNoise = max.p+1
numRmses = l - horizon - minSizeForSigPlusNoise
rmses = numeric(numRmses)
for (iPred in 1:numRmses){
  f = fore.sigplusnoise.wge(series[1:(minSizeForSigPlusNoise-1+iPred)],linear = linear, max.p = max.p, n.ahead = horizon, lastn = FALSE, plot = FALSE)
  rmse = sqrt(mean((series[(minSizeForSigPlusNoise+iPred):(minSizeForSigPlusNoise+iPred+horizon-1)]-f$f)^2))
  rmses[iPred]=rmse
}

# plot histogram of RMSEs
hist(rmses)

# Mean RMSE
mean(rmses) # 0.09415174

# Compare to rolling window for ARIMA
d.log = artrans.wge(log.mhp,1)
est.11.1 = est.arma.wge(log.mhp,p=11,q=1)
roll.win.rmse.wge(log.mhp,horizon=horizon,s=0,d=1,phi=est.11.1$phi,theta=est.11.1$theta) # 0.27881, way worse

# How many back do you have to go before it calculates an AR(5)?
f = fore.sigplusnoise.wge(series[1:2],linear = linear, max.p = max.p, n.ahead = horizon, lastn = FALSE, plot = FALSE) # AR(1)
f = fore.sigplusnoise.wge(series[1:3],linear = linear, max.p = max.p, n.ahead = horizon, lastn = FALSE, plot = FALSE) # AR(1)
f = fore.sigplusnoise.wge(series[1:4],linear = linear, max.p = max.p, n.ahead = horizon, lastn = FALSE, plot = FALSE) # AR(3)
f = fore.sigplusnoise.wge(series[1:5],linear = linear, max.p = max.p, n.ahead = horizon, lastn = FALSE, plot = FALSE) # AR(3)
f = fore.sigplusnoise.wge(series[1:6],linear = linear, max.p = max.p, n.ahead = horizon, lastn = FALSE, plot = FALSE) # AR(5)

# How do the ASEs compare?
preds_spn = fore.sigplusnoise.wge(log.mhp,linear = linear, max.p = max.p, n.ahead = horizon, lastn = TRUE, plot = TRUE)
ase_spn = mean((log.mhp[(l-horizon+1):l] - preds_spn$f)^2) # 0.007794239
preds_arima = fore.arima.wge(log.mhp,s=0,d=1,phi=est.11.1$phi,theta=est.11.1$theta, n.ahead = horizon, lastn = TRUE, plot = TRUE)
ase_arima = mean((log.mhp[(l-horizon+1):l] - preds_arima$f)^2) # 0.5823427

# try calling the function
source("functions_Aaron.R")
roll.win.sigplusnoise.ada(series, horizon)

# Trying to get rid of the output
suppressMessages({f = fore.sigplusnoise.wge(series[1:2],linear = linear, max.p = max.p, n.ahead = horizon, lastn = FALSE, plot = FALSE)})
sink("NUL")
f = fore.sigplusnoise.wge(series[1:2],linear = linear, max.p = max.p, n.ahead = horizon, lastn = FALSE, plot = FALSE)
sink()

# windows vs linux
os <- Sys.info()[["sysname"]]
