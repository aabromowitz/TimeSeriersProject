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

################################################################################

# Dr Sadler was saying that I'd need to get the b0, b1, and phi's for all the data
# Then use those to predict

# Getting b0, b1, and phi's for all the data
series = log.mhp
horizon = 12
linear = TRUE
method = "mle"
freq=0
max.p=5
f = fore.sigplusnoise.wge(series,linear = linear, max.p = max.p, n.ahead = horizon, lastn = FALSE, plot = FALSE)
b0 = f$b0hat
b1 = f$b1hat
phis = f$phi.z # 5 phis

num_phi = length(phis) # Will need at least 5 before to forecast, if an AR(5)
l = length(series)
numRmses = l - horizon - num_phi + 1 # If you had a data set of size 17, an AR(5), and a horizon of 12, you'd only be able to calculate 1 RMSE
rmses = numeric(numRmses)

for (iFore in 1:numRmses){
  tTrain = iFore:(iFore+num_phi-1)
  tTest = (iFore + num_phi):(iFore + num_phi + horizon - 1)
  resid = series[tTrain] - (b0 + b1*tTrain) # true - predicted
  fResid = fore.arma.wge(resid,phi=phis,n.ahead=horizon,lastn=FALSE,plot=FALSE)
  fSeries = b0 + b1*tTest + fResid$f
  rmse = sqrt(mean((series[tTest]-fSeries)^2))
  rmses[iFore]=rmse
}

hist(rmses)
mean(rmses)

# Try out function
source("functions_Aaron.R")
roll.win.rmse.linplusnoise.ada(series, horizon) # 0.06524557
roll.win.rmse.linplusnoise.ada(series, horizon, max.p=1) # 0.06684004
roll.win.rmse.linplusnoise.ada(series, horizon, max.p=2) # 0.06736302
roll.win.rmse.linplusnoise.ada(series, horizon, max.p=3) # 0.06736302
roll.win.rmse.linplusnoise.ada(series, horizon, max.p=4) # 0.0660697
roll.win.rmse.linplusnoise.ada(series, horizon, max.p=5) # 0.06524557
roll.win.rmse.linplusnoise.ada(series, horizon, max.p=6) # 0.06514193
roll.win.rmse.linplusnoise.ada(series, horizon, max.p=7) # 0.06514193
roll.win.rmse.linplusnoise.ada(series, horizon, max.p=8) # 0.06514193
roll.win.rmse.linplusnoise.ada(series, horizon, max.p=16) # 0.06514193


# Compre to rolling window for ARIMA
roll.win.rmse.wge(series,horizon,d=1)

# What was the rolling window before?
roll.win.sigplusnoise.ada.v1(series, horizon) # 0.09438185, ooh lower