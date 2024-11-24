# Pull in data
file_path = "https://raw.githubusercontent.com/aabromowitz/TimeSeriersProject/refs/heads/main/MSPUS.csv"
mhp <- read.csv(file_path, header = TRUE)

# Make sure mhp is on the right scale
# mhp observation 49 is 1975 Q1
mhp_1975 = mhp[49:246,]
mhp_1975$DATE <- as.Date(mhp_1975$DATE)
summary(mhp_1975)

library(tswge)
mhp <- mhp_1975$MSPUS
log.mhp = log(mhp)
plotts.sample.wge(mhp)
plotts.wge(mhp)
plotts.sample.wge(log.mhp)
d.mhp = artrans.wge(mhp,1)
d.log.mhp = artrans.wge(log.mhp,1)
h.short = 4
h.long = 20
l = length(mhp)

# dickey fuller
library(tseries)
adf.test(d.mhp) # stationary
adf.test(mhp) # non-stationary
adf.test(log.mhp) # non-stationary
adf.test(d.log.mhp) # stationary

# overfit Factor table
est = est.arma.wge(log.mhp,p=16)
factor.wge(phi=est$phi) # two high 0 values, could be (1-B)^2

# signal plus noise
f=fore.sigplusnoise.wge(log.mhp,n.ahead=h.long,lastn=TRUE)
ase = mean((mhp[(l-h.long+1):l]-exp(f$f))^2)/1e6
ase # 1057.556

f=fore.sigplusnoise.wge(log.mhp,n.ahead=30,lastn=TRUE)
ase = mean((mhp[(l-30+1):l]-exp(f$f))^2)/1e6
ase # 583.3038

f=fore.sigplusnoise.wge(log.mhp,max.p=6,n.ahead=30,lastn=TRUE)
ase = mean((mhp[(l-30+1):l]-exp(f$f))^2)/1e6
ase # 595.6527

f=fore.sigplusnoise.wge(log.mhp,max.p=5,n.ahead=30,lastn=TRUE)
ase = mean((mhp[(l-30+1):l]-exp(f$f))^2)/1e6
ase # 583.3038

f=fore.sigplusnoise.wge(log.mhp,max.p=5,n.ahead=h.long,lastn=TRUE)
ase = mean((mhp[(l-h.long+1):l]-exp(f$f))^2)/1e6
ase # 1057.556

f=fore.sigplusnoise.wge(log.mhp,max.p=6,n.ahead=h.long,lastn=TRUE)
ase = mean((mhp[(l-h.long+1):l]-exp(f$f))^2)/1e6
ase # 1104.897

# ARMA on log data
aic5.ar.wge(log.mhp,p=0:100,type='aic',method='burg') # 96 best, highest 99
aic5.ar.wge(log.mhp,p=0:100,type='bic',method='burg') # 5 best, highest 8
aic5.wge(log.mhp,p=0:16,q=0:2) # 2/2 best, highest p 8, highest q 2
aic5.wge(log.mhp,p=0:10,q=0:4,type='aic') # 2/2 best
# p    q        aic
# 2    2  -7.138789
# 3    2  -7.126389
# 4    1  -7.124175
# 8    1  -7.121201
# 4    2  -7.117225
aic5.wge(log.mhp,p=0:8,q=0:2,type='bic') # 2/2 best
aic5.wge(log.mhp,p=0:8,q=0:8,type='aicc') # same as aic
# candidates: 2/2, 3/1, 8/1

# ARMA(2,2)
est = est.arma.wge(log.mhp,p=2,q=2)
roll.win.rmse.wge(log.mhp,h.short,phi=est$phi,theta=est$theta) # 0.044488
roll.win.rmse.wge(log.mhp,h.long,phi=est$phi,theta=est$theta) # 0.12779
f = fore.arma.wge(log.mhp,phi=est$phi,theta=est$theta,n.ahead=h.short,lastn=TRUE)
ase = mean((mhp[(l-h.short+1):l]-exp(f$f))^2)/1e6
ase # 112.4314
f = fore.arma.wge(log.mhp,phi=est$phi,theta=est$theta,n.ahead=h.long,lastn=TRUE)
ase = mean((mhp[(l-h.long+1):l]-exp(f$f))^2)/1e6
ase # 9167.59

# ARMA(3,1)
est = est.arma.wge(log.mhp,p=3,q=1)
roll.win.rmse.wge(log.mhp,h.short,phi=est$phi,theta=est$theta) # 0.037073
roll.win.rmse.wge(log.mhp,h.long,phi=est$phi,theta=est$theta) # 0.11506
f = fore.arma.wge(log.mhp,phi=est$phi,theta=est$theta,n.ahead=h.short,lastn=TRUE)
ase = mean((mhp[(l-h.short+1):l]-exp(f$f))^2)/1e6
ase # 61.89658
f = fore.arma.wge(log.mhp,phi=est$phi,theta=est$theta,n.ahead=h.long,lastn=TRUE)
ase = mean((mhp[(l-h.long+1):l]-exp(f$f))^2)/1e6
ase # 7205.137

# ARMA(8,1)
est = est.arma.wge(log.mhp,p=8,q=1)
roll.win.rmse.wge(log.mhp,h.short,phi=est$phi,theta=est$theta) # 0.039342
roll.win.rmse.wge(log.mhp,h.long,phi=est$phi,theta=est$theta) # 0.11429
f = fore.arma.wge(log.mhp,phi=est$phi,theta=est$theta,n.ahead=h.short,lastn=TRUE)
ase = mean((mhp[(l-h.short+1):l]-exp(f$f))^2)/1e6
ase # 106.0808
f = fore.arma.wge(log.mhp,phi=est$phi,theta=est$theta,n.ahead=h.long,lastn=TRUE)
ase = mean((mhp[(l-h.long+1):l]-exp(f$f))^2)/1e6
ase # 8813.243

# ARIMA(p,1,q) on log data
aic5.ar.wge(d.log.mhp,p=0:100,type='aic',method='burg') # 95 best, highest 98
aic5.ar.wge(d.log.mhp,p=0:100,type='bic',method='burg') # 3 best, highest 5
aic5.wge(d.log.mhp,p=0:16,q=0:2) # 1/2 best, highest p 6, highest q 2
aic5.wge(d.log.mhp,p=0:8,q=0:4,type='aic') # 1/2 best highest p 5, highest q 4
aic5.wge(d.log.mhp,p=0:7,q=0:6,type='aic') # 1/2 best
# p    q        aic
# 1    2  -7.188559
# 2    2  -7.180263
# 5    2  -7.174131
# 3    2  -7.170135
# 3    4  -7.168648
aic5.wge(d.log.mhp,p=0:5,q=0:4,type='bic') # 1/2 best
aic5.wge(d.log.mhp,p=0:5,q=0:4,type='aicc') # same as aic, but flipped values, #2 is 7/2
# candidates: 1/2

# ARMA(1,1,2)
est = est.arma.wge(d.log.mhp,p=1,q=2)
roll.win.rmse.wge(log.mhp,h.short,d=1,phi=est$phi,theta=est$theta) # 0.036501
roll.win.rmse.wge(log.mhp,h.long,d=1,phi=est$phi,theta=est$theta) # 0.13269
f = fore.arima.wge(log.mhp,d=1,phi=est$phi,theta=est$theta,n.ahead=h.short,lastn=TRUE)
ase = mean((mhp[(l-h.short+1):l]-exp(f$f))^2)/1e6
ase # 84.37902
f = fore.arima.wge(log.mhp,d=1,phi=est$phi,theta=est$theta,n.ahead=h.long,lastn=TRUE)
ase = mean((mhp[(l-h.long+1):l]-exp(f$f))^2)/1e6
ase # 7091.032

# ARIMA(p,2,q) on log data
d2.log.mhp = artrans.wge(d.log.mhp,1)
aic5.wge(d2.log.mhp,p=0:16,q=0:2) # 1/1 best highest p 4 highest q 2
aic5.wge(d2.log.mhp,p=0:6,q=0:4,type='aic') # 1/1 best
# p    q        bic
# 1    1  -7.057617
# 2    1  -7.040445
# 1    2  -7.019571
# 2    2  -7.012870
# 4    0  -6.999758
aic5.wge(d2.log.mhp,p=0:4,q=0:2,type='bic')
# candidates: 1/1

# ARIMA(1,2,1)
est = est.arma.wge(d2.log.mhp,p=1,q=1)
dev.off()
roll.win.rmse.wge(log.mhp,h.short,d=2,phi=est$phi,theta=est$theta) # 0.049161
roll.win.rmse.wge(log.mhp,h.long,d=1,phi=est$phi,theta=est$theta) # 0.15465
f = fore.arima.wge(log.mhp,d=2,phi=est$phi,theta=est$theta,n.ahead=h.short,lastn=TRUE)
ase = mean((mhp[(l-h.short+1):l]-exp(f$f))^2)/1e6
ase # 162.1383
f = fore.arima.wge(log.mhp,d=2,phi=est$phi,theta=est$theta,n.ahead=h.long,lastn=TRUE)
ase = mean((mhp[(l-h.long+1):l]-exp(f$f))^2)/1e6
ase # 6263.556

f = fore.arima.wge(log.mhp,d=2,phi=est$phi,theta=est$theta,n.ahead=30,lastn=TRUE)
ase = mean((mhp[(l-30+1):l]-exp(f$f))^2)/1e6
ase # 810.1207

# "Best" (but still bad) ARIMA model is the ARIMA(1,1,2)
aic = est$aic
aic # -7.188559
resid = f$resid
xbar=est$xbar
xbar # 0.01211603
vara = est$avar
vara # 0.0007251235
est$phi # 0.6901326
est$theta # 0.9720137 -0.4007360
# in book, writes the model in log notation
# they include both the mu and the var

# Forecast language:
# In [horizon], we are 95% confident that the [measure] will be between [forecast$ll] and [forecast$ul]. Our best estimate is [forecast$f].
# Ex: In two months, we are 95% confident that the number of airline passengers will be between 399,415 (e^.599 * 1000) and 468,717 (e^6.15 * 1000). 
# Our best estimate is 432,681 (e^6.07 * 1000) passengers.

# mean for arima
set.seed(1)
g = gen.arima.wge(250,phi=est$phi,theta=est$theta,d=1,mu=xbar,vara=vara)
set.seed(1)
g = gen.arima.wge(250,phi=est$phi,theta=est$theta,d=1,mu=(xbar+20),vara=vara) # Same as above, but moved up by 20

# looking at log airline data
plotts.wge(airlog)
plotts.wge(airline)

# Plotting residuals
plotts.wge(resid)
acf(resid,lag.max=100) 
parzen.wge(resid)
# look like white noise

# ljung box test
ljung.wge(resid,K=24,p=1,q=2) # p = 0.2272667, white
ljung.wge(resid,K=48,p=1,q=2) # p = 0.5584003, white

# normally distributed?
hist(resid)
shapiro.test(resid) # 0.03818, evidence against normality, but pretty close to 0.05
qqnorm(resid)

# Multiple ACFs 
set.seed(2)
sims = 30
ACF = acf(log.mhp, plot = "FALSE")
plot(ACF$lag ,ACF$acf , type = "l", lwd = 6)
for( i in 1: sims)
{
  ACF2 = acf(gen.arima.wge(l, phi = est$phi, theta=est$theta,d=1, plot="FALSE"), plot = "FALSE")
  lines(ACF2$lag ,ACF2$acf, lwd = 2, col = "red")
}

# Multiple Parzen 
sims = 30
SpecDen = parzen.wge(log.mhp, plot = "FALSE")
plot(SpecDen$freq,SpecDen$pzgram, type = "l", lwd = 6)
for( i in 1: sims)
{
  SpecDen2 = parzen.wge(gen.aruma.wge(l,phi=est$phi, theta=est$theta,d=1, plot ="FALSE"), plot = "FALSE")
  lines(SpecDen2$freq,SpecDen2$pzgram, lwd = 2, col = "red")
}