# Standard RMD code for pulling in variables
library(tidyverse)
library(tswge)
library(vars)
library(lubridate)
library(nnfor)
file_path = "https://raw.githubusercontent.com/aabromowitz/TimeSeriersProject/refs/heads/main/MSPUS.csv"
mhp <- read.csv(file_path, header = TRUE)
file_path = "https://raw.githubusercontent.com/aabromowitz/TimeSeriersProject/refs/heads/main/RHORUSQ156N.csv"
hor <- read.csv(file_path, header = TRUE)
file_path = "https://raw.githubusercontent.com/aabromowitz/TimeSeriersProject/refs/heads/main/COMPUTSA.csv"
huc <- read.csv(file_path, header = TRUE)
file_path = "https://raw.githubusercontent.com/aabromowitz/TimeSeriersProject/refs/heads/main/MSACSR.csv"
snh <- read.csv(file_path, header = TRUE)
file_path = "https://raw.githubusercontent.com/aabromowitz/TimeSeriersProject/refs/heads/main/USSTHPI.csv"
hpi <- read.csv(file_path, header = TRUE)
snh_monthly <- snh
huc_monthly <- huc
snh$DATE = as.Date(snh$DATE)
snh$month <- month(snh$DATE)
head(snh)
snh_quarterly <- snh %>%
  filter(snh$month == 1 | snh$month == 4 | snh$month == 7 | snh$month == 10)
summary(snh_quarterly)
huc$DATE = as.Date(huc$DATE)
huc$month <- month(huc$DATE)
head(huc)
huc_quarterly <- huc %>%
  filter(huc$month == 1 | huc$month == 4 | huc$month == 7 | huc$month == 10)
summary(huc_quarterly)
hor_1975 = hor[41:238,]
hor_1975$DATE <- as.Date(hor_1975$DATE)
summary(hor_1975)
huc_1975 = huc_quarterly[29:226,]
summary(huc_1975)
mhp_1975 = mhp[49:246,]
mhp_1975$DATE <- as.Date(mhp_1975$DATE)
summary(mhp_1975)
snh_1975 = snh_quarterly[49:246,]
summary(snh_1975)
hpi$DATE <- as.Date(hpi$DATE)
summary(hpi)
fed_housing_data_NL = data.frame(Year_Quarter = mhp_1975$DATE, Ownership_Rate = hor_1975$RHORUSQ156N, Housing_Units_Completed = huc_1975$COMPUTSA, Supply_New_Houses = snh_1975$MSACSR, Housing_Price_Index = hpi$USSTHPI, Median_Sales_Price = mhp_1975$MSPUS)
fed_housing_data = data.frame(Year_Quarter = as.Date(mhp_1975$DATE), Ownership_Rate = hor_1975$RHORUSQ156N, Housing_Units_Completed = huc_1975$COMPUTSA, Supply_New_Houses = snh_1975$MSACSR, Housing_Price_Index = hpi$USSTHPI, Median_Sales_Price = log(mhp_1975$MSPUS))
train = fed_housing_data[1:168,]
test = fed_housing_data[169:198,]
h.short = 4
h.long = 20
################################################################################

# Model for Home Ownership rate
x = fed_housing_data$Ownership_Rate 
plotts.sample.wge(x)
d = artrans.wge(x,1)
l = length(x)

# ARMA
aic5.ar.wge(x,p=0:120,type='aic',method='burg') # 14 best, 116 highest
aic5.ar.wge(x,p=0:120,type='bic',method='burg') # 3 best, 9 highest
aic5.wge(x,p=0:16,q=0:2,type='aic') # 9/1 best, p 14 highest, q 2 highest
aic5.wge(x,p=0:16,q=0:4,type='aic') # 9/1 best, p 14 highest, q 3 highest
# p    q        aic
# 9    1  -2.144284
# 9    3  -2.135221
# 9    2  -2.134332
# 14    0  -2.128761
# 13    1  -2.125028
aic5.wge(x,p=0:14,q=0:3,type='aicc') # 9/1 best, p 10 highest, q 3 highest
aic5.wge(x,p=0:10,q=0:3,type='bic') # 3/0 best, p 5 highest, q 2 highest
# candidates: 9/1, 3/0, 9/3

# ARMA(9,1)
dev.off()
est = est.arma.wge(x,p=9,q=1)
roll.win.rmse.wge(x,h.short,phi=est$phi,theta=est$theta) # 0.38811
roll.win.rmse.wge(x,h.long,phi=est$phi,theta=est$theta) # 0.7746
f = fore.arima.wge(x,phi=est$phi,theta=est$theta,n.ahead=h.short,lastn=TRUE)
ase = mean((x[(l-h.short+1):l]-f$f)^2)
ase # 0.02468854
f = fore.arima.wge(x,phi=est$phi,theta=est$theta,n.ahead=h.long,lastn=TRUE)
ase = mean((x[(l-h.long+1):l]-f$f)^2)
ase # 1.108741

# AR(3)
est = est.ar.wge(x,p=3)
roll.win.rmse.wge(x,h.short,phi=est$phi) # 0.41219
roll.win.rmse.wge(x,h.long,phi=est$phi) # 0.9828
f = fore.arima.wge(x,phi=est$phi,n.ahead=h.short,lastn=TRUE)
ase = mean((x[(l-h.short+1):l]-f$f)^2)
ase # 0.05228463
f = fore.arima.wge(x,phi=est$phi,n.ahead=h.long,lastn=TRUE)
ase = mean((x[(l-h.long+1):l]-f$f)^2)
ase # 2.311676

# ARMA(9,3)
est = est.arma.wge(x,p=9,q=3)
roll.win.rmse.wge(x,h.short,phi=est$phi,theta=est$theta) # 0.38427
roll.win.rmse.wge(x,h.long,phi=est$phi,theta=est$theta) # 0.7691
f = fore.arima.wge(x,phi=est$phi,theta=est$theta,n.ahead=h.short,lastn=TRUE)
ase = mean((x[(l-h.short+1):l]-f$f)^2)
ase # 0.04347333
f = fore.arima.wge(x,phi=est$phi,theta=est$theta,n.ahead=h.long,lastn=TRUE)
ase = mean((x[(l-h.long+1):l]-f$f)^2)
ase # 1.246112

# ARIMA(p,1,q)
aic5.ar.wge(d,p=0:120,type='aic',method='burg') # 8 best, 13 highest
aic5.ar.wge(d,p=0:13,type='bic',method='burg') # 2 best, 4 highest
aic5.wge(d,p=0:15,q=0:2,type='aic') # 8/1 best, p 9 highest, q 2 highest
aic5.wge(d,p=0:11,q=0:4,type='aic') # 8/1 best, p 9 highest, q 3 highest
# p    q        aic
# 8    1  -2.113107
# 8    0  -2.111537
# 9    0  -2.108892
# 8    2  -2.105050
# 8    3  -2.103671
aic5.wge(d,p=0:9,q=0:3,type='aicc') # 8/1 best, p 9 highest, q 2 highest
aic5.wge(d,p=0:9,q=0:2,type='bic') # 2/0 best, p 4 highest, q 2 highest
# candidates: 8/1, 2/0

# ARIMA(8,1,1)
est = est.arma.wge(d,p=8,q=1)
roll.win.rmse.wge(x,h.short,d=1,phi=est$phi,theta=est$theta) # 0.38949
roll.win.rmse.wge(x,h.long,d=1,phi=est$phi,theta=est$theta) # 0.9264
f = fore.arima.wge(x,d=1,phi=est$phi,theta=est$theta,n.ahead=h.short,lastn=TRUE)
ase = mean((x[(l-h.short+1):l]-f$f)^2)
ase # 0.03083697
f = fore.arima.wge(x,d=1,phi=est$phi,theta=est$theta,n.ahead=h.long,lastn=TRUE)
ase = mean((x[(l-h.long+1):l]-f$f)^2)
ase # 2.077416

# ARIMA(2,1,0)
est = est.ar.wge(d,p=2)
roll.win.rmse.wge(x,h.short,d=1,phi=est$phi) # 0.42058
roll.win.rmse.wge(x,h.long,d=1,phi=est$phi) # 1.0676
f = fore.arima.wge(x,d=1,phi=est$phi,n.ahead=h.short,lastn=TRUE)
ase = mean((x[(l-h.short+1):l]-f$f)^2)
ase # 0.06127898
f = fore.arima.wge(x,d=1,phi=est$phi,n.ahead=h.long,lastn=TRUE)
ase = mean((x[(l-h.long+1):l]-f$f)^2)
ase # 2.952113

# ARIMA(8,1,3)
est = est.arma.wge(d,p=8,q=3)
roll.win.rmse.wge(x,h.short,d=1,phi=est$phi,theta=est$theta) # 0.38152
roll.win.rmse.wge(x,h.long,d=1,phi=est$phi,theta=est$theta) # 0.9254
f = fore.arima.wge(x,d=1,phi=est$phi,theta=est$theta,n.ahead=h.short,lastn=TRUE)
ase = mean((x[(l-h.short+1):l]-f$f)^2)
ase # 0.03581426
f = fore.arima.wge(x,d=1,phi=est$phi,theta=est$theta,n.ahead=h.long,lastn=TRUE)
ase = mean((x[(l-h.long+1):l]-f$f)^2)
ase # 2.114378

# MLP
# d= 0, 5 hidden nodes
set.seed(3)
t.train.short= 1:(l-h.short)
t.test.short=(l-h.short+1):l
t.train.long= 1:(l-h.long)
t.test.long=(l-h.long+1):l
train.short = x[t.train.short]
test.short = x[t.test.short]
train.long = x[t.train.long]
test.long = x[t.test.long]
fit.mlp = mlp(ts(x))
plot(fit.mlp)
roll.win.rmse.nn.wge(x,horizon=h.short,fit.mlp) # 0.40014
roll.win.rmse.nn.wge(x,horizon=h.long,fit.mlp) # 1.0223
fit.mlp = mlp(ts(train.short))
fore.mlp=forecast(fit.mlp,h=h.short)
plot(seq(1,l,1),x,type="b")
points(seq(l-h.short+1,l,1),fore.mlp$mean,type="b",pch=15)
plot(fore.mlp)
ase=mean((test.short-fore.mlp$mean)^2) 
ase # 0.0802052
fit.mlp = mlp(ts(train.long))
fore.mlp=forecast(fit.mlp,h=h.long)
plot(seq(1,l,1),x,type="b")
points(seq(l-h.long+1,l,1),fore.mlp$mean,type="b",pch=15)
plot(fore.mlp)
ase=mean((test.long-fore.mlp$mean)^2) 
ase # 2.913479

# d= 0, 2 hidden nodes
set.seed(4)
t.train.short= 1:(l-h.short)
t.test.short=(l-h.short+1):l
t.train.long= 1:(l-h.long)
t.test.long=(l-h.long+1):l
train.short = x[t.train.short]
test.short = x[t.test.short]
train.long = x[t.train.long]
test.long = x[t.test.long]
fit.mlp = mlp(ts(x),hd=2)
plot(fit.mlp)
roll.win.rmse.nn.wge(x,horizon=h.short,fit.mlp) # 0.41087
roll.win.rmse.nn.wge(x,horizon=h.long,fit.mlp) # 0.9965
fit.mlp = mlp(ts(train.short),hd=2)
fore.mlp=forecast(fit.mlp,h=h.short)
plot(seq(1,l,1),x,type="b")
points(seq(l-h.short+1,l,1),fore.mlp$mean,type="b",pch=15)
plot(fore.mlp)
ase=mean((test.short-fore.mlp$mean)^2) 
ase # 0.05294034
fit.mlp = mlp(ts(train.long),hd=2)
fore.mlp=forecast(fit.mlp,h=h.long)
plot(seq(1,l,1),x,type="b")
points(seq(l-h.long+1,l,1),fore.mlp$mean,type="b",pch=15)
plot(fore.mlp)
ase=mean((test.long-fore.mlp$mean)^2) 
ase # 3.251308

# d= 0, 10 hidden nodes
set.seed(5)
t.train.short= 1:(l-h.short)
t.test.short=(l-h.short+1):l
t.train.long= 1:(l-h.long)
t.test.long=(l-h.long+1):l
train.short = x[t.train.short]
test.short = x[t.test.short]
train.long = x[t.train.long]
test.long = x[t.test.long]
fit.mlp = mlp(ts(x),hd=10)
plot(fit.mlp)
roll.win.rmse.nn.wge(x,horizon=h.short,fit.mlp) # 0.39209
roll.win.rmse.nn.wge(x,horizon=h.long,fit.mlp) # 1.0278
fit.mlp = mlp(ts(train.short),hd=10)
fore.mlp=forecast(fit.mlp,h=h.short)
plot(seq(1,l,1),x,type="b")
points(seq(l-h.short+1,l,1),fore.mlp$mean,type="b",pch=15)
plot(fore.mlp)
ase=mean((test.short-fore.mlp$mean)^2) 
ase # 0.07209673
fit.mlp = mlp(ts(train.long),hd=10)
fore.mlp=forecast(fit.mlp,h=h.long)
plot(seq(1,l,1),x,type="b")
points(seq(l-h.long+1,l,1),fore.mlp$mean,type="b",pch=15)
plot(fore.mlp)
ase=mean((test.long-fore.mlp$mean)^2) 
ase # 3.521949

# d= 1, 5 hidden nodes
set.seed(6)
fit.mlp = mlp(ts(x),difforder=1)
plot(fit.mlp)
roll.win.rmse.nn.wge(x,horizon=h.short,fit.mlp) # 0.3551
roll.win.rmse.nn.wge(x,horizon=h.long,fit.mlp) # 0.9795
fit.mlp = mlp(ts(train.short),difforder=1)
fore.mlp=forecast(fit.mlp,h=h.short)
plot(seq(1,l,1),x,type="b")
points(seq(l-h.short+1,l,1),fore.mlp$mean,type="b",pch=15)
plot(fore.mlp)
ase=mean((test.short-fore.mlp$mean)^2) 
ase # 0.08180303
fit.mlp = mlp(ts(train.long),difforder=1)
fore.mlp=forecast(fit.mlp,h=h.long)
plot(seq(1,l,1),x,type="b")
points(seq(l-h.long+1,l,1),fore.mlp$mean,type="b",pch=15)
plot(fore.mlp)
ase=mean((test.long-fore.mlp$mean)^2) 
ase # 3.059631

# Will officially go with ARMA(9,1) predictions
x = fed_housing_data$Ownership_Rate 
est = est.arma.wge(x,p=9,q=1)
f = fore.arima.wge(x,phi=est$phi,theta=est$theta,n.ahead=h.short,lastn=TRUE)
hor.pred.short = f$f
f = fore.arima.wge(x,phi=est$phi,theta=est$theta,n.ahead=h.long,lastn=TRUE)
hor.pred.long = f$f

################################################################################

# Housing units completed
x = fed_housing_data$Housing_Units_Completed 
plotts.sample.wge(x)
d = artrans.wge(x,1)
l = length(x)

# ARMA
aic5.ar.wge(x,p=0:120,type='aic',method='burg') # 116 best, 120 highest
aic5.ar.wge(x,p=0:120,type='bic',method='burg') # 1 best, 5 highest
aic5.wge(x,p=0:8,q=0:2,type='aic') # 8/2 best, p 8 highest, q 2 highest
aic5.wge(x,p=0:10,q=0:4,type='aic') # 10/0 best, p 10 highest, q 1 highest
aic5.wge(x,p=0:12,q=0:3,type='aic') # 10/0 best, p 10 highest, q 1 highest
# p    q        aic
# 10    0   9.024872
# 9    0   9.025447
# 9    1   9.028069
# 8    0   9.028335
# 8    1   9.032021
aic5.wge(x,p=0:10,q=0:1,type='aicc') # 9/0 best, p 10 highest, q 1 highest
aic5.wge(x,p=0:10,q=0:1,type='bic') # 1/0 best, p 4 highest, q 1 highest
# candidates: 10/0, 1/0

# AR(10)
dev.off()
est = est.ar.wge(x,p=10)
roll.win.rmse.wge(x,h.short,phi=est$phi) # 107.492
roll.win.rmse.wge(x,h.long,phi=est$phi) # 237.179
f = fore.arima.wge(x,phi=est$phi,n.ahead=h.short,lastn=TRUE)
ase = mean((x[(l-h.short+1):l]-f$f)^2)/1e3
ase # 12.98288
f = fore.arima.wge(x,phi=est$phi,n.ahead=h.long,lastn=TRUE)
ase = mean((x[(l-h.long+1):l]-f$f)^2)/1e3
ase # 8.546039

# AR(1)
est = est.ar.wge(x,p=2)
roll.win.rmse.wge(x,h.short,phi=est$phi) # 116.71
roll.win.rmse.wge(x,h.long,phi=est$phi) # 258.693
f = fore.arima.wge(x,phi=est$phi,n.ahead=h.short,lastn=TRUE)
ase = mean((x[(l-h.short+1):l]-f$f)^2)/1e3
ase # 19.65468
f = fore.arima.wge(x,phi=est$phi,n.ahead=h.long,lastn=TRUE)
ase = mean((x[(l-h.long+1):l]-f$f)^2)/1e3
ase # 10.37798

# ARMA(9,1)
est = est.arma.wge(x,p=9,q=1)
roll.win.rmse.wge(x,h.short,phi=est$phi,theta=est$theta) # 106.605
roll.win.rmse.wge(x,h.long,phi=est$phi,theta=est$theta) # 235.69
f = fore.arima.wge(x,phi=est$phi,theta=est$theta,n.ahead=h.short,lastn=TRUE)
ase = mean((x[(l-h.short+1):l]-f$f)^2)/1e3
ase # 13.44075
f = fore.arima.wge(x,phi=est$phi,theta=est$theta,n.ahead=h.long,lastn=TRUE)
ase = mean((x[(l-h.long+1):l]-f$f)^2)/1e3
ase # 8.516217

# ARIMA(p,1,q)
aic5.ar.wge(d,p=0:160,type='aic',method='burg') # 160 best, 160 highest
aic5.ar.wge(d,p=0:160,type='bic',method='burg') # 0 best, 4 highest
aic5.wge(d,p=0:6,q=0:2,type='aic') # 2/1 best, p 4 highest, q 2 highest
aic5.wge(d,p=0:6,q=0:4,type='aic') # 2/1 best, p 4 highest, q 2 highest
# p    q        aic
# 2    1   9.084278
# 3    0   9.084565
# 1    2   9.088551
# 3    1   9.088634
# 4    0   9.090280
aic5.wge(d,p=0:4,q=0:2,type='aicc') # 2/1 best, p 4 highest, q 2 highest
aic5.wge(d,p=0:4,q=0:2,type='bic') # 0/0 best, p 2 highest, q 2 highest
# candidates: 2/1, 3/1

# ARIMA(2,1,1)
est = est.arma.wge(d,p=2,q=1)
roll.win.rmse.wge(x,h.short,d=1,phi=est$phi,theta=est$theta) # 119.763
roll.win.rmse.wge(x,h.long,d=1,phi=est$phi,theta=est$theta) # 314.594
f = fore.arima.wge(x,d=1,phi=est$phi,theta=est$theta,n.ahead=h.short,lastn=TRUE)
ase = mean((x[(l-h.short+1):l]-f$f)^2)/1e3
ase # 16.2009
f = fore.arima.wge(x,d=1,phi=est$phi,theta=est$theta,n.ahead=h.long,lastn=TRUE)
ase = mean((x[(l-h.long+1):l]-f$f)^2)/1e3
ase # 8.137481

# ARIMA(3,1,1)
est = est.arma.wge(d,p=3,q=1)
roll.win.rmse.wge(x,h.short,d=1,phi=est$phi,theta=est$theta) # 116.348
roll.win.rmse.wge(x,h.long,d=1,phi=est$phi,theta=est$theta) # 308.772
f = fore.arima.wge(x,d=1,phi=est$phi,theta=est$theta,n.ahead=h.short,lastn=TRUE)
ase = mean((x[(l-h.short+1):l]-f$f)^2)/1e3
ase # 16.36543
f = fore.arima.wge(x,d=1,phi=est$phi,theta=est$theta,n.ahead=h.long,lastn=TRUE)
ase = mean((x[(l-h.long+1):l]-f$f)^2)/1e3
ase # 8.160344

# MLP
# d= 0, 5 hidden nodes
set.seed(7)
t.train.short= 1:(l-h.short)
t.test.short=(l-h.short+1):l
t.train.long= 1:(l-h.long)
t.test.long=(l-h.long+1):l
train.short = x[t.train.short]
test.short = x[t.test.short]
train.long = x[t.train.long]
test.long = x[t.test.long]
fit.mlp = mlp(ts(x))
plot(fit.mlp)
roll.win.rmse.nn.wge(x,horizon=h.short,fit.mlp) # 109.389
roll.win.rmse.nn.wge(x,horizon=h.long,fit.mlp) # 223.535
fit.mlp = mlp(ts(train.short))
fore.mlp=forecast(fit.mlp,h=h.short)
plot(seq(1,l,1),x,type="b")
points(seq(l-h.short+1,l,1),fore.mlp$mean,type="b",pch=15)
plot(fore.mlp)
ase=mean((test.short-fore.mlp$mean)^2)/1e3
ase # 15.86988
fit.mlp = mlp(ts(train.long))
fore.mlp=forecast(fit.mlp,h=h.long)
plot(seq(1,l,1),x,type="b")
points(seq(l-h.long+1,l,1),fore.mlp$mean,type="b",pch=15)
plot(fore.mlp)
ase=mean((test.long-fore.mlp$mean)^2)/1e3
ase # 16.79112

# d= 0, 2 hidden nodes
set.seed(8)
fit.mlp = mlp(ts(x),hd=2)
plot(fit.mlp)
roll.win.rmse.nn.wge(x,horizon=h.short,fit.mlp) # 110.592
roll.win.rmse.nn.wge(x,horizon=h.long,fit.mlp) # 226.479
fit.mlp = mlp(ts(train.short),hd=2)
fore.mlp=forecast(fit.mlp,h=h.short)
plot(seq(1,l,1),x,type="b")
points(seq(l-h.short+1,l,1),fore.mlp$mean,type="b",pch=15)
plot(fore.mlp)
ase=mean((test.short-fore.mlp$mean)^2)/1e3
ase # 15.79413
fit.mlp = mlp(ts(train.long),hd=2)
fore.mlp=forecast(fit.mlp,h=h.long)
plot(seq(1,l,1),x,type="b")
points(seq(l-h.long+1,l,1),fore.mlp$mean,type="b",pch=15)
plot(fore.mlp)
ase=mean((test.long-fore.mlp$mean)^2)/1e3
ase # 17.86414

# d= 1, 5 hidden nodes
set.seed(9)
fit.mlp = mlp(ts(x),difforder=1)
plot(fit.mlp)
roll.win.rmse.nn.wge(x,horizon=h.short,fit.mlp) # 105.802
roll.win.rmse.nn.wge(x,horizon=h.long,fit.mlp) # 289.21
fit.mlp = mlp(ts(train.short),difforder=1)
fore.mlp=forecast(fit.mlp,h=h.short)
plot(seq(1,l,1),x,type="b")
points(seq(l-h.short+1,l,1),fore.mlp$mean,type="b",pch=15)
plot(fore.mlp)
ase=mean((test.short-fore.mlp$mean)^2)/1e3
ase # 12.4386
fit.mlp = mlp(ts(train.long),difforder=1)
fore.mlp=forecast(fit.mlp,h=h.long)
plot(seq(1,l,1),x,type="b")
points(seq(l-h.long+1,l,1),fore.mlp$mean,type="b",pch=15)
plot(fore.mlp)
ase=mean((test.long-fore.mlp$mean)^2)/1e3 
ase # 16.29619

# d= 1, 2 hidden nodes
set.seed(10)
fit.mlp = mlp(ts(x),difforder=1,hd=2)
plot(fit.mlp)
roll.win.rmse.nn.wge(x,horizon=h.short,fit.mlp) # 111.548
roll.win.rmse.nn.wge(x,horizon=h.long,fit.mlp) # 293.12
fit.mlp = mlp(ts(train.short),difforder=1,hd=2)
fore.mlp=forecast(fit.mlp,h=h.short)
plot(seq(1,l,1),x,type="b")
points(seq(l-h.short+1,l,1),fore.mlp$mean,type="b",pch=15)
plot(fore.mlp)
ase=mean((test.short-fore.mlp$mean)^2)/1e3
ase # 14.3813
fit.mlp = mlp(ts(train.long),difforder=1,hd=2)
fore.mlp=forecast(fit.mlp,h=h.long)
plot(seq(1,l,1),x,type="b")
points(seq(l-h.long+1,l,1),fore.mlp$mean,type="b",pch=15)
plot(fore.mlp)
ase=mean((test.long-fore.mlp$mean)^2)/1e3 
ase # 16.36875

# Will officially go with ARMA(9,1) predictions
x = fed_housing_data$Housing_Units_Completed  
est = est.arma.wge(x,p=9,q=1)
f = fore.arima.wge(x,phi=est$phi,theta=est$theta,n.ahead=h.short,lastn=TRUE)
huc.pred.short = f$f
f = fore.arima.wge(x,phi=est$phi,theta=est$theta,n.ahead=h.long,lastn=TRUE)
huc.pred.long = f$f

################################################################################

# House price index
x = fed_housing_data$Housing_Price_Index 
plotts.sample.wge(x)
d = artrans.wge(x,1)
l = length(x)

# Second difference seems like it could be useful
d2 = artrans.wge(d,1)

# Maybe even logging as well
x.log = log(x)
d.log = artrans.wge(x.log,1)
d2.log = artrans.wge(d.log,1)
dev.off()

# Seeing if there is a frequency at 1/4
parzen.wge(d2.log)
parzen.wge(d2)

# Differencing out the 0.25 frequency
factor.wge(c(rep(0,3),1))
d3 = artrans.wge(d2,c(0,-1))
d3.log = artrans.wge(d2.log,c(0,-1))

# Wait, did I just do (1-b^4)?
dev.off()
parzen.wge(x) # nope, definitely no 0.25 f in there

# What does differencing out the full (1-b^4) do?
d3.2 = artrans.wge(d2,c(rep(0,3),1))

# First, trying ARMA with d3
aic5.ar.wge(d3,p=0:160,type='aic',method='burg') # 159 best, 160 highest
aic5.ar.wge(d3,p=0:160,type='bic',method='burg') # 2 best, 8 highest
aic5.wge(d3,p=0:10,q=0:2,type='aic') # 5/2 best, p 9 highest, q 2 highest
aic5.wge(d3,p=0:12,q=0:4,type='aic') # 5/2 best, p 8 highest, q 4 highest
aic5.wge(d3,p=0:10,q=0:6,type='aic') # 4/5 best, p 6 highest, q 5 highest
# p    q        aic
# 4    5   2.318766
# 3    5   2.318801
# 5    5   2.331189
# 6    5   2.335669
# 2    5   2.343238
aic5.wge(d3,p=0:6,q=0:5,type='aicc') # 3/5 best, p 6 highest, q 5 highest
aic5.wge(d3,p=0:6,q=0:5,type='bic') # 0/2 best, p 2 highest, q 5 highest
# candidates: 4/5, 0/2, 6/5

# d3 ARMA(4,5)
dev.off()
est = est.arma.wge(d3,p=4,q=5)
m = mult.wge(fac1=est$phi,fac2=c(0,-1))
factor.wge(m$model.coef) # looks good
roll.win.rmse.wge(x,h.short,d=2,phi=m$model.coef,theta=est$theta) # 6.8122
roll.win.rmse.wge(x,h.long,d=2,phi=m$model.coef,theta=est$theta) # 41.022
f = fore.arima.wge(x,d=2,phi=m$model.coef,theta=est$theta,n.ahead=h.short,lastn=TRUE)
ase = mean((x[(l-h.short+1):l]-f$f)^2)
ase # 16.01932
f = fore.arima.wge(x,d=2,phi=m$model.coef,theta=est$theta,n.ahead=h.long,lastn=TRUE)
ase = mean((x[(l-h.long+1):l]-f$f)^2)
ase # 2109.469

# d3 ARMA(0,2)
est = est.arma.wge(d3,p=0,q=2)
m = mult.wge(fac1=est$phi,fac2=c(0,-1))
factor.wge(m$model.coef) # looks good
roll.win.rmse.wge(x,h.short,d=2,phi=m$model.coef,theta=est$theta) # 5.392
roll.win.rmse.wge(x,h.long,d=2,phi=m$model.coef,theta=est$theta) # 25.866
f = fore.arima.wge(x,d=2,phi=m$model.coef,theta=est$theta,n.ahead=h.short,lastn=TRUE)
ase = mean((x[(l-h.short+1):l]-f$f)^2)
ase # 189.898
f = fore.arima.wge(x,d=2,phi=m$model.coef,theta=est$theta,n.ahead=h.long,lastn=TRUE)
ase = mean((x[(l-h.long+1):l]-f$f)^2)
ase # 9827.962

# d3 ARMA(6,5)
est = est.arma.wge(d3,p=6,q=5)
m = mult.wge(fac1=est$phi,fac2=c(0,-1))
factor.wge(m$model.coef) # looks good
roll.win.rmse.wge(x,h.short,d=2,phi=m$model.coef,theta=est$theta) # 6.5370
roll.win.rmse.wge(x,h.long,d=2,phi=m$model.coef,theta=est$theta) # 39.436
f = fore.arima.wge(x,d=2,phi=m$model.coef,theta=est$theta,n.ahead=h.short,lastn=TRUE)
ase = mean((x[(l-h.short+1):l]-f$f)^2)
ase # 14.86637
f = fore.arima.wge(x,d=2,phi=m$model.coef,theta=est$theta,n.ahead=h.long,lastn=TRUE)
ase = mean((x[(l-h.long+1):l]-f$f)^2)
ase # 558.2356

# Try ARMA with d2.log
aic5.ar.wge(d2.log,p=0:200,type='aic',method='burg') # 195 best, 195 highest
aic5.ar.wge(d2.log,p=0:195,type='bic',method='burg') # 2 best, 8 highest
aic5.wge(d2.log,p=0:28,q=0:2,type='aic') # 3/0 best, p 5 highest, q 2 highest
aic5.wge(d2.log,p=0:8,q=0:4,type='aic') # 3/0 best, p 5 highest, q 2 highest
# p    q        aic
# 3    0  -9.124972
# 4    0  -9.118892
# 3    1  -9.118074
# 3    2  -9.109825
# 5    0  -9.108971
aic5.wge(d2.log,p=0:5,q=0:2,type='aicc') # 3/0 best, p 4 highest, q 2 highest
aic5.wge(d2.log,p=0:4,q=0:2,type='bic') # 3/0 best, p 4 highest, q 1 highest
# candidates: 3/0, 3/2

# d2.log AR(3)
dev.off()
est = est.ar.wge(d2.log,p=3)
roll.win.rmse.wge(x.log,h.short,d=2,phi=est$phi) # 0.016801
roll.win.rmse.wge(x.log,h.long,d=2,phi=est$phi) # 0.08855
f = fore.arima.wge(x.log,d=2,phi=est$phi,n.ahead=h.short,lastn=TRUE)
ase = mean((x[(l-h.short+1):l]-exp(f$f))^2)
ase # 4.396291
f = fore.arima.wge(x.log,d=2,phi=est$phi,n.ahead=h.long,lastn=TRUE)
ase = mean((x[(l-h.long+1):l]-exp(f$f))^2)
ase # 7244.708

# d2.log ARMA(3,2)
est = est.arma.wge(d2.log,p=3,q=2)
roll.win.rmse.wge(x.log,h.short,d=2,phi=est$phi,theta=est$theta) # 0.016905
roll.win.rmse.wge(x.log,h.long,d=2,phi=est$phi,theta=est$theta) # 0.088613
f = fore.arima.wge(x.log,d=2,phi=est$phi,theta=est$theta,n.ahead=h.short,lastn=TRUE)
ase = mean((x[(l-h.short+1):l]-exp(f$f))^2)
ase # 19.04625
f = fore.arima.wge(x.log,d=2,phi=est$phi,theta=est$theta,n.ahead=h.long,lastn=TRUE)
ase = mean((x[(l-h.long+1):l]-exp(f$f))^2)
ase # 7394.011

# Try ARMA with (1-b^4) differenced out
aic5.ar.wge(d3.2,p=0:200,type='aic',method='burg') # 191 best, 191 highest
aic5.ar.wge(d3.2,p=0:191,type='bic',method='burg') # 191 best, 191 highest
aic5.wge(d3.2,p=0:28,q=0:2,type='aic') # 14/1 best, p 17 highest, q 1 highest
# p    q        aic
# 14    1   2.470959
# 16    0   2.473225
# 16    1   2.481359
# 15    1   2.481362
# 17    0   2.481835
aic5.wge(d3.2,p=0:17,q=0:1,type='aicc') # 14/1 best, p 16 highest, q 1 highest
aic5.wge(d3.2,p=0:16,q=0:1,type='bic') # 10/1 best, p 12 highest, q 1 highest
# candidates: 10/1, 14/1

# d3.2 ARMA(10,1)
est = est.arma.wge(d3.2,p=10,q=1)
roll.win.rmse.wge(x,h.short,d=2,s=4,phi=est$phi,theta=est$theta) # 5.9488
roll.win.rmse.wge(x,h.long,d=2,s=4,phi=est$phi,theta=est$theta) # 37.9498
f = fore.arima.wge(x,d=2,s=4,phi=est$phi,theta=est$theta,n.ahead=h.short,lastn=TRUE)
ase = mean((x[(l-h.short+1):l]-f$f)^2)
ase # 24.94154
f = fore.arima.wge(x,d=2,s=4,phi=est$phi,theta=est$theta,n.ahead=h.long,lastn=TRUE)
ase = mean((x[(l-h.long+1):l]-f$f)^2)
ase # 9426.235

# d3.2 ARMA(14,1)
est = est.arma.wge(d3.2,p=14,q=1)
roll.win.rmse.wge(x,h.short,d=2,s=4,phi=est$phi,theta=est$theta) # 5.9268
roll.win.rmse.wge(x,h.long,d=2,s=4,phi=est$phi,theta=est$theta) # 37.2596
f = fore.arima.wge(x,d=2,s=4,phi=est$phi,theta=est$theta,n.ahead=h.short,lastn=TRUE)
ase = mean((x[(l-h.short+1):l]-f$f)^2)
ase # 26.42709
f = fore.arima.wge(x,d=2,s=4,phi=est$phi,theta=est$theta,n.ahead=h.long,lastn=TRUE)
ase = mean((x[(l-h.long+1):l]-f$f)^2)
ase # 8350.941

# Try signal plus noise with d2
f = fore.arima.wge(x,d=2,n.ahead=h.short,lastn=TRUE)
resid = f$resid
plotts.sample.wge(resid)
f2 = fore.sigplusnoise.wge(resid,linear=FALSE,freq=0.25,max.p=5,n.ahead=h.short,lastn=TRUE)
ase = mean((x[(l-h.short+1):l]-f$f-f2$f)^2)
ase # 1130.361
f2 = fore.sigplusnoise.wge(resid,linear=FALSE,freq=0.25,max.p=5,n.ahead=h.long,lastn=TRUE)
ase = mean((x[(l-h.long+1):l]-f$f)^2)
ase # 24824.99

# MLP
# d= 0, 5 hidden nodes
set.seed(10)
t.train.short= 1:(l-h.short)
t.test.short=(l-h.short+1):l
t.train.long= 1:(l-h.long)
t.test.long=(l-h.long+1):l
train.short = x[t.train.short]
test.short = x[t.test.short]
train.long = x[t.train.long]
test.long = x[t.test.long]
fit.mlp = mlp(ts(x))
plot(fit.mlp)
roll.win.rmse.nn.wge(x,horizon=h.short,fit.mlp) # 4.24433
roll.win.rmse.nn.wge(x,horizon=h.long,fit.mlp) # 27.4536
fit.mlp = mlp(ts(train.short))
fore.mlp=forecast(fit.mlp,h=h.short)
plot(seq(1,l,1),x,type="b")
points(seq(l-h.short+1,l,1),fore.mlp$mean,type="b",pch=15)
plot(fore.mlp)
ase=mean((test.short-fore.mlp$mean)^2)
ase # 499.0734
fit.mlp = mlp(ts(train.long))
fore.mlp=forecast(fit.mlp,h=h.long)
plot(seq(1,l,1),x,type="b")
points(seq(l-h.long+1,l,1),fore.mlp$mean,type="b",pch=15)
plot(fore.mlp)
ase=mean((test.long-fore.mlp$mean)^2)
ase # 7797.95

# d= 2, 10 hidden nodes
set.seed(11)
fit.mlp = mlp(ts(x),difforder=2,hd=10)
plot(fit.mlp)
roll.win.rmse.nn.wge(x,horizon=h.short,fit.mlp) # 3.8800
roll.win.rmse.nn.wge(x,horizon=h.long,fit.mlp) # 24.9757
fit.mlp = mlp(ts(train.short),difforder=2,hd=10)
fore.mlp=forecast(fit.mlp,h=h.short)
plot(seq(1,l,1),x,type="b")
points(seq(l-h.short+1,l,1),fore.mlp$mean,type="b",pch=15)
plot(fore.mlp)
ase=mean((test.short-fore.mlp$mean)^2)
ase # 1397.616
fit.mlp = mlp(ts(train.long),difforder=2,hd=10)
fore.mlp=forecast(fit.mlp,h=h.long)
plot(seq(1,l,1),x,type="b")
points(seq(l-h.long+1,l,1),fore.mlp$mean,type="b",pch=15)
plot(fore.mlp)
ase=mean((test.long-fore.mlp$mean)^2)
ase # 7203.322

# Will officially go with ARMA(9,1) predictions
dev.off()
x = fed_housing_data$Housing_Price_Index 
d = artrans.wge(x,1)
d2 = artrans.wge(d,1)
d3 = artrans.wge(d2,c(0,-1))
est = est.arma.wge(d3,p=6,q=5)
m = mult.wge(fac1=est$phi,fac2=c(0,-1))
factor.wge(m$model.coef) # looks good
f = fore.arima.wge(x,d=2,phi=m$model.coef,theta=est$theta,n.ahead=h.short,lastn=TRUE)
hpi.pred.short = f$f
f = fore.arima.wge(x,d=2,phi=m$model.coef,theta=est$theta,n.ahead=h.long,lastn=TRUE)
hpi.pred.long = f$f
