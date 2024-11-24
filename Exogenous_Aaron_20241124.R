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