# Standard RMD code for pulling in variables
library(tidyverse)
library(tswge)
library(vars)
library(lubridate)
library(nnfor)
library(vars)
library(caret)
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
huc$DATE = as.Date(huc$DATE)
huc$month <- month(huc$DATE)
head(huc)
huc_quarterly <- huc %>%
  filter(huc$month == 1 | huc$month == 4 | huc$month == 7 | huc$month == 10)
summary(huc_quarterly)
hor_1975 = hor[41:238,]
hor_1975$DATE <- as.Date(hor_1975$DATE)
huc_1975 = huc_quarterly[29:226,]
mhp_1975 = mhp[49:246,]
mhp_1975$DATE <- as.Date(mhp_1975$DATE)
snh_1975 = snh_quarterly[49:246,]
hpi$DATE <- as.Date(hpi$DATE)
fed_housing_data_NL = data.frame(Year_Quarter = mhp_1975$DATE, Ownership_Rate = hor_1975$RHORUSQ156N, Housing_Units_Completed = huc_1975$COMPUTSA, Supply_New_Houses = snh_1975$MSACSR, Housing_Price_Index = hpi$USSTHPI, Median_Sales_Price = mhp_1975$MSPUS)
fed_housing_data = data.frame(Year_Quarter = as.Date(mhp_1975$DATE), Ownership_Rate = hor_1975$RHORUSQ156N, Housing_Units_Completed = huc_1975$COMPUTSA, Supply_New_Houses = snh_1975$MSACSR, Housing_Price_Index = hpi$USSTHPI, Median_Sales_Price = log(mhp_1975$MSPUS))
train = fed_housing_data[1:168,]
test = fed_housing_data[169:198,]

h.short = 4
h.long = 20
l = length(fed_housing_data$Ownership_Rate)
fed_housing_data_short = fed_housing_data
fed_housing_data_long = fed_housing_data
x = fed_housing_data$Ownership_Rate 
est = est.arma.wge(x,p=9,q=1)
f = fore.arima.wge(x,phi=est$phi,theta=est$theta,n.ahead=h.short,lastn=TRUE)
hor.pred.short = f$f
f = fore.arima.wge(x,phi=est$phi,theta=est$theta,n.ahead=h.long,lastn=TRUE)
hor.pred.long = f$f
fed_housing_data_short$Ownership_Rate[(l-h.short+1):l] = hor.pred.short
fed_housing_data_long$Ownership_Rate[(l-h.long+1):l] = hor.pred.long
x = fed_housing_data$Housing_Units_Completed  
est = est.arma.wge(x,p=9,q=1)
f = fore.arima.wge(x,phi=est$phi,theta=est$theta,n.ahead=h.short,lastn=TRUE)
huc.pred.short = f$f
f = fore.arima.wge(x,phi=est$phi,theta=est$theta,n.ahead=h.long,lastn=TRUE)
huc.pred.long = f$f
fed_housing_data_short$Housing_Units_Completed[(l-h.short+1):l] = huc.pred.short
fed_housing_data_long$Housing_Units_Completed[(l-h.long+1):l] = huc.pred.long
x = fed_housing_data$Housing_Price_Index 
d = artrans.wge(x,1)
d2 = artrans.wge(d,1)
d3 = artrans.wge(d2,c(0,-1))
est = est.arma.wge(d3,p=6,q=5)
m = mult.wge(fac1=est$phi,fac2=c(0,-1))
f = fore.arima.wge(x,d=2,phi=m$model.coef,theta=est$theta,n.ahead=h.short,lastn=TRUE)
hpi.pred.short = f$f
f = fore.arima.wge(x,d=2,phi=m$model.coef,theta=est$theta,n.ahead=h.long,lastn=TRUE)
hpi.pred.long = f$f
fed_housing_data_short$Housing_Price_Index[(l-h.short+1):l] = hpi.pred.short
fed_housing_data_long$Housing_Price_Index[(l-h.long+1):l] = hpi.pred.long
x = fed_housing_data$Supply_New_Houses   
est = est.arma.wge(x,p=1,q=1)
f = fore.arima.wge(x,phi=est$phi,theta=est$theta,n.ahead=h.short,lastn=TRUE)
snh.pred.short = f$f
f = fore.arima.wge(x,phi=est$phi,theta=est$theta,n.ahead=h.long,lastn=TRUE)
snh.pred.long = f$f
fed_housing_data_short$Supply_New_Houses[(l-h.short+1):l] = snh.pred.short
fed_housing_data_long$Supply_New_Houses[(l-h.long+1):l] = snh.pred.long
dev.off()

################################################################################

# VAR Models
x = fed_housing_data
x$Year_Quarter = c()
VARselect(x,lag.max=6,type="both",season=NULL,exogen=NULL) # AIC = 4, SC/BIC = 1
VARselect(x,lag.max=8,type="both",season=NULL,exogen=NULL) # AIC = 4, SC/BIC = 1
VARselect(x,lag.max=10,type="both",season=NULL,exogen=NULL) # AIC = 4, SC/BIC = 1
VARselect(x,lag.max=12,type="both",season=NULL,exogen=NULL) # AIC = 4, SC/BIC = 1
VARselect(x,lag.max=16,type="both",season=NULL,exogen=NULL) # AIC = 4, SC/BIC = 1
VARselect(x,lag.max=24,type="both",season=NULL,exogen=NULL) # AIC = 4, SC/BIC = 1
# Candidates: 4, 1, 2

# lag = 4
fit = VAR(x,p=4,type='both') 
summary(fit) # trend and const were significant, but only lag up to 2 for variable of interest, huc not very predictive
preds=predict(fit,n.ahead=h.short)
ase = mean((fed_housing_data_NL$Median_Sales_Price[(l-h.short+1):l]-exp(preds$fcst$Median_Sales_Price[,1]))^2)/1e6
ase # 46.3595
plot(seq(1,l,1),x$Median_Sales_Price,type="b")
points(seq(l-h.short+1,l,1),preds$fcst$Median_Sales_Price[1:h.short,1],type="b",pch=15)
fanchart(preds)
preds=predict(fit,n.ahead=h.long)
ase = mean((fed_housing_data_NL$Median_Sales_Price[(l-h.long+1):l]-exp(preds$fcst$Median_Sales_Price[,1]))^2)/1e6
ase # 3499.949
plot(seq(1,l,1),x$Median_Sales_Price,type="b")
points(seq(l-h.long+1,l,1),preds$fcst$Median_Sales_Price[1:h.long,1],type="b",pch=15)
fanchart(preds)

# lag = 1
fit = VAR(x,p=1,type='both') 
summary(fit) # trend and const were significant, but only lag up to 2 for variable of interest
preds=predict(fit,n.ahead=h.short)
ase = mean((fed_housing_data_NL$Median_Sales_Price[(l-h.short+1):l]-exp(preds$fcst$Median_Sales_Price[,1]))^2)/1e6
ase # 463.9545
plot(seq(1,l,1),x$Median_Sales_Price,type="b")
points(seq(l-h.short+1,l,1),preds$fcst$Median_Sales_Price[1:h.short,1],type="b",pch=15)
fanchart(preds)
preds=predict(fit,n.ahead=h.long)
ase = mean((fed_housing_data_NL$Median_Sales_Price[(l-h.long+1):l]-exp(preds$fcst$Median_Sales_Price[,1]))^2)/1e6
ase # 4374.405
plot(seq(1,l,1),x$Median_Sales_Price,type="b")
points(seq(l-h.long+1,l,1),preds$fcst$Median_Sales_Price[1:h.long,1],type="b",pch=15)
fanchart(preds)

# lag = 2
fit = VAR(x,p=2,type='both') 
summary(fit) # trend and const were significant, but only lag up to 2 for variable of interest
preds=predict(fit,n.ahead=h.short)
ase = mean((fed_housing_data_NL$Median_Sales_Price[(l-h.short+1):l]-exp(preds$fcst$Median_Sales_Price[,1]))^2)/1e6
ase # 144.6791
plot(seq(1,l,1),x$Median_Sales_Price,type="b")
points(seq(l-h.short+1,l,1),preds$fcst$Median_Sales_Price[1:h.short,1],type="b",pch=15)
fanchart(preds)
preds=predict(fit,n.ahead=h.long)
ase = mean((fed_housing_data_NL$Median_Sales_Price[(l-h.long+1):l]-exp(preds$fcst$Median_Sales_Price[,1]))^2)/1e6
ase # 4297.166
plot(seq(1,l,1),x$Median_Sales_Price,type="b")
points(seq(l-h.long+1,l,1),preds$fcst$Median_Sales_Price[1:h.long,1],type="b",pch=15)
fanchart(preds)

# lag = 24
fit = VAR(x,p=24,type='both') 
summary(fit) # trend and const were significant, but only lag up to 2 for variable of interest
preds=predict(fit,n.ahead=h.short)
ase = mean((fed_housing_data_NL$Median_Sales_Price[(l-h.short+1):l]-exp(preds$fcst$Median_Sales_Price[,1]))^2)/1e6
ase # 2177.036
plot(seq(1,l,1),x$Median_Sales_Price,type="b")
points(seq(l-h.short+1,l,1),preds$fcst$Median_Sales_Price[1:h.short,1],type="b",pch=15)
fanchart(preds)
preds=predict(fit,n.ahead=h.long)
ase = mean((fed_housing_data_NL$Median_Sales_Price[(l-h.long+1):l]-exp(preds$fcst$Median_Sales_Price[,1]))^2)/1e6
ase # 19535.52
plot(seq(1,l,1),x$Median_Sales_Price,type="b")
points(seq(l-h.long+1,l,1),preds$fcst$Median_Sales_Price[1:h.long,1],type="b",pch=15)
fanchart(preds)

# Try without huc
x$Housing_Units_Completed = c()
VARselect(x,lag.max=6,type="both",season=NULL,exogen=NULL) # AIC = 5, SC/BIC = 2
VARselect(x,lag.max=8,type="both",season=NULL,exogen=NULL) # AIC = 5, SC/BIC = 2
VARselect(x,lag.max=10,type="both",season=NULL,exogen=NULL) # AIC = 9, SC/BIC = 9
VARselect(x,lag.max=12,type="both",season=NULL,exogen=NULL) # AIC = 11, SC/BIC = 2
VARselect(x,lag.max=16,type="both",season=NULL,exogen=NULL) # AIC = 11, SC/BIC = 2
# Candidates: 5, 2

# lag = 5
fit = VAR(x,p=5,type='both') 
summary(fit) 
preds=predict(fit,n.ahead=h.short)
ase = mean((fed_housing_data_NL$Median_Sales_Price[(l-h.short+1):l]-exp(preds$fcst$Median_Sales_Price[,1]))^2)/1e6
ase # 43.20367
plot(seq(1,l,1),x$Median_Sales_Price,type="b")
points(seq(l-h.short+1,l,1),preds$fcst$Median_Sales_Price[1:h.short,1],type="b",pch=15)
fanchart(preds)
preds=predict(fit,n.ahead=h.long)
ase = mean((fed_housing_data_NL$Median_Sales_Price[(l-h.long+1):l]-exp(preds$fcst$Median_Sales_Price[,1]))^2)/1e6
ase # 4084.292
plot(seq(1,l,1),x$Median_Sales_Price,type="b")
points(seq(l-h.long+1,l,1),preds$fcst$Median_Sales_Price[1:h.long,1],type="b",pch=15)
fanchart(preds)

# lag = 2
fit = VAR(x,p=2,type='both') 
summary(fit) 
preds=predict(fit,n.ahead=h.short)
ase = mean((fed_housing_data_NL$Median_Sales_Price[(l-h.short+1):l]-exp(preds$fcst$Median_Sales_Price[,1]))^2)/1e6
ase # 131.653
plot(seq(1,l,1),x$Median_Sales_Price,type="b")
points(seq(l-h.short+1,l,1),preds$fcst$Median_Sales_Price[1:h.short,1],type="b",pch=15)
fanchart(preds)
preds=predict(fit,n.ahead=h.long)
ase = mean((fed_housing_data_NL$Median_Sales_Price[(l-h.long+1):l]-exp(preds$fcst$Median_Sales_Price[,1]))^2)/1e6
ase # 4187.361
plot(seq(1,l,1),x$Median_Sales_Price,type="b")
points(seq(l-h.long+1,l,1),preds$fcst$Median_Sales_Price[1:h.long,1],type="b",pch=15)
fanchart(preds)

################################################################################

# Start by looking at the lag plots
l = ccf(x$Median_Sales_Price,x$Ownership_Rate,lag.max=80)
which.max(l$acf)
l$acf[20] # 0.3090685
l$lag[20] # 0
l = ccf(x$Median_Sales_Price,x$Housing_Units_Completed,lag.max=80)
which.max(l$acf)
l$acf[150] # 0.263895
l$lag[150] # 69, 21 seemed alright
l = ccf(x$Median_Sales_Price,x$Supply_New_Houses,lag.max=60)
which.min(l$acf)
l$acf[70] # -0.2592145
l$lag[70] # 9
l = ccf(x$Median_Sales_Price,x$Housing_Price_Index,lag.max=60)
which.max(l$acf)
l$acf[61] # 0.933195
l$lag[61] # 0

# Try looking at lags
x = fed_housing_data
x.short = fed_housing_data_short
x.long = fed_housing_data_long
x$Year_Quarter = c()
x.short$Year_Quarter = c()
x.long$Year_Quarter = c()
t=1:l
t.train.short= 1:(l-h.short)
t.test.short=(l-h.short+1):l
t.train.long= 1:(l-h.long)
t.test.long=(l-h.long+1):l
x$Housing_Units_Completed_l21 = dplyr::lag(x$Housing_Units_Completed,21)
x$Supply_New_Houses_l9 = dplyr::lag(x$Supply_New_Houses,9)
x.short$Housing_Units_Completed_l21 = dplyr::lag(x.short$Housing_Units_Completed,21)
x.short$Supply_New_Houses_l9 = dplyr::lag(x.short$Supply_New_Houses,9)
x.long$Housing_Units_Completed_l21 = dplyr::lag(x.long$Housing_Units_Completed,21)
x.long$Supply_New_Houses_l9 = dplyr::lag(x.long$Supply_New_Houses,9)
ksfit = lm(x$Median_Sales_Price~x$Ownership_Rate+x$Housing_Units_Completed_l21+x$Supply_New_Houses_l9+x$Housing_Price_Index+t)
summary(ksfit)
AIC(ksfit) # -454.2937
# Try removing Ownership_Rate, since it has a bad p value
ksfit = lm(x$Median_Sales_Price~x$Housing_Units_Completed_l21+x$Supply_New_Houses_l9+x$Housing_Price_Index+t)
summary(ksfit)
AIC(ksfit) # -456.0482
# go with no Ownership_Rate, since the resulting model has better p values and improves AIC
aic5.wge(ksfit$residuals,p=0:16,q=0:4,type='aic') # best 2/0 highest p 3 highest q 2
fit=arima(x.short$Median_Sales_Price[t.train.short],order=c(2,0,0),xreg=cbind(t.train.short,x.short$Housing_Units_Completed_l21[t.train.short],x.short$Supply_New_Houses_l9[t.train.short],x.short$Housing_Price_Index[t.train.short]))
preds = predict(fit,newxreg = data.frame(t=t.test.short,Housing_Units_Completed_l21=x.short$Housing_Units_Completed_l21[t.test.short],Supply_New_Houses_l9=x.short$Supply_New_Houses_l9[t.test.short],Housing_Price_Index=x.short$Housing_Price_Index[t.test.short]))
ase = mean((fed_housing_data_NL$Median_Sales_Price[t.test.short]-exp(preds$pred))^2)/1e6
ase # 588.675
plot(seq(1,l,1),x.short$Median_Sales_Price,type="b")
points(seq((l-h.short+1),l,1),preds$pred,type="b",pch=15)
fit=arima(x.long$Median_Sales_Price[t.train.long],order=c(2,0,0),xreg=cbind(t.train.long,x$Housing_Units_Completed_l21[t.train.long],x$Supply_New_Houses_l9[t.train.long],x$Housing_Price_Index[t.train.long]))
preds = predict(fit,newxreg = data.frame(t=t.test.short,Housing_Units_Completed_l21=x.long$Housing_Units_Completed_l21[t.test.long],Supply_New_Houses_l9=x.long$Supply_New_Houses_l9[t.test.long],Housing_Price_Index=x.long$Housing_Price_Index[t.test.long]))
ase = mean((fed_housing_data_NL$Median_Sales_Price[t.test.long]-exp(preds$pred))^2)/1e6
ase # 1431.873
plot(seq(1,l,1),x.long$Median_Sales_Price,type="b")
points(seq((l-h.long+1),l,1),preds$pred,type="b",pch=15)

# Try t by itself
x = fed_housing_data
x.short = fed_housing_data_short
x.long = fed_housing_data_long
x$Year_Quarter = c()
x.short$Year_Quarter = c()
x.long$Year_Quarter = c()
ksfit = lm(x$Median_Sales_Price~t)
summary(ksfit)
AIC(ksfit) # -310.9747
aic5.wge(ksfit$residuals,p=0:16,q=0:4,type='aic') # best 2/2, 12/2
fit=arima(x.short$Median_Sales_Price[t.train.short],order=c(2,0,2),xreg=cbind(t.train.short))
preds = predict(fit,newxreg = data.frame(t=t.test.short))
ase = mean((fed_housing_data_NL$Median_Sales_Price[t.test.short]-exp(preds$pred))^2)/1e6
ase # 210.0874
plot(seq(1,l,1),x.short$Median_Sales_Price,type="b")
points(seq((l-h.short+1),l,1),preds$pred,type="b",pch=15)
fit=arima(x.long$Median_Sales_Price[t.train.long],order=c(2,0,2),xreg=cbind(t.train.long))
preds = predict(fit,newxreg = data.frame(t=t.test.long))
ase = mean((fed_housing_data_NL$Median_Sales_Price[t.test.long]-exp(preds$pred))^2)/1e6
ase # 1642.856
plot(seq(1,l,1),x.long$Median_Sales_Price,type="b")
points(seq((l-h.long+1),l,1),preds$pred,type="b",pch=15)

fit=arima(x.short$Median_Sales_Price[t.train.short],order=c(12,0,2),xreg=cbind(t.train.short))
preds = predict(fit,newxreg = data.frame(t=t.test.short))
ase = mean((fed_housing_data_NL$Median_Sales_Price[t.test.short]-exp(preds$pred))^2)/1e6
ase # 238.9019
plot(seq(1,l,1),x.short$Median_Sales_Price,type="b")
points(seq((l-h.short+1),l,1),preds$pred,type="b",pch=15)
fit=arima(x.long$Median_Sales_Price[t.train.long],order=c(12,0,2),xreg=cbind(t.train.long))
preds = predict(fit,newxreg = data.frame(t=t.test.long))
ase = mean((fed_housing_data_NL$Median_Sales_Price[t.test.long]-exp(preds$pred))^2)/1e6
ase # 1777.58
plot(seq(1,l,1),x.long$Median_Sales_Price,type="b")
points(seq((l-h.long+1),l,1),preds$pred,type="b",pch=15)

# Try t + Supply_New_Houses_l14
x = fed_housing_data
x.short = fed_housing_data_short
x.long = fed_housing_data_long
x$Year_Quarter = c()
x.short$Year_Quarter = c()
x.long$Year_Quarter = c()
x$Supply_New_Houses_l14 = dplyr::lag(x$Supply_New_Houses,14)
x.short$Supply_New_Houses_l14 = dplyr::lag(x.short$Supply_New_Houses,14)
x.long$Supply_New_Houses_l14 = dplyr::lag(x.long$Supply_New_Houses,14)
ksfit = lm(x$Median_Sales_Price~x$Supply_New_Houses_l14+t)
summary(ksfit)
AIC(ksfit) # -423.6164
aic5.wge(ksfit$residuals,p=0:12,q=0:2,type='aic') # best 4/0 highest p 5 highest q 1
fit=arima(x.short$Median_Sales_Price[t.train.short],order=c(4,0,0),xreg=cbind(t.train.short,x.short$Supply_New_Houses_l14[t.train.short]))
preds = predict(fit,newxreg = data.frame(t=t.test.short,Supply_New_Houses_l14=x.short$Supply_New_Houses_l14[t.test.short]))
ase = mean((fed_housing_data_NL$Median_Sales_Price[t.test.short]-exp(preds$pred))^2)/1e6
ase # 227.6938
plot(seq(1,l,1),x.short$Median_Sales_Price,type="b")
points(seq((l-h.short+1),l,1),preds$pred,type="b",pch=15)
fit=arima(x.long$Median_Sales_Price[t.train.long],order=c(4,0,0),xreg=cbind(t.train.long,x$Supply_New_Houses_l14[t.train.long]))
preds = predict(fit,newxreg = data.frame(t=t.test.long,Supply_New_Houses_l14=x.long$Supply_New_Houses_l14[t.test.long]))
ase = mean((fed_housing_data_NL$Median_Sales_Price[t.test.long]-exp(preds$pred))^2)/1e6
ase # 2169.451
plot(seq(1,l,1),x.long$Median_Sales_Price,type="b")
points(seq((l-h.long+1),l,1),preds$pred,type="b",pch=15)

# Try t + Supply_New_Houses_l14 + Supply_New_Houses_l5
x = fed_housing_data
x.short = fed_housing_data_short
x.long = fed_housing_data_long
x$Year_Quarter = c()
x.short$Year_Quarter = c()
x.long$Year_Quarter = c()
x$Supply_New_Houses_l14 = dplyr::lag(x$Supply_New_Houses,14)
x.short$Supply_New_Houses_l14 = dplyr::lag(x.short$Supply_New_Houses,14)
x.long$Supply_New_Houses_l14 = dplyr::lag(x.long$Supply_New_Houses,14)
x$Supply_New_Houses_l5 = dplyr::lag(x$Supply_New_Houses,5)
x.short$Supply_New_Houses_l5 = dplyr::lag(x.short$Supply_New_Houses,5)
x.long$Supply_New_Houses_l5 = dplyr::lag(x.long$Supply_New_Houses,5)
ksfit = lm(x$Median_Sales_Price~x$Supply_New_Houses_l14+x$Supply_New_Houses_l5+t)
summary(ksfit)
AIC(ksfit) # -449.1047
aic5.wge(ksfit$residuals,p=0:8,q=0:2,type='aic') # best 3/2 highest p 6 highest q 2
aic5.wge(ksfit$residuals,p=0:8,q=0:4,type='aic') # best 3/2 highest p 5 highest q 3
fit=arima(x.short$Median_Sales_Price[t.train.short],order=c(3,0,2),xreg=cbind(t.train.short,x.short$Supply_New_Houses_l14[t.train.short],x.short$Supply_New_Houses_l5[t.train.short]))
preds = predict(fit,newxreg = data.frame(t=t.test.short,Supply_New_Houses_l14=x.short$Supply_New_Houses_l14[t.test.short],Supply_New_Houses_l5=x.short$Supply_New_Houses_l5[t.test.short]))
ase = mean((fed_housing_data_NL$Median_Sales_Price[t.test.short]-exp(preds$pred))^2)/1e6
ase # 212.8009
plot(seq(1,l,1),x.short$Median_Sales_Price,type="b")
points(seq((l-h.short+1),l,1),preds$pred,type="b",pch=15)
fit=arima(x.long$Median_Sales_Price[t.train.long],order=c(3,0,2),xreg=cbind(t.train.long,x.long$Supply_New_Houses_l14[t.train.long],x.long$Supply_New_Houses_l5[t.train.long]))
preds = predict(fit,newxreg = data.frame(t=t.test.long,Supply_New_Houses_l14=x.long$Supply_New_Houses_l14[t.test.long],Supply_New_Houses_l5=x.long$Supply_New_Houses_l5[t.test.long]))
ase = mean((fed_housing_data_NL$Median_Sales_Price[t.test.long]-exp(preds$pred))^2)/1e6
ase # 1908.763
plot(seq(1,l,1),x.long$Median_Sales_Price,type="b")
points(seq((l-h.long+1),l,1),preds$pred,type="b",pch=15)

# Try t + Supply_New_Houses_l14 + Supply_New_Houses_l5 + Housing_Units_Completed_l20
x = fed_housing_data
x.short = fed_housing_data_short
x.long = fed_housing_data_long
x$Year_Quarter = c()
x.short$Year_Quarter = c()
x.long$Year_Quarter = c()
x$Supply_New_Houses_l14 = dplyr::lag(x$Supply_New_Houses,14)
x.short$Supply_New_Houses_l14 = dplyr::lag(x.short$Supply_New_Houses,14)
x.long$Supply_New_Houses_l14 = dplyr::lag(x.long$Supply_New_Houses,14)
x$Supply_New_Houses_l5 = dplyr::lag(x$Supply_New_Houses,5)
x.short$Supply_New_Houses_l5 = dplyr::lag(x.short$Supply_New_Houses,5)
x.long$Supply_New_Houses_l5 = dplyr::lag(x.long$Supply_New_Houses,5)
x$Housing_Units_Completed_l20 = dplyr::lag(x$Housing_Units_Completed,20)
x.short$Housing_Units_Completed_l20 = dplyr::lag(x.short$Housing_Units_Completed,20)
x.long$Housing_Units_Completed_l20 = dplyr::lag(x.long$Housing_Units_Completed,20)
ksfit = lm(x$Median_Sales_Price~x$Supply_New_Houses_l14+x$Supply_New_Houses_l5+x$Housing_Units_Completed_l20+t)
summary(ksfit)
AIC(ksfit) # -463.9736
aic5.wge(ksfit$residuals,p=0:6,q=0:2,type='aic') # best 3/2 highest p 6 highest q 2
aic5.wge(ksfit$residuals,p=0:8,q=0:4,type='aic') # best 3/3 highest p 4 highest q 4
aic5.wge(ksfit$residuals,p=0:6,q=0:6,type='aic') # best 3/3 highest p 4 highest q 4
fit=arima(x.short$Median_Sales_Price[t.train.short],order=c(3,0,3),xreg=cbind(t.train.short,x.short$Supply_New_Houses_l14[t.train.short],x.short$Supply_New_Houses_l5[t.train.short],x.short$Housing_Units_Completed_l20[t.train.short]))
preds = predict(fit,newxreg = data.frame(t=t.test.short,Supply_New_Houses_l14=x.short$Supply_New_Houses_l14[t.test.short],Supply_New_Houses_l5=x.short$Supply_New_Houses_l5[t.test.short],Housing_Units_Completed_l20=x.short$Housing_Units_Completed_l20[t.test.short]))
ase = mean((fed_housing_data_NL$Median_Sales_Price[t.test.short]-exp(preds$pred))^2)/1e6
ase # 157.7445
plot(seq(1,l,1),x.short$Median_Sales_Price,type="b")
points(seq((l-h.short+1),l,1),preds$pred,type="b",pch=15)
fit=arima(x.long$Median_Sales_Price[t.train.long],order=c(3,0,3),xreg=cbind(t.train.long,x.long$Supply_New_Houses_l14[t.train.long],x.long$Supply_New_Houses_l5[t.train.long],x.long$Housing_Units_Completed_l20[t.train.long]))
preds = predict(fit,newxreg = data.frame(t=t.test.long,Supply_New_Houses_l14=x.long$Supply_New_Houses_l14[t.test.long],Supply_New_Houses_l5=x.long$Supply_New_Houses_l5[t.test.long],Housing_Units_Completed_l20=x.long$Housing_Units_Completed_l20[t.test.long]))
ase = mean((fed_housing_data_NL$Median_Sales_Price[t.test.long]-exp(preds$pred))^2)/1e6
ase # 2063.374
plot(seq(1,l,1),x.long$Median_Sales_Price,type="b")
points(seq((l-h.long+1),l,1),preds$pred,type="b",pch=15)

# Try t + Supply_New_Houses_l14 + Supply_New_Houses_l5 + Housing_Units_Completed_l20 + Supply_New_Houses_l10
t=1:l
t.train.short= 1:(l-h.short)
t.test.short=(l-h.short+1):l
t.train.long= 1:(l-h.long)
t.test.long=(l-h.long+1):l
x = fed_housing_data
x.short = fed_housing_data_short
x.long = fed_housing_data_long
x$Year_Quarter = c()
x.short$Year_Quarter = c()
x.long$Year_Quarter = c()
x$Supply_New_Houses_l14 = dplyr::lag(x$Supply_New_Houses,14)
x.short$Supply_New_Houses_l14 = dplyr::lag(x.short$Supply_New_Houses,14)
x.long$Supply_New_Houses_l14 = dplyr::lag(x.long$Supply_New_Houses,14)
x$Supply_New_Houses_l5 = dplyr::lag(x$Supply_New_Houses,5)
x.short$Supply_New_Houses_l5 = dplyr::lag(x.short$Supply_New_Houses,5)
x.long$Supply_New_Houses_l5 = dplyr::lag(x.long$Supply_New_Houses,5)
x$Housing_Units_Completed_l20 = dplyr::lag(x$Housing_Units_Completed,20)
x.short$Housing_Units_Completed_l20 = dplyr::lag(x.short$Housing_Units_Completed,20)
x.long$Housing_Units_Completed_l20 = dplyr::lag(x.long$Housing_Units_Completed,20)
x$Supply_New_Houses_l10 = dplyr::lag(x$Supply_New_Houses,10)
x.short$Supply_New_Houses_l10 = dplyr::lag(x.short$Supply_New_Houses,10)
x.long$Supply_New_Houses_l10 = dplyr::lag(x.long$Supply_New_Houses,10)
ksfit = lm(x$Median_Sales_Price~x$Supply_New_Houses_l14+x$Supply_New_Houses_l5+x$Housing_Units_Completed_l20+x$Supply_New_Houses_l10+t)
summary(ksfit)
AIC(ksfit) # -481.4335
aic5.wge(ksfit$residuals,p=0:4,q=0:2,type='aic') # best 3/1 highest p 4 highest q 2
aic5.wge(ksfit$residuals,p=0:6,q=0:4,type='aic') # best 6/3 highest p 4 highest q 3
aic5.wge(ksfit$residuals,p=0:8,q=0:6,type='aic') # best 7/0 highest p 8 highest q 6
aic5.wge(ksfit$residuals,p=0:10,q=0:8,type='aic') # best 5/7 highest p 8 highest q 7
fit=arima(x.short$Median_Sales_Price[t.train.short],order=c(5,0,7),xreg=cbind(t.train.short,x.short$Supply_New_Houses_l14[t.train.short],x.short$Supply_New_Houses_l5[t.train.short],x.short$Housing_Units_Completed_l20[t.train.short],x.short$Supply_New_Houses_l10[t.train.short]))
preds = predict(fit,newxreg = data.frame(t=t.test.short,Supply_New_Houses_l14=x.short$Supply_New_Houses_l14[t.test.short],Supply_New_Houses_l5=x.short$Supply_New_Houses_l5[t.test.short],Housing_Units_Completed_l20=x.short$Housing_Units_Completed_l20[t.test.short],Supply_New_Houses_l10=x.short$Supply_New_Houses_l10[t.test.short]))
ase = mean((fed_housing_data_NL$Median_Sales_Price[t.test.short]-exp(preds$pred))^2)/1e6
ase # 131.6795
plot(seq(1,l,1),x.short$Median_Sales_Price,type="b")
points(seq((l-h.short+1),l,1),preds$pred,type="b",pch=15)
fit=arima(x.long$Median_Sales_Price[t.train.long],order=c(5,0,7),xreg=cbind(t.train.long,x.long$Supply_New_Houses_l14[t.train.long],x.long$Supply_New_Houses_l5[t.train.long],x.long$Housing_Units_Completed_l20[t.train.long],x.long$Supply_New_Houses_l10[t.train.long]))
preds = predict(fit,newxreg = data.frame(t=t.test.long,Supply_New_Houses_l14=x.long$Supply_New_Houses_l14[t.test.long],Supply_New_Houses_l5=x.long$Supply_New_Houses_l5[t.test.long],Housing_Units_Completed_l20=x.long$Housing_Units_Completed_l20[t.test.long],Supply_New_Houses_l10=x.long$Supply_New_Houses_l10[t.test.long]))
ase = mean((fed_housing_data_NL$Median_Sales_Price[t.test.long]-exp(preds$pred))^2)/1e6
ase # 2534.7
plot(seq(1,l,1),x.long$Median_Sales_Price,type="b")
points(seq((l-h.long+1),l,1),preds$pred,type="b",pch=15)

# I forget, how were the errors for the signal plus noise models?
x = fed_housing_data$Median_Sales_Price
f = fore.sigplusnoise.wge(x,linear=TRUE,max.p=6,n.ahead=h.short,lastn=TRUE)
ase = mean((fed_housing_data_NL$Median_Sales_Price[(l-h.short+1):l]-exp(f$f))^2)/1e6
ase # 50.91791
f = fore.sigplusnoise.wge(x,linear=TRUE,max.p=6,n.ahead=h.long,lastn=TRUE)
ase = mean((fed_housing_data_NL$Median_Sales_Price[(l-h.long+1):l]-exp(f$f))^2)/1e6
ase # 1104.897

################################################################################

# Maybe just try out to 20, to see what happens
x = fed_housing_data
l = length(x$Median_Sales_Price)
x$t = 1:l
x$Year_Quarter= c()
for (ii in 1:20){
  hor_name = paste('Ownership_Rate_l',ii,sep='')
  x[[hor_name]] = dplyr::lag(x$Ownership_Rate,ii)
  huc_name = paste('Housing_Units_Completed_l',ii,sep='')
  x[[huc_name]] = dplyr::lag(x$Housing_Units_Completed,ii)
  snh_name = paste('Supply_New_Houses_l',ii,sep='')
  x[[snh_name]] = dplyr::lag(x$Supply_New_Houses,ii)
  hpi_name = paste('Housing_Price_Index_l',ii,sep='')
  x[[hpi_name]] = dplyr::lag(x$Housing_Price_Index,ii)
}

# First variable should be job level
set.seed(17)
vars <- names(x)
vars <- vars[vars!="Median_Sales_Price"]
num_vars <- length(vars)
var_rmse <- data.frame("vars" = vars)
num_folds <- 5
for (j in 1:num_vars) {
  var <- vars[j]
  print(paste(j,'/',num_vars,': ',var,sep=''))
  folds <- createFolds(x$Median_Sales_Price, k = num_folds)
  rmse <- numeric(num_folds)
  for (i in 1:num_folds) {
    tryCatch({
      train_indices <- unlist(folds[-i])
      test_indices <- unlist(folds[i])
      train <- x[train_indices, ]
      test <- x[test_indices, ]
      form <- as.formula(paste("Median_Sales_Price ~ ",var,sep=""))
      model <- lm(form, data = train)
      predictions <- predict(model, newdata = test, type = "response")
      remove_na <- na.omit(predictions - test$Median_Sales_Price)
      rmse[i] <- sqrt(mean(remove_na^2))
    }, error = function(e) { 
      rmse <- rmse[-i]
    })
  }
  var_rmse$rmse[var_rmse$var == var] <- mean(rmse)
} # t: rmse = 0.1097755
form <- as.formula(paste("Median_Sales_Price ~ t",sep=""))
model <- lm(form, data = x)
summary(model)

# Try adding a variable to t
set.seed(18)
vars <- names(x)
vars <- vars[vars!="Median_Sales_Price"]
vars <- vars[vars!="t"]
num_vars <- length(vars)
var_rmse <- data.frame("vars" = vars)
num_folds <- 5
for (j in 1:num_vars) {
  var <- vars[j]
  print(paste(j,'/',num_vars,': ',var,sep=''))
  folds <- createFolds(x$Median_Sales_Price, k = num_folds)
  rmse <- numeric(num_folds)
  for (i in 1:num_folds) {
    tryCatch({
      train_indices <- unlist(folds[-i])
      test_indices <- unlist(folds[i])
      train <- x[train_indices, ]
      test <- x[test_indices, ]
      form <- as.formula(paste("Median_Sales_Price ~ t + ",var,sep=""))
      model <- lm(form, data = train)
      predictions <- predict(model, newdata = test, type = "response")
      remove_na <- na.omit(predictions - test$Median_Sales_Price)
      rmse[i] <- sqrt(mean(remove_na^2))
    }, error = function(e) { 
      rmse <- rmse[-i]
    })
  }
  var_rmse$rmse[var_rmse$var == var] <- mean(rmse)
} # Supply_New_Houses_l14: rmse = 0.07477624
form <- as.formula(paste("Median_Sales_Price ~ t + Supply_New_Houses_l14",sep=""))
model <- lm(form, data = x)
summary(model)

# Add a variable
set.seed(19)
vars <- names(x)
vars <- vars[vars!="Median_Sales_Price"]
vars <- vars[vars!="t"]
vars <- vars[vars!="Supply_New_Houses_l14"]
num_vars <- length(vars)
var_rmse <- data.frame("vars" = vars)
num_folds <- 5
for (j in 1:num_vars) {
  var <- vars[j]
  print(paste(j,'/',num_vars,': ',var,sep=''))
  folds <- createFolds(x$Median_Sales_Price, k = num_folds)
  rmse <- numeric(num_folds)
  for (i in 1:num_folds) {
    tryCatch({
      train_indices <- unlist(folds[-i])
      test_indices <- unlist(folds[i])
      train <- x[train_indices, ]
      test <- x[test_indices, ]
      form <- as.formula(paste("Median_Sales_Price ~ t + Supply_New_Houses_l14 + ",var,sep=""))
      model <- lm(form, data = train)
      predictions <- predict(model, newdata = test, type = "response")
      remove_na <- na.omit(predictions - test$Median_Sales_Price)
      rmse[i] <- sqrt(mean(remove_na^2))
    }, error = function(e) { 
      rmse <- rmse[-i]
    })
  }
  var_rmse$rmse[var_rmse$var == var] <- mean(rmse)
} # Supply_New_Houses_l5: rmse = 0.06970225
form <- as.formula(paste("Median_Sales_Price ~ t + Supply_New_Houses_l14 + Supply_New_Houses_l5",sep=""))
model <- lm(form, data = x)
summary(model)

# Remove a variable
set.seed(20)
start_form_str <- 'Median_Sales_Price ~ t + Supply_New_Houses_l14 + Supply_New_Houses_l5'
vars <- unlist(strsplit(start_form_str, "\\+"))
vars <- trimws(vars)
vars[1] <- substr(vars[1], 22, nchar(vars[1]))
num_vars <- length(vars)
var_rmse <- data.frame("vars" = vars)
num_folds <- 5
for (j in 1:num_vars) {
  var <- vars[j]
  print(paste(j,'/',num_vars,': ',var,sep=''))
  folds <- createFolds(x$Median_Sales_Price, k = num_folds)
  rmse <- numeric(num_folds)
  for (i in 1:num_folds) {
    tryCatch({
      train_indices <- unlist(folds[-i])
      test_indices <- unlist(folds[i])
      train <- x[train_indices, ]
      test <- x[test_indices, ]
      form <- as.formula(paste(start_form_str," - ",var,sep=""))
      model <- lm(form, data = train)
      predictions <- predict(model, newdata = test, type = "response")
      remove_na <- na.omit(predictions - test$Median_Sales_Price)
      rmse[i] <- sqrt(mean(remove_na^2))
    }, error = function(e) { 
      rmse <- rmse[-i]
    })
  }
  var_rmse$rmse[var_rmse$var == var] <- mean(rmse)
}

# Add a variable
set.seed(21)
vars <- names(x)
vars <- vars[vars!="Median_Sales_Price"]
vars <- vars[vars!="t"]
vars <- vars[vars!="Supply_New_Houses_l14"]
vars <- vars[vars!="Supply_New_Houses_l5"]
num_vars <- length(vars)
var_rmse <- data.frame("vars" = vars)
num_folds <- 5
for (j in 1:num_vars) {
  var <- vars[j]
  print(paste(j,'/',num_vars,': ',var,sep=''))
  folds <- createFolds(x$Median_Sales_Price, k = num_folds)
  rmse <- numeric(num_folds)
  for (i in 1:num_folds) {
    tryCatch({
      train_indices <- unlist(folds[-i])
      test_indices <- unlist(folds[i])
      train <- x[train_indices, ]
      test <- x[test_indices, ]
      form <- as.formula(paste("Median_Sales_Price ~ t + Supply_New_Houses_l14 + Supply_New_Houses_l5 + ",var,sep=""))
      model <- lm(form, data = train)
      predictions <- predict(model, newdata = test, type = "response")
      remove_na <- na.omit(predictions - test$Median_Sales_Price)
      rmse[i] <- sqrt(mean(remove_na^2))
    }, error = function(e) { 
      rmse <- rmse[-i]
    })
  }
  var_rmse$rmse[var_rmse$var == var] <- mean(rmse)
} # Housing_Units_Completed_l20: rmse = 0.06410340
form <- as.formula(paste("Median_Sales_Price ~ t + Supply_New_Houses_l14 + Supply_New_Houses_l5 + Housing_Units_Completed_l20",sep=""))
model <- lm(form, data = x)
summary(model)

# Remove a variable
set.seed(22)
start_form_str <- 'Median_Sales_Price ~ t + Supply_New_Houses_l14 + Supply_New_Houses_l5 + Housing_Units_Completed_l20'
vars <- unlist(strsplit(start_form_str, "\\+"))
vars <- trimws(vars)
vars[1] <- substr(vars[1], 22, nchar(vars[1]))
num_vars <- length(vars)
var_rmse <- data.frame("vars" = vars)
num_folds <- 5
for (j in 1:num_vars) {
  var <- vars[j]
  print(paste(j,'/',num_vars,': ',var,sep=''))
  folds <- createFolds(x$Median_Sales_Price, k = num_folds)
  rmse <- numeric(num_folds)
  for (i in 1:num_folds) {
    tryCatch({
      train_indices <- unlist(folds[-i])
      test_indices <- unlist(folds[i])
      train <- x[train_indices, ]
      test <- x[test_indices, ]
      form <- as.formula(paste(start_form_str," - ",var,sep=""))
      model <- lm(form, data = train)
      predictions <- predict(model, newdata = test, type = "response")
      remove_na <- na.omit(predictions - test$Median_Sales_Price)
      rmse[i] <- sqrt(mean(remove_na^2))
    }, error = function(e) { 
      rmse <- rmse[-i]
    })
  }
  var_rmse$rmse[var_rmse$var == var] <- mean(rmse)
}

# Add a variable
set.seed(23)
vars <- names(x)
vars <- vars[vars!="Median_Sales_Price"]
vars <- vars[vars!="t"]
vars <- vars[vars!="Supply_New_Houses_l14"]
vars <- vars[vars!="Supply_New_Houses_l5"]
vars <- vars[vars!="Housing_Units_Completed_l20"]
num_vars <- length(vars)
var_rmse <- data.frame("vars" = vars)
num_folds <- 5
for (j in 1:num_vars) {
  var <- vars[j]
  print(paste(j,'/',num_vars,': ',var,sep=''))
  folds <- createFolds(x$Median_Sales_Price, k = num_folds)
  rmse <- numeric(num_folds)
  for (i in 1:num_folds) {
    tryCatch({
      train_indices <- unlist(folds[-i])
      test_indices <- unlist(folds[i])
      train <- x[train_indices, ]
      test <- x[test_indices, ]
      form <- as.formula(paste("Median_Sales_Price ~ t + Supply_New_Houses_l14 + Supply_New_Houses_l5 + Housing_Units_Completed_l20 + ",var,sep=""))
      model <- lm(form, data = train)
      predictions <- predict(model, newdata = test, type = "response")
      remove_na <- na.omit(predictions - test$Median_Sales_Price)
      rmse[i] <- sqrt(mean(remove_na^2))
    }, error = function(e) { 
      rmse <- rmse[-i]
    })
  }
  var_rmse$rmse[var_rmse$var == var] <- mean(rmse)
} # Supply_New_Houses_l10: rmse = 0.06120728
form <- as.formula(paste("Median_Sales_Price ~ t + Supply_New_Houses_l14 + Supply_New_Houses_l5 + Housing_Units_Completed_l20 + Supply_New_Houses_l10",sep=""))
model <- lm(form, data = x)
summary(model)

# Remove a variable
set.seed(24)
start_form_str <- 'Median_Sales_Price ~ t + Supply_New_Houses_l14 + Supply_New_Houses_l5 + Housing_Units_Completed_l20 + Supply_New_Houses_l10'
vars <- unlist(strsplit(start_form_str, "\\+"))
vars <- trimws(vars)
vars[1] <- substr(vars[1], 22, nchar(vars[1]))
num_vars <- length(vars)
var_rmse <- data.frame("vars" = vars)
num_folds <- 5
for (j in 1:num_vars) {
  var <- vars[j]
  print(paste(j,'/',num_vars,': ',var,sep=''))
  folds <- createFolds(x$Median_Sales_Price, k = num_folds)
  rmse <- numeric(num_folds)
  for (i in 1:num_folds) {
    tryCatch({
      train_indices <- unlist(folds[-i])
      test_indices <- unlist(folds[i])
      train <- x[train_indices, ]
      test <- x[test_indices, ]
      form <- as.formula(paste(start_form_str," - ",var,sep=""))
      model <- lm(form, data = train)
      predictions <- predict(model, newdata = test, type = "response")
      remove_na <- na.omit(predictions - test$Median_Sales_Price)
      rmse[i] <- sqrt(mean(remove_na^2))
    }, error = function(e) { 
      rmse <- rmse[-i]
    })
  }
  var_rmse$rmse[var_rmse$var == var] <- mean(rmse)
}