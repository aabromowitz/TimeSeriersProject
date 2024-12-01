library(tidyverse)
library(tswge)
library(vars)
library(lubridate)
library(vars)
library(nnfor)
library(caret)

# Variable of Interest - Quarterly from 1/1/63
file_path = "https://raw.githubusercontent.com/aabromowitz/TimeSeriersProject/refs/heads/main/MSPUS.csv"
mhp <- read.csv(file_path, header = TRUE)

# Home ownership rate - Quarterly from 1/1/65
file_path = "https://raw.githubusercontent.com/aabromowitz/TimeSeriersProject/refs/heads/main/RHORUSQ156N.csv"
hor <- read.csv(file_path, header = TRUE)

# Housing units completed - Monthly from 1/1/1968
file_path = "https://raw.githubusercontent.com/aabromowitz/TimeSeriersProject/refs/heads/main/COMPUTSA.csv"
huc <- read.csv(file_path, header = TRUE)

# Supply of new houses - Monthly from 1/1/1963
file_path = "https://raw.githubusercontent.com/aabromowitz/TimeSeriersProject/refs/heads/main/MSACSR.csv"
snh <- read.csv(file_path, header = TRUE)

# House price index - Quarterly from 1/1/1975
file_path = "https://raw.githubusercontent.com/aabromowitz/TimeSeriersProject/refs/heads/main/USSTHPI.csv"
hpi <- read.csv(file_path, header = TRUE)

# Converting Monthly Data to Quarterly Data

# Preserve Monthly format
snh_monthly <- snh
huc_monthly <- huc

# Supply of New Houses Variable 
snh$DATE = as.Date(snh$DATE)
snh$month <- month(snh$DATE)
head(snh)
snh_quarterly <- snh %>%
  filter(snh$month == 1 | snh$month == 4 | snh$month == 7 | snh$month == 10)
summary(snh_quarterly)

# Housing Units Completed Variable
huc$DATE = as.Date(huc$DATE)
huc$month <- month(huc$DATE)
head(huc)
huc_quarterly <- huc %>%
  filter(huc$month == 1 | huc$month == 4 | huc$month == 7 | huc$month == 10)
summary(huc_quarterly)

# Using same time frames, which would be starting at 1975 Q1 and ending at 2024 Q2 (due to hpi data)

# hor observation 41 is 1975 Q1
hor_1975 = hor[41:238,]
hor_1975$DATE <- as.Date(hor_1975$DATE)
summary(hor_1975)

# huc_quarterlly observation 29
huc_1975 = huc_quarterly[29:226,]
summary(huc_1975)

# mhp observation 49 is 1975 Q1
mhp_1975 = mhp[49:246,]
mhp_1975$DATE <- as.Date(mhp_1975$DATE)
summary(mhp_1975)

# snh_quarterly observation 49 is 1975 Q1
snh_1975 = snh_quarterly[49:246,]
summary(snh_1975)

# Housing Price Index variable already from 1975 Q1 - 2024 Q2
hpi$DATE <- as.Date(hpi$DATE)
summary(hpi)

# Create Dataframe
# Combined (Train/Test) Set NOT LOGGED
fed_housing_data_NL = data.frame(Year_Quarter = mhp_1975$DATE, Ownership_Rate = hor_1975$RHORUSQ156N, Housing_Units_Completed = huc_1975$COMPUTSA, Supply_New_Houses = snh_1975$MSACSR, Housing_Price_Index = hpi$USSTHPI, Median_Sales_Price = mhp_1975$MSPUS)

# Combined (Train/Test) Set LOGGED
fed_housing_data = data.frame(Year_Quarter = as.Date(mhp_1975$DATE), Ownership_Rate = hor_1975$RHORUSQ156N, Housing_Units_Completed = huc_1975$COMPUTSA, Supply_New_Houses = snh_1975$MSACSR, Housing_Price_Index = hpi$USSTHPI, Median_Sales_Price = log(mhp_1975$MSPUS))

# Train & Test sets
train = fed_housing_data[1:168,]
test = fed_housing_data[169:198,]

# plotting variables
xmin_plot = 150
ymin_plot = 12.3
ymax_plot = 13.1

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

# Try VAR + MLR
x = fed_housing_data
x.short = fed_housing_data_short
x.long = fed_housing_data_long
x$Year_Quarter = c()
x.short$Year_Quarter = c()
x.long$Year_Quarter = c()
l = length(x$Median_Sales_Price)
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
fit=arima(x.short$Median_Sales_Price[t.train.short],order=c(2,0,0),xreg=cbind(t.train.short,x.short$Housing_Units_Completed_l21[t.train.short],x.short$Supply_New_Houses_l9[t.train.short],x.short$Housing_Price_Index[t.train.short]))
preds = predict(fit,newxreg = data.frame(t=t.test.short,Housing_Units_Completed_l21=x.short$Housing_Units_Completed_l21[t.test.short],Supply_New_Houses_l9=x.short$Supply_New_Houses_l9[t.test.short],Housing_Price_Index=x.short$Housing_Price_Index[t.test.short]))
mlr_pred_short = preds$pred
fit=arima(x.long$Median_Sales_Price[t.train.long],order=c(2,0,0),xreg=cbind(t.train.long,x$Housing_Units_Completed_l21[t.train.long],x$Supply_New_Houses_l9[t.train.long],x$Housing_Price_Index[t.train.long]))
preds = predict(fit,newxreg = data.frame(t=t.test.short,Housing_Units_Completed_l21=x.long$Housing_Units_Completed_l21[t.test.long],Supply_New_Houses_l9=x.long$Supply_New_Houses_l9[t.test.long],Housing_Price_Index=x.long$Housing_Price_Index[t.test.long]))
mlr_pred_long = preds$pred

x = fed_housing_data
x$Year_Quarter = c()
fit = VAR(x[1:(l-h.short),],p=4,type='both')
preds=predict(fit,n.ahead=h.short)
var_pred_short = preds$fcst$Median_Sales_Price[,1]
fit = VAR(x[1:(l-h.long),],p=4,type='both')
preds=predict(fit,n.ahead=h.long)
var_pred_long = preds$fcst$Median_Sales_Price[,1]

ensemble1_short = (mlr_pred_short + var_pred_short)/2
ensemble1_long = (mlr_pred_long + var_pred_long)/2

ase = mean((fed_housing_data_NL$Median_Sales_Price[(l-h.short+1):l]-exp(ensemble1_short))^2)/1e6
ase # 190.5264
ase = mean((fed_housing_data_NL$Median_Sales_Price[(l-h.long+1):l]-exp(ensemble1_long))^2)/1e6
ase # 1048.507

plot(seq(xmin_plot,l,1),x$Median_Sales_Price[xmin_plot:l],type="l",col="black",xlab="Time",ylab="log Median Housing Sales Price",main="VAR Short Term Forecast",ylim=c(ymin_plot,ymax_plot))
lines(seq((l-h.short+1),l,1),ensemble1_short,col="red")
plot(seq(xmin_plot,l,1),x$Median_Sales_Price[xmin_plot:l],type="l",col="black",xlab="Time",ylab="log Median Housing Sales Price",main="VAR Long Term Forecast",ylim=c(ymin_plot,ymax_plot))
lines(seq((l-h.long+1),l,1),ensemble1_long,col="red")

# Try MLR + SPN
log.mhp = fed_housing_data$Median_Sales_Price
fit.mle.sig_h4 = fore.sigplusnoise.wge(log.mhp, linear = TRUE, method = 'mle', freq = 0, max.p = 6, n.ahead = 4, lastn = TRUE)
spn_pred_short = fit.mle.sig_h4$f
fit.mle.sig_h20 = fore.sigplusnoise.wge(log.mhp, linear = TRUE, method = 'mle', freq = 0, max.p = 6, n.ahead = 20, lastn = TRUE)
spn_pred_long = fit.mle.sig_h20$f

ensemble2_short = (mlr_pred_short + spn_pred_short)/2
ensemble2_long = (mlr_pred_long + spn_pred_long)/2

ase = mean((fed_housing_data_NL$Median_Sales_Price[(l-h.short+1):l]-exp(ensemble2_short))^2)/1e6
ase # 207.8022
ase = mean((fed_housing_data_NL$Median_Sales_Price[(l-h.long+1):l]-exp(ensemble2_long))^2)/1e6
ase # 653.9763

plot(seq(xmin_plot,l,1),x$Median_Sales_Price[xmin_plot:l],type="l",col="black",xlab="Time",ylab="log Median Housing Sales Price",main="VAR Short Term Forecast",ylim=c(ymin_plot,ymax_plot))
lines(seq((l-h.short+1),l,1),ensemble2_short,col="red")
plot(seq(xmin_plot,l,1),x$Median_Sales_Price[xmin_plot:l],type="l",col="black",xlab="Time",ylab="log Median Housing Sales Price",main="VAR Long Term Forecast",ylim=c(ymin_plot,ymax_plot))
lines(seq((l-h.long+1),l,1),ensemble2_long,col="red")

# VAR + SPN
ensemble3_short = (var_pred_short + spn_pred_short)/2
ensemble3_long = (var_pred_long + spn_pred_long)/2

ase = mean((fed_housing_data_NL$Median_Sales_Price[(l-h.short+1):l]-exp(ensemble3_short))^2)/1e6
ase # 28.44064
ase = mean((fed_housing_data_NL$Median_Sales_Price[(l-h.long+1):l]-exp(ensemble3_long))^2)/1e6
ase # 1853.413

plot(seq(xmin_plot,l,1),x$Median_Sales_Price[xmin_plot:l],type="l",col="black",xlab="Time",ylab="log Median Housing Sales Price",main="VAR Short Term Forecast",ylim=c(ymin_plot,ymax_plot))
lines(seq((l-h.short+1),l,1),ensemble3_short,col="red")
plot(seq(xmin_plot,l,1),x$Median_Sales_Price[xmin_plot:l],type="l",col="black",xlab="Time",ylab="log Median Housing Sales Price",main="VAR Long Term Forecast",ylim=c(ymin_plot,ymax_plot))
lines(seq((l-h.long+1),l,1),ensemble3_long,col="red")

# VAR + 2*SPN
ensemble4_short = (var_pred_short + 2*spn_pred_short)/3
ensemble4_long = (var_pred_long + 2*spn_pred_long)/3

ase = mean((fed_housing_data_NL$Median_Sales_Price[(l-h.short+1):l]-exp(ensemble4_short))^2)/1e6
ase # 31.79989
ase = mean((fed_housing_data_NL$Median_Sales_Price[(l-h.long+1):l]-exp(ensemble4_long))^2)/1e6
ase # 1554.375

plot(seq(xmin_plot,l,1),x$Median_Sales_Price[xmin_plot:l],type="l",col="black",xlab="Time",ylab="log Median Housing Sales Price",main="VAR Short Term Forecast",ylim=c(ymin_plot,ymax_plot))
lines(seq((l-h.short+1),l,1),ensemble4_short,col="red")
plot(seq(xmin_plot,l,1),x$Median_Sales_Price[xmin_plot:l],type="l",col="black",xlab="Time",ylab="log Median Housing Sales Price",main="VAR Long Term Forecast",ylim=c(ymin_plot,ymax_plot))
lines(seq((l-h.long+1),l,1),ensemble4_long,col="red")

# MLR + MLP
log.mhp = fed_housing_data$Median_Sales_Price
msp.194 = ts(log.mhp[1:194])
mspFit.4 = mlp(msp.194, comb = 'median')
f.4 = forecast(mspFit.4, h = 4)
mlp_pred_short = f.4$mean
msp.178 = ts(log.mhp[1:178])
mspFit.20 = mlp(msp.178, comb = 'median')
f.20 = forecast(mspFit.20, h = 20)
mlp_pred_long = f.20$mean

ensemble5_short = (mlr_pred_short + mlp_pred_short)/2
ensemble5_long = (mlr_pred_long + mlp_pred_long)/2

ase = mean((fed_housing_data_NL$Median_Sales_Price[(l-h.short+1):l]-exp(ensemble5_short))^2)/1e6
ase # 174.2601
ase = mean((fed_housing_data_NL$Median_Sales_Price[(l-h.long+1):l]-exp(ensemble5_long))^2)/1e6
ase # 826.6906

plot(seq(xmin_plot,l,1),x$Median_Sales_Price[xmin_plot:l],type="l",col="black",xlab="Time",ylab="log Median Housing Sales Price",main="VAR Short Term Forecast",ylim=c(ymin_plot,ymax_plot))
lines(seq((l-h.short+1),l,1),ensemble5_short,col="red")
plot(seq(xmin_plot,l,1),x$Median_Sales_Price[xmin_plot:l],type="l",col="black",xlab="Time",ylab="log Median Housing Sales Price",main="VAR Long Term Forecast",ylim=c(ymin_plot,ymax_plot))
lines(seq((l-h.long+1),l,1),ensemble5_long,col="red")

# VAR + LSTM
file_path = "https://raw.githubusercontent.com/aabromowitz/TimeSeriersProject/refs/heads/main/LSTM_predictions_short_term.csv"
pred <- read.csv(file_path, header = TRUE)
lstm_pred_short = pred$Prediction
file_path = "https://raw.githubusercontent.com/aabromowitz/TimeSeriersProject/refs/heads/main/LSTM_predictions_long_term.csv"
pred <- read.csv(file_path, header = TRUE)
lstm_pred_long = pred$Prediction

ensemble6_short = (var_pred_short + lstm_pred_short)/2
ensemble6_long = (var_pred_long + lstm_pred_long)/2

ase = mean((fed_housing_data_NL$Median_Sales_Price[(l-h.short+1):l]-exp(ensemble6_short))^2)/1e6
ase # 44.06369
ase = mean((fed_housing_data_NL$Median_Sales_Price[(l-h.long+1):l]-exp(ensemble6_long))^2)/1e6
ase # 877.9261

plot(seq(xmin_plot,l,1),x$Median_Sales_Price[xmin_plot:l],type="l",col="black",xlab="Time",ylab="log Median Housing Sales Price",main="VAR Short Term Forecast",ylim=c(ymin_plot,ymax_plot))
lines(seq((l-h.short+1),l,1),ensemble6_short,col="red")
plot(seq(xmin_plot,l,1),x$Median_Sales_Price[xmin_plot:l],type="l",col="black",xlab="Time",ylab="log Median Housing Sales Price",main="VAR Long Term Forecast",ylim=c(ymin_plot,ymax_plot))
lines(seq((l-h.long+1),l,1),ensemble6_long,col="red")