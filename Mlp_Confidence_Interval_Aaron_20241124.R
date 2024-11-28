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
l = length(fed_housing_data$Median_Sales_Price)

################################################################################

x = fed_housing_data$Median_Sales_Price
plotts.sample.wge(x)

# Use MLP to get a prediction
set.seed(13)
fit.mlp = mlp(ts(x))
plot(fit.mlp)
plot(x,col='blue')
lines(1:length(fit.mlp$fitted),fit.mlp$fitted,col='red')
lines((length(x)-length(fit.mlp$fitted)+1):length(x),fit.mlp$fitted,col='green')
lx = length(x)
lf = length(fit.mlp$fitted)

# Are residuals white noise?
resid = x[(lx-lf+1):lx]-fit.mlp$fitted
plotts.sample.wge(resid)
acf(resid,lag.max=100)
ljung.wge(resid,K=24) # p = 0.224054, white
ljung.wge(resid,K=48) # p = 0.4910656, white
hist(resid) # normally distributed as well

# Try creating a new distribution, by randomly pulling from white noise
set.seed(14)
newx = numeric(lx)
newx[1] = x[1]
for (ii in 2:lx){
  newx[ii] = newx[ii-1]+sample(resid,1)
}
plotts.sample.wge(newx)
plotts.sample.wge(x)

################################################################################

# Try by adding a randomly sampled residual to the forecast
set.seed(15)
newx2 = numeric(lx)
for (ii in 1:(lx-lf)){
  newx2[ii] = x[ii]+sample(resid,1)
}
for (ii in (lx-lf+1):lx){
  newx2[ii] = fit.mlp$fitted[ii-(lx-lf)]+sample(resid,1)
}
newx2
plotts.sample.wge(newx2)
plotts.sample.wge(x)

# Try generating 1000 h.long forecasts, based on a generated realization
set.seed(16)
num_gen = 1000
confint = matrix(0, num_gen, h.long)
for (jj in 1:num_gen){
  
  if ((jj %% 10) == 0){
    print(jj)
  }
  
  # Generate realization
  newx2 = numeric(lx)
  for (ii in 1:(lx-lf)){
    newx2[ii] = x[ii]+sample(resid,1)
  }
  for (ii in (lx-lf+1):lx){
    newx2[ii] = fit.mlp$fitted[ii-(lx-lf)]+sample(resid,1)
  }
  
  # Create mlp model
  t.train.long= 1:(lx-h.long)
  train.long = newx2[t.train.long]
  fit.mlp = mlp(ts(train.long)) 
  fore.mlp=forecast(fit.mlp.fore,h=h.long)
  confint[jj,] = fore.mlp$mean
}

################################################################################

# Fixing mistake with remaking the model

x = fed_housing_data$Median_Sales_Price
set.seed(13)
fit.mlp = mlp(ts(x))
l = length(x)
lf = length(fit.mlp$fitted)
resid = x[(l-lf+1):l]-fit.mlp$fitted
set.seed(16)
num_gen = 1000
confint = matrix(0, num_gen, h.long)
start_time <- Sys.time()
# for (jj in 1:num_gen){
# for (jj in 728:num_gen){
for (jj in 983:num_gen){
  
  if ((jj %% 5) == 0){
    elapsed_time <- Sys.time() - start_time
    print(paste(jj,': ',elapsed_time,sep=""))
  }
  
  # Generate realization
  newx2 = numeric(l)
  for (ii in 1:(l-lf)){
    newx2[ii] = x[ii]+sample(resid,1)
  }
  for (ii in (l-lf+1):l){
    newx2[ii] = fit.mlp$fitted[ii-(l-lf)]+sample(resid,1)
  }
  
  # Create mlp model
  t.train.long= 1:(l-h.long)
  train.long = newx2[t.train.long]
  fit.mlp.f = mlp(ts(train.long)) # This should be a different mlp than the one created at the beginning, outside the loop
  fore.mlp=forecast(fit.mlp.f,h=h.long) # reference the new mlp
  confint[jj,] = fore.mlp$mean
}

write.csv(confint,'confint_2.csv')

# percentiles_per_column <- apply(holder, 2, function(column) {
percentiles_per_column <- apply(confint, 2, function(column) {
  quantile(column, probs = c(0.025, 0.975))
})

# Transpose the result for better readability
percentiles_per_column <- t(percentiles_per_column)

# Print the result
print(percentiles_per_column)

xtNewForBS = colMeans(confint)

plotts.wge(c(fit.mlp$fitted[t.train.long],xtNewForBS))
lines(seq((l-h.long+1),l,1),percentiles_per_column[,1],col = "blue")
lines(seq((l-h.long+1),l,1),percentiles_per_column[,2],col = "blue")
x = fed_housing_data$Median_Sales_Price
lines(seq((l-h.long+1),l,1),x[(l-h.long+1):l],col = "red")
