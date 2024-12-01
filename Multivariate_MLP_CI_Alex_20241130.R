ts.X = data.frame(hor = ts(fed_housing_data$Ownership_Rate), huc = ts(fed_housing_data$Housing_Units_Completed), snh = ts(fed_housing_data$Supply_New_Houses), hpi = ts(fed_housing_data$Housing_Price_Index))

# Trying what Dr Sadler did, but for an mlp model

# xt = gen.arma.wge(100,phi = .9)
file_path = 'https://raw.githubusercontent.com/aabromowitz/TimeSeriersProject/refs/heads/main/MSPUS.csv'
mhp <- read.csv(file_path, header = TRUE)
mhp_1975 = mhp[49:246,]
xt = log(mhp_1975$MSPUS)

# Horizon = 4
h.long = 4
l = length(xt)
xtTrain = xt[1:(l-h.long)]

# est = est.arma.wge(xtTrain,p = 1)
library(nnfor)
set.seed(30)
fit.mlp = mlp(ts(xt)) # mlp(ts(xt), comb = 'median', xreg = ts.X, difforder = 1, sel.lag = TRUE) 
###### Error in newdata[, object$model.list$variables] : subscript out of bounds ####
lf = length(fit.mlp$fitted)
res = xt[(l-lf+1):l]-fit.mlp$fitted

# Are residuals white noise?
plotts.sample.wge(res)
acf(res,lag.max=100)
ljung.wge(res,K=24) # p = 0.3804433, white noise
ljung.wge(res,K=48) # p = 0.6786805, white
hist(res) # look normally distributed

# BSSim = function(ts,res,phi,horizon) {
num_back = l - lf
BSSim = function(ts,res,fit,num_back,horizon) {
  origin = length(ts)
  ForHolder = numeric(horizon)
  for(i in (origin+1):(origin+horizon)){
    # ts[i] = ts[i-1]*phi + sample(res,1)
    # print(i)
    needed_to_predict = ts[(i-num_back):(i-1)]
    fore=forecast(fit,y=needed_to_predict,h=1)
    ts[i]=fore$mean+sample(res,1)
    ForHolder[(i-origin)]=ts[i]
  }
  return(ForHolder)
}

# BSSamples = 100000
BSSamples = 100 # I'm worried 100k will take a really long time, so make it less

# holder = matrix(nrow = BSSamples,ncol = 50)
holder = matrix(nrow = BSSamples,ncol = h.long)

for(i in 1:BSSamples)
{
  # xtNewFor = BSSim(xtTrain,est$res,est$phi,50)
  if ((i %% 100) == 0){
    print(i)
  }
  xtNewFor = BSSim(xtTrain,res,fit.mlp,num_back,h.long)
  holder[i,] = xtNewFor
}

# Calculate percentiles for each column
percentiles_per_column <- apply(holder, 2, function(column) {
  quantile(column, probs = c(0.025, 0.975))
})

# Transpose the result for better readability
percentiles_per_column <- t(percentiles_per_column)

# Print the result
print(percentiles_per_column)

xtNewForBS = colMeans(holder)

result = fit.mlp$fitted

# plotts.wge(c(xtTrain-est$res,xtNewForBS))
#plotts.wge(c(fit.mlp$fitted,xtNewForBS),ylim=c(min(fit.mlp$fitted)-.1,max(fit.mlp$fitted)+.1), )
#lines(seq(lf+1,lf+h.long,1),percentiles_per_column[,1],col = "blue")
#lines(seq(lf+1,lf+h.long,1),percentiles_per_column[,2],col = "blue")
#lines(seq(lf+1,lf+h.long,1),xt[(l-h.long+1):l],col = "red")

# Alex edited seq because lines were being plotted that didn't line up with the actual time
# plotts.wge(c(xtTrain-est$res,xtNewForBS))
plot(log.mhp[6:198], type = 'l', main = 'Multivariate MLP 1 Year Forecast with Confidence Intervals', 
     ylim=c(min(fit.mlp$fitted)-.1,max(fit.mlp$fitted)+.1))
lines(seq(190,193,1),percentiles_per_column[,1], lty = 3, col = "blue")
lines(seq(190,193,1),percentiles_per_column[,2], lty = 3, col = "blue")
lines(seq(190,193,1),xtNewForBS, col = "red")

# Zoomed In
plot(log.mhp[149:198], type = 'l', main = 'Multivariate MLP 1 Year Forecast with Confidence Intervals', 
     ylim=c(min(fit.mlp$fitted)-.1,max(fit.mlp$fitted)+.1))
lines(seq(47,50,1),percentiles_per_column[,1], lty = 3, col = "blue")
lines(seq(47,50,1),percentiles_per_column[,2], lty = 3, col = "blue")
lines(seq(47, 50,1),xtNewForBS,ylim=c(min(fit.mlp$fitted)-.1,max(fit.mlp$fitted)+.1),col = "red")

# Horizon = 20  
h.long = 20
l = length(xt)
xtTrain = xt[1:(l-h.long)]

# est = est.arma.wge(xtTrain,p = 1)
library(nnfor)
set.seed(30)
fit.mlp = mlp(ts(xt)) # mlp(ts(xtTrain), comb = 'median', xreg = ts.train.X.20, difforder = 1, sel.lag = TRUE)
###### Error in newdata[, object$model.list$variables] : subscript out of bounds ####
lf = length(fit.mlp$fitted)
res = xt[(l-lf+1):l]-fit.mlp$fitted

# Are residuals white noise?
plotts.sample.wge(res)
acf(res,lag.max=100)
ljung.wge(res,K=24) # p = 0.3804433, white noise
ljung.wge(res,K=48) # p = 0.6786805, white
hist(res) # look normally distributed

# BSSim = function(ts,res,phi,horizon) {
num_back = l - lf
BSSim = function(ts,res,fit,num_back,horizon) {
  origin = length(ts)
  ForHolder = numeric(horizon)
  for(i in (origin+1):(origin+horizon)){
    # ts[i] = ts[i-1]*phi + sample(res,1)
    # print(i)
    needed_to_predict = ts[(i-num_back):(i-1)]
    fore=forecast(fit,y=needed_to_predict,h=1)
    ts[i]=fore$mean+sample(res,1)
    ForHolder[(i-origin)]=ts[i]
  }
  return(ForHolder)
}

# BSSamples = 100000
BSSamples = 1000 # I'm worried 100k will take a really long time, so make it less

# holder = matrix(nrow = BSSamples,ncol = 50)
holder = matrix(nrow = BSSamples,ncol = h.long)

for(i in 1:BSSamples)
{
  # xtNewFor = BSSim(xtTrain,est$res,est$phi,50)
  if ((i %% 100) == 0){
    print(i)
  }
  xtNewFor = BSSim(xtTrain,res,fit.mlp,num_back,h.long)
  holder[i,] = xtNewFor
}

# Calculate percentiles for each column
percentiles_per_column <- apply(holder, 2, function(column) {
  quantile(column, probs = c(0.025, 0.975))
})

# Transpose the result for better readability
percentiles_per_column <- t(percentiles_per_column)

# Print the result
print(percentiles_per_column)

xtNewForBS = colMeans(holder)

# plotts.wge(c(xtTrain-est$res,xtNewForBS))
#plotts.wge(c(fit.mlp$fitted,xtNewForBS),ylim=c(min(fit.mlp$fitted)-.1,max(fit.mlp$fitted)+.1), )
#lines(seq(lf+1,lf+h.long,1),percentiles_per_column[,1],col = "blue")
#lines(seq(lf+1,lf+h.long,1),percentiles_per_column[,2],col = "blue")
#lines(seq(lf+1,lf+h.long,1),xt[(l-h.long+1):l],col = "red")

# Alex edited seq because lines were being plotted that didn't line up with the actual time
# plotts.wge(c(xtTrain-est$res,xtNewForBS))
plot(log.mhp[6:198], type = 'l', main = 'Multivariate MLP 5 Year Forecast with Confidence Intervals', 
     ylim=c(min(fit.mlp$fitted)-.1,max(fit.mlp$fitted)+.1))
lines(seq(174,193,1),percentiles_per_column[,1], lty = 3, col = "blue")
lines(seq(174,193,1),percentiles_per_column[,2], lty = 3, col = "blue")
lines(seq(174,193,1),xtNewForBS,col = "red")

# Zoomed In
plot(log.mhp[149:198], type = 'l', main = 'Multivariate MLP 5 Year Forecast with Confidence Intervals',
     ylim=c(min(fit.mlp$fitted)-.1,max(fit.mlp$fitted)+.1))
lines(seq(31,50,1),percentiles_per_column[,1], lty = 3, col = "blue")
lines(seq(31,50,1),percentiles_per_column[,2], lty = 3, col = "blue")
lines(seq(31,50,1),xtNewForBS, col = "red")

################################################################################

# make dataframe
ts.X = data.frame(hor = ts(fed_housing_data$Ownership_Rate), huc = ts(fed_housing_data$Housing_Units_Completed), snh = ts(fed_housing_data$Supply_New_Houses), hpi = ts(fed_housing_data$Housing_Price_Index))

# Make an mlp
xt = fed_housing_data$Median_Sales_Price
fit = mlp(ts(xt), comb = 'median', xreg = ts.X, difforder = 1, sel.lag = TRUE) 

# Make the residuals
l = length(xt)
lf = length(fit$fitted)
res = xt[(l-lf+1):l]-fit$fitted
xtTrain = xt[1:(l-h.long)]
plotts.wge(res)

# Make the function
num_back = l - lf
BSSim = function(xtTrain,res,fit,num_back,horizon) {
  origin = length(xtTrain)
  ForHolder = numeric(horizon)
  for(i in (origin+1):(origin+horizon)){
    # ts[i] = ts[i-1]*phi + sample(res,1)
    # print(i)
    needed_to_predict = xtTrain[(i-num_back):(i-1)]
    needed_to_predict_xreg = ts.X[(i-num_back):i,] # You need an additional one foreward, since you need those to do the prediction for h = 1
    # fore=forecast(fit,y=needed_to_predict,h=1)
    fore=forecast(fit,y=needed_to_predict,h=1,xreg=needed_to_predict_xreg)
    xtTrain[i]=fore$mean+sample(res,1)
    ForHolder[(i-origin)]=xtTrain[i]
  }
  return(ForHolder)
}

# Run the function
BSSamples = 1000 # I'm worried 100k will take a really long time, so make it less
holder = matrix(nrow = BSSamples,ncol = h.long)
for(i in 1:BSSamples)
{
  # xtNewFor = BSSim(xtTrain,est$res,est$phi,50)
  if ((i %% 100) == 0){
    print(i)
  }
  xtNewFor = BSSim(xtTrain,res,fit,num_back,h.long)
  holder[i,] = xtNewFor
}

# Calculate percentiles for each column
percentiles_per_column <- apply(holder, 2, function(column) {
  quantile(column, probs = c(0.025, 0.975))
})

# Transpose the result for better readability
percentiles_per_column <- t(percentiles_per_column)

# Print the result
print(percentiles_per_column)

xtNewForBS = colMeans(holder)

# plotts.wge(c(xtTrain-est$res,xtNewForBS))
# plotts.wge(c(fit$fitted,xtNewForBS),ylim=c(min(fit$fitted)-.1,max(fit$fitted)+.1)) # black is predictions
plotts.wge(c(fit$fitted,xtNewForBS),ylim=c(12.4,max(fit$fitted)+.1),xlim=c(150,length(c(fit$fitted,xtNewForBS)))) # black is predictions
lines(seq(lf+1,lf+h.long,1),percentiles_per_column[,1],col = "blue") # blue are confidence intervals
lines(seq(lf+1,lf+h.long,1),percentiles_per_column[,2],col = "blue")
lines(seq(lf+1,lf+h.long,1),xt[(l-h.long+1):l],col = "red") # Actual values
