# Residual Bootstrap Confidence Intervals

BSSim = function(ts,res,phi,horizon) {
  origin = length(ts)
  ForHolder = numeric(horizon)
  for(i in (origin+1):(origin+horizon)){
    ts[i] = ts[i-1]*phi + sample(res,1)
    ForHolder[(i-origin)]=ts[i]
  }
  return(ForHolder)
}

xt = gen.arma.wge(100,phi = .9)

xtTrain = xt[1:95]

est = est.arma.wge(xtTrain,p = 1)

est$res

BSSamples = 100000

holder = matrix(nrow = BSSamples,ncol = 50)

for(i in 1:BSSamples)
{
  #new series
  #xtNew = (xtTrain - est$res) + sample(est$res,95,replace = TRUE)
  #Simulation Method
  xtNewFor = BSSim(xtTrain,est$res,est$phi,50)
  holder[i,] = xtNewFor
  
  #Forcast Method
  #xtNewFor = fore.arma.wge(xtNew,phi = est$phi, n.ahead = 50, lastn = FALSE)
  #xtNewFor$f
  #holder[i,] = xtNewFor$f
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

plotts.wge(c(xtTrain-est$res,xtNewForBS),ylim = c(-6,6))
lines(seq(96,145,1),percentiles_per_column[,1],col = "blue")
lines(seq(96,145,1),percentiles_per_column[,2],col = "blue")

################################################################################

# Seeing if this works for a linear signal plus noise
n = 100
b0 = 10.871
b1 = .011
vara = 0.0006929526
phi = c(.716,.367,0.137,-0.062,-.076,-.118)
# xt = gen.sigplusnoise.wge(l,b0=b0,b1=b1,phi=phi,vara=vara)
xt = gen.arma.wge(l,phi = .9)
plotts.wge(xt)

xtTrain = xt[1:(l-h.long)]
# fit.sig = fore.sigplusnoise.wge(xtTrain, linear = TRUE, method = 'mle', freq = 0, max.p = 6, n.ahead = h.long)
est = est.arma.wge(xtTrain,p = 1)

# BSSamples = 100000
BSSamples = 1000
holder = matrix(nrow = BSSamples,ncol = h.long)

for(i in 1:BSSamples)
{
  xtNewFor = BSSim(xtTrain,est$res,est$phi,h.long)
  # xtNewFor = BSSim(xtTrain,fit.sig$resid,fit.sig$phi.z,h.long)
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

plotts.wge(c(xtTrain-est$res,xtNewForBS))
lines(seq((l-h.long+1),l,1),percentiles_per_column[,1],col = "blue")
lines(seq((l-h.long+1),l,1),percentiles_per_column[,2],col = "blue")

################################################################################

# Trying what Dr Sadler did, but for an mlp model

# xt = gen.arma.wge(100,phi = .9)
file_path = "https://raw.githubusercontent.com/aabromowitz/TimeSeriersProject/refs/heads/main/MSPUS.csv"
mhp <- read.csv(file_path, header = TRUE)
mhp_1975 = mhp[49:246,]
xt = log(mhp_1975$MSPUS)

# xtTrain = xt[1:95]
h.long = 20
l = length(xt)
xtTrain = xt[1:(l-h.long)]

# est = est.arma.wge(xtTrain,p = 1)
library(nnfor)
set.seed(30)
fit.mlp = mlp(ts(xt))
lf = length(fit.mlp$fitted)
res = xt[(l-lf+1):l]-fit.mlp$fitted

# Are residuals white noise?
plotts.sample.wge(res)
acf(resid,lag.max=100)
ljung.wge(resid,K=24) # p = 0.3804433, white noise
ljung.wge(resid,K=48) # p = 0.6786805, white
hist(resid) # look normally distributed

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
BSSamples = 2000 # I'm worried 100k will take a really long time, so make it less

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
plotts.wge(c(fit$fitted,xtNewForBS),ylim=c(min(fit$fitted)-.1,max(fit$fitted)+.1))
lines(seq(lf+1,lf+h.long,1),percentiles_per_column[,1],col = "blue")
lines(seq(lf+1,lf+h.long,1),percentiles_per_column[,2],col = "blue")
lines(seq(lf+1,lf+h.long,1),xt[(l-h.long+1):l],col = "red")
