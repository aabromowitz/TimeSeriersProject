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
xt = gen.sigplusnoise.wge(l,b0=b0,b1=b1,phi=phi,vara=vara)
plotts.wge(x)

xtTrain = xt[1:(l-h.long)]
fit.sig = fore.sigplusnoise.wge(xtTrain, linear = TRUE, method = 'mle', freq = 0, max.p = 6, n.ahead = h.long)
est = est.arma.wge(xtTrain,p = 1)

# BSSamples = 100000
BSSamples = 1000
holder = matrix(nrow = BSSamples,ncol = h.long)

for(i in 1:BSSamples)
{
  xtNewFor = BSSim(xtTrain,fit.sig$resid,est$phi,50)
  holder[i,] = xtNewFor
}