roll.win.sigplusnoise.ada.v1 <- function(series, horizon, linear = TRUE, method = "mle", freq=0, max.p=5) {
  
  # Determine number of RMSEs to calculate
  l = length(series)
  minSizeForSigPlusNoise = max(2,max.p+1)
  numRmses = l - horizon - minSizeForSigPlusNoise
  
  # Loop through all the windows, and calculate RMSE for each
  rmses = numeric(numRmses)
  for (iPred in 1:numRmses){
    # This gave me worse results for some reason
    # invisible(capture.output(f = fore.sigplusnoise.wge(series[1:(minSizeForSigPlusNoise-1+iPred)],linear = linear, max.p = max.p, n.ahead = horizon, lastn = FALSE, plot = FALSE)))
    
    # suppress output
    os <- Sys.info()[["sysname"]]
    if (os == "Windows"){
      sink("NUL")
    } else {
      sink("/dev/null")
    }
    
    # forecast
    f = fore.sigplusnoise.wge(series[1:(minSizeForSigPlusNoise-1+iPred)],linear = linear, max.p = max.p, n.ahead = horizon, lastn = FALSE, plot = FALSE)
    
    # suppression removed after fore.sigplusnoise.wge
    sink()
    
    # calculate RMSE
    rmse = sqrt(mean((series[(minSizeForSigPlusNoise+iPred):(minSizeForSigPlusNoise+iPred+horizon-1)]-f$f)^2))
    rmses[iPred]=rmse
  }
  
  # plot histogram of RMSEs
  hist(rmses)
  
  # Mean RMSE
  mean(rmses)
}

roll.win.rmse.linplusnoise.ada <- function(series, horizon, method = "mle", freq=0, max.p=5) {
  
  # suppress output
  os <- Sys.info()[["sysname"]]
  if (os == "Windows"){
    sink("NUL")
  } else {
    sink("/dev/null")
  }
  
  # Determine overall b0, b1, and phis for the series
  f = fore.sigplusnoise.wge(series,linear = TRUE, max.p = max.p, n.ahead = horizon, lastn = FALSE, plot = FALSE)
  b0 = f$b0hat
  b1 = f$b1hat
  phis = f$phi.z
  
  # suppression removed after fore.sigplusnoise.wge
  sink()
  
  # Determine number of RMSEs to calculate
  num_phi = length(phis) # Will need at least 5 before to forecast, if an AR(5)
  l = length(series)
  numRmses = l - horizon - num_phi + 1 # If you had a data set of size 17, an AR(5), and a horizon of 12, you'd only be able to calculate 1 RMSE
  
  # Loop through all the windows, and calculate RMSE for each
  rmses = numeric(numRmses)
  for (iFore in 1:numRmses){
    # Get the training and test time intervals
    tTrain = iFore:(iFore+num_phi-1)
    tTest = (iFore + num_phi):(iFore + num_phi + horizon - 1)
    
    # Calculate the residuals based on linear part
    resid = series[tTrain] - (b0 + b1*tTrain) # true - predicted
    
    # Forecast the residuals for the horizon
    fResid = fore.arma.wge(resid,phi=phis,n.ahead=horizon,lastn=FALSE,plot=FALSE)
    
    # Use the forecasted residuals and the line to forecast the predictions
    fSeries = b0 + b1*tTest + fResid$f
    
    # Calculate RMSE
    rmse = sqrt(mean((series[tTest]-fSeries)^2))
    rmses[iFore]=rmse
  }
  
  # For some reason, I have to remove the suppression again
  sink()
  
  # plot histogram of RMSEs
  hist(rmses)
  
  # Mean RMSE
  mean(rmses)
}