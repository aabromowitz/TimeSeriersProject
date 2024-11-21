roll.win.sigplusnoise.ada <- function(series, horizon, linear = TRUE, method = "mle", freq=0, max.p=5) {
  
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