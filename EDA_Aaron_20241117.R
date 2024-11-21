# Pull in variable of interest, median housing prices
file_path = "https://raw.githubusercontent.com/aabromowitz/TimeSeriersProject/refs/heads/main/MSPUS.csv"
mhp <- read.csv(file_path, header = TRUE)
library(tswge)
plotts.sample.wge(mhp$MSPUS)
d.mph = artrans.wge(mhp$MSPUS,1)
acf(mhp$MSPUS,ylab='',main='ACF')
parzen.wge(mhp$MSPUS)

# Huh, that looks like the variance is increasing over time.
# But you can't just log it, because it's negative a lot of the time.
# So trying to log the original data
mhp.log = log(mhp$MSPUS)
plotts.sample.wge(mhp.log)
d.log = artrans.wge(mhp.log,1)

# Checking on ARMA after the differencing
aic5.ar.wge(d.log,p=0:60,type='aic',method='burg') # 4 is best, 20 is highest
aic5.ar.wge(d.log,p=0:20,type='bic',method='burg') # 1 is best, 4 is highest
aic5.wge(d.log,p=0:20,q=0:2,type='aic') # 11-1 is best, 15/2 is highest
aic5.wge(d.log,p=0:15,q=0:4,type='aic') # 11-1 best, 12/4 highest
aic5.wge(d.log,p=0:12,q=0:6,type='aic') # 11-1 best, 12/4 highest
aic5.wge(d.log,p=0:12,q=0:4,type='bic') # 1-2 best

# ts for the housing price data
plotts.wge(ts(mhp$MSPUS, frequency=4,start=c(1963,1)))
plot(ts(mhp$MSPUS/1000, frequency=4,start=c(1963,1)))
plot(ts(mhp$MSPUS/1000, frequency=4,start=c(1963,1)),xlab='Year',ylab='Housing price (in thousands of dollars)')
title(main='Median US Housing Price from 1963')

# Look at the home ownership rate
file_path = "https://raw.githubusercontent.com/aabromowitz/TimeSeriersProject/refs/heads/main/RHORUSQ156N.csv"
hor <- read.csv(file_path, header = TRUE)
hor <- hor$RHORUSQ156N
plotts.sample.wge(hor)
aic5.wge(hor)
d.hor = artrans.wge(hor,1)

# plots for home ownership rate
dev.off()
plot(ts(hor, frequency=4,start=c(1965,1)),xlab='Year',ylab='Percentage')
title(main='Home Ownership Rate from 1965')

# Pull in housing units completed
file_path = "https://raw.githubusercontent.com/aabromowitz/TimeSeriersProject/refs/heads/main/COMPUTSA.csv"
huc <- read.csv(file_path, header = TRUE)
huc <- huc$COMPUTSA

# plot housing units completed
plot(ts(huc, frequency=12,start=c(1968,1)),xlab='Year',ylab='')
title(main='Housing Units Completed from 1968')
acf(huc,ylab='',main='ACF')
x = parzen.wge(huc)

# Pull in supply of new houses
file_path = "https://raw.githubusercontent.com/aabromowitz/TimeSeriersProject/refs/heads/main/MSACSR.csv"
snh <- read.csv(file_path, header = TRUE)
snh <- snh$MSACSR

# plot supply of new houses
plot(ts(snh, frequency=12,start=c(1963,1)),xlab='Year',ylab='')
title(main='Supply of New Houses from 1963')
acf(snh,ylab='',main='ACF')
x = parzen.wge(snh)

# I wonder if the spectral density here actually has peaks
d.snh = artrans.wge(snh,1)

# Pull in house price index
file_path = "https://raw.githubusercontent.com/aabromowitz/TimeSeriersProject/refs/heads/main/USSTHPI.csv"
hpi <- read.csv(file_path, header = TRUE)
hpi <- hpi$USSTHPI

# plot house price index
dev.off()
plot(ts(hpi, frequency=4,start=c(1975,1)),xlab='Year',ylab='')
title(main='House Price Index from 1975')
acf(hpi,ylab='',main='ACF')
x = parzen.wge(hpi)
>>>>>>> d2153a0d831bcb01ef8bf14de5ee40393f6dbdc7
