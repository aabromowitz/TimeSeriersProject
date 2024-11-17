# Pull in variable of interest, median housing prices
file_path = "https://raw.githubusercontent.com/aabromowitz/TimeSeriersProject/refs/heads/main/MSPUS.csv"
mhp <- read.csv(file_path, header = TRUE)
library(tswge)
plotts.sample.wge(mhp$MSPUS)
d.mph = artrans.wge(mhp$MSPUS,1)

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

# Pull in all the different CSVs
