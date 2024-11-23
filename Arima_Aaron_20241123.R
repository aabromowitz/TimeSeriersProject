# Pull in data
file_path = "https://raw.githubusercontent.com/aabromowitz/TimeSeriersProject/refs/heads/main/MSPUS.csv"
mhp <- read.csv(file_path, header = TRUE)
library(tswge)
log.mhp = log(mhp)
plotts.sample.wge(mhp)
plotts.wge(mhp)
plotts.sample.wge(log.mhp)
d.mhp = artrans.wge(mhp,1)
d.log.mhp = artrans.wge(log.mhp,1)
h.short = 4
h.long = 20

# dickey fuller
library(tseries)
adf.test(x)