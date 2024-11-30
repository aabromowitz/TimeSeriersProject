# Standard RMD code for pulling in variables
library(tidyverse)
library(tswge)
library(vars)
library(lubridate)
library(nnfor)
library(vars)
library(caret)
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
huc$DATE = as.Date(huc$DATE)
huc$month <- month(huc$DATE)
head(huc)
huc_quarterly <- huc %>%
  filter(huc$month == 1 | huc$month == 4 | huc$month == 7 | huc$month == 10)
summary(huc_quarterly)
hor_1975 = hor[41:238,]
hor_1975$DATE <- as.Date(hor_1975$DATE)
huc_1975 = huc_quarterly[29:226,]
mhp_1975 = mhp[49:246,]
mhp_1975$DATE <- as.Date(mhp_1975$DATE)
snh_1975 = snh_quarterly[49:246,]
hpi$DATE <- as.Date(hpi$DATE)
fed_housing_data_NL = data.frame(Year_Quarter = mhp_1975$DATE, Ownership_Rate = hor_1975$RHORUSQ156N, Housing_Units_Completed = huc_1975$COMPUTSA, Supply_New_Houses = snh_1975$MSACSR, Housing_Price_Index = hpi$USSTHPI, Median_Sales_Price = mhp_1975$MSPUS)
fed_housing_data = data.frame(Year_Quarter = as.Date(mhp_1975$DATE), Ownership_Rate = hor_1975$RHORUSQ156N, Housing_Units_Completed = huc_1975$COMPUTSA, Supply_New_Houses = snh_1975$MSACSR, Housing_Price_Index = hpi$USSTHPI, Median_Sales_Price = log(mhp_1975$MSPUS))
train = fed_housing_data[1:168,]
test = fed_housing_data[169:198,]

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
# Univariate LSTM model

## 1. Install Required Libraries
# install.packages("keras")
# install.packages("tensorflow")
# library(keras)
# remove.packages("keras")
# remove.packages("keras3")
# remove.packages("tensorflow")
# install.packages("keras3") # or remotes::install_github("rstudio/keras")

# library(keras3) # I was getting some error with keras, but keras3 worked
library(keras)
library(tensorflow)
# tensorflow::install_tensorflow()
# keras3::install_keras()
# keras::install_keras()

## 2. Prepare Your Time Series Data
# Load and preprocess your data. Typically, you'll normalize it and create sequences for LSTM training.

# Example: Generate synthetic data
# time_series <- sin(1:100) + rnorm(100, sd = 0.1)
time_series <- fed_housing_data$Median_Sales_Price

# Normalize data
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
# time_series <- normalize(time_series)

# Create sequences
sequence_length <- 10
X <- array(0, dim = c(length(time_series) - sequence_length, sequence_length, 1))
y <- array(0, dim = c(length(time_series) - sequence_length, 1))

# X <- array_reshape(X, c(dim(X)[1], sequence_length, 1))
# y <- array_reshape(y, c(dim(y)[1], 1))
# y <- array_reshape(y, dim = c(dim(y)[1], 1))
# y <- array(y, dim = c(nrow(y), 1))
# y = array(y)
for (i in 1:(length(time_series) - sequence_length)) {
  X[i,,1] <- time_series[i:(i + sequence_length - 1)]
  y[i,1] <- time_series[i + sequence_length]
}
# y = array(y)

# X_train <- array(data = runif(1000), dim = c(100, 10, 10))  # Example 3D data
# y_train <- to_categorical(sample(1:10, 100, replace=TRUE))

## 3. Build the LSTM Model
# Define the LSTM architecture:
model <- keras_model_sequential() %>%
  layer_lstm(units = 50, input_shape = c(sequence_length, 1), return_sequences = TRUE) %>%
  time_distributed(layer_dense(units = 1))

model %>% compile(
  optimizer = "adam",
  loss = "mean_squared_error"
)

## 4. Train the Model
callback <- callback_early_stopping(monitor = "val_loss", patience = 5)
history <- model %>% fit(
  x = X,
  y = y,
  epochs = 50,
  batch_size = 16,
  validation_split = 0.2,
  callbacks = list(callback)
)

# Save off the dataframes as CSVs
write.csv(fed_housing_data_NL,'fed_housing_data_NL.csv')
write.csv(fed_housing_data,'fed_housing_data.csv')
write.csv(fed_housing_data_short,'fed_housing_data_short.csv')
write.csv(fed_housing_data_long,'fed_housing_data_long.csv')