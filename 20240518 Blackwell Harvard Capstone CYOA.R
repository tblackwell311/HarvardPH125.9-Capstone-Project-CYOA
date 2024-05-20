# 20240518 Note: This version adds if(!require) statements to all packages and automatically downloads the datasets.



################################################################################
# PART 1 - DATA PREPARATION ####
################################################################################

#In this section we are going to merge, filter and format different data sources.

#########################
#Load necessary libraries
#########################

# Define a function to install and load a package
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

# List of packages to be used
packages <- c("dplyr", "ggplot2", "stats", "forecast", "gridExtra", 
              "lubridate", "caret", "knitr", "cowplot")

# Install and load each package
for (pkg in packages) {
  install_and_load(pkg)
}

#####################
# Load the sales file 
#####################
sales_file_path <- "https://raw.githubusercontent.com/tblackwell311/HarvardPH125.9-Capstone-Project-CYOA/main/sales.csv"

# Load the CSV file into sales_df
sales_df <- read.csv(sales_file_path)

# Print the first few rows of the dataframe
head(sales_df)

#############################
# Load the features_file_path
#############################
features_file_path <- "https://raw.githubusercontent.com/tblackwell311/HarvardPH125.9-Capstone-Project-CYOA/main/features.csv"

# Load the CSV file into features_df
features_df <- read.csv(features_file_path)

# Print the first few rows of the dataframe
head(features_df)

###########################
# Load the stores_file_path
###########################
stores_file_path <- "https://raw.githubusercontent.com/tblackwell311/HarvardPH125.9-Capstone-Project-CYOA/main/stores.csv"

# Load the CSV file into features_df
stores_df <- read.csv(stores_file_path)

#examine the structure of each df
str(sales_df)
str(features_df)
str(stores_df)

##############################
#Merge and finalize data_frame
##############################

# Merge sales and features dataframes on store and date columns
merged_df <- inner_join(sales_df, features_df, by = c("Store", "Date"))

# Merge the merged_df with stores_df on store column
final_df <- inner_join(merged_df, stores_df, by = "Store")

# Convert Date column to Date format (assuming mm/dd/yyyy format)
final_df$Date <- as.Date(final_df$Date, format = "%m/%d/%Y")

## ---- Examine structure of final_df
str(final_df)

rm("features_df", "merged_df", "sales_df", "stores_df")

################################################################################
# PART 2 - EXPLORATION ####
################################################################################
#In this section we are going to understand the data set, its structure and specificities.

# Check for missing values
sum(is.na(final_df))

# Histogram of sales
library(ggplot2)
ggplot(final_df, aes(x = Sales)) +
  geom_histogram(binwidth = 1000, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Sales", x = "Sales", y = "Frequency")

# Remove rows with zero sales
final_df <- final_df[final_df$Sales != 0, ]

# Print the updated number of rows
cat("Number of rows after removing 0 sales:", nrow(final_df), "\n")

# Histogram of sales
library(ggplot2)
ggplot(final_df, aes(x = Sales)) +
  geom_histogram(binwidth = 1000, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Sales after removing 0 sales", x = "Sales", y = "Frequency")

# Calculate the IQR (Interquartile Range) for Sales
Q1 <- quantile(final_df$Sales, 0.25)
Q3 <- quantile(final_df$Sales, 0.75)
IQR <- Q3 - Q1

# Define the upper and lower bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Remove outliers
final_df <- final_df[final_df$Sales >= lower_bound & final_df$Sales <= upper_bound, ]

# Print the updated number of rows
cat("Number of rows after removing outliers:", nrow(final_df), "\n")

# Histogram of sales
library(ggplot2)
ggplot(final_df, aes(x = Sales)) +
  geom_histogram(binwidth = 1000, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Sales after removing outliers", x = "Sales", y = "Frequency")

# Extract the 'Sales' column
sales_data <- final_df$Sales

# Generate theoretical quantiles for a normal distribution
theoretical_quantiles <- qnorm((1:length(sales_data) - 0.5) / length(sales_data))

# Calculate empirical quantiles from the actual data
empirical_quantiles <- sort(sales_data)

# Create the QQ plot
ggplot(data = NULL, aes(x = theoretical_quantiles, y = empirical_quantiles)) +
  geom_point(color = 'blue', alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = 'red', linetype = 'dashed') +
  labs(x = "Theoretical Quantiles", y = "Empirical Quantiles", title = "QQ Plot for Sales") +
  theme_minimal()

rm(theoretical_quantiles)
rm(empirical_quantiles)

# Calculate skewness and kurtosis
skew <- mean((sales_data - mean(sales_data))^3) / (sd(sales_data)^3)
kurt <- mean((sales_data - mean(sales_data))^4) / (sd(sales_data)^4)

rm(sales_data)

# Print skewness and kurtosis
cat("Skewness:", round(skew, 2), "\n")
cat("Kurtosis:", round(kurt, 2), "\n")

#Interpretation of Skewness value of 0.18 indicates a slight positive skew, meaning the data is slightly skewed to the right
#Interpretation of Kurtosis value of 2.7 falls between the typical range for a normal distribution which has a Kurtosis of 3.
#This suggests the Sales data is slightly flatter than a normal distrubtion.

################################################################################
# PART 3 - FEATURE ENGINEERING ####
################################################################################

#In this section, we are going to create informative features to capture the effects of all phenomenon that could impact sales

#######################
# SEASONALITY - OVERALL
#######################

# Aggregate sales data across all stores
total_sales <- aggregate(Sales ~ Date, data = final_df, sum)

# Create a time series object for total sales
total_ts <- ts(total_sales$Sales, start = c(2019, 12), frequency = 365)

rm(total_sales)

# Perform decomposition (choose additive or multiplicative)
decomposition_result <- decompose(total_ts, type = "additive")

# Extract components (seasonal, trend, random)
seasonal_component <- decomposition_result$seasonal
trend_component <- decomposition_result$trend
random_component <- decomposition_result$random

# Plot the components
plot(decomposition_result, xlab = "Year", ylab = "Sales")  

#########################
# SEASONALITY - BY REGION
#########################

###CALC DAILY AVG SALES BY REGION###

# Aggregate sales data by Region and Date
region_sales <- aggregate(Sales ~ Date + Region, data = final_df, sum)
head(region_sales)

# Calculate the unique store counts for each day by Region
store_counts_per_day <- aggregate(Sales ~ Date + Region, data = final_df, FUN = function(x) length(unique(x)))
head(store_counts_per_day)

# Merge the unique store counts back into the region_sales data frame
region_sales <- merge(region_sales, store_counts_per_day, by = c("Date", "Region"))
head(region_sales)

rm(store_counts_per_day)

# Calculate the daily average sales
region_sales$Daily_Avg_Sales <- region_sales$Sales.x / region_sales$Sales.y
head(region_sales)
tail(region_sales, n = 20)

###PLOT EACH REGION###

# Create a subset for each region
nm_sales <- subset(region_sales, Region == "New Mexico")
upstateNY_sales <- subset(region_sales, Region == "Upstate NY")
co_springs_sales <- subset(region_sales, Region == "Colorado Springs")
midwest_sales <- subset(region_sales, Region == "Midwest")

# Create line charts for each region
plot_nm <- ggplot(nm_sales, aes(x = Date, y = Daily_Avg_Sales)) +
  geom_line(color = "blue") +
  labs(title = "New Mexico", x = "Date", y = "Daily Avg Sales")

plot_upstateNY <- ggplot(upstateNY_sales, aes(x = Date, y = Daily_Avg_Sales)) +
  geom_line(color = "green") +
  labs(title = "Upstate NY", x = "Date", y = "Daily Avg Sales")

plot_co_springs <- ggplot(co_springs_sales, aes(x = Date, y = Daily_Avg_Sales)) +
  geom_line(color = "red") +
  labs(title = "Colorado Springs", x = "Date", y = "Daily Avg Sales")

plot_midwest <- ggplot(midwest_sales, aes(x = Date, y = Daily_Avg_Sales)) +
  geom_line(color = "purple") +
  labs(title = "Midwest", x = "Date", y = "Daily Avg Sales")

# Arrange the plots on the same page
grid.arrange(plot_nm, plot_upstateNY, plot_co_springs, plot_midwest, ncol = 2)

############################
# Feature Engineering - Date
############################

#Create Temporal Features from 'Date'
final_df$Month <- month(final_df$Date)
final_df$Day <- day(final_df$Date)
final_df$Quarter <- quarter(final_df$Date)
final_df$DayOfWeek <- wday(final_df$Date)

#############################
# Feature Engineering - Temps
#############################

# Calculate the mean of TAVG
mean_TAVG <- mean(final_df$TAVG, na.rm = TRUE)

# Calculate the difference from the mean for TAVG
final_df$TAVG_diff_from_mean <- final_df$TAVG - mean_TAVG

############################
# Feature Engineering - CPI
############################

# Calculate the mean of CPI
mean_CPI <- mean(final_df$CPI, na.rm = TRUE)

# Calculate the difference from the mean for CPI
final_df$CPI_diff_from_mean <- final_df$CPI - mean_CPI

####################################
# Feature Engineering - Unemployment
####################################

# Calculate the mean of UNEMPLOYMENT
mean_UNEMPLOYMENT <- mean(final_df$UNEMPLOYMENT, na.rm = TRUE)

# Calculate the difference from the mean for UNEMPLOYMENT
final_df$UNEMPLOYMENT_diff_from_mean <- final_df$UNEMPLOYMENT - mean_UNEMPLOYMENT

###############################
# Feature Engineering - Regions
###############################

# Perform one-hot encoding for the 'Region' variable
# Convert categorical variables into a binary matrix
one_hot_region <- model.matrix(~ Region - 1, data = final_df)

# Combine the one-hot encoded matrix with the original dataframe
final_df <- cbind(final_df, one_hot_region)

rm(one_hot_region)

###############################
# Feature Engineering - PAYDAYS
###############################

# Initialize Days_until_next_payday to 0
final_df$Days_until_next_payday <- 0

# Find indices where PAY equals 1
payday_indices <- which(final_df$PAY == 1)

# Loop through each observation
for (i in 1:length(final_df$PAY)) {
  # Find the next payday index
  next_payday_index <- payday_indices[which(payday_indices > i)[1]]
  
  # Calculate the number of days until the next payday
  if (!is.na(next_payday_index)) {
    final_df$Days_until_next_payday[i] <- next_payday_index - i
  }
}

################################
# Feature Engineering - Holidays
################################

# Initialize variables for days before and after a holiday
final_df$Before_holiday <- 0
final_df$After_holiday <- 0

# Find indices where HOLIDAY equals 1
holiday_indices <- which(final_df$HOLIDAY == 1)

# Create indicator variables for days before and after a holiday
for (i in holiday_indices) {
  final_df$Before_holiday[(i-3):(i-1)] <- 1  # Set values to 1 for three days before the holiday
  final_df$After_holiday[(i+1):(i+3)] <- 1   # Set values to 1 for three days after the holiday
}

str(final_df)

################################################################################
# PART 4 - CREATE TRAINING AND TESTING SETS ####
################################################################################

# Create train and test sets

# Set the seed for reproducibility
set.seed(123)

# Define split date (end of 2022)
split_date <- as.Date("2022-12-31")

# Split data into training and test sets
training_data <- filter(final_df, Date <= split_date)
testing_data <- filter(final_df, Date > split_date)

# Get first and last row of training data
first_row_train <- head(training_data, 1)
last_row_train <- tail(training_data, 1)

# Get first and last row of testing data
first_row_test <- head(testing_data, 1)
last_row_test <- tail(testing_data, 1)

# Print the results (optional)
print("First row of training data:")
print(first_row_train)

print("Last row of training data:")
print(last_row_train)

print("First row of testing data:")
print(first_row_test)

print("Last row of testing data:")
print(last_row_test)

str(training_data)
str(testing_data)

rm(final_df)

################################################################################
# PART 5 - TRADITIONAL TIME SERIES MODELS
################################################################################

################
# Naive Forecast
################
# Last Observed Value for All Future Periods

set.seed(123)

# Extract the 'Sales' column from the training and testing data
train_sales <- training_data$Sales
test_sales <- testing_data$Sales



# Create a vector to store the naive forecast values
naive_forecast <- rep(tail(train_sales, 1), length(test_sales))



# Plot historical sales and the naive forecast
plot(training_data$Date, training_data$Sales, type = "l", col = "blue", xlab = "Date", ylab = "Sales", main = "Historical Sales and Naive Forecast")
lines(testing_data$Date, naive_forecast, col = "red")
legend("topleft", legend = c("Historical Sales", "Naive Forecast"), col = c("blue", "red"), lty = 1)



# Calculate evaluation metrics for the naive forecast
rmse_naive <- sqrt(mean((test_sales - naive_forecast)^2))
mape_naive <- mean(abs((test_sales - naive_forecast) / test_sales)) * 100
forecast_bias <- mean(test_sales - naive_forecast)
fa_naive <- mean(1 - abs(test_sales - naive_forecast) / pmax(test_sales, naive_forecast)) * 100

summary_table <- data.frame(
  Evaluation_Metric = c("RMSE", "MAPE", "Forecast_Bias", "Forecast_Accuracy"),
  Naive = c(rmse_naive, mape_naive, forecast_bias, fa_naive))

kable(summary_table, caption = "Summary of Models", align = "c")



#########################
# LINEAR REGRESSION MODEL
#########################

set.seed(123)

# Create linear regression model
lm_model <- lm(Sales ~ MON + TUE + WED + THU + FRI + SAT + SUN + GAS + HOLIDAY + PAY + TAVG_diff_from_mean + CPI_diff_from_mean + UNEMPLOYMENT_diff_from_mean + Days_until_next_payday + + Before_holiday + After_holiday, data = training_data)

summary(lm_model)

# Predict sales using the linear regression model on training_data
lm_predicted_sales <- predict(lm_model, newdata = training_data)
head(lm_predicted_sales)

# Predict sales using the linear regression model on testing_data
lm_predicted_sales_testing <- predict(lm_model, newdata = testing_data)
head(lm_predicted_sales_testing)

# Plot observed sales vs. predicted sales
plot(training_data$Sales, lm_predicted_sales, 
     xlab = "Observed Sales", ylab = "Predicted Sales", 
     main = "Observed vs. Predicted Sales", 
     col = "blue", pch = 20)

# Add a diagonal line to visualize perfect predictions
abline(a = 0, b = 1, col = "red")

rmse_lm <- sqrt(mean((training_data$Sales - lm_predicted_sales)^2))
mape_lm <- mean(abs((training_data$Sales - lm_predicted_sales) / training_data$Sales)) * 100
forecast_bias_lm <- mean(training_data$Sales - lm_predicted_sales)
fa_lm <- mean(1 - abs(training_data$Sales - lm_predicted_sales) / pmax(training_data$Sales, lm_predicted_sales)) * 100

summary_table <- data.frame(
  Evaluation_Metric = c("RMSE", "MAPE", "Forecast Bias", "Forecast Accuracy"),
    Linear = c(rmse_lm, mape_lm, forecast_bias_lm, fa_lm))

kable(summary_table, caption = "Summary of Models", align = "c")

Naive = c(rmse_naive, mape_naive, forecast_bias, fa_naive),


################
# ARIMA Forecast
################

set.seed(123)

# Define the time series data
sales_ts <- ts(training_data$Sales, frequency = 363)  

# Create ACF plot
acf(sales_ts, main = "Autocorrelation Function (ACF)")
acf_values <- acf(sales_ts)$acf
acf_table <- data.frame(Lag = 0:(length(acf_values)-1), ACF = acf_values)
acf_table

#Interpreting the ACF plot: 
#The dotted lines (signficance threshholds) are close to zero, suggesting no linear relationship between observations at the corresponding lags.
#The autocorrelation coefficients are relatively constant with a slight decay as the lag increases suggests the presence of a moving average process in the time series data.
#The ACF results indicate a significant autocorrelation structure in the time series data, with high autocorrelation coefficients persisting over multiple lags. 

# Create PACF plot
pacf(sales_ts, main = "Partial Autocorrelation Function (PACF)")
pacf_values <- pacf(sales_ts)$acf
pacf_table <- data.frame(Lag = 0:(length(pacf_values) - 1), PACF = pacf_values)
pacf_table

#Interpreting the PACF plot:
#As the lag increases, PACF fluctuate around zero, indicating neither a positive or negative correlation
#Some coefficients become non-significant, close to zero at certain lags, indicating a weak
#In general we are seeing weak positive and negative autocorrelations for the lags
#This indicates that there are some residual patterns or dependencies in the time series data that are not explained by the immediate preceding observations.
#These residual patterns could be due to factors such as seasonality, trend or other underlying dynamics.

# Autoregressive Component (p)
# Integrated Component (d)
# Moving Average Component (q)
# Seasonal Autoregressive Component (P)
# Seasonal Integrated Component (D)
# Seasonal Moving Average Component (Q)
# Seasonal Frequency (m)
p <- 1
d <- 0
q <- 1
P <- 1
D <- 0
Q <- 1
m <- 30 #a seasonal pattern that repeats every 30 days

# Fit an ARIMA model to the training data
arima_model <- arima(training_data$Sales, order = c(p, d, q), seasonal = list(order = c(P, D, Q), period = m))
summary(arima_model)

# Generate predictions on the training data
arima_predictions <- predict(arima_model, n.ahead = length(training_data$Sales))

# Make testing forecasts using the ARIMA model
arima_forecast <- forecast(arima_model, h = length(testing_data$Sales))

# Extract the forecasted train values
arima_forecasted_train_values <- arima_predictions$pred
head(arima_forecasted_train_values)

# Extract the point forecasts from the forecast object
arima_forecast_values <- arima_forecast$mean



# Plot the original data and the ARIMA forecast
plot(training_data$Date, training_data$Sales, type = "l", col = "blue", xlab = "Date", ylab = "Sales", main = "ARIMA Forecast on Training Data")
lines(training_data$Date, arima_forecasted_train_values, col = "red")
legend("topright", legend = c("Training Data", "ARIMA Forecast"), col = c("blue", "red"), lty = 1, lwd = 2)

# Plot historical data and ARIMA forecast without x-axis
plot(arima_forecast, main = "Historical Sales and ARIMA Forecast", xlab = "", ylab = "Sales", xlim = c(min(training_data$Date), max(testing_data$Date)), xaxt = "n", ylim = c(0, 15000))
lines(training_data$Date, training_data$Sales, col = rgb(0, 0, 1, alpha = 0.5), type = "l", lwd = 2)
lines(testing_data$Date, testing_data$Sales, col = rgb(1, 0, 0, alpha = 0.5), type = "l", lwd = 2)
lines(arima_forecast$mean, col = "black", lwd = 2)
legend("topright", legend = c("Training Data", "Testing Data", "ARIMA Forecast"), col = c(rgb(0, 0, 1, alpha = 0.5), rgb(1, 0, 0, alpha = 0.5), "black"), lty = 1, lwd = 2)

# Add custom x-axis labels
axis(1, at = seq(min(training_data$Date), max(testing_data$Date), by = "month"), labels = seq(min(training_data$Date), max(testing_data$Date), by = "month"), las = 2)



# Calculate evaluation metrics for the ARIMA forecast
rmse_arima <- sqrt(mean((testing_data$Sales - arima_forecast_values)^2))
mape_arima <- mean(abs((testing_data$Sales - arima_forecast_values) / testing_data$Sales)) * 100
forecast_bias_arima <- mean(testing_data$Sales - arima_forecast_values)
fa_arima <- mean(1 - abs(testing_data$Sales - arima_forecast_values) / pmax(testing_data$Sales, arima_forecast_values)) * 100

summary_table <- data.frame(
  Evaluation_Metric = c("RMSE", "MAPE", "Forecast_Bias", "Forecast_Accuracy"),
  Naive = c(rmse_naive, mape_naive, forecast_bias, fa_naive),
  Linear = c(rmse_lm, mape_lm, forecast_bias_lm, fa_lm),
  ARIMA = c(rmse_arima, mape_arima, forecast_bias_arima, fa_arima))

kable(summary_table, caption = "Summary of Models", align = "c")



######################################################
# Seasonal-Trend Decomposition using Loess Forecasting
######################################################
# stlf() function, which can handle high-frequency time series data and provides seasonal forecasts using exponential smoothing with a Box-Cox transformation

set.seed(123)

forecast_horizon <- length(testing_data$Sales)

# Fit the STLF model to your time series data
stlf_model <- stlf(sales_ts, h = forecast_horizon)

# Generate forecasts using the STLF model for testing data
stlf_forecast_testing <- forecast(stlf_model, h = forecast_horizon)

# Extract forecasted sales values from the STLF forecast object
stlf_forecasted_sales <- stlf_forecast_testing$mean



# Plot actual sales and forecasted sales
plot(testing_data$Date, testing_data$Sales, type = "l", col = "blue", lwd = 2, ylim = c(0, max(max(testing_data$Sales), max(stlf_forecasted_sales))), xlab = "Date", ylab = "Sales", main = "STLF Actual vs. Forecasted Sales")
lines(testing_data$Date, stlf_forecasted_sales, col = "red", lwd = 2)
legend("topleft", legend = c("Actual Sales", "Forecasted Sales"), col = c("blue", "red"), lty = 1, lwd = 2)


# Calculate evaluation metrics
rmse_stlf <- sqrt(mean((testing_data$Sales - stlf_forecasted_sales)^2))
mape_stlf <- mean(abs((testing_data$Sales - stlf_forecasted_sales) / testing_data$Sales)) * 100
forecast_bias_stlf <- mean(testing_data$Sales - stlf_forecasted_sales)
fa_stlf <- mean(1 - abs(testing_data$Sales - stlf_forecasted_sales) / pmax(testing_data$Sales, stlf_forecasted_sales)) * 100

summary_table <- data.frame(
  Evaluation_Metric = c("RMSE", "MAPE", "Forecast Bias", "Forecast Accuracy"),
  Naive = c(rmse_naive, mape_naive, forecast_bias, fa_naive),
  Linear = c(rmse_lm, mape_lm, forecast_bias_lm, fa_lm),
  ARIMA = c(rmse_arima, mape_arima, forecast_bias_arima, fa_arima),
  STLF = c(rmse_stlf, mape_stlf, forecast_bias_stlf, fa_stlf))

kable(summary_table, caption = "Summary of Models", align = "c")


#####################
# Prophet Model - v4
#####################
# Source: https://mode.com/example-gallery/forecasting_prophet_r_cookbook
# Prophet always expects two columns in the input DataFrame: ds and y, containing the date and numeric values respectively.

library(reshape2)
library(prophet)

set.seed(123)

# Select Date and Sales columns from training_data
prophet_data <- training_data[, c("Date", "Sales")]

plot(prophet_data)
str(prophet_data)

# Parse date column
prophet_data <- mutate (
  prophet_data,
  Date = ymd(Date) # parse date column using lubridate ymd_hms function
)

#Mutate new columns that Prophet can read
prophet_data <- mutate (
  prophet_data,
  ds = Date,  # Create new ds column from date using mutate
  y = Sales   # Create new y column from value using mutate
)

str(prophet_data)

#Warning! I have not repurposed the date column to be used as the index of the dataframe
# https://mode.com/example-gallery/forecasting_prophet_r_cookbook

# Plot Date and y columns
plot(prophet_data$ds, prophet_data$y, type = "l", xlab = "Date", ylab = "Sales")

# The BoxCox.lambda() function will choose a value of lambda
lam = BoxCox.lambda(prophet_data$Sales, method = "loglik")
lam
prophet_data$y = BoxCox(prophet_data$Sales, lam)
df.m <- melt(df, measure.vars=c("value", "y"))

str(prophet_data)

# Plot for Date & Sales
plot_sales <- ggplot(prophet_data, aes(x = Date, y = Sales)) +
  geom_line() +
  labs(title = "Date & Sales",
       x = "Date",
       y = "Sales")

# Plot for ds & y
plot_y <- ggplot(prophet_data, aes(x = ds, y = y)) +
  geom_line() +
  labs(title = "ds & y",
       x = "ds",
       y = "y")

# Arrange plots vertically
plot_grid(plot_sales, plot_y, nrow = 2)

#Remove date and sales columns
prophet_data <- subset(prophet_data, select = c(ds, y))

str(prophet_data)

# Fit the Prophet model - run time <1min
  timing <- system.time({
  m <- prophet(prophet_data, daily.seasonality = TRUE)
  })
  timing
  
# Number of rows in the training data
  num_training_rows <- nrow(training_data)
  num_training_rows

#Make a future data frame  
prophet_future <- make_future_dataframe(m, periods = 1460)

#Run Prophet forecast - Warning! 28 minute run-time, if you run training length
timing <- system.time({
  prophet_forecast <- predict(m, prophet_future)
})
timing

#Plot Prophet forecast
plot(m, prophet_forecast)

#Visualize individual forecast components
prophet_plot_components(m, prophet_forecast) # remove hour of day manually

## Inverse Box-Cox Transformation ##
inverse_forecast <- prophet_forecast
inverse_forecast$yhat_untransformed = InvBoxCox(prophet_forecast$yhat, lam)

# Extract the point forecasts from the forecast object
prophet_forecast_values <- inverse_forecast$yhat_untransformed
head(prophet_forecast_values)


# Create a data frame for both dates and forecasted values
prophet_forecast_df <- data.frame(
  ds = prophet_future,
  yhat = prophet_forecast_values
)

# Add Prophet forecast to the plot
plot(prophet_forecast_df$ds, prophet_forecast_df$yhat, type = "l", col = "blue", xlab = "Date", ylab = "Sales", main = "Prophet Forecast")

length(prophet_forecast_values)
#2913
trimmed_testing_data <- tail(testing_data$Sales, 2913)
length(trimmed_testing_data)

# Calculate evaluation metrics for the Prophet forecast
rmse_prophet <- sqrt(mean((trimmed_testing_data - prophet_forecast_values)^2))
mape_prophet <- mean(abs((trimmed_testing_data - prophet_forecast_values) / trimmed_testing_data)) * 100
forecast_bias_prophet <- mean(trimmed_testing_data - prophet_forecast_values)
fa_prophet <- mean(1 - abs(trimmed_testing_data - prophet_forecast_values) / pmax(trimmed_testing_data, prophet_forecast_values)) * 100

summary_table <- data.frame(
  Evaluation_Metric = c("RMSE", "MAPE", "Forecast Bias", "Forecast Accuracy"),
  Naive = c(rmse_naive, mape_naive, forecast_bias, fa_naive),
  Linear = c(rmse_lm, mape_lm, forecast_bias_lm, fa_lm),
  ARIMA = c(rmse_arima, mape_arima, forecast_bias_arima, fa_arima),
  STLF = c(rmse_stlf, mape_stlf, forecast_bias_stlf, fa_stlf),
  PROPHET = c(rmse_prophet, mape_prophet, forecast_bias_prophet, fa_prophet))

kable(summary_table, caption = "Summary of Models", align = "c")


################################################################################
# PART 6 - MACHINE LEARNING MODELS
################################################################################

#####################
# RANDOM FOREST MODEL
#####################

# Install and load required package
install.packages("randomForest")
library(randomForest)

# Set a seed for reproducibility
set.seed(123)

# Define predictors and response variable
predictors <- c("TAVG", "GAS", "CPI", "UNEMPLOYMENT", "HOLIDAY", "PAY", "MON", "TUE", "WED", "THU", 
                "FRI", "SAT", "SUN", "Region", "City", "State", "Zip.Code", "Month", "Day", 
                "Quarter", "DayOfWeek", "TAVG_diff_from_mean", "CPI_diff_from_mean", 
                "UNEMPLOYMENT_diff_from_mean", "Days_until_next_payday", "Before_holiday", 
                "After_holiday")
response <- "Sales"

# Convert categorical variables to factors
training_data$Region <- as.factor(training_data$Region)
training_data$City <- as.factor(training_data$City)
training_data$State <- as.factor(training_data$State)

# Train the Random Forest model - Warning! 112 minute run time
timing <- system.time({
rf_model <- randomForest(Sales ~ ., data = training_data[, c(predictors, response)])
})
timing

# View model summary
print(rf_model)

# View variable importance
print(importance(rf_model))

# Subset the testing_data_subset to include only the predictors used during training
testing_data_subset <- testing_data[, predictors]
colnames(testing_data_subset)
predictors

# Ensure factor levels of categorical variables in testing data match those in training data
testing_data_subset$Region <- factor(testing_data_subset$Region, levels = levels(training_data$Region))
testing_data_subset$City <- factor(testing_data_subset$City, levels = levels(training_data$City))
testing_data_subset$State <- factor(testing_data_subset$State, levels = levels(training_data$State))

# Make predictions using the Random Forest model on the subsetted testing data
timing <- system.time({
rf_predictions <- predict(rf_model, newdata = testing_data_subset)
})
timing



# Create a dates vector for the first 365 predictions
dates_vector <- seq(as.Date("2023-01-01"), by = "day", length.out = 365)

# Simple plot of first 365 rf_predictions with no default x-axis
plot(rf_predictions[1:365], type = "l", xlab = "2023", ylab = "Predicted Sales", main = "Random Forest Predictions", xaxt = "n")

# Define the subset of dates to display on the x-axis (e.g., every 30 days)
axis_dates <- dates_vector[seq(1, length(dates_vector), by = 30)]

# Add dates to the x-axis
axis(1, at = seq(1, 365, by = 30), labels = format(axis_dates, "%b %d"), las = 2)



######################################
# EVALUATION METRICS - RANDOM FOREST
#####################################

###Calculate RMSE###

# Ensure both actual values and predicted values are numeric
actual_values <- as.numeric(testing_data$Sales)
rf_predictions <- as.numeric(rf_predictions)

trim_length <- 18256

trimmed_actual_values <- actual_values[1:trim_length]
trimmed_rf_predictions <- rf_predictions[1:trim_length]

# 1. Compute residuals
residuals <- trimmed_rf_predictions - trimmed_actual_values
residuals

# 2. Square the residuals
squared_errors <- residuals^2
squared_errors
any(is.na(squared_errors))

# 3. Calculate the mean squared error (MSE)
mse <- mean(squared_errors, na.rm = TRUE)
mse

# 4. Calculate the Root Mean Squared Error (RMSE)
rmse_rf <- sqrt(mse)
print(rmse_rf)

###Calculate MAPE###

# 1 Calculate absolute errors
absolute_errors <- abs(trimmed_rf_predictions - trimmed_actual_values)
absolute_errors

#2 Calculate percentage errors
percentage_errors <- (absolute_errors / trimmed_actual_values) * 100
percentage_errors

# 3 Handle special cases
percentage_errors[actual_values == 0] <- 0
percentage_errors

any(is.na(percentage_errors))
percentage_errors <- as.numeric(percentage_errors)

# 4 Calculate MAPE
mape_rf <- mean(percentage_errors, na.rm = TRUE)
mape_rf

###Calculate Forecast Bias###

#1 Calculate differences
rf_differences <- trimmed_actual_values - trimmed_rf_predictions
rf_differences

#2 Remove NA values
rf_differences <- rf_differences[!is.na(rf_differences)]
rf_differences

#3 Calculate the mean
forecast_bias_rf <- mean(rf_differences)
forecast_bias_rf

###Calculate Forecast Accuracy###

#1 Calculate absolute errors
rf_absolute_errors <- abs(trimmed_actual_values - trimmed_rf_predictions)
rf_absolute_errors

#2 Calculate percentage errors
rf_percentage_errors <- (rf_absolute_errors / trimmed_actual_values) * 100
rf_percentage_errors

# 3 Remove NA Values
rf_percentage_errors <- rf_percentage_errors[!is.na(rf_percentage_errors)]
rf_percentage_errors

# Calculate Forecast Accuracy
threshold <- 5
accurate_predictions <- rf_percentage_errors <= threshold
forecast_accuracy_rf <- mean(accurate_predictions) * 100
forecast_accuracy_rf

summary_table <- data.frame(
  Evaluation_Metric = c("RMSE", "MAPE", "Forecast Bias", "Forecast Accuracy"),
  Naive = c(rmse_naive, mape_naive, forecast_bias, fa_naive),
  Linear = c(rmse_lm, mape_lm, forecast_bias_lm, fa_lm),
  ARIMA = c(rmse_arima, mape_arima, forecast_bias_arima, fa_arima),
  STLF = c(rmse_stlf, mape_stlf, forecast_bias_stlf, fa_stlf),
  PROPHET = c(rmse_prophet, mape_prophet, forecast_bias_prophet, fa_prophet),
  RANDOM_FOREST = c(rmse_rf, mape_rf, forecast_bias_rf, forecast_accuracy_rf))

kable(summary_table, caption = "Summary of Models", align = "c")

# while the Random Forest model appears to provide reasonably accurate predictions (as indicated by the RMSE and MAPE), there is room for improvement in terms of reducing forecast bias and increasing forecast accuracy.



##################################
# GRADIENT BOOSTING MACHINES MODEL
##################################

install.packages("gbm")
library(gbm)

set.seed(123)

# Define predictors and response variable
predictors <- c("TAVG", "GAS", "CPI", "UNEMPLOYMENT", "HOLIDAY", "PAY", "MON", "TUE", "WED", "THU", 
                "FRI", "SAT", "SUN", "Region", "City", "State", "Zip.Code", "Month", "Day", 
                "Quarter", "DayOfWeek", "TAVG_diff_from_mean", "CPI_diff_from_mean", 
                "UNEMPLOYMENT_diff_from_mean", "Days_until_next_payday", "Before_holiday", 
                "After_holiday")
response <- "Sales"

# Convert categorical variables to factors
training_data$Region <- as.factor(training_data$Region)
training_data$City <- as.factor(training_data$City)
training_data$State <- as.factor(training_data$State)

# Subset the testing_data_subset to include only the predictors used during training
testing_data_subset <- testing_data[, predictors]
colnames(testing_data_subset)
predictors

# Ensure factor levels of categorical variables in testing data match those in training data
testing_data_subset$Region <- factor(testing_data_subset$Region, levels = levels(training_data$Region))
testing_data_subset$City <- factor(testing_data_subset$City, levels = levels(training_data$City))
testing_data_subset$State <- factor(testing_data_subset$State, levels = levels(training_data$State))

any(is.na(testing_data_subset))



##### Train the GBM model with training_data #####


timing <- system.time({ 
  gbm_model <- gbm(
    formula = as.formula(paste(response, "~", paste(predictors, collapse = " + "))),
    data = training_data,
    distribution = "gaussian",
    n.trees = 200,
    interaction.depth = 5,
    shrinkage = 0.01,
    bag.fraction = 0.5,
    cv.folds = 5,  # Optional: perform cross-validation
    verbose = TRUE
  )
})
timing

##### Validate the GBM model with training_data #####
gbm_training_predictions <- predict(gbm_model, newdata = training_data)



##### Validate the GBM model with testing_data #####
gbm_predictions <- predict(gbm_model, newdata = testing_data_subset)


# Plot predicted vs. actual for training data
plot(training_data$Sales, gbm_training_predictions, 
     xlab = "Actual", ylab = "Predicted",
     main = "GBM Model: Training Data",
     col = "blue")
abline(0, 1, col = "red")

# Plot predicted vs. actual for testing data
plot(testing_data$Sales, gbm_predictions, 
     xlab = "Actual", ylab = "Predicted",
     main = "GBM Model: Testing Data",
     col = "green")
abline(0, 1, col = "red")


################################
# EVALUATION METRICS - GBM MODEL
################################

###Calculate RMSE###

# Ensure both actual values and predicted values are numeric
actual_values <- as.numeric(testing_data$Sales)
gbm_predictions <- as.numeric(gbm_predictions)

trim_length <- 18256

trimmed_actual_values <- actual_values[1:trim_length]
trimmed_gbm_predictions <- gbm_predictions[1:trim_length]

# 1. Compute residuals
residuals <- trimmed_gbm_predictions - trimmed_actual_values

# 2. Square the residuals
squared_errors <- residuals^2

# 3. Calculate the mean squared error (MSE)
mse <- mean(squared_errors, na.rm = TRUE)
mse

# 4. Calculate the Root Mean Squared Error (RMSE)
rmse_gbm <- sqrt(mse)
print(rmse_gbm)

###Calculate MAPE###

# 1 Calculate absolute errors
absolute_errors <- abs(trimmed_gbm_predictions - trimmed_actual_values)

#2 Calculate percentage errors
percentage_errors <- (absolute_errors / trimmed_actual_values) * 100

# 3 Handle special cases
percentage_errors[actual_values == 0] <- 0

any(is.na(percentage_errors))
percentage_errors <- as.numeric(percentage_errors)

# 4 Calculate MAPE
mape_gbm <- mean(percentage_errors, na.rm = TRUE)
mape_gbm

###Calculate Forecast Bias###

#1 Calculate differences
gbm_differences <- trimmed_actual_values - trimmed_gbm_predictions


#2 Remove NA values
gbm_differences <- gbm_differences[!is.na(gbm_differences)]

#3 Calculate the mean
forecast_bias_gbm <- mean(gbm_differences)
forecast_bias_gbm

###Calculate Forecast Accuracy###

#1 Calculate absolute errors
gbm_absolute_errors <- abs(trimmed_actual_values - trimmed_gbm_predictions)


#2 Calculate percentage errors
gbm_percentage_errors <- (gbm_absolute_errors / trimmed_actual_values) * 100


# 3 Remove NA Values
gbm_percentage_errors <- gbm_percentage_errors[!is.na(gbm_percentage_errors)]


# Calculate Forecast Accuracy
threshold <- 5
accurate_predictions <- gbm_percentage_errors <= threshold
fa_gbm <- mean(accurate_predictions) * 100
fa_gbm



summary_table <- data.frame(
  Evaluation_Metric = c("RMSE", "MAPE", "Forecast Bias", "Forecast Accuracy"),
  Naive = c(rmse_naive, mape_naive, forecast_bias, fa_naive),
  Linear = c(rmse_lm, mape_lm, forecast_bias_lm, fa_lm),
  ARIMA = c(rmse_arima, mape_arima, forecast_bias_arima, fa_arima),
  STLF = c(rmse_stlf, mape_stlf, forecast_bias_stlf, fa_stlf),
  PROPHET = c(rmse_prophet, mape_prophet, forecast_bias_prophet, fa_prophet),
  RANDOM_FOREST = c(rmse_rf, mape_rf, forecast_bias_rf, forecast_accuracy_rf),
  GBM = c(rmse_gbm, mape_gbm, forecast_bias_gbm, fa_gbm))


kable(summary_table, caption = "Summary of Models", align = "c")





################################################################################
# ENSEMBLE STACKING
################################################################################

# Based on the results of all models, I have chosen to stack the Linear, ARIMA and GBM models.
# These will be known as the Base Models
# Side note: the Prophet model evaluation metrics are better than the ARIMA model
# However the Prophet model cannot provide the matching output needed to evaluate versus testing_data.

#######################
# 1.  TRAIN BASE MODELS
#######################
# We have already trained each of the base models on the training data.
# We have also used cross-validation to tune hyperparameters to optimize their performance.

##########################
# 2.  GENERATE PREDICTIONS
##########################
# Make predictions on the training data using each base model.  
# The predictions will serve as the input for the meta-model.
# The ensures the meta-model learns from the base model's predictions before applying it to unseen data.

# Training data length: 62,503

# Linear predictions:  lm_predicted_sales (n = 62,503)
# ARIMA predictions: arima_forecasted_train_values (n = 18256)
# GBM predictions: gbm_training_predictions (n = 62503)

##############################
# 3.  PREPARE META-MODEL INPUT
##############################

# Combine predictions into a single dataset

# Combine predictions from base models into a dataframe
meta_features <- data.frame(
  "Prediction_Model1" = lm_predicted_sales,
  "Prediction_Model2" = arima_forecasted_train_values,
  "Prediction_Model3" = gbm_training_predictions
)
head(meta_features)


# Create meta-model input dataset
meta_input <- data.frame(
  "true_target" = training_data$Sales,
  meta_features,
  "Date" = training_data$Date
)
head(meta_input)


# Reorder columns to make Date the first column
meta_input <- meta_input[, c("Date", names(meta_input)[-which(names(meta_input) == "Date")])]
head(meta_input)


########################################
# 3a.  FEATURE ENGINEERING ON META_INPUT
########################################

####################################################################################
# RESIDUALS FEATURES - EXCLUDE BECAUSE THEY HELP PERFECTLY PREDICT THE TRUE_TARGET
####################################################################################

# Compute residuals for each base model (difference between true target values and predictions)
# This captures the model error and provides additional information to learn from
#residuals_model1 <- meta_input$true_target - meta_input$Prediction_Model1
#residuals_model2 <- meta_input$true_target - meta_input$Prediction_Model2
#residuals_model3 <- meta_input$true_target - meta_input$Prediction_Model3

# Create a dataframe with residuals
#residuals_df <- data.frame(
  #"Residuals_Model1" = residuals_model1,
  #"Residuals_Model2" = residuals_model2,
  #"Residuals_Model3" = residuals_model3
#)

# Bind residuals to the meta_input dataset
#meta_input <- cbind(meta_input, residuals_df)

# View the updated meta_input dataset
#head(meta_input)

##################
# RANKING FEATURES
##################
#Using ranking features helps meta-model identify which models consistently perform better or worse across different instances.

# Compute ranking features for each base model
rank_model1 <- rank(meta_input$Prediction_Model1)
rank_model2 <- rank(meta_input$Prediction_Model2)
rank_model3 <- rank(meta_input$Prediction_Model3)

# Create a dataframe with ranking features
ranking_df <- data.frame(
  "Rank_Model1" = rank_model1,
  "Rank_Model2" = rank_model2,
  "Rank_Model3" = rank_model3
)

# Bind ranking features to the meta_input dataset
meta_input <- cbind(meta_input, ranking_df)

# View the updated meta_input dataset
head(meta_input)

#####################
# DIFFERENCE FEATURES
#####################
# The absolute differences or percent differences between pairs of predictions from the base models.
# This can help meta-model correct for bias or errors in individual predictions

# Compute difference features between pairs of predictions
difference_model1_model2 <- abs(meta_input$Prediction_Model1 - meta_input$Prediction_Model2)
difference_model1_model3 <- abs(meta_input$Prediction_Model1 - meta_input$Prediction_Model3)
difference_model2_model3 <- abs(meta_input$Prediction_Model2 - meta_input$Prediction_Model3)

# Create a dataframe with difference features
difference_df <- data.frame(
  "Difference_Model1_Model2" = difference_model1_model2,
  "Difference_Model1_Model3" = difference_model1_model3,
  "Difference_Model2_Model3" = difference_model2_model3
)

# Bind difference features to the meta_input dataset
meta_input <- cbind(meta_input, difference_df)

# View the updated meta_input dataset
head(meta_input)

#####################
# GEOMETRIC MEAN
#####################
# The geometric mean tends to give less weight to extreme values and can help mitigate the influence of outliers

# Compute geometric mean of predictions for each row
geometric_mean_predictions <- apply(meta_input[, c("Prediction_Model1", "Prediction_Model2", "Prediction_Model3")], 1, function(row) {
  exp(mean(log(row), na.rm = TRUE))
})

# Add geometric mean as a new feature to the meta_input dataset
meta_input$Geometric_Mean_Predictions <- geometric_mean_predictions

# View the updated meta_input dataset
head(meta_input)

####################
# STANDARD DEVIATION
####################

# Compute standard deviation of predictions for each row
standard_deviation_predictions <- apply(meta_input[, c("Prediction_Model1", "Prediction_Model2", "Prediction_Model3")], 1, sd)

# Add standard deviation as a new feature to the meta_input dataset
meta_input$Standard_Deviation_Predictions <- standard_deviation_predictions

# View the updated meta_input dataset
head(meta_input)

########
# MEDIAN
########

# Compute median of predictions for each row
median_predictions <- apply(meta_input[, c("Prediction_Model1", "Prediction_Model2", "Prediction_Model3")], 1, median, na.rm = TRUE)

# Add median as a new feature to the meta_input dataset
meta_input$Median_Predictions <- median_predictions

# View the updated meta_input dataset
head(meta_input)


#####################
# TIME-BASED FEATURES
#####################

# Extract time-based features
meta_input$HOLIDAY <- as.numeric(training_data$HOLIDAY)
meta_input$PAY <- as.numeric(training_data$PAY)
meta_input$MON <- as.numeric(weekdays(training_data$Date) == "Monday")
meta_input$TUE <- as.numeric(weekdays(training_data$Date) == "Tuesday")
meta_input$WED <- as.numeric(weekdays(training_data$Date) == "Wednesday")
meta_input$THU <- as.numeric(weekdays(training_data$Date) == "Thursday")
meta_input$FRI <- as.numeric(weekdays(training_data$Date) == "Friday")
meta_input$SAT <- as.numeric(weekdays(training_data$Date) == "Saturday")
meta_input$SUN <- as.numeric(weekdays(training_data$Date) == "Sunday")
meta_input$Month <- month(training_data$Date)
meta_input$Day <- day(training_data$Date)
meta_input$Quarter <- quarter(training_data$Date)
meta_input$DayOfWeek <- wday(training_data$Date)
meta_input$DayOfWeek <- as.numeric(meta_input$DayOfWeek)
meta_input$Days_until_next_payday <- training_data$Days_until_next_payday
meta_input$Before_holiday <- training_data$Before_holiday
meta_input$After_holiday <- training_data$After_holiday

# View the updated meta_input dataset
head(meta_input)


###########################################
# 4a.  TRAIN META-MODEL - LINEAR REGRESSION
###########################################

set.seed(123)

# Define the formula for the linear regression model
linear_formula <- as.formula("true_target ~ . - true_target")

# Fit the linear regression model
lm_meta_model <- lm(linear_formula, data = meta_input)

summary(lm_meta_model)


# Extract predicted sales from the linear regression meta-model
meta_lm_predicted_sales <- predict(lm_meta_model, newdata = meta_input)
head(meta_lm_predicted_sales)


# Plot the predicted sales and true target values
ggplot(meta_input, aes(x = true_target, y = meta_lm_predicted_sales)) +
  geom_point(color = "blue", alpha = 0.5) +  # Scatter plot of predicted sales
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # 45-degree line
  labs(x = "True Target (Actual Sales)", y = "Predicted Sales") +  # Axis labels
  ggtitle("Stacked Linear Model - Predicted Sales vs. True Target") +  # Title
  theme_minimal()  # Theme for the plot



rmse_meta_lm <- sqrt(mean((meta_input$true_target - meta_lm_predicted_sales)^2))
mape_meta_lm <- mean(abs((meta_input$true_target - meta_lm_predicted_sales) / meta_input$true_target)) * 100
forecast_bias_meta_lm <- mean(meta_input$true_target - meta_lm_predicted_sales)
fa_meta_lm <- mean(1 - abs(meta_input$true_target - meta_lm_predicted_sales) / pmax(meta_input$true_target, meta_lm_predicted_sales)) * 100

summary_table <- data.frame(
  Stacked_Linear = c(rmse_meta_lm, mape_meta_lm, forecast_bias_meta_lm, fa_meta_lm))

kable(summary_table, caption = "Summary of Models", align = "c")


# NAs Examination Results:
# SUN + DayOfWeek are throwing a singularity error, but upon thorough examination, the data structure of these variables is correct.
# These variables may not provide much unique information or predictive power for forecasting, leading to numerical instability in the regression coefficients.
# Or the regression model might be overfitting the data, leading to numerical issues during estimation.


#############################
# 4b.  TRAIN META-MODEL - GBM
#############################

set.seed(123)

# Define predictors (excluding true_target and Date if present)
predictors <- setdiff(names(meta_input), c("true_target", "Date"))

# Define response variable
response <- "true_target"

# Train the GBM model
stacked_gbm_model <- gbm(
  formula = as.formula(paste(response, "~", paste(predictors, collapse = " + "))),
  data = meta_input,
  distribution = "gaussian",
  n.trees = 500,
  interaction.depth = 9,
  shrinkage = 0.01,
  bag.fraction = 0.9,
  cv.folds = 10,  # Optional: perform cross-validation
  verbose = TRUE
)

# Print the summary of the GBM model
summary(stacked_gbm_model)


# Make predictions on the training data
stacked_gbm_predictions <- predict(stacked_gbm_model, newdata = meta_input, type = "response")
head(stacked_gbm_predictions)


# Plot the predicted values versus the true target values
ggplot(meta_input, aes(x = true_target, y = stacked_gbm_predictions)) +
  geom_point(color = "blue", alpha = 0.5) +  # Scatter plot of predicted values
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # 45-degree line
  labs(x = "True Target (Actual Sales)", y = "Predicted Sales") +  # Axis labels
  ggtitle("Stacked GBM Model - Predicted Sales vs. True Target") +  # Title
  theme_minimal()  # Theme for the plot



rmse_stacked_gbm <- sqrt(mean((meta_input$true_target - stacked_gbm_predictions)^2))
mape_stacked_gbm <- mean(abs((meta_input$true_target - stacked_gbm_predictions) / meta_input$true_target)) * 100
forecast_bias_stacked_gbm <- mean(meta_input$true_target - stacked_gbm_predictions)
fa_stacked_gbm <- mean(1 - abs(meta_input$true_target - stacked_gbm_predictions) / pmax(meta_input$true_target, stacked_gbm_predictions)) * 100

summary_table <- data.frame(
  Stacked_Linear = c(rmse_meta_lm, mape_meta_lm, forecast_bias_meta_lm, fa_meta_lm),
  Stacked_GBM = c(rmse_stacked_gbm, mape_stacked_gbm, forecast_bias_stacked_gbm, fa_stacked_gbm))

kable(summary_table, caption = "Summary of Models", align = "c")




#######################################
# 4c.  TRAIN META-MODEL - RANDOM FOREST
#######################################

# Define the formula for the random forest model
stacked_rf_formula <- as.formula("true_target ~ .")

# Train the Random Forest model - Warning!  67 min run time!!!!
timing <- system.time({
stacked_rf_model <- randomForest(stacked_rf_formula, data = meta_input)
})
timing

# Print the summary of the Random Forest model
print(stacked_rf_model)

# Generate predictions
stacked_rf_predictions <- predict(stacked_rf_model, newdata = meta_input)

# View the predictions
head(stacked_rf_predictions)

# Plot the predicted values versus the true target values
ggplot(meta_input, aes(x = true_target, y = stacked_rf_predictions)) +
  geom_point(color = "blue", alpha = 0.5) +  # Scatter plot of predicted values
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # 45-degree line
  labs(x = "True Target (Actual Sales)", y = "Predicted Sales") +  # Axis labels
  ggtitle("Stacked Randon Forest Model - Predicted Sales vs. True Target") +  # Title
  theme_minimal()  # Theme for the plot


rmse_stacked_rf <- sqrt(mean((meta_input$true_target - stacked_rf_predictions)^2))
mape_stacked_rf <- mean(abs((meta_input$true_target - stacked_rf_predictions) / meta_input$true_target)) * 100
forecast_bias_stacked_rf <- mean(meta_input$true_target - stacked_rf_predictions)
fa_stacked_rf <- mean(1 - abs(meta_input$true_target - stacked_rf_predictions) / pmax(meta_input$true_target, stacked_rf_predictions)) * 100

summary_table <- data.frame(
  Stacked_Linear = c(rmse_meta_lm, mape_meta_lm, forecast_bias_meta_lm, fa_meta_lm),
  Stacked_GBM = c(rmse_stacked_gbm, mape_stacked_gbm, forecast_bias_stacked_gbm, fa_stacked_gbm),
  Stacked_RF = c(rmse_stacked_rf, mape_stacked_rf, forecast_bias_stacked_rf, fa_stacked_rf))

kable(summary_table, caption = "Summary of Models", align = "c")



##################################################################################
#  5. Predictions on Test Data
##################################################################################

# Use the predictions of the base models on the test data as input features for the meta model
# Use the trained meta-model to generate the finale ensemble predictions on the test data

# In order to reduce potential for errors, all of the vectors for meta_input are retaining the same naming structure as original meta_input
# This means all previous meta_input will be overwritten for training data


##############################
# Step 1.  Prepare the test data
##############################
# Combine predictions into a single dataset

# Combine predictions from base models into a dataframe
meta_features <- data.frame(
  "Prediction_Model1" = lm_predicted_sales_testing, #n=18256
  "Prediction_Model2" = arima_forecast_values, #n=18256
  "Prediction_Model3" = gbm_predictions #18256
)
head(meta_features)
nrow(meta_features)

# Create meta-model input dataset
meta_input <- data.frame(
  "true_target" = testing_data$Sales,
  meta_features,
  "Date" = testing_data$Date
)
head(meta_input)

# Reorder columns to make Date the first column
meta_input <- meta_input[, c("Date", names(meta_input)[-which(names(meta_input) == "Date")])]
head(meta_input)

########################################
# 3a.  FEATURE ENGINEERING ON META_INPUT
########################################

##################
# RANKING FEATURES
##################
#Using ranking features helps meta-model identify which models consistently perform better or worse across different instances.

# Compute ranking features for each base model
rank_model1 <- rank(meta_input$Prediction_Model1)
rank_model2 <- rank(meta_input$Prediction_Model2)
rank_model3 <- rank(meta_input$Prediction_Model3)

# Create a dataframe with ranking features
ranking_df <- data.frame(
  "Rank_Model1" = rank_model1,
  "Rank_Model2" = rank_model2,
  "Rank_Model3" = rank_model3
)

# Bind ranking features to the meta_input dataset
meta_input <- cbind(meta_input, ranking_df)

# View the updated meta_input dataset
head(meta_input)

#####################
# DIFFERENCE FEATURES
#####################
# The absolute differences or percent differences between pairs of predictions from the base models.
# This can help meta-model correct for bias or errors in individual predictions

# Compute difference features between pairs of predictions
difference_model1_model2 <- abs(meta_input$Prediction_Model1 - meta_input$Prediction_Model2)
difference_model1_model3 <- abs(meta_input$Prediction_Model1 - meta_input$Prediction_Model3)
difference_model2_model3 <- abs(meta_input$Prediction_Model2 - meta_input$Prediction_Model3)

# Create a dataframe with difference features
difference_df <- data.frame(
  "Difference_Model1_Model2" = difference_model1_model2,
  "Difference_Model1_Model3" = difference_model1_model3,
  "Difference_Model2_Model3" = difference_model2_model3
)

# Bind difference features to the meta_input dataset
meta_input <- cbind(meta_input, difference_df)

# View the updated meta_input dataset
head(meta_input)

#####################
# GEOMETRIC MEAN
#####################
# The geometric mean tends to give less weight to extreme values and can help mitigate the influence of outliers

# Compute geometric mean of predictions for each row
geometric_mean_predictions <- apply(meta_input[, c("Prediction_Model1", "Prediction_Model2", "Prediction_Model3")], 1, function(row) {
  exp(mean(log(row), na.rm = TRUE))
})

# Add geometric mean as a new feature to the meta_input dataset
meta_input$Geometric_Mean_Predictions <- geometric_mean_predictions

# View the updated meta_input dataset
head(meta_input)

####################
# STANDARD DEVIATION
####################

# Compute standard deviation of predictions for each row
standard_deviation_predictions <- apply(meta_input[, c("Prediction_Model1", "Prediction_Model2", "Prediction_Model3")], 1, sd)

# Add standard deviation as a new feature to the meta_input dataset
meta_input$Standard_Deviation_Predictions <- standard_deviation_predictions

# View the updated meta_input dataset
head(meta_input)

########
# MEDIAN
########

# Compute median of predictions for each row
median_predictions <- apply(meta_input[, c("Prediction_Model1", "Prediction_Model2", "Prediction_Model3")], 1, median, na.rm = TRUE)

# Add median as a new feature to the meta_input dataset
meta_input$Median_Predictions <- median_predictions

# View the updated meta_input dataset
head(meta_input)


#####################
# TIME-BASED FEATURES
#####################

# Extract time-based features
meta_input$HOLIDAY <- as.numeric(testing_data$HOLIDAY)
meta_input$PAY <- as.numeric(testing_data$PAY)
meta_input$MON <- as.numeric(weekdays(testing_data$Date) == "Monday")
meta_input$TUE <- as.numeric(weekdays(testing_data$Date) == "Tuesday")
meta_input$WED <- as.numeric(weekdays(testing_data$Date) == "Wednesday")
meta_input$THU <- as.numeric(weekdays(testing_data$Date) == "Thursday")
meta_input$FRI <- as.numeric(weekdays(testing_data$Date) == "Friday")
meta_input$SAT <- as.numeric(weekdays(testing_data$Date) == "Saturday")
meta_input$SUN <- as.numeric(weekdays(testing_data$Date) == "Sunday")
meta_input$Month <- month(testing_data$Date)
meta_input$Day <- day(testing_data$Date)
meta_input$Quarter <- quarter(testing_data$Date)
meta_input$DayOfWeek <- wday(testing_data$Date)
meta_input$DayOfWeek <- as.numeric(meta_input$DayOfWeek)
meta_input$Days_until_next_payday <- testing_data$Days_until_next_payday
meta_input$Before_holiday <- testing_data$Before_holiday
meta_input$After_holiday <- testing_data$After_holiday

# View the updated meta_input dataset
head(meta_input)
nrow(meta_input)



###########################################
# 4a.  TRAIN META-MODEL - LINEAR REGRESSION
###########################################

set.seed(123)

# Define the formula for the linear regression model
linear_formula <- as.formula("true_target ~ . - true_target")

# Fit the linear regression model
lm_meta_model <- lm(linear_formula, data = meta_input)

summary(lm_meta_model)


# Extract predicted sales from the linear regression meta-model
meta_lm_predicted_sales <- predict(lm_meta_model, newdata = meta_input)
head(meta_lm_predicted_sales)


# Plot the predicted sales and true target values
ggplot(meta_input, aes(x = true_target, y = meta_lm_predicted_sales)) +
  geom_point(color = "blue", alpha = 0.5) +  # Scatter plot of predicted sales
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # 45-degree line
  labs(x = "True Target (Actual Sales)", y = "Predicted Sales") +  # Axis labels
  ggtitle("Stacked Linear Model - Predicted Sales vs. True Target") +  # Title
  theme_minimal()  # Theme for the plot



rmse_meta_lm <- sqrt(mean((meta_input$true_target - meta_lm_predicted_sales)^2))
mape_meta_lm <- mean(abs((meta_input$true_target - meta_lm_predicted_sales) / meta_input$true_target)) * 100
forecast_bias_meta_lm <- mean(meta_input$true_target - meta_lm_predicted_sales)
fa_meta_lm <- mean(1 - abs(meta_input$true_target - meta_lm_predicted_sales) / pmax(meta_input$true_target, meta_lm_predicted_sales)) * 100

summary_table <- data.frame(
  Stacked_Linear = c(rmse_meta_lm, mape_meta_lm, forecast_bias_meta_lm, fa_meta_lm))

kable(summary_table, caption = "Summary of Models", align = "c")


# NAs Examination Results:
# SUN + DayOfWeek are throwing a singularity error, but upon thorough examination, the data structure of these variables is correct.
# These variables may not provide much unique information or predictive power for forecasting, leading to numerical instability in the regression coefficients.
# Or the regression model might be overfitting the data, leading to numerical issues during estimation.


#############################
# 4b.  TRAIN META-MODEL - GBM
#############################

set.seed(123)

# Define predictors (excluding true_target and Date if present)
predictors <- setdiff(names(meta_input), c("true_target", "Date"))

# Define response variable
response <- "true_target"

# Train the GBM model
stacked_gbm_model <- gbm(
  formula = as.formula(paste(response, "~", paste(predictors, collapse = " + "))),
  data = meta_input,
  distribution = "gaussian",
  n.trees = 500,
  interaction.depth = 9,
  shrinkage = 0.01,
  bag.fraction = 0.9,
  cv.folds = 10,  # Optional: perform cross-validation
  verbose = TRUE
)

# Print the summary of the GBM model
summary(stacked_gbm_model)


# Make predictions on the training data
stacked_gbm_predictions <- predict(stacked_gbm_model, newdata = meta_input, type = "response")
head(stacked_gbm_predictions)


# Plot the predicted values versus the true target values
ggplot(meta_input, aes(x = true_target, y = stacked_gbm_predictions)) +
  geom_point(color = "blue", alpha = 0.5) +  # Scatter plot of predicted values
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # 45-degree line
  labs(x = "True Target (Actual Sales)", y = "Predicted Sales") +  # Axis labels
  ggtitle("Stacked GBM Model - Predicted Sales vs. True Target") +  # Title
  theme_minimal()  # Theme for the plot



rmse_stacked_gbm <- sqrt(mean((meta_input$true_target - stacked_gbm_predictions)^2))
mape_stacked_gbm <- mean(abs((meta_input$true_target - stacked_gbm_predictions) / meta_input$true_target)) * 100
forecast_bias_stacked_gbm <- mean(meta_input$true_target - stacked_gbm_predictions)
fa_stacked_gbm <- mean(1 - abs(meta_input$true_target - stacked_gbm_predictions) / pmax(meta_input$true_target, stacked_gbm_predictions)) * 100

summary_table <- data.frame(
  Stacked_Linear = c(rmse_meta_lm, mape_meta_lm, forecast_bias_meta_lm, fa_meta_lm),
  Stacked_GBM = c(rmse_stacked_gbm, mape_stacked_gbm, forecast_bias_stacked_gbm, fa_stacked_gbm))

kable(summary_table, caption = "Summary of Models", align = "c")




#######################################
# 4c.  TRAIN META-MODEL - RANDOM FOREST
#######################################

# Define the formula for the random forest model
stacked_rf_formula <- as.formula("true_target ~ .")

# Train the Random Forest model - Warning!  67 min run time!!!!
timing <- system.time({
  stacked_rf_model <- randomForest(stacked_rf_formula, data = meta_input)
})
timing

# Print the summary of the Random Forest model
print(stacked_rf_model)

# Generate predictions
stacked_rf_predictions <- predict(stacked_rf_model, newdata = meta_input)

# View the predictions
head(stacked_rf_predictions)


# Plot the predicted values versus the true target values
ggplot(meta_input, aes(x = true_target, y = stacked_rf_predictions)) +
  geom_point(color = "blue", alpha = 0.5) +  # Scatter plot of predicted values
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # 45-degree line
  labs(x = "True Target (Actual Sales)", y = "Predicted Sales") +  # Axis labels
  ggtitle("Stacked Random Forest Model - Predicted Sales vs. True Target") +  # Title
  theme_minimal()  # Theme for the plot


ggplot(meta_input, aes(x = Date)) +
  geom_line(aes(y = true_target, color = "Target"), linewidth = 1.2) +
  geom_line(aes(y = stacked_rf_predictions, color = "Predictions"), linewidth = 1.2) +
  labs(x = "Date", y = "Sales", color = "Legend") +
  ggtitle("Stacked RF Model - Predictions vs True Target") +
  theme_cowplot()




rmse_stacked_rf <- sqrt(mean((meta_input$true_target - stacked_rf_predictions)^2))
mape_stacked_rf <- mean(abs((meta_input$true_target - stacked_rf_predictions) / meta_input$true_target)) * 100
forecast_bias_stacked_rf <- mean(meta_input$true_target - stacked_rf_predictions)
fa_stacked_rf <- mean(1 - abs(meta_input$true_target - stacked_rf_predictions) / pmax(meta_input$true_target, stacked_rf_predictions)) * 100

summary_table <- data.frame(
  Stacked_Linear = c(rmse_meta_lm, mape_meta_lm, forecast_bias_meta_lm, fa_meta_lm),
  Stacked_GBM = c(rmse_stacked_gbm, mape_stacked_gbm, forecast_bias_stacked_gbm, fa_stacked_gbm),
  Stacked_RF = c(rmse_stacked_rf, mape_stacked_rf, forecast_bias_stacked_rf, fa_stacked_rf))

kable(summary_table, caption = "Summary of Models", align = "c")

################################################################################
# APPENDIX
################################################################################

#######################
# Seasonal ARIMIA Model
#######################
# Note: The SARIMA model below delivered evaluation metrics with a high RMSE & MAPE,
# large forecast bias, and accuracy that was no better than the Naive baseline forecast.
# In order for this model to be effective, it would need to be tuned using the SARIMA-X
# method where X represents the external predictors.  Additional packages like 'forecast'
# or 'fable' in conjunction with 'auto-arima()' are needed to build the SARIMA-X model
# where you specify both the time series data and the external predictors.


set.seed(123)

# Fit a SARIMA model
timing <- system.time({
  sarima_model <- auto.arima(sales_ts, seasonal=TRUE) #<- Warning - takes 8 to 10 min to run
  summary(sarima_model)
})
timing

length(testing_data$Sales)

# Generate forecasts using the SARIMA model for testing data
sarima_forecast_testing <- forecast(sarima_model, h = 18256)

# Extract forecasted sales values from the SARIMA forecast object
sarima_forecasted_values <- sarima_forecast_testing$mean

length(testing_data$Sales)
length(sarima_forecasted_values)


# Plot training data and testing data
plot(training_data$Date, training_data$Sales, type = "l", col = "blue", xlab = "Date", ylab = "Sales", main = "Training and Testing Data with SARIMA Forecast")
lines(testing_data$Date, testing_data$Sales, col = "red")

# Plot SARIMA forecasted values
lines(testing_data$Date, sarima_forecasted_values, col = "green")

# Add legend
legend("topright", legend = c("Training Data", "Testing Data", "SARIMA Forecast"), col = c("blue", "red", "green"), lty = 1)

# Adjust plot limits
ylim <- range(training_data$Sales, testing_data$Sales, sarima_forecasted_values)
ylim <- c(floor(ylim[1] / 1000) * 1000, ceiling(ylim[2] / 1000) * 1000)
ylim(ylim)



# Calculate evaluation metrics for the SARIMA forecast
rmse_sarima <- sqrt(mean((testing_data$Sales - sarima_forecasted_values)^2))
mape_sarima <- mean(abs((testing_data$Sales - sarima_forecasted_values) / testing_data$Sales)) * 100
forecast_bias_sarima <- mean(testing_data$Sales - sarima_forecasted_values)
fa_sarima <- mean(1 - abs(testing_data$Sales - sarima_forecasted_values) / pmax(testing_data$Sales, sarima_forecasted_values)) * 100

summary_table <- data.frame(
  Evaluation_Metric = c("RMSE", "MAPE", "Forecast Bias", "Forecast Accuracy"),
  Naive = c(rmse_naive, mape_naive, forecast_bias, fa_naive),
  Linear = c(rmse_lm, mape_lm, forecast_bias_lm, fa_lm),
  ARIMA = c(rmse_arima, mape_arima, forecast_bias_arima, fa_arima),
  SARIMA = c(rmse_sarima, mape_sarima, forecast_bias_sarima, fa_sarima))

kable(summary_table, caption = "Summary of Models", align = "c")
