
x <- read.csv(file="C:/Users/urmas/Documents/TalTech/Andmekaeve/assignment2/input/iphone.csv", header=TRUE, sep=",")

#---------------------------------------------------------------------------------------------
# Preprocess and forecast steps.
#---------------------------------------------------------------------------------------------
# Steps 1-7: Preprocess data.
preprocess <- function(x){
  # Returns processest quarterly data.
  # 1:Moving avg, 2:Baseline, 3: Yt/CMA, 4: Qt Avg
  # 5: Deseasonalized, 6: idx, 7: Original x variable.
  num_datapoints <- nrow(x)
  res <- matrix(0, nrow=num_datapoints, ncol=7)
  colnames(res) <- c("MovingAvg","Baseline","YtDivCMA","QuarterlyAvg","Deseasonalized","idx","x_orig")

  idx <- 1:num_datapoints
  x_orig <- x[,2]
  res[,6] <- idx
  res[,7] <- x_orig

  # Step 1: Moving avg.
  for (i in 4:num_datapoints){ res[i-1,1] <- mean(x_orig[(i-3):i]) }
  mov_avg <- res[,1]

  # Step 2: Baseline.
  idx_for_baseline <- 3:(num_datapoints-2)
  baseline <- (mov_avg[idx_for_baseline] + mov_avg[(idx_for_baseline+1)]) / 2
  res[idx_for_baseline, 2] <- baseline

  # Step 3: Yt/CMA.
  y_div_baseline <- x_orig[idx_for_baseline] / baseline
  res[idx_for_baseline,3] <- y_div_baseline
  
  # Step 4: Quarterly avg.
  num_quarters_for_mean <- (num_datapoints / 4) - 1
  q1 <- mean(res[seq(5, num_datapoints, 4)[1:num_quarters_for_mean],3])
  q2 <- mean(res[seq(6, num_datapoints, 4)[1:num_quarters_for_mean],3])
  q3 <- mean(res[seq(3, num_datapoints, 4)[1:num_quarters_for_mean],3])
  q4 <- mean(res[seq(4, num_datapoints, 4)[1:num_quarters_for_mean],3])

  # Step 5: Copy Quarterly avg into results.
  res[seq(1, num_datapoints, 4), 4] <- q1
  res[seq(2, num_datapoints, 4), 4] <- q2
  res[seq(3, num_datapoints, 4), 4] <- q3
  res[seq(4, num_datapoints, 4), 4] <- q4

  # Step 6: Deseasonalize.
  res[,5] <- x_orig / res[,4]

  return(res)
}

# Step 8: Forecast with linear model.
linear_forecast <- function(num_periods, intercept, x_variable, num_periods_to_forecast=4){
  return((1:(num_periods+num_periods_to_forecast)) * x_variable + intercept)
}

# Step 9: Forecast with seasonality.
forecast_with_seasonality <- function(linear_forecast_data, preprocessed_data){
  # Returns matrix[nrows(linear_forecast_data), 2].
  # 1.Col: Forecasted values, 2.Col: Quarterly average.
  num_rows <- length(linear_forecast_data)
  num_rows_prep <- nrow(preprocessed_data)
  res <- matrix(0, nrow=num_rows, ncol=2)
  colnames(res) <- c("Forecast","QuarterlyAvg")
  slice1 <- 1:num_rows_prep
  slice2 <- (num_rows_prep+1):num_rows
  res[slice1,1] <- linear_forecast_data[slice1] * preprocessed_data[,4]
  res[slice2,1] <- linear_forecast_data[slice2] * preprocessed_data[1:length(slice2),4]
  res[slice1,2] <- preprocessed_data[,4]
  res[slice2,2] <- preprocessed_data[1:length(slice2),4]
  return(res)
}

# End-to-end: Forecast with linear regression.
forecast_with_linear_regression <- function(x, num_periods_to_forecast=4){
  prep_data <- preprocess(x)
  df <- as.data.frame(prep_data)
  linearMod <- lm(Deseasonalized ~ idx, data=df)
  modelSummary <- summary(linearMod)
  modelCoeffs <- modelSummary$coefficients
  intercept <- modelCoeffs[1]
  x_variable <- modelCoeffs[2]
  linear <- linear_forecast(nrow(x), intercept, x_variable, num_periods_to_forecast)
  seasonal_forecast <- forecast_with_seasonality(linear, prep_data)
  num_rows <- nrow(seasonal_forecast)
  res <- matrix(0, nrow=num_rows, ncol=4)
  colnames(res) <- c("Linear","Seasonal", "QuarterlyAvg", "Actual")
  res[,1] <- linear
  res[,2] <- seasonal_forecast[,1]
  res[,3] <- seasonal_forecast[,2]
  res[,4] <- NA
  res[(1:nrow(x)),4] <- x[,2]
  return(res)
}

#---------------------------------------------------------------------------------------------
# Report
#---------------------------------------------------------------------------------------------
lin_reg <- forecast_with_linear_regression(x, 4)

x_axis <- 1:nrow(lin_reg)
linear <- lin_reg[,1]
seasonal <- lin_reg[,2]
actual <- lin_reg[,4]
#xlim <- c(min(x_axis), max(x_axis))
ylim <- c(floor(min(lin_reg, na.rm=TRUE)), ceiling(max(lin_reg, na.rm=TRUE)))

colors <- c(rgb(0.8,0.4,0.1,0.7), rgb(0.5,0.8,0.2,0.5), rgb(0.2,0.4,0.1,0.7))
point_types <- c(1,19,4)

# Make a basic graph , xaxp=c(0,20,20)
plot(linear~x_axis, main="Prediction with seasonality", cex.main=1.0, type="b", bty="l", cex=0.8,
     xlab="quarters", ylab="sales", col=colors[1], lwd=2, pch=point_types[1], ylim=ylim, cex.axis=0.9)
axis(side=1, tck=1, lty=3, cex.axis=0.9) # gridlines
lines(seasonal~x_axis, col=colors[2], lwd=2, pch=point_types[2], type="b", cex=0.7)
lines(actual~x_axis, col=colors[3], lwd=2, pch=point_types[3], type="b", cex=0.8) # pch=3 +

# Add a legend
legend("bottomleft", legend=c("linear", "seasonal", "actual"), col=colors, 
       pch=point_types, bty="n", pt.cex=1.0, cex=0.9, text.col="black", horiz=F, inset=c(0.03, 0.03))

#---------------------------------------------------------------------------------------------
# Debug step-by-step
#---------------------------------------------------------------------------------------------
prep_data <- preprocess(x)
df <- as.data.frame(prep_data)
linearMod <- lm(Deseasonalized ~ idx, data=df)
modelSummary <- summary(linearMod)
modelCoeffs <- modelSummary$coefficients
intercept <- modelCoeffs[1]
x_variable <- modelCoeffs[2]
linear <- linear_forecast(nrow(x), intercept, x_variable, num_periods_to_forecast=4)
seasonal_forecast <- forecast_with_seasonality(linear, prep_data)

modelSummary

