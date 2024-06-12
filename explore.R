library(dplyr)
library(tidyr)
library(tsoutliers)
library(seastests)
library(tseries)
library(TSA)
library(RJDemetra)
library(crayon)
library(leaps)
library(gets)
library(forecast)

# Load data framee
landingsV2 <- read.csv("Perso/landingsV2.csv")

# Create landings-weight data frame
land_w <- landingsV2 %>%
  select(YEAR, quarter, X3A_CODE, totwghtlandg)%>%
  group_by(YEAR, quarter, X3A_CODE) %>%
  summarise(totwghtlandg = sum(totwghtlandg, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = X3A_CODE, values_from = totwghtlandg) %>%
  mutate(across(everything(), ~replace_na(., 0)))

# Create landings-value data frame
land_v <- landingsV2 %>%
  select(YEAR, quarter, X3A_CODE, totvallandg)%>%
  group_by(YEAR, quarter, X3A_CODE) %>%
  summarise(totvallandg = sum(totvallandg, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = X3A_CODE, values_from = totvallandg) %>%
  mutate(across(everything(), ~replace_na(., 0)))



# Transform each variables in a time-series format----
ts_list <- list()

for (i in 3:ncol(land_w)) {
  col_name <- names(land_w)[i]
  ts_name <- paste0("ts_", col_name)
  ts_data <- assign(ts_name, ts(data = land_w[, col_name], start = c(2013, 01), frequency = 4))
  ts_list[[col_name]] <- ts_data
}


# Atypical points Correction----
for (col_name in names(ts_list)) {
  ts_name <- paste0("ts_", col_name)
  ts_name_tso <- paste0("tso_", col_name)
  ts_name_adj <- paste0("ts_", col_name, "_adj")  # New name for the adjusted series
  
  ts_data <- get(ts_name)
  
  # Try ARIMA models
  arima_fit <- tryCatch({
    fit <- tso(ts_data)
    cat("\033[1m\033[31m", "TSO for", col_name, ":\033[0m\n")
    print(fit)
    assign(ts_name_tso, fit)
    assign(ts_name_adj, fit$yadj)  # Assign the adjusted series to a new name
    
    # Check if there are atypical points before plotting
    if (!is.null(fit$outliers) && nrow(fit$outliers) > 0) {
      plot(fit)
      title(main = paste("TSO for", col_name))
    }
    
    TRUE
  }, error = function(e) {
    cat("\033[1m\033[31m", "Error for", col_name, ":\033[0m\n")
    cat(e$message, "\n")
    FALSE
  })
  
  # If ARIMA adjustment fails, go to the next iteration
  if (!arima_fit) next
  
}

#' We have an error for : ANE, NEP5, NEP6 and MUT6. This is normal for NEP5, NEP6
#' and MUT6 because the data is really poor, but this is NOT normal for ANE.
#' We have to try a different method for this variable.

#tso(ts_ANE) # No suitable ARIMA model found

# Let's try the X13-ARIMA-SEATS method
myregx13 <- regarima_x13(ts_ANE, spec ="RG5c")
summary(myregx13) # 2 outliers, AO (I-2015) and AO (I-2019)
n <- nrow(ts_ANE)
adjseries <- matrix(myregx13$model$effects[1:n])
ts_ANE_adj <- ts(data = adjseries, start=c(2013,01),frequency=4)
# Now ts_ANE is adjusted with the X13-ARIMA-SEATS method

# Let's get rid of the non-corrected time series

# From the dataframe:
land_w <- land_w %>% 
  select(-NEP5, -NEP6, -MUT6)

# From the list:
ts_list <- ts_list[!(names(ts_list) %in% c("NEP5", "NEP6", "MUT6"))]

  




# Seasonal Adjustment----
seasonal_combined_test <- c()
seasonal_seasdum <- c()

for (col_name in names(ts_list)) {
  ts_name <- paste0("ts_", col_name, "_adj")  # Name of adjusted time series
  ts_data <- get(ts_name)  # Retrieve data from the adjusted time series
  
  # Apply the combined_test() test to the adjusted time series
  ct_res <- combined_test(ts_data)
  
  # Check each p-value and display results if at least one valid H1
  ct_results <- ct_res$stat
  ct_pvals <- ct_res$Pval
  if (ct_results == TRUE) {
    cat("\n")
    cat("\n")
    print(paste0("Combined_test results for the series ", ts_name))
    print(ct_res)
    seasonal_combined_test <- c(seasonal_combined_test, ts_name)
  }
  
  # Apply the seasdum() test to the adjusted time series
  sd_res <- seasdum(ts_data)
  
  # Check p-value and display results if < 0.05
  if (sd_res$Pval < 0.05) {
    cat("\n")
    cat("\n")
    print(paste0("Résultats du seasdum pour la série ", ts_name))
    print(sd_res)
    seasonal_seasdum <- c(seasonal_seasdum, ts_name)
  }
}

# Display lists of series with seasonality
print(seasonal_combined_test)
length(seasonal_combined_test) # 31

print(seasonal_seasdum)
length(seasonal_seasdum) #38



# Calculate the differences between the two lists
only_combined_test <- setdiff(seasonal_combined_test, seasonal_seasdum)
only_seasdum <- setdiff(seasonal_seasdum, seasonal_combined_test)
both_tests <- intersect(seasonal_combined_test, seasonal_seasdum)

# Show differences
only_combined_test
only_seasdum
both_tests

# Let's correct the seasonality for the series that are detected by both tests


# Seasonality correction with STL for series in both_tests
for (ts_name in both_tests) {
  ts_data <- get(ts_name)  # Retrieve time series data
  decomp <- stl(ts_data[,1], s.window = "periodic")  # STL decomposition
  seasonal <- decomp$time.series[, "seasonal"]  # Get the seasonal component
  ts_data_adjusted <- ts_data - seasonal  # Correcting the seasonal component
  assign(ts_name, ts_data_adjusted)  # Update the adjusted time series
}




# Checking seasonality after correction
seasonal_combined_test_after <- c()
seasonal_seasdum_after <- c()

for (ts_name in both_tests) {
  ts_data <- get(ts_name)  # Retrieve data from the adjusted time series
  
  # Apply the combined_test() test to the adjusted time series
  ct_res <- combined_test(ts_data)
  
  # Check each p-value and display results if at least one is < 0.05
  ct_pvals <- ct_res$Pval
  ct_results <- ct_res$stat
  if (ct_results == TRUE) {
    cat(bold(underline("\033[1m\033[31m", "Corrected combined_test results for the series ", ts_name, ":\033[0m\n")))
    print(ct_res)
    seasonal_combined_test_after <- c(seasonal_combined_test_after, ts_name)
  }
  
  # Apply the seasdum() test to the adjusted time series
  sd_res <- seasdum(ts_data)
  
  # Check and print results if p<0.05
  if (sd_res$Pval < 0.05) {
    cat(bold(underline("\033[1m\033[31m", "Results of Seasdum after correction for the TS: ", ts_name, ":\033[0m\n")))
    print(sd_res)
    seasonal_seasdum_after <- c(seasonal_seasdum_after, ts_name)
  }
}


cat(bold("\nSeries still showing seasonality according to combined_test after correction:\n"))
print(seasonal_combined_test_after)

cat(bold("\nSeries still showing seasonality according to seasdum after correction:\n"))
print(seasonal_seasdum_after)

# It seems that "ts_ANE_adj" "ts_JLX_adj" "ts_OCT_adj" "ts_PEN_adj" "ts_PIL_adj",
# are is still showing seasonality according to seasdum after correction.


# Let's check with different tests on this variable:
for (x in seasonal_combined_test_after) {
  cat(bold(underline("Test results for the", x, "\n")))
  print(fried(get(x)))
  cat("\n")
  print(kw(get(x)))
  cat("\n")
  print(qs(get(x)))
  cat("\n")
  print(seasdum(get(x)))
  cat("\n")
  cat("\n")
}

# According to the tests, most of them show no seasonality except the QS test.


# Stationarity----
for (i in names(ts_list)) {
  ts_name <- paste0("ts_", i, "_adj")  # Name of the TS
  adf_result <- adf.test(get(ts_name))  # Apply ADF test for stationnarity
  adf_result
  # Check if P-Value > 0.05
  if (adf_result$p.value > 0.05) {
    # Apply a difference to the time series if it is not stationary
    assign(ts_name, diff(get(ts_name))) 
    print(paste("The TS", ts_name, "has been differentiated"))
  }
  else {
    print(paste("The TS", ts_name, "is stationary"))
  }
}

# Differenciate a second time if necessary
for (i in names(ts_list)) {
  ts_name <- paste0("ts_", i, "_adj")  # Name of the TS
  adf_result <- adf.test(get(ts_name))  # Apply ADF test for stationnarity
  adf_result
  # Check if P-Value > 0.05
  if (adf_result$p.value > 0.05) {
    # Apply a difference to the time series if it is not stationary
    assign(ts_name, diff(get(ts_name))) 
    print(paste("The TS", ts_name, "has been differentiated a second time"))
  }
}


for (i in names(ts_list)) {
  ts_name <- paste0("ts_", i, "_adj")  # Name of the TS
  adf_result <- adf.test(get(ts_name))  # Apply ADF test for stationnarity
  adf_result
  if (adf_result$p.value > 0.05) {
    # Check if P-Value > 0.05
    print(paste("La série", ts_name, "is not stationary"))
  }
}

# All the time series are now stationary, let's adjust the other time series
# so that they have the same length as the shortest ones, that have been corrected
# two times. So he have to retrieve 2 values from the "already stationary" ones.

# Align time series length----


series_list <- ls(pattern = "^ts_.*_adj$")

# Ger rid of ts_name_adj which is not a time serie
series_list <- series_list[series_list != "ts_name_adj"]

min_length <- min(sapply(mget(series_list), length))

for (series_name in series_list) {
  series <- get(series_name)
  start_index <- length(series) - min_length + 1
  start_time <- time(series)[start_index]
  series_trimmed <- window(series, start = start_time)
  assign(series_name, series_trimmed, envir = .GlobalEnv)
}



# Atypical points correction AGAIN----
# series_list_adj <- lapply(series_list, function(series_name) {
#   series <- get(series_name)
#   fit <- tso(series)
#   
#   # Vérifier si des outliers ont été détectés
#   if (!is.null(fit$outliers) && nrow(fit$outliers) > 0) {
#     print(paste("Des outliers ont été détectés pour", series_name))
#   }
#   
#   series_adj <- fit$yadj
#   new_name <- paste0(series_name, "_adj")
#   assign(new_name, series_adj, envir = .GlobalEnv)
#   return(series_adj)
# })
# 
# 


# Create a data frame with all the time-series----
# 38 is the lenght of my time series, you have to change it to yours
# +2 is for the "Year" and éQuarter" columns
ts_df <- data.frame(matrix(nrow = 38, ncol = length(series_list) + 1))
names(ts_df) <- c("Time", series_list)

# Get the sequence of quarters
quarters <- seq(as.Date("2013-07-01"), by = "quarter", length.out = 38)

# Create the "Time" column
ts_df$Time <- paste(format(quarters, "%Y"), "-", quarters %>% quarter(), sep = "")

# Loop through each time series and add it as a column
for (i in seq_along(series_list)) {
  ts_name <- series_list[i]
  ts_data <- as.numeric(get(ts_name))  # Convert to numeric vector
  ts_df[, ts_name] <- ts_data
}

View(ts_df)


# Plot
ggplot(data = ts_df, aes(x = Time, y = ts_JLX_adj, group = 1)) +
  geom_line()+
  theme(axis.text.x=element_text(angle=-90, hjust=0.001))


# Variables selection with Y=HKE1567----
## BestSubSet Method----

# Let's get rid of the "Time" column
ts_df2 <- ts_df[, -1]

# Let's take 80% of the rows of the data frame as training base
train_size <- floor(0.8 * nrow(ts_df2))

# Let's create the training base
train <- ts_df2[1:train_size, ]

# Let's apply the variables selection
leaps <- regsubsets(ts_HKE1567_adj ~ .,data= train, nbest=1, method=c("exhaustive"))
leaps
res.sum <- summary(leaps) 
data.frame(Adj.R2=which.max(res.sum$adjr2),
           # Cp = RSS/s^2 + N - 2*p, we have more variables than data points
           # so it's doesn't make sens to use CP
           CP=which.min(res.sum$cp), 
           BIC=which.min(res.sum$bic))
# 2 of the criteria are advising a 8 variables model

#  plot  a  table  of  models  showing  variables  in  each  model 
plot(leaps,scale="adjr2",main="Adjusted R2")
# Best Model: ANE, ARA67, JOD, MGR, OCT, POD, SQZ, WEX
plot(leaps, scale = "bic", main = "BIC")
# Same




HKE_df <- ts_df %>% 
  select(ts_HKE1567_adj, ts_ANE_adj, ts_ARA67_adj, ts_JOD_adj, ts_MGR_adj, 
         ts_OCT_adj, ts_POD_adj, ts_SQZ_adj, ts_WEX_adj)

y_real <- tail(HKE_df$ts_HKE1567_adj, n=8)
y_real <- as.numeric(y_real)

