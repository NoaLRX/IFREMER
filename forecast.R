library(gets)
library(mgcv)

########################### PLEASE READ ME ####################################
################################################################################
# This script contains two functions that use different forecasting models. 
# The first "eco_models" function uses 6 econometric forecasting models, while 
# the second "ml_models" function uses 9 Machine Learning and Deep Learning models.

# To use the functions, you need to have a dataframe with your Y variable and
# your X variables. Note that your Y variable should be the first columns of the dataframe.

# The functions only take two arguments: your dataframe with all your variables
# (DATAFRAME) and the name of the Y variable column from your dataframe (Y_VARIABLE).
# For example if I have a dataframe called "df" whose first columns is my Y variable
# df$Y_variable, then I can use the function by typing eco_models(df, "Y_variable")
################################################################################
################################################################################


eco_models <- function(DATAFRAME, Y_VARIABLE){
  # Modele ARX avec GETS----
  n <- nrow(DATAFRAME)
  train_size <- round(0.8 * n)
  train_data <- DATAFRAME[1:train_size, ]
  test_data <- DATAFRAME[(train_size+1):n, ]
  mX_train <- data.matrix(train_data[,-1]) 
  mX_test <- data.matrix(test_data[,-1])  
  y_train <- train_data[[Y_VARIABLE]]
  y_test <- test_data[[Y_VARIABLE]]
  model <- arx(y_train, mc = TRUE, ar = 1, mxreg = mX_train, vcov.type = "ordinary")
  n_test <- nrow(test_data)
  p_arxget <- predict(model, n.ahead = n_test, newmxreg = mX_test)
  p_arxget <- as.numeric(p_arxget)
  rmse_arxget <- sqrt(mean((y_real - p_arxget)^2, na.rm = TRUE))
  print(paste("RMSE ARX-GETS: ", rmse_arxget))
  
  
  ## Modele ARX avec auto_arima----
  split <- round(nrow(DATAFRAME) * 0.8)
  train_df <- DATAFRAME[1:split, ]
  test_df <- DATAFRAME[(split+1):nrow(DATAFRAME), ]
  y_train <- train_df[[Y_VARIABLE]]
  xreg_train <- data.matrix(train_df[,-1])
  modelx_train <- auto.arima(y_train, max.q = 0, xreg = xreg_train, seasonal = FALSE, stationary = TRUE)
  xreg_test <- data.matrix(test_df[,-1])
  p_arx <- predict(modelx_train, newxreg = xreg_test, n.ahead = nrow(test_df))$pred
  p_arx <- as.numeric(p_arx)
  rmse_arx <- sqrt(mean((y_real - p_arx)^2, na.rm = TRUE))
  print(paste("RMSE ARX: ", rmse_arx))
  
  
  ## Modele ARMAX avec auto_arima----
  split <- round(nrow(DATAFRAME) * 0.8)
  train_df <- DATAFRAME[1:split, ]
  test_df <- DATAFRAME[(split+1):nrow(DATAFRAME), ]
  y_train <- train_df[[Y_VARIABLE]]
  xreg_train <- data.matrix(train_df[,-1])
  modelx_train <- auto.arima(y_train, xreg = xreg_train, seasonal = FALSE, stationary = TRUE)
  j <- ncol(modelx_train$var.coef)
  tstat <- matrix(nrow=j, ncol=1)
  for(i in 1:j)
  {
    tstat[i,1] <- modelx_train$coef[i]/sqrt(modelx_train$var.coef[i,i])
  }
  tstat
  xreg_test <- data.matrix(test_df[,-1])
  p_armax <- predict(modelx_train, newxreg = xreg_test, n.ahead = nrow(test_df))$pred
  p_armax <- as.numeric(p_armax)
  rmse_armax <- sqrt(mean((y_real - p_armax)^2, na.rm = TRUE))
  print(paste("RMSE ARMAX: ", rmse_armax))
  
  
  
  ## Naive model----
  y_train <- ts(train_df[[Y_VARIABLE]])
  naive_model <- rwf(y_train, h=nrow(test_df))
  p_naive <- naive_model$mean
  p_naive <-  as.numeric(p_naive)
  rmse_naive <- sqrt(mean((y_real - p_naive)^2, na.rm = TRUE))
  print(paste("RMSE Naive: ", rmse_naive))
  
  
  ## Model LM----
  set.seed(123)
  split <- round(nrow(DATAFRAME) * 0.8)
  train_df <- DATAFRAME[1:split, ]
  test_df <- DATAFRAME[(split + 1):nrow(DATAFRAME), ]
  model_lm <- lm(formula(paste(Y_VARIABLE, "~", ".")), data = train_df)
  p_lm <- predict(model_lm, newdata = test_df)
  p_lm <- as.numeric(p_lm)
  rmse_lm <- sqrt(mean((y_real - p_lm) ^ 2, na.rm = TRUE))
  print(paste("RMSE LM: ", rmse_lm))
  
  
  ## Model AR1----
  y <- ts(DATAFRAME[[Y_VARIABLE]])
  model_ar1 <- Arima(y, order = c(1, 0, 0), seasonal = list(order = c(0, 0, 0), period=12),lambda=1)
  forecast_ar1 <- forecast(model_ar1, h = 57)
  p_ar1 <- forecast_ar1$mean
  p_ar1 <- as.numeric(p_ar1)
  rmse_ar1 <- sqrt(mean((y_real - p_ar1)^2, na.rm = TRUE))
  print(paste("RMSE AR1: ", rmse_ar1))
  
  
  ## Model GAM----
  ## Data preparation
  set.seed(123)
  smp_size <- ceiling(0.80 * nrow(DATAFRAME))
  train_ind <- sample(seq_len(nrow(DATAFRAME)), size = smp_size)
  train <- DATAFRAME[train_ind, ]
  test <- DATAFRAME[-train_ind, ]
  
  other_vars <- setdiff(names(train), Y_VARIABLE)
  smooth_terms <- paste0("s(", other_vars, ")")
  model_formula <- reformulate(termlabels = smooth_terms, response = Y_VARIABLE)
  
  model_gam <- gam(model_formula, data = train)
  
  ## Prediction and RMSE calculation
  p_gam <- predict(model_gam, newdata = test)
  p_gam <- as.numeric(p_gam)
  y_real <- test[[Y_VARIABLE]]
  
  rmse_gam <- sqrt(mean((y_real - p_gam)^2, na.rm = TRUE))
  print(paste("RMSE GAM :", rmse_gam))
  
}

eco_models(HKE_df, "ts_HKE1567_adj")