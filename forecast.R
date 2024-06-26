library(gets)
library(seastests)
library(mgcv)
library(neuralnet)
library(earth)
library(e1071)
library(caret)
library(randomForest)
library(xgboost)
library(class)
library(forecast)
library(tsoutliers)
library(dplyr)
library(tidyr)
library(crayon)
library(tseries)
library(leaps)

df <- read.csv2("/Users/noa/Documents/Administratif/M1/MEMOIRE/DATA/BDD_1999_V6.csv")
df <- df[,2:ncol(df)]
df <- df %>%
  select(-c("Food_lag"))

# TS TRANSFORMATION----
ts_transfo <- function(DATAFRAME, YEAR, MONTH, FREQUENCY) {
  
  # Initialize the list to store the time-series
  ts_list <- list()
  
  # Loop over each column of the dataframe
  for (i in 1:ncol(DATAFRAME)) {
    col_name <- names(DATAFRAME)[i]
    ts_name <- paste0("ts_", col_name)
    ts_data <- ts(data = DATAFRAME[, col_name], start = c(YEAR, MONTH), frequency = FREQUENCY)
    ts_list[[col_name]] <- ts_data
    assign(ts_name, ts_data, envir = .GlobalEnv)
    
  }
  assign("ts_list", ts_list, envir = .GlobalEnv)
  return(ts_list)
}
ts_transfo(df, 1999, 01, 12)



# ATYPICAL POINTS CORRECTION----
atypical_tso <- function(){
  ts_list_adj <- list()  # Initialize the new list
  
  for (col_name in names(ts_list)) {
    ts_name <- paste0("ts_", col_name)
    ts_name_tso <- paste0("tso_", col_name)
    ts_name_adj <- paste0("ts_", col_name, "_adj")
    
    ts_data <- get(ts_name)
    
    # Try ARIMA models
    arima_fit <- tryCatch({
      fit <- tso(ts_data)
      cat("\033[1m\033[31m", "TSO for", col_name, ":\033[0m\n")
      print(fit)
      assign(ts_name_tso, fit)
      assign(ts_name_adj, fit$yadj, envir = .GlobalEnv)
      
      # Add the adjusted series to ts_list_adj
      ts_list_adj[[col_name]] <- fit$yadj
      
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
  
  # Assign ts_list_adj to the global environment
  assign("ts_list_adj", ts_list_adj, envir = .GlobalEnv)
}
atypical_tso()


# SEASONALITY DETECTION----
seaso_detect <- function(){
 
   # Initialize lists to store the results
  seasonal_combined_test <- c()
  seasonal_seasdum <- c()
  
  # Loop over each TS
  for (col_name in names(ts_list_adj)) {
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
  
  assign("seasonal_combined_test", seasonal_combined_test, envir = .GlobalEnv)
  assign("seasonal_seasdum", seasonal_seasdum, envir = .GlobalEnv)
  
  
  # Calculate the differences between the two lists
  only_combined_test <- setdiff(seasonal_combined_test, seasonal_seasdum)
  only_seasdum <- setdiff(seasonal_seasdum, seasonal_combined_test)
  both_tests <- union(only_combined_test, only_seasdum)
  
  # Save the results
  assign("only_combined_test", only_combined_test, envir = .GlobalEnv)
  assign("only_seasdum", only_seasdum, envir = .GlobalEnv)
  assign("both_tests", both_tests, envir = .GlobalEnv)
  
  # Show differences
  cat("\n")
  cat("\n")
  cat("Seasonality detected only in combined_test function: ", paste(only_combined_test, collapse = ", "), "\n")
  cat("\n")
  cat("\n")
  cat("Seasonality detected only in seasdum function: ", paste(only_seasdum, collapse = ", "), "\n")
  cat("\n")
  cat("\n")
  cat("Seasonality detected in both test: ", paste(both_tests, collapse = ", "), "\n")
  
  
 
}
seaso_detect()


# SEASONALITY CORRECTION----
seaso_correct <- function(){
  
  for (ts_name in both_tests) {
    ts_data <- get(ts_name)  # Retrieve time series data
    decomp <- stl(ts_data, s.window = "periodic")  # STL decomposition
    seasonal <- decomp$time.series[, "seasonal"]  # Get the seasonal component
    ts_data_adjusted <- ts_data - seasonal  # Correcting the seasonal component
    assign(ts_name, ts_data_adjusted, envir = .GlobalEnv)  # Update the adjusted time series
  }
}
seaso_correct()


# SEASONALITY VERIFICATION----
seaso_verif <- function(){
  
  # Initialize lists to store the results
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
}
seaso_verif()


# STATIONARITY VERIFICATION----
statio <- function() {
  # Stationarity----
  for (i in names(ts_list_adj)) {
    ts_name <- paste0("ts_", i, "_adj")  # Name of the TS
    adf_result <- adf.test(get(ts_name))  # Apply ADF test for stationnarity
    adf_result
    
    # Check if P-Value > 0.05
    if (adf_result$p.value > 0.05) {
      # Apply a difference to the time series if it is not stationary
      assign(ts_name, diff(get(ts_name)), envir = .GlobalEnv)
      print(paste("The TS", ts_name, "has been differentiated"))
    }
    else {
      print(paste("The TS", ts_name, "is stationary"))
    }
  }
  
  # Differentiate a second time if necessary
  for (i in names(ts_list_adj)) {
    ts_name <- paste0("ts_", i, "_adj")  # Name of the TS
    adf_result <- adf.test(get(ts_name))  # Apply ADF test for stationnarity
    adf_result
    # Check if P-Value > 0.05
    if (adf_result$p.value > 0.05) {
      print(paste("The TS", ts_name, "is still not stationary"))
      # Apply a difference to the time series if it is not stationary
      assign(ts_name, diff(get(ts_name)), envir = .GlobalEnv)
      print(paste("The TS", ts_name, "has been differentiated a second time"))
    }
  }
  
  
  for (i in names(ts_list_adj)) {
    ts_name <- paste0("ts_", i, "_adj")  # Name of the TS
    adf_result <- adf.test(get(ts_name))  # Apply ADF test for stationnarity
    adf_result
    if (adf_result$p.value > 0.05) {
      # Check if P-Value > 0.05
      cat(bold(
        underline(
          "\033[1m\033[31m",
          "The time serie ",
          ts_name,
          "is still not stationnary",
          ":\033[0m\n"
        )
      ))
    }
  }
  
}
statio()

# ALLIGN TIME SERIES LENGTH----
allign_ts <- function() {
  series_list <- ls(pattern = "^ts_.*_adj$", envir = .GlobalEnv, all.names = TRUE)
  series_list <- series_list[series_list != "ts_list_adj"]
  assign("series_list", series_list, envir = .GlobalEnv)
  
  min_length <- Inf
  for (series_name in series_list) {
    series_obj <- get(series_name, envir = .GlobalEnv)
    series_lengths <- sapply(series_obj, length)
    min_length <- min(min_length, min(series_lengths, na.rm = TRUE))
  }
  
  for (series_name in series_list) {
    series_obj <- get(series_name, envir = .GlobalEnv)
    series_trimmed <- lapply(series_obj, function(x) {
      start_index <- length(x) - min_length + 1
      start_time <- time(x)[start_index]
      window(x, start = start_time)
    })
    assign(series_name, series_trimmed, envir = .GlobalEnv)
  }
}
allign_ts()

# CREATE DATAFRAME OF TS----
create_df <- function(first_column = NULL) {
  ts_to_df <- function(ts_obj, series_name) {
    if (is.null(ts_obj$y)) {
      df <- as.data.frame(ts_obj)
    } else {
      df <- as.data.frame(ts_obj$y)
    }
    df$Year <- as.numeric(rownames(df))
    
    if (ncol(df) == 2) {
      names(df)[1] <- series_name
      df$Quarter <- "Annual"
    } else {
      df_long <- tidyr::pivot_longer(
        df,
        cols = -Year,
        names_to = "Quarter",
        values_to = series_name
      )
      df <- df_long
    }
    
    return(df)
  }
  
  first_series <- get(series_list[1])
  result_df <- ts_to_df(first_series, series_list[1])
  
  for (series_name in series_list[-1]) {
    ts_obj <- get(series_name)
    
    df <- ts_to_df(ts_obj, series_name)
    
    result_df <- merge(result_df,
                       df,
                       by = c("Year", "Quarter"),
                       all = TRUE)
  }
  
  result_df <- result_df[order(result_df$Year, result_df$Quarter), ]
  
  # Réorganiser les colonnes si first_column est spécifié
  if (!is.null(first_column) && first_column %in% names(result_df)) {
    col_order <- c("Year", "Quarter", first_column)
    remaining_cols <- setdiff(names(result_df), col_order)
    result_df <- result_df[, c(col_order, remaining_cols)]
  } else {
    result_df <- result_df[, c("Year", "Quarter", series_list)]
  }
  
  result_df <- result_df[, !names(result_df) %in% "Year"]
  result_df <- result_df[, !names(result_df) %in% "Quarter"]
  new_names <- names(result_df)
  new_names <- gsub("^ts_", "", new_names)  
  new_names <- gsub("_adj$", "", new_names) 
  names(result_df) <- new_names
  assign("result_df", result_df, envir = .GlobalEnv)
  View(result_df)
}
create_df("ts_Food_adj")

# VARIABLES SELECTION----
vselec <- function(Y_VARIABLE) {
  ## BestSubSet Method----
  train_size <- floor(0.8 * nrow(result_df))
  train <- result_df[1:train_size, ]
  
  formula <- as.formula(paste(Y_VARIABLE, "~ ."))
  
  leaps <- regsubsets(
    formula,
    data = train,
    nbest = 1,
    method = c("exhaustive")
  )
  
  res.sum <- summary(leaps)
  
  results <- data.frame(
    Adj.R2 = which.max(res.sum$adjr2),
    CP = which.min(res.sum$cp),
    BIC = which.min(res.sum$bic)
  )
  
  print(results)
  
  plot(leaps, scale = "adjr2", main = "Adjusted R2")
  plot(leaps, scale = "bic", main = "BIC")
  
  print(leaps)
  
  
  # GETS Method----
  train_size <- floor(0.8 * nrow(result_df))
  train <- result_df[1:train_size, ]
  mX <- data.matrix(train)
  modele_arx <- arx(train[[Y_VARIABLE]], mc = TRUE, ar = 1, mxreg = mX, vcov.type = "ordinary")
  modele_arx
  seuil_p_value <- 0.05
  variables <- colnames(train[, 2:13])
  VRAI <- TRUE
  while (VRAI) {
    mX <- data.matrix(train[, variables])
    modele_arx <- arx(train[[Y_VARIABLE]], mc = TRUE, ar = 1, mxreg = mX, vcov.type = "ordinary")
    
    p_values <- modele_arx[["mean.results"]][["p-value"]][-c(1, 2)]
    max_p_value <- max(p_values)
    
    if (max_p_value > seuil_p_value) {
      variable_a_supprimer <- variables[which.max(p_values)]
      variables <- setdiff(variables, variable_a_supprimer)
    } else {
      VRAI <- FALSE
    }
  }
  arx_final <- arx(train$Food, mc = TRUE, ar = 1, mxreg = mX, vcov.type = "ordinary")
  modele_gets <- getsm(arx_final) # Avoir les coeff du modele ARX  + lunchbox test
  modele_gets
  
}
vselec("Food")

result_df <- result_df %>%
  select("Food", "Metal", "Raw_Material", "Huile", "Wheat", "Sucre")

# FUNCTION ECO----
eco_models <- function(DATAFRAME, Y_VARIABLE, PERIOD){
  
  n <- nrow(DATAFRAME)
  train_size <- round(0.8 * n)
  train_data <- DATAFRAME[1:train_size, ]
  test_data <- DATAFRAME[(train_size + 1):n, ]
  y_real <- test_data[[Y_VARIABLE]]
  
  
  # ARX model with Gets----
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
  
  
  ## ARX model with auto_arima----
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
  
  
  ## ARMAX model with auto_arima----
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
  
  
  ## LM model----
  set.seed(123)
  split <- round(nrow(DATAFRAME) * 0.8)
  train_df <- DATAFRAME[1:split, ]
  test_df <- DATAFRAME[(split + 1):nrow(DATAFRAME), ]
  model_lm <- lm(formula(paste(Y_VARIABLE, "~", ".")), data = train_df)
  p_lm <- predict(model_lm, newdata = test_df)
  p_lm <- as.numeric(p_lm)
  rmse_lm <- sqrt(mean((y_real - p_lm) ^ 2, na.rm = TRUE))
  print(paste("RMSE LM: ", rmse_lm))
  
  
  ## AR1 model----
  y <- ts(DATAFRAME[[Y_VARIABLE]])
  model_ar1 <- Arima(y, order = c(1, 0, 0), seasonal = list(order = c(0, 0, 0), period = PERIOD), lambda = 1)
  forecast_ar1 <- forecast(model_ar1, h = nrow(test_data))
  p_ar1 <- forecast_ar1$mean[1:length(y_real)]
  p_ar1 <- as.numeric(p_ar1)
  rmse_ar1 <- sqrt(mean((y_real - p_ar1)^2, na.rm = TRUE))
  print(paste("RMSE AR1: ", rmse_ar1))
  
  
  ## GAM model----
  ## Data preparation
  set.seed(123)
  n <- nrow(DATAFRAME)
  train_size <- round(0.8 * n)
  train_data <- DATAFRAME[1:train_size, ]
  test_data <- DATAFRAME[(train_size + 1):n, ]
  y_real <- test_data[[Y_VARIABLE]]
  
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
  
  
  # Create a list to store the results
  results <- list()
  
  # Store the results in the R Global Environment
  assign("y_real", y_real, envir = .GlobalEnv)
  assign("p_arxget", p_arxget, envir = .GlobalEnv)
  assign("p_arx", p_arx, envir = .GlobalEnv)
  assign("p_armax", p_armax, envir = .GlobalEnv)
  assign("p_naive", p_naive, envir = .GlobalEnv)
  assign("p_lm", p_lm, envir = .GlobalEnv)
  assign("p_ar1", p_ar1, envir = .GlobalEnv)
  assign("p_gam", p_gam, envir = .GlobalEnv)
  
  
}
eco_models(result_df, "Food", 12)

# FUNCTION ML----
ml_models <- function(DATAFRAME, Y_VARIABLE){
  
  ## MLP model----
  set.seed(123)
  n <- nrow(DATAFRAME)
  train_size <- round(0.8 * n)
  train_data <- DATAFRAME[1:train_size, ]
  test_data <- DATAFRAME[(train_size + 1):n, ]
  y_real <- test_data[[Y_VARIABLE]]
  
  mlp_model <- neuralnet(formula(paste(Y_VARIABLE, "~", ".")), data = train, hidden = 1)
  
  p_mlp <- predict(mlp_model, test[,-1])
  #p_mlp <- as.numeric(p_mlp$net.result)
  
  rmse_ml <- sqrt(mean((y_real - p_mlp)^2, na.rm = TRUE))
  print(paste("RMSE for MLP:", rmse_ml)) 
  
  
  ## Mars model----
  n <- nrow(DATAFRAME)
  train_size <- round(0.8 * n)
  train <- DATAFRAME[1:train_size, ]
  test <- DATAFRAME[(train_size+1):n, ]
  
  mars_model <- earth(formula(paste(Y_VARIABLE, "~", ".")), data = train)
  
  p_mars <- predict(mars_model, newdata = test)
  p_mars <- as.numeric(p_mars)

  
  rmse_mars <- sqrt(mean((y_real - p_mars)^2, na.rm = TRUE))
  print(paste("RMSE for MARS:", rmse_mars))
  
  
  ## SVM model----
  # Find optimal parameters
  svm_grid <- expand.grid(C = c(0.1, 1, 10), sigma = c(0.001, 0.01, 0.1))
  train <- DATAFRAME
  
  svm_model_opt <- train(
    as.formula(paste(Y_VARIABLE, "~.")), #
    data = train,
    method = "svmRadial",
    preProcess = c("center", "scale"),
    tuneGrid = svm_grid
  )
  
  svm_model_best <- svm(
    as.formula(paste(Y_VARIABLE, "~.")), 
    data = train,
    kernel = "radial",
    sigma = svm_model_opt$bestTune$sigma,
    cost = svm_model_opt$bestTune$C,
    scale = TRUE
  )
  
  p_svm <- predict(svm_model_best, newdata = test)
  p_svm <- as.numeric(p_svm)
  
  rmse_svm <- sqrt(mean((y_real - p_svm)^2))
  print(paste("RMSE for SVM:", rmse_svm))
  
  
  
  ## Modèle Random Forest ----
  set.seed(123)
  n <- nrow(DATAFRAME)
  train_size <- round(0.8 * n)
  train <- DATAFRAME[1:train_size, ]
  test <- DATAFRAME[(train_size+1):n, ]
  rf_model <- randomForest(formula(paste(Y_VARIABLE, "~", ".")), data = train, ntree=200)
  p_rf <- predict(rf_model, newdata = test)
  
  rmse_rf <- sqrt(mean((y_real - p_rf)^2))
  print(paste("RMSE Random Forest:", rmse_rf)) # 1.89
  
  
  # XGB BOOST----
  zz <- file("xgb_warnings.txt", open = "wt")
  sink(zz, type = "output")
  
  
  n <- nrow(DATAFRAME)
  train_size <- round(0.8 * n)
  train <- DATAFRAME[1:train_size, ]
  test <- DATAFRAME[(train_size + 1):n, ]
  xgb_grid <- expand.grid(
    nrounds = c(100, 200),
    eta = c(0.01, 0.1),
    max_depth = c(6, 10),
    gamma = c(0, 1),
    colsample_bytree = c(0.6, 0.8, 1),
    min_child_weight = c(1, 5),
    subsample = c(0.5, 0.75, 1)
  )
  tr_control <- trainControl(method = "cv", number = 10)
  xgb_model <- train(
    formula(paste(Y_VARIABLE, "~", ".")),
    data = train,
    method = "xgbTree",
    trControl = tr_control,
    tuneGrid = xgb_grid
  )
  
  set.seed(123)
  
  dtrain <- xgb.DMatrix(data = as.matrix(train[, -1]), label = train[[Y_VARIABLE]])
  dtest <- xgb.DMatrix(data = as.matrix(test[, -1]), label = test[[Y_VARIABLE]])
  
  params <- list(
    booster = "gbtree",
    objective = "reg:squarederror",
    eta = xgb_model$bestTune$eta,
    gamma = xgb_model$bestTune$gamma,
    max_depth = xgb_model$bestTune$max_depth,
    min_child_weight = xgb_model$bestTune$min_child_weight,
    subsample = xgb_model$bestTune$subsample,
    colsample_bytree = xgb_model$bestTune$colsample_bytree
  )
  
  xgb_model_opt <- xgb.train(params = params,
                             data = dtrain,
                             nrounds = 200)
  
  p_xgb <- predict(xgb_model_opt, newdata = dtest)
  p_xgb <- as.numeric(p_xgb)

  # Close the file and restore the output to the console
  sink()
  if (!isOpen(zz)) {
    close(zz)
  }
  
  # Print results
  rmse_xgb <- sqrt(mean((y_real - p_xgb) ^ 2))
  print(paste("RMSE for XGB:", rmse_xgb))
  
  
  
  # kNN Model ----
  set.seed(123)
  total_rows <- nrow(DATAFRAME)
  train_rows <- round(0.8 * total_rows)
  train_set <- DATAFRAME[1:train_rows, ]
  test_set <- DATAFRAME[(train_rows + 1):total_rows, ]
  train_labels <- train_set[[Y_VARIABLE]]
  train_data <- train_set[, -1]
  test_labels <- test_set[[Y_VARIABLE]]
  test_data <- test_set[, -1]
  
  
  max_k <- sqrt(train_rows)
  k_grid <- expand.grid(k = seq(1, max_k, by = 1))
  trControl <- trainControl(method = "cv", number = 10)
  knn_model <- train(
    formula(paste(Y_VARIABLE, "~", ".")), data = train, method = "knn", tuneGrid = k_grid, trControl = trControl)
  results <- knn_model$results
  min_rmse <- min(results$RMSE, na.rm = TRUE)
  
  knn_model <- train(
    formula(paste(Y_VARIABLE, "~", ".")), 
    data = train_set, method = "knn", trControl = trControl, tuneGrid = expand.grid(k = 8))
  
  p_knn <- predict(knn_model, newdata = test_data)
  p_knn <- as.numeric(p_knn)
  
  rmse_knn <- sqrt(mean((y_real - p_knn)^2))
  print(paste("RMSE for kNN:", rmse_knn))
  
  # Store the results in the R Global Environment
  assign("p_mlp", p_mlp, envir = .GlobalEnv)
  assign("p_mars", p_mars, envir = .GlobalEnv)
  assign("p_svm", p_svm, envir = .GlobalEnv)
  assign("p_rf", p_rf, envir = .GlobalEnv)
  assign("p_xgb", p_xgb, envir = .GlobalEnv)
  assign("p_knn", p_knn, envir = .GlobalEnv)
  
 
}
ml_models(result_df, "Food")

# FUNCTION PLOT----
plot_models <- function() {
  ## Create the results dataframe----
  prev_df <- data.frame(
    ARMAX = p_armax,
    ARX = p_arx,
    LM = p_lm,
    ARX_GET = p_arxget,
    AR1 = p_ar1,
    GAM = p_gam,
    LM = p_lm,
    MLP = p_mlp,
    MARS = p_mars,
    SVM = p_svm,
    RF = p_rf,
    XGB = p_xgb,
    kNN = p_knn,
    y_real = y_real
  )
  
  prev_df$id <- seq.int(nrow(prev_df))
  
  ## Plot Econometrics Models----
  eco_plot <- ggplot(prev_df, aes(x = id, group = 1)) +
    geom_line(aes(y = y_real, color = "Observé"),
              size = 2,
              show.legend = TRUE) +
    geom_line(aes(y = ARMAX, color = "ARMAX"),
              size = 1,
              alpha = 0.7) +
    geom_line(aes(y = ARX, color = "ARX"), size = 1, alpha = 0.7) +
    geom_line(aes(y = LM, color = "LM"), size = 1, alpha = 0.7) +
    geom_line(aes(y = ARX_GET, color = "ARX_GETS"),
              size = 1,
              alpha = 0.7) +
    geom_line(aes(y = AR1, color = "AR1"), size = 1, alpha = 0.7) +
    scale_color_manual(
      values = c(
        "Observé" = "black",
        "ARMAX" = "purple",
        "ARX" = "green",
        "LM" = "darkblue",
        "NAIVE" = "orange",
        "ARX_GETS" = "red",
        "AR1" = "darkcyan"
      )
    ) +
    labs(x = "Time", y = "HKE LPUE Forecast", color = "Models") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    ggtitle("Econometrics models forecasting")
  
  print(eco_plot)
  
  
  ## Plot ML Models----
  ml_plot <- ggplot(prev_df, aes(x = id, group = 1)) +
    geom_line(aes(y = y_real, color = "Observé"),
              size = 2,
              show.legend = TRUE) +
    geom_line(aes(y = GAM, color = "GAM"), size = 1, alpha = 1) +
    geom_line(aes(y = MLP, color = "MLP"), size = 1, alpha = 0.7) +
    geom_line(aes(y = MARS, color = "MARS"),
              size = 1,
              alpha = 0.7) +
    geom_line(aes(y = SVM, color = "SVM"), size = 1, alpha = 0.7) +
    geom_line(aes(y = RF, color = "RF"), size = 1, alpha = 0.7) +
    geom_line(aes(y = XGB, color = "XGB"), size = 1, alpha = 0.7) +
    geom_line(aes(y = kNN, color = "kNN"), size = 1, alpha = 0.7) +
    scale_color_manual(
      values = c(
        "Observé" = "black",
        "GAM" = "purple",
        "MLP" = "green",
        "MARS" = "darkblue",
        "SVM" = "orange",
        "RF" = "red",
        "XGB" = "cyan",
        "kNN" = "magenta1",
        "LSTM" = "grey",
        "LSTM_CNN" = "indianred",
        "TDNN" = "gold"
      )
    ) +
    labs(x = "Temps", y = "HKE LPUE Forecast", color = "Models") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    ggtitle("Machine-Learning models forecasting")
  
  print(ml_plot)
  
  assign("prev_df", prev_df, envir = .GlobalEnv)
  
}
plot_models()

# INDICATORS COMPUTING----
indic_models <- function(){
  mse <- sapply(prev_df[, 1:ncol(prev_df)], function(x) mean((prev_df$y_real - x)^2))
  rmse <- sqrt(mse)
  ccsed <- sapply(prev_df[, 1:ncol(prev_df)], function(x) sqrt(sum((cumsum(x - prev_df$y_real))^2)) - sqrt(sum((cumsum(prev_df$ar1 - prev_df$y_real))^2)))
  r2oos <- 1 - mse / mean((prev_df$y_real - prev_df$AR1)^2)
  indic <- data.frame(MSE = mse, RMSE = rmse, CCSED = ccsed, R2_OOS = r2oos)
  print(indic)
  assign("indic", indic, envir = .GlobalEnv)
  
  
}
indic_models()

# CPSE PLOTS & VALUES----
cspe <- function(){
  ## COMPUTE CSPE VALUES----
  prev_df$CSPEarmax <- cumsum((prev_df$y_real - prev_df$ARMAX)^2)
  prev_df$CSPEarx <- cumsum((prev_df$y_real - prev_df$ARX)^2)
  prev_df$CSPElm <- cumsum((prev_df$y_real - prev_df$LM)^2)
  prev_df$CSPEarxget <- cumsum((prev_df$y_real - prev_df$ARX_GET)^2)
  prev_df$CSPEar1 <- cumsum((prev_df$y_real - prev_df$AR1)^2)
  prev_df$CSPEGAM <- cumsum((prev_df$y_real - prev_df$GAM)^2)
  prev_df$CSPEMLP <- cumsum((prev_df$y_real - prev_df$MLP)^2)
  prev_df$CSPEMARS <- cumsum((prev_df$y_real - prev_df$MARS)^2)
  prev_df$CSPESVM <- cumsum((prev_df$y_real - prev_df$SVM)^2)
  prev_df$CSPERF <- cumsum((prev_df$y_real - prev_df$RF)^2)
  prev_df$CSPEXGB <- cumsum((prev_df$y_real - prev_df$XGB)^2)
  prev_df$CSPEkNN <- cumsum((prev_df$y_real - prev_df$kNN)^2)
  
  ## PLOTTING----
  ### GENERAL PLOT
  all <- ggplot(prev_df, aes(x = id, group = 1)) +
    geom_line(aes(y = CSPEarmax, color = "ARMAX"), size = 1) +
    geom_line(aes(y = CSPEarx, color = "ARX"), size = 1) +
    geom_line(aes(y = CSPElm, color = "LM"), size = 1) +
    geom_line(aes(y = CSPEarxget, color = "ARX_GETS"), size = 1) +
    geom_line(aes(y = CSPEar1, color = "AR1"), size = 1) +
    geom_line(aes(y = CSPEGAM, color = "GAM"), size = 1) +
    geom_line(aes(y = CSPEMLP, color = "MLP"), size = 1) +
    geom_line(aes(y = CSPEMARS, color = "MARS"), size = 1) +
    geom_line(aes(y = CSPESVM, color = "SVM"), size = 1) +
    geom_line(aes(y = CSPERF, color = "RF"), size = 1) +
    geom_line(aes(y = CSPEXGB, color = "XGB"), size = 1) +
    geom_line(aes(y = CSPEkNN, color = "kNN"), size = 1) +
    scale_color_manual(
      values = c(
        "ARMAX" = "purple",
        "ARX" = "green",
        "LM" = "darkblue",
        "NAIVE" = "orange",
        "ARX_GETS" = "cyan",
        "AR1" = "red",
        "GAM" = "gold2",
        "MLP" = "grey",
        "MARS" = "pink",
        "SVM" = "brown",
        "RF" = "darkgreen",
        "XGB" = "blue",
        "kNN" = "magenta",
        "LSTM" = "chocolate1",
        "LSTMCNN" = "black",
        "TDNN" = "seagreen2"
      )
    ) +
    labs(x = "Time", y = "CSPE", color = "Models") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    ggtitle(" All models CSPE's")
  
  print(all)
  
  ### ML VS ECO MODELS PLOT
  sep <- ggplot(prev_df, aes(x=id, group=1)) +
    geom_line(aes(y=CSPEarmax, color="Econometrics"), size=1,alpha=0.7) +
    geom_line(aes(y=CSPEarx, color="Econometrics"), size=1,alpha=0.7) +
    geom_line(aes(y=CSPElm, color="Econometrics"), size=1,alpha=0.7) +
    geom_line(aes(y=CSPEarxget, color="Econometrics"), size=1,alpha=0.7) +
    geom_line(aes(y=CSPEGAM, color="Econometrics"), size=1,alpha=0.7) +
    geom_line(aes(y = CSPEMLP, color = "Machine-Learning"), size = 1,alpha=0.7) +
    geom_line(aes(y = CSPEMARS, color = "Machine-Learning"), size = 1,alpha=0.7) +
    geom_line(aes(y = CSPESVM, color = "Machine-Learning"), size = 1,alpha=0.7) +
    geom_line(aes(y = CSPERF, color = "Machine-Learning"), size = 1,alpha=0.7) +
    geom_line(aes(y = CSPEXGB, color = "Machine-Learning"), size = 1,alpha=0.7) +
    geom_line(aes(y = CSPEkNN, color = "Machine-Learning"), size = 1,alpha=0.7) +
    scale_color_manual("", values=c("Econometrics" = "blue", "Machine-Learning" = "red"))+
    labs(x = "Time", y = "CSPE", color = "Models") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    ggtitle(" Machine-Learning vs Econometrics models CSPE's")
  
  plot(sep)
  
}
cspe()

# DIEBOLD MARIANO TEST----
diebold <- function(){
  
  ## CREATING DATA FRAME----
  dm_test_results <- data.frame(matrix(NA, nrow = 12, ncol = 12))
  colnames(dm_test_results) <- c("ARMAX", "ARX", "ARX_GETS", "LM","AR1",
                                 "GAM","MLP","MARS","SVM","RF","XGB","kNN")
  rownames(dm_test_results) <- c("ARMAX", "ARX", "ARX_GETS", "LM","AR1",
                                 "GAM","MLP","MARS","SVM","RF","XGB","kNN")
  
  
  diebold_df <- data.frame(Darmax = prev_df$y_real - prev_df$ARMAX,
                           Darx = prev_df$y_real - prev_df$ARX,
                           Dlm = prev_df$y_real - prev_df$LM,
                           Darxget = prev_df$y_real - prev_df$ARX_GET,
                           Dgam = prev_df$y_real - prev_df$GAM,
                           Dmlp = prev_df$y_real - prev_df$MLP,
                           Dmars = prev_df$y_real - prev_df$MARS,
                           Dsvm = prev_df$y_real - prev_df$SVM,
                           Drf = prev_df$y_real - prev_df$RF,
                           Dxgb = prev_df$y_real - prev_df$XGB,
                           Dknn = prev_df$y_real - prev_df$kNN,
                           Dar1 = prev_df$y_real - prev_df$AR1)
  
  
  # TESTING // AR1----
  
  cat("\n")
  cat("ARMAX:\n")
  print(dm.test(diebold_df$Dar1, diebold_df$Darmax, alternative = "less", h = 1))
  
  cat("\n")
  cat("\nARX:\n")
  print(dm.test(diebold_df$Dar1, diebold_df$Darx, alternative = "less", h = 1))
  
  cat("\n")
  cat("\nLM:\n")
  print(dm.test(diebold_df$Dar1, diebold_df$Dlm, alternative = "less", h = 1))
  
  cat("\n")
  cat("\nARXGET:\n")
  print(dm.test(diebold_df$Dar1, diebold_df$Darxget, alternative = "less", h = 1))
  
  cat("\n")
  cat("\nGAM:\n")
  print(dm.test(diebold_df$Dar1, diebold_df$Dgam, alternative = "less", h = 1))
  
  cat("\n")
  cat("\nMLP:\n")
  print(dm.test(diebold_df$Dar1, diebold_df$Dmlp, alternative = "less", h = 1))
  
  cat("\n")
  cat("\nMARS:\n")
  print(dm.test(diebold_df$Dar1, diebold_df$Dmars, alternative = "less", h = 1))
  
  cat("\n")
  cat("\nSVM:\n")
  print(dm.test(diebold_df$Dar1, diebold_df$Dsvm, alternative = "less", h = 1))
  
  cat("\n")
  cat("\nRandom Forest:\n")
  print(dm.test(diebold_df$Dar1, diebold_df$Drf, alternative = "less", h = 1))
  
  cat("\n")
  cat("\nXGB:\n")
  print(dm.test(diebold_df$Dar1, diebold_df$Dxgb, alternative = "less", h = 1))
  
  cat("\n")
  cat("\nKNN:\n")
  print(dm.test(diebold_df$Dar1, diebold_df$Dknn, alternative = "less", h = 1))
  
  
  
}
diebold()

