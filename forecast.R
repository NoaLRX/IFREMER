library(gets)
library(mgcv)
library(neuralnet)
library(earth)
library(e1071)
library(caret)
library(randomForest)
library(xgboost)
library(class)

########################### PLEASE READ ME ####################################
################################################################################
# This script contains two functions that use different forecasting models. 
# The first "eco_models" function uses 6 econometric forecasting models, while 
# the second "ml_models" function uses 6 Machine Learning and Deep Learning models.

# To use the functions, you need to have a dataframe with your Y variable and
# your X variables. Note that your Y variable should be the first columns of the dataframe.

# The functions only take two arguments: your dataframe with all your variables
# (DATAFRAME) and the name of the Y variable column from your dataframe (Y_VARIABLE).
# For example if I have a dataframe called "df" whose first columns is my Y variable
# df$Y_variable, then I can use the function by typing eco_models(df, "Y_variable")
################################################################################
################################################################################


eco_models <- function(DATAFRAME, Y_VARIABLE){
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
  model_ar1 <- Arima(y, order = c(1, 0, 0), seasonal = list(order = c(0, 0, 0), period=12),lambda=1)
  forecast_ar1 <- forecast(model_ar1, h = 57)
  p_ar1 <- forecast_ar1$mean
  p_ar1 <- as.numeric(p_ar1)
  rmse_ar1 <- sqrt(mean((y_real - p_ar1)^2, na.rm = TRUE))
  print(paste("RMSE AR1: ", rmse_ar1))
  
  
  ## GAM model----
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

ml_models <- function(DATAFRAME, Y_VARIABLE){
  
  ## MLP model----
  set.seed(123)
  n <- nrow(DATAFRAME)
  train_size <- round(0.8 * n)
  train <- DATAFRAME[1:train_size, ]
  test <- DATAFRAME[(train_size+1):n, ]
  
  mlp_model <- neuralnet(formula(paste(Y_VARIABLE, "~", ".")), data = train, hidden = 1)
  
  p_mlp <- compute(mlp_model, test[,-1])
  p_mlp <- as.numeric(p_mlp$net.result)
  
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
}



eco_models(HKE_df, "ts_HKE1567_adj")
ml_models(HKE_df, "ts_HKE1567_adj")


# Probleme avec le modele GAM qui projette sur 7 valeurs au lieu de 8
prev_df <- data.frame(
  ARMAX = p_armax,
  ARX = p_arx,
  LM = p_lm,
  ARX_GET = p_arxget,
  #AR1 = p_ar1,
  GAM = p_gam,
  LM = p_lm,
  MLP = p_mlp,
  MARS = p_mars,
  SVM = p_svm,
  RF = p_rf,
  XGB = p_xgb,
  #kNN = p_knn,
  #LSTM = p_lstm,
  y_real = y_real
)






## GAM model----
## Data preparation
set.seed(123)
smp_size <- ceiling(0.80 * nrow(HKE_df))
train_ind <- sample(seq_len(nrow(HKE_df)), size = smp_size)
train <- HKE_df[train_ind, ]
test <- HKE_df[-train_ind, ]

other_vars <- setdiff(names(train), ts_HKE1567_adj)
smooth_terms <- paste0("s(", other_vars, ")")
model_formula <- reformulate(termlabels = smooth_terms, response = ts_HKE1567_adj)

model_gam <- gam(model_formula, data = train)

## Prediction and RMSE calculation
p_gam <- predict(model_gam, newdata = test)
p_gam <- as.numeric(p_gam)
length(p_gam)
length(test)
y_real <- test[[ts_HKE1567_adj]]

rmse_gam <- sqrt(mean((y_real - p_gam)^2, na.rm = TRUE))
print(paste("RMSE GAM :", rmse_gam))