library(dplyr)
library(tidyr)
library(tsoutliers)
library(seastests)
library(tseries)
library(TSA)

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


# Correction of atypical points----
for (col_name in names(ts_list)) {
  ts_name <- paste0("ts_", col_name)
  ts_name_tso <- paste0("tso_", col_name)
  
  ts_data <- get(ts_name)
  
  # Try ARIMA models
  arima_fit <- tryCatch({
    fit <- tso(ts_data)
    cat("\033[1m\033[31m", "TSO for", col_name, ":\033[0m\n")
    print(fit)
    assign(ts_name_tso, fit)
    assign(ts_name, fit$yadj)
    
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
  
  # If ARIMA adjustment fail, go next
  if (!arima_fit) next
  
}

# We have an error for : ANE, NEP5, NEP6 and MUT6
# library(RJDemetra)
# myregx13 <- regarima_x13(ts_ANE, spec = "RG5c")
# myregx13$specification
# n <- nrow(ts_ANE)
# adjseries <- matrix(myregx13$model$effects[1:n])
# ts_ANE <- ts(data = adjseries, start=c(2013,01),frequency=4) 
# plot(ts_ANE)

# Seasonal Adjustment----
seasonal_combined_test <- c()
seasonal_seasdum <- c()

for (col_name in names(ts_list)) {
  ts_name <- paste0("ts_", col_name)  # Nom de la série temporelle ajustée
  ts_data <- get(ts_name)  # Récupérer les données de la série temporelle ajustée
  
  # Appliquer le test combined_test() sur la série temporelle ajustée
  ct_res <- combined_test(ts_data)
  
  # Vérifier chaque p-value et afficher les résultats si au moins une valide H1
  ct_results <- ct_res$stat
  ct_pvals <- ct_res$Pval
  if (ct_results == TRUE) {
    cat("\n")
    cat("\n")
    print(paste0("Résultats du combined_test pour la série ", ts_name))
    print(ct_res)
    seasonal_combined_test <- c(seasonal_combined_test, ts_name)
  }
  
  # Appliquer le test seasdum() sur la série temporelle ajustée
  sd_res <- seasdum(ts_data)
  
  # Vérifier la p-value et afficher les résultats si elle est < 0.05
  if (sd_res$Pval < 0.05) {
    cat("\n")
    cat("\n")
    print(paste0("Résultats du seasdum pour la série ", ts_name))
    print(sd_res)
    seasonal_seasdum <- c(seasonal_seasdum, ts_name)
  }
}

# Afficher les listes des séries présentant de la saisonnalité
print(seasonal_combined_test)
length(seasonal_combined_test) # 31

print(seasonal_seasdum)
length(seasonal_seasdum) #38


# Calculer les différences entre les deux listes
only_combined_test <- setdiff(seasonal_combined_test, seasonal_seasdum)
only_seasdum <- setdiff(seasonal_seasdum, seasonal_combined_test)
both_tests <- intersect(seasonal_combined_test, seasonal_seasdum)

# Afficher les différences
print(only_combined_test)
print(only_seasdum)
print(both_tests)



# Correction de la saisonnalité avec STL pour les séries dans both_tests
for (ts_name in both_tests) {
  ts_data <- get(ts_name)  # Récupérer les données de la série temporelle
  decomp <- stl(ts_data[,1], s.window = "periodic")  # Décomposition STL
  seasonal <- decomp$time.series[, "seasonal"]  # Obtenir la composante saisonnière
  ts_data_adjusted <- ts_data - seasonal  # Corriger la composante saisonnière
  assign(ts_name, ts_data_adjusted)  # Mettre à jour la série temporelle ajustée
}




# Vérification de la saisonnalité après correction
seasonal_combined_test_after <- c()
seasonal_seasdum_after <- c()

for (ts_name in both_tests) {
  ts_data <- get(ts_name)  # Récupérer les données de la série temporelle ajustée
  
  # Appliquer le test combined_test() sur la série temporelle ajustée
  ct_res <- combined_test(ts_data)
  
  # Vérifier chaque p-value et afficher les résultats si au moins une est < 0.05
  ct_pvals <- ct_res$Pval
  ct_results <- ct_res$stat
  if (ct_results == TRUE) {
    cat(bold(underline("\033[1m\033[31m", "Résultats du combined_test après correction pour la série ", ts_name, ":\033[0m\n")))
    print(ct_res)
    seasonal_combined_test_after <- c(seasonal_combined_test_after, ts_name)
  }
  
  # Appliquer le test seasdum() sur la série temporelle ajustée
  sd_res <- seasdum(ts_data)
  
  # Check and print results if p<0.05
  if (sd_res$Pval < 0.05) {
    cat(bold(underline("\033[1m\033[31m", "Résultats du seasdum après correction pour la série ", ts_name, ":\033[0m\n")))
    print(sd_res)
    seasonal_seasdum_after <- c(seasonal_seasdum_after, ts_name)
  }
}


cat(bold("\nSéries présentant encore de la saisonnalité selon combined_test après correction:\n"))
print(seasonal_combined_test_after)

cat(bold("\nSéries présentant encore de la saisonnalité selon seasdum après correction:\n"))
print(seasonal_seasdum_after)


for (x in seasonal_combined_test_after){
  cat(bold(underline("Résultats des tests pour la série", x,"\n")))
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



# Stationarity----
for (i in names(ts_list)) {
  ts_name <- paste0("ts_", i)  # Nom de la série temporelle
  adf_result <- adf.test(get(ts_name))  # Appliquer le test ADF
  adf_result
  if (adf_result$p.value > 0.05) {  # Vérifier la significativité statistique
    assign(ts_name, diff(get(ts_name)))  # Différencier la série temporelle
    print(paste("La série", ts_name, "a été différenciée"))
  }
}

for (i in names(ts_list)) {
  ts_name <- paste0("ts_", i)  # Nom de la série temporelle
  adf_result <- adf.test(get(ts_name))  # Appliquer le test ADF
  adf_result
  if (adf_result$p.value > 0.05) {  # Vérifier la significativité statistique
    print(paste("La série", ts_name, "n'est pas stationnaire"))
  }
}












