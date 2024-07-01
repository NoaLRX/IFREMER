library(readxl)
library(ggplot2)

gva <- read_xlsx("data/fbar_gva.xlsx", sheet = 1)
fbar <- read_xlsx("data/fbar_gva.xlsx", sheet = 2)
fbar_t <- t(fbar)
fbar_t <- data.frame(FBAR2025 = colnames(fbar)[-1], t(fbar[-1]))
colnames(fbar_t)[-1] <- fbar[[1]]
row.names(fbar_t) <- NULL


head(fbar_t)
head(gva)



# Création du dataframe
data <- data.frame(
  scenario = c("SQ", "A", "D", "L"),
  gva_moyennes = c(1.1490955, 0.8275081, 0.7686793, 1.1498247),
  fbar_moyennes = c(2.2827133, 0.5801356, 0.5791226, 2.3088715)
)

# Création du graphique avec ggplot2
ggplot(data, aes(x = gva_moyennes, y = fbar_moyennes, label = scenario, shape = scenario)) +
  geom_point(size = 4, color = "blue") +
  geom_text(hjust = -0.3, vjust = 0) +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  labs(x = "GVA Moyenne", 
       y = "FBAR Moyenne", 
       title = "Relation entre GVA et FBAR par scénario",
       shape = "Scénario") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray", linetype = "dashed"),
        panel.grid.minor = element_line(color = "lightgray", linetype = "dotted")) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black")












library(ggplot2)

set.seed(123) # Pour la reproductibilité

# Fonction pour générer des données fictives
generate_fictional_data <- function(data, variation = 0.50) {
  new_data <- data
  new_data$gva_moyennes <- new_data$gva_moyennes * runif(nrow(new_data), 1 - variation, 1 + variation)
  new_data$fbar_moyennes <- new_data$fbar_moyennes * runif(nrow(new_data), 1 - variation, 1 + variation)
  return(new_data)
}

# Données IAM
data_IAM <- data.frame(
  scenario = c("A", "D", "L"),
  gva_moyennes = c( 0.78, 0.73, 1.01),
  fbar_moyennes = c( 0.5801356, 0.5791226, 2.3088715),
  model = "IAM"
)

# Données BEMTOOL
data_BEMTOOL <- data.frame(
  scenario = c("A", "D", "L"),
  gva_moyennes = c(0.38, 0.47, -1.01),
  fbar_moyennes = c(0.79, 0.74, 1.07),
  model = "BEMTOOL"
)

# Données Fictive
data_FICTIV <- data.frame(
  scenario = c("A", "D", "L"),
  gva_moyennes = c(0.65, 0.47, -1.01),
  fbar_moyennes = c(0.56, 0.44, 2.20),
  model = "Fake Model 3"
)

# Données Fictive 2
data_FICTIV2 <- data.frame(
  scenario = c("A", "D", "L"),
  gva_moyennes = c(0.35, 0.77, -1.20),
  fbar_moyennes = c(0.49, 0.44, 1.3),
  model = "Fake Model 4"
)

# # Générer des données pour 3 modèles fictifs
# data_model1 <- generate_fictional_data(data_original)
# data_model1$model <- "Model 1"
# 
# data_model2 <- generate_fictional_data(data_original)
# data_model2$model <- "Model 2"
# 
# data_model3 <- generate_fictional_data(data_original)
# data_model3$model <- "Model 3"

# Combiner toutes les données
all_data <- rbind(data_IAM, data_BEMTOOL)

# Créer le graphique
ggplot(all_data, aes(x = gva_moyennes, y = fbar_moyennes, label = scenario, shape = scenario, color = model)) +
  geom_point(size = 4, stroke = 1.5, fill = NA) +
  geom_text(hjust = -0.3, vjust = 0, show.legend = FALSE) +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  scale_color_manual(values = c("darkblue", "red", "darkgreen", "purple")) +
  labs(x = "Ratio GVA 2025 /  SQ 2025", 
       y = "Rato F2025 / FMSY", 
       title = "Relation between GVA & FBAR per scenario & model",
       shape = "Scénario",
       color = "Modèle") +
  theme_bw() +
  theme(panel.grid.major = element_line(color = "gray", linetype = "dashed"),
        panel.grid.minor = element_line(color = "lightgray", linetype = "dotted")) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", linewidth = 1.5) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", linewidth = 1.5)+
  theme(plot.title = element_text(face = "bold")) 





library(ggforce)

library(ggforce)

ggplot(all_data, aes(x = gva_moyennes, y = fbar_moyennes, color = model, shape = scenario)) +
  geom_point(size = 4)
  scale_shape_manual(values = c(15, 16, 17, 18),) +
  scale_fill_manual(values = c("red", "green", "blue", "orange")) +
  scale_color_manual(values = c("darkblue", "orange", "darkgreen", "purple")) +
  labs(x = "Ratio GVA 2025 / GVA 2022",
       y = "Rato F2025 / FMSY",
       title = "Relation between GVA & FBAR per scenario & model",
       shape = "Scénario",
       color = "Modèle") +
  theme_bw() +
  theme(panel.grid.major = element_line(color = "gray", linetype = "dashed"),
        panel.grid.minor = element_line(color = "lightgray", linetype = "dotted")) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", linewidth = 1.5) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", linewidth = 1.5) +
  theme(plot.title = element_text(face = "bold"))




