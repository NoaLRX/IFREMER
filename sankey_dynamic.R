# Dynamic Sankey's Diagram----
library(plotly)
library(dplyr)
library(RColorBrewer)

# Helper function to create Sankey diagram
create_sankey <- function(data, title, subtitle, value_col, is_contribution = FALSE, color_palette = NULL, highlight_fleet = NULL) {
  # Renommer la colonne de valeur en 'value' pour la cohérence
  data <- data %>% rename(value = !!as.name(value_col))
  
  # Déterminer les colonnes source et target en fonction du type de diagramme
  if (is_contribution) {
    source_col <- "X3A_CODE"
    target_col <- "FleetIAM"
    color_by <- "X3A_CODE"
  } else {
    source_col <- "FleetIAM"
    target_col <- "X3A_CODE"
    color_by <- "FleetIAM"
  }
  
  # Préparation des données
  all_nodes <- unique(c(data[[source_col]], data[[target_col]]))
  node_labels <- all_nodes
  
  data <- data %>%
    mutate(
      source = match(!!as.name(source_col), all_nodes) - 1,
      target = match(!!as.name(target_col), all_nodes) - 1
    )
  
  # Création d'une palette de couleurs si non fournie
  if (is.null(color_palette)) {
    n_colors <- length(unique(data[[color_by]]))
    color_palette <- colorRampPalette(brewer.pal(8, if(is_contribution) "Set2" else "Set1"))(n_colors)
    names(color_palette) <- unique(data[[color_by]])
  }
  
  # Attribution des couleurs aux liens
  link_colors <- color_palette[data[[color_by]]]
  
  # Normalisation des valeurs pour ajuster l'opacité
  min_val <- min(data$value, na.rm = TRUE)
  max_val <- max(data$value, na.rm = TRUE)
  norm_values <- (data$value - min_val) / (max_val - min_val)
  
  # Ajustement des couleurs et de l'opacité
  link_colors_adjusted <- sapply(1:length(link_colors), function(i) {
    adjustcolor(link_colors[i], alpha.f = 0.4 + 0.6 * norm_values[i])
  })
  
  link_order <- rep(0, length(data[[source_col]]))
  
  if (!is.null(highlight_fleet)) {
    highlight_indices <- which(data[[source_col]] == highlight_fleet | data[[target_col]] == highlight_fleet)
    link_colors_adjusted[highlight_indices] <- link_colors[highlight_indices]
    link_order[highlight_indices] <- 1
  }
  
  # Définir les couleurs des nœuds
  node_colors <- c(color_palette, rep("#CCCCCC", length(unique(data[[target_col]]))))
  
  fig <- plot_ly(
    type = "sankey",
    orientation = "h",
    node = list(
      label = node_labels,
      color = node_colors,
      pad = 15,
      thickness = 20,
      line = list(color = "black", width = 0.5)
    ),
    link = list(
      source = data$source,
      target = data$target,
      value = data$value,
      color = link_colors_adjusted,
      label = paste(data[[source_col]], "->", data[[target_col]]),
      customdata = data[[color_by]],
      ordering = link_order
    )
  )
  
  fig <- fig %>% layout(
    title = list(text = paste0(title, "<br>", "<sup>", subtitle, "</sup>"), font = list(size = 16)),
    font = list(size = 10),
    xaxis = list(showgrid = FALSE, zeroline = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE)
  )
  
  return(fig)
}

# Function for Dependence ALL diagram
create_depend_all <- function() {
  # DEPENDANCE ALL----
  #' This one will plot the dependance (in term of landings in values) for 5 most
  #' important fleets and the 10 most important species (excluding ZZZ) from 2013
  #' to 2022. You should adjust the number of fleets and species you want to display
  #' in the values of "tentaxons" and "tenfleets", according to your data.
  select <- landings %>%
    select(X3A_CODE, totvallandg) %>%
    group_by(X3A_CODE) %>%
    summarise(totvallandg = sum(totvallandg, na.rm = TRUE),
              .groups = "drop") %>%
    arrange(desc(totvallandg))
  
  # Let's take the  most important species in term of landings (euros) across 2013:2022
  tentaxons <- unique(head(select["X3A_CODE"], n = 10))
  tentaxons <- unlist(tentaxons)
  
  select2 <- landings %>%
    select(FleetIAM, totvallandg) %>%
    group_by(FleetIAM) %>%
    summarise(totvallandg = sum(totvallandg, na.rm = TRUE),
              .groups = "drop") %>%
    arrange(desc(totvallandg))
  
  # Let's take the  most important fleet in term of landings (euros) across 2013:2022
  tenfleets <- unique(head(select2["FleetIAM"], n = 5))
  tenfleets <- unlist(tenfleets)
  
  sankey1 <- landings %>%
    filter (COUNTRY == "FRA") %>%
    filter (X3A_CODE != "ZZZ") %>% # Let's get rid of ZZZ species
    filter (X3A_CODE %in% tentaxons) %>%
    filter (FleetIAM %in% tenfleets) %>%
    select (FleetIAM, X3A_CODE, totvallandg) %>%
    group_by(FleetIAM, X3A_CODE) %>%
    summarise(totvallandg = sum(totvallandg, na.rm = TRUE),
              .groups = "drop")
  
  create_sankey(sankey1, "Fleets dependencies to each species (2013-2022)", 
                "6 most important fleets and 10 most important species, both in term of landings (euros)",
                "totvallandg", is_contribution = FALSE)
}

# Function for Dependence 2021-2022 diagram
create_depend_2122 <- function() {
  select <- landings %>%
    filter(YEAR == c("2021", "2022")) %>%
    select(X3A_CODE, totvallandg) %>%
    group_by(X3A_CODE) %>%
    summarise(totvallandg = sum(totvallandg, na.rm = TRUE),
              .groups = "drop") %>%
    arrange(desc(totvallandg))
  
  # Let's take the  most important species in term of landings (euros) across 2013:2022
  tentaxons <- unique(head(select["X3A_CODE"], n = 10))
  tentaxons <- unlist(tentaxons)
  
  select2 <- landings %>%
    select(FleetIAM, totvallandg) %>%
    group_by(FleetIAM) %>%
    summarise(totvallandg = sum(totvallandg, na.rm = TRUE),
              .groups = "drop") %>%
    arrange(desc(totvallandg))
  
  # Let's take the  most important fleet in term of landings (euros) across 2013:2022
  tenfleets <- unique(head(select2["FleetIAM"], n = 5))
  tenfleets <- unlist(tenfleets)
  
  sankey2 <- landings %>%
    filter (COUNTRY == "FRA") %>%
    filter (X3A_CODE != "ZZZ") %>%
    filter (X3A_CODE %in% tentaxons) %>%
    filter (FleetIAM %in% tenfleets) %>%
    select (FleetIAM, X3A_CODE, totvallandg) %>%
    group_by(FleetIAM, X3A_CODE) %>%
    summarise(totvallandg = sum(totvallandg, na.rm = TRUE),
              .groups = "drop")
  
  create_sankey(sankey2, "Fleets dependencies to each species (2021-2022)", 
                "6 most important fleets and 10 most important species, both in term of landings (euros)",
                "totvallandg", is_contribution = FALSE)
}

# Function for Contribution ALL diagram
create_contrib_all <- function() {
  select <- landings %>%
    select(X3A_CODE, totwghtlandg) %>%
    group_by(X3A_CODE) %>%
    summarise(totwghtlandg = sum(totwghtlandg, na.rm = TRUE),
              .groups = "drop") %>%
    arrange(desc(totwghtlandg))
  
  # Let's take the  most important species in term of landings kg across 2013:2022
  tentaxons <- unique(head(select["X3A_CODE"], n = 10))
  tentaxons <- unlist(tentaxons)
  
  select2 <- landings %>%
    select(FleetIAM, totwghtlandg) %>%
    group_by(FleetIAM) %>%
    summarise(totwghtlandg = sum(totwghtlandg, na.rm = TRUE),
              .groups = "drop") %>%
    arrange(desc(totwghtlandg))
  
  # Let's take the  most important fleet in term of landings kg across 2013:2022
  tenfleets <- unique(head(select2["FleetIAM"], n = 6))
  tenfleets <- unlist(tenfleets)
  
  sankey3 <- landings %>%
    filter (COUNTRY == "FRA") %>%
    filter (X3A_CODE != "ZZZ") %>% # Let's get rid of ZZZ species
    filter (X3A_CODE %in% tentaxons) %>%
    filter (FleetIAM %in% tenfleets) %>%
    select (X3A_CODE, FleetIAM, totwghtlandg) %>%
    group_by(X3A_CODE, FleetIAM) %>%
    summarise(totwghtlandg = sum(totwghtlandg, na.rm = TRUE),
              .groups = "drop")
  
  # Création d'une palette de couleurs pour les espèces
  n_species <- length(unique(sankey3$X3A_CODE))
  species_colors <- colorRampPalette(brewer.pal(8, "Set2"))(n_species)
  names(species_colors) <- unique(sankey3$X3A_CODE)
  
  create_sankey(sankey3, "Contribution of each species to landings of each fleet (2013-2022)", 
                "6 most important fleets and 10 most important species, both in term of landings (kg)",
                "totwghtlandg", is_contribution = TRUE)
}


# Function for Contribution 2021-2022 diagram
create_contrib_2122 <- function() {
  select <- landings %>%
    filter(YEAR == c("2021", "2022")) %>%
    select(X3A_CODE, totwghtlandg) %>%
    group_by(X3A_CODE) %>%
    summarise(totwghtlandg = sum(totwghtlandg, na.rm = TRUE),
              .groups = "drop") %>%
    arrange(desc(totwghtlandg))
  
  # Let's take the  most important species in term of landings kg across 2021:2022
  tentaxons <- unique(head(select["X3A_CODE"], n = 10))
  tentaxons <- unlist(tentaxons)
  
  select2 <- landings %>%
    select(FleetIAM, totwghtlandg) %>%
    group_by(FleetIAM) %>%
    summarise(totwghtlandg = sum(totwghtlandg, na.rm = TRUE),
              .groups = "drop") %>%
    arrange(desc(totwghtlandg))
  
  # Let's take the  most important fleet in term of landings kg across 2021:2022
  tenfleets <- unique(head(select2["FleetIAM"], n = 6))
  tenfleets <- unlist(tenfleets)
  
  sankey4 <- landings %>%
    filter (COUNTRY == "FRA") %>%
    filter (X3A_CODE != "ZZZ") %>% # Let's get rid of ZZZ species
    filter (X3A_CODE %in% tentaxons) %>%
    filter (FleetIAM %in% tenfleets) %>%
    select (X3A_CODE, FleetIAM, totwghtlandg) %>%
    group_by(X3A_CODE, FleetIAM) %>%
    summarise(totwghtlandg = sum(totwghtlandg, na.rm = TRUE),
              .groups = "drop")
  
  # Création d'une palette de couleurs pour les espèces
  n_species <- length(unique(sankey4$X3A_CODE))
  species_colors <- colorRampPalette(brewer.pal(8, "Set2"))(n_species)
  names(species_colors) <- unique(sankey4$X3A_CODE)
  
  
  create_sankey(sankey4, "Contribution of each species to landings of each fleet (2021-2022)", 
                "6 most important fleets and 10 most important species, both in term of landings (kg)",
                "totwghtlandg", is_contribution = TRUE)
}

# Create the combined plot with tabs
fig <- subplot(
  create_depend_all(),
  create_depend_2122(),
  create_contrib_all(),
  create_contrib_2122(),
  nrows = 1,
  shareX = TRUE,
  shareY = TRUE,
  titleX = TRUE,
  titleY = TRUE
) %>%
  layout(
    title = list(text = "Sankey Diagrams: Fleet Dependencies and Contributions", font = list(size = 20)),
    updatemenus = list(
      list(
        type = "buttons",
        direction = "right",
        x = 0.1,
        y = 1.2,
        buttons = list(
          list(
            method = "update",
            args = list(list(visible = c(
              TRUE, FALSE, FALSE, FALSE
            ))),
            label = "Dependence ALL"
          ),
          list(
            method = "update",
            args = list(list(visible = c(
              FALSE, TRUE, FALSE, FALSE
            ))),
            label = "Dependence 2021-2022"
          ),
          list(
            method = "update",
            args = list(list(visible = c(
              FALSE, FALSE, TRUE, FALSE
            ))),
            label = "Contribution ALL"
          ),
          list(
            method = "update",
            args = list(list(visible = c(
              FALSE, FALSE, FALSE, TRUE
            ))),
            label = "Contribution 2021-2022"
          )
        )
      )
    )
  )
# Display the figure
fig
#htmlwidgets::saveWidget(fig, "index.html", selfcontained = TRUE)