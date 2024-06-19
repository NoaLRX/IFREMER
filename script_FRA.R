library(dplyr)
library(tidyr)
library(readxl)
library(data.table)
library(ggcorrplot)
library(ggplot2)
library(patchwork)
library(reshape2)
library(forecast)
library(tseries)
library(tsoutliers)
library(gridExtra)
library(grid)
library(RColorBrewer)
library(ggalluvial)
library(cowplot)
library(waffle)
library(showtext)
library(ggtext)
library(markdown)
library(forcats)



#' Warning: This code is the French version of the script, we are only looking
#' at French fleets and landings.

# Importing, merging and cleaning DATA----
### Creation of the "IAM" fleet with table J
## Data importation
FDI_J <- read.csv("Data/Data_EWG2311/tableJ_EWG2311.csv")

#' We clear inactive boats, filter on EMU1 (GSA,1,2,5,6,7) and french fleets
FDI_J <- subset(FDI_J,
                fishing_tech != "INACTIVE" &
                  #country_code != "ITA" &
                  country_code == "FRA" &  
                  supra_region == "MBS" & 
                  principal_sub_region %in% c("GSA1", "GSA2", "GSA5", "GSA6", "GSA7"))


# Changing columns names
FDI_J <- FDI_J %>%
  rename(
    COUNTRY = country_code,
    YEAR = year,
    SUPRA_REGION = supra_region,
    FISHING_TECH = fishing_tech,
    VESSEL_LENGTH = vessel_length,
    GEO_INDICATOR = geo_indicator) 


### Creating the IAM fleets typo in table J
FDI_J <- FDI_J %>%
  mutate(FleetIAM = case_when(
    # COUNTRY == "ESP" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL0612") ~ "ESP_DTS_<12m",
    # COUNTRY == "ESP" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL1218") ~ "ESP_DTS_12-18m",
    # COUNTRY == "ESP" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL1824") ~ "ESP_DTS_18-24m",
    # COUNTRY == "ESP" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL2440") ~ "ESP_DTS_>=24m",
    # COUNTRY == "ESP" & FISHING_TECH == "DFN" & VESSEL_LENGTH %in% c("VL0612") ~ "ESP_DFN_06-12m",
    # COUNTRY == "ESP" & FISHING_TECH == "DFN" & VESSEL_LENGTH %in% c("VL1218") ~ "ESP_DFN_12-18m",
    # COUNTRY == "ESP" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL0612") ~ "ESP_HOK_06-12m",
    # COUNTRY == "ESP" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL1218") ~ "ESP_HOK_12-18m",
    # COUNTRY == "ESP" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL1824") ~ "ESP_HOK_18-24m",
    # COUNTRY == "ESP" & VESSEL_LENGTH %in% c("VL0006", "VL0612") ~ "Other_<=12m",
    # COUNTRY == "ESP" & VESSEL_LENGTH %in% c("VL1218", "VL1824","VL2440","VL40XX") ~ "Other_>=12m",
    # COUNTRY == "ESP" ~ "Other",
    COUNTRY == "FRA" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL1824") ~ "FRA_DTS_18-24m",
    COUNTRY == "FRA" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL2440") ~ "FRA_DTS_>24m",
    COUNTRY == "FRA" & FISHING_TECH == "DFN" & VESSEL_LENGTH %in% c("VL0006") ~ "FRA_DFN_00-06m",
    COUNTRY == "FRA" & FISHING_TECH == "DFN" & VESSEL_LENGTH %in% c("VL0612") ~ "FRA_DFN_06-12m",
    COUNTRY == "FRA" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL0006") ~ "FRA_HOK_00-06m",
    COUNTRY == "FRA" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL0612") ~ "FRA_HOK_06-12m",
    COUNTRY == "FRA" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL1218") ~ "FRA_HOK_12-18m",
    COUNTRY == "FRA" & VESSEL_LENGTH %in% c("VL0006", "VL0612") ~ "Other_<=12m",
    COUNTRY == "FRA" & VESSEL_LENGTH %in% c("VL1218", "VL1824","VL2440","VL40XX") ~ "Other_>=12m",
    COUNTRY == "FRA" ~ "Other",
    TRUE ~ NA_character_))


### Creating a segment column
FDI_J <- FDI_J %>%
  mutate(Segment = case_when(
    # COUNTRY == "ESP" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL0612") ~ "Spanish trawlers < 12m",
    # COUNTRY == "ESP" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL1218") ~ "Spanish trawlers 12-18m",
    # COUNTRY == "ESP" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL1824") ~ "Spanish trawlers 18-24m",
    # COUNTRY == "ESP" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL2440") ~ "Spanish trawlers >=24m",
    # COUNTRY == "ESP" & FISHING_TECH == "DFN" & VESSEL_LENGTH %in% c("VL0612") ~ "Spanish netters 06-12m",
    # COUNTRY == "ESP" & FISHING_TECH == "DFN" & VESSEL_LENGTH %in% c("VL1218") ~ "Spanish netters 12-18m",
    # COUNTRY == "ESP" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL0612") ~ "Spanish vessels using hooks 06-12m",
    # COUNTRY == "ESP" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL1218") ~ "Spanish vessels using hooks 12-18m",
    # COUNTRY == "ESP" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL1824") ~ "Spanish vessels using hooks 18-24m",
    # COUNTRY == "ESP" & VESSEL_LENGTH %in% c("VL0006", "VL0612") ~ "Other inf 12m",
    # COUNTRY == "ESP" & VESSEL_LENGTH %in% c("VL1218", "VL1824","VL2440","VL40XX") ~ "Other sup 12m",
    # COUNTRY == "ESP" ~ "Other",
    COUNTRY == "FRA" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL1824") ~ "French demersal trawlers 18-24m",
    COUNTRY == "FRA" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL2440") ~ "French demersal trawlers 24-40m",
    COUNTRY == "FRA" & FISHING_TECH == "DFN" & VESSEL_LENGTH %in% c("VL0006") ~ "French netters < 6m",
    COUNTRY == "FRA" & FISHING_TECH == "DFN" & VESSEL_LENGTH %in% c("VL0612") ~ "French netters 06-12m",
    COUNTRY == "FRA" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL0006") ~ "Hook < 6m",
    COUNTRY == "FRA" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL0612") ~ "Hook 06-12m",
    COUNTRY == "FRA" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL1218") ~ "Hook 12-18m",
    COUNTRY == "FRA" & VESSEL_LENGTH %in% c("VL0006", "VL0612") ~ "Other inf 12m",
    COUNTRY == "FRA" & VESSEL_LENGTH %in% c("VL1218", "VL1824","VL2440","VL40XX") ~ "Other sup 12m",
    COUNTRY == "FRA" ~ "Other",
    TRUE ~ NA_character_))



### Importing the effort table (table G)
## Data importation
FDI_G <- read.csv("Data/Data_EWG2311/tableG_EWG2311.csv")

FDI_G <- data.frame(FDI_G) 
dim(FDI_G)

# Changing column names
FDI_G <- FDI_G %>%
  rename(
    COUNTRY = country_code,
    YEAR = year,
    SUPRA_REGION = supra_region,
    FISHING_TECH = fishing_tech,
    VESSEL_LENGTH = vessel_length,
    GEO_INDICATOR = geo_indicator)

### Merging fishing effort (table G) and fleets (table J)
effort <-  merge(FDI_G, unique(FDI_J[,c("COUNTRY","YEAR","VESSEL_LENGTH","FISHING_TECH","SUPRA_REGION"
                                        ,"Segment","FleetIAM")]))






## Using Gregoire's taxons selection----
#' Note : we changed a little bit Gregoire's taxons selection, now ARA have 
#' his own group and ARI_bis corresponds to ARV, ARS, SSH and AJN.
#' Now MUT also have his own group and MUX_bis is GOA,GOX,MUR,MUX.
taxons_select <- read_excel("Perso/Species_Focus_CodeDescription_NOA.xlsx")
FDI_A <- read.csv("Data/Data_EWG2311/tableA_EWG2311.csv")

FDI_A <- subset(FDI_A,
                fishing_tech != "INACTIVE" &
                  #country_code != "ITA" &
                  country_code == "FRA" &  
                  supra_region == "MBS" & 
                  sub_region %in% c("GSA1", "GSA2", "GSA5", "GSA6", "GSA7") &
                  !(species %in% c("BFT"))) ### excluding Red Tunas

FDI_A <- FDI_A %>%
  rename(
    COUNTRY = country_code,
    YEAR = year,
    SUPRA_REGION = supra_region,
    FISHING_TECH = fishing_tech,
    VESSEL_LENGTH = vessel_length,
    GEO_INDICATOR = geo_indicator,
    SPECIES = species,
    SUB_REGION = sub_region)





#### Merging gregoire's taxons selection and table A
# Convert dataframes in data.table format
FDI_A <- as.data.table(FDI_A)
taxons_select <- as.data.table(taxons_select)


# Create a "key" to speed up the research
setkey(taxons_select, `Other taxonomic code included`)

# Initialize the new column
FDI_A[, New_Code := NA_character_]

# Divide FDI_A in multiple chunks 
split_size <- 100000 # Adjust this value according to the memory available
splits <- split(FDI_A, ceiling(seq_len(FDI_A[, .N]) / split_size))

# Joint by chunks 
for (chunk in splits) {
  for (code in unique(taxons_select$`Other taxonomic code included`)) {
    codes <- unlist(strsplit(code, ", "))
    chunk[SPECIES %in% codes, New_Code := taxons_select[code, x.X3A_CODE]]}}

# Gather chunks 
FDI_A <- rbindlist(splits)

length(unique(FDI_A$New_Code)) # 37 taxons "non vides", 46 au total

# Put "NA" in "ZZZ"
FDI_A <- FDI_A %>%
  rename(X3A_CODE = New_Code) %>%
  mutate(X3A_CODE = ifelse(is.na(X3A_CODE), "ZZZ", X3A_CODE))


landings <- FDI_A

length(unique(landings$X3A_CODE)) # 46 taxons
length(unique(taxons_select$X3A_CODE)) # for a total of 50
setdiff(unique(taxons_select$X3A_CODE), unique(landings$X3A_CODE)) 
# Species not present : ARI_bis, KTT, PHA and XOX

#### Creating the IAM fleets typo in table "landings"
landings <- landings %>%
  mutate(FleetIAM = case_when(
    # COUNTRY == "ESP" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL0612") ~ "ESP_DTS_<12m",
    # COUNTRY == "ESP" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL1218") ~ "ESP_DTS_12-18m",
    # COUNTRY == "ESP" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL1824") ~ "ESP_DTS_18-24m",
    # COUNTRY == "ESP" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL2440") ~ "ESP_DTS_>=24m",
    # COUNTRY == "ESP" & FISHING_TECH == "DFN" & VESSEL_LENGTH %in% c("VL0612") ~ "ESP_DFN_06-12m",
    # COUNTRY == "ESP" & FISHING_TECH == "DFN" & VESSEL_LENGTH %in% c("VL1218") ~ "ESP_DFN_12-18m",
    # COUNTRY == "ESP" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL0612") ~ "ESP_HOK_06-12m",
    # COUNTRY == "ESP" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL1218") ~ "ESP_HOK_12-18m",
    # COUNTRY == "ESP" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL1824") ~ "ESP_HOK_18-24m",
    # COUNTRY == "ESP" & VESSEL_LENGTH %in% c("VL0006", "VL0612") ~ "Other_<=12m",
    # COUNTRY == "ESP" & VESSEL_LENGTH %in% c("VL1218", "VL1824","VL2440","VL40XX") ~ "Other_>=12m",
    # COUNTRY == "ESP" ~ "Other",
    COUNTRY == "FRA" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL1824") ~ "FRA_DTS_18-24m",
    COUNTRY == "FRA" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL2440") ~ "FRA_DTS_>24m",
    COUNTRY == "FRA" & FISHING_TECH == "DFN" & VESSEL_LENGTH %in% c("VL0006") ~ "FRA_DFN_00-06m",
    COUNTRY == "FRA" & FISHING_TECH == "DFN" & VESSEL_LENGTH %in% c("VL0612") ~ "FRA_DFN_06-12m",
    COUNTRY == "FRA" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL0006") ~ "FRA_HOK_00-06m",
    COUNTRY == "FRA" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL0612") ~ "FRA_HOK_06-12m",
    COUNTRY == "FRA" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL1218") ~ "FRA_HOK_12-18m",
    COUNTRY == "FRA" & VESSEL_LENGTH %in% c("VL0006", "VL0612") ~ "Other_<=12m",
    COUNTRY == "FRA" & VESSEL_LENGTH %in% c("VL1218", "VL1824","VL2440","VL40XX") ~ "Other_>=12m",
    COUNTRY == "FRA" ~ "Other",
    TRUE ~ NA_character_))


#### Creating a segment column
landings <- landings %>%
  mutate(Segment = case_when(
    # COUNTRY == "ESP" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL0612") ~ "Spanish trawlers < 12m",
    # COUNTRY == "ESP" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL1218") ~ "Spanish trawlers 12-18m",
    # COUNTRY == "ESP" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL1824") ~ "Spanish trawlers 18-24m",
    # COUNTRY == "ESP" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL2440") ~ "Spanish trawlers >=24m",
    # COUNTRY == "ESP" & FISHING_TECH == "DFN" & VESSEL_LENGTH %in% c("VL0612") ~ "Spanish netters 06-12m",
    # COUNTRY == "ESP" & FISHING_TECH == "DFN" & VESSEL_LENGTH %in% c("VL1218") ~ "Spanish netters 12-18m",
    # COUNTRY == "ESP" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL0612") ~ "Spanish vessels using hooks 06-12m",
    # COUNTRY == "ESP" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL1218") ~ "Spanish vessels using hooks 12-18m",
    # COUNTRY == "ESP" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL1824") ~ "Spanish vessels using hooks 18-24m",
    # COUNTRY == "ESP" & VESSEL_LENGTH %in% c("VL0006", "VL0612") ~ "Other inf 12m",
    # COUNTRY == "ESP" & VESSEL_LENGTH %in% c("VL1218", "VL1824","VL2440","VL40XX") ~ "Other sup 12m",
    # COUNTRY == "ESP" ~ "Other",
    COUNTRY == "FRA" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL1824") ~ "French demersal trawlers 18-24m",
    COUNTRY == "FRA" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL2440") ~ "French demersal trawlers 24-40m",
    COUNTRY == "FRA" & FISHING_TECH == "DFN" & VESSEL_LENGTH %in% c("VL0006") ~ "French netters < 6m",
    COUNTRY == "FRA" & FISHING_TECH == "DFN" & VESSEL_LENGTH %in% c("VL0612") ~ "French netters 06-12m",
    COUNTRY == "FRA" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL0006") ~ "Hook < 6m",
    COUNTRY == "FRA" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL0612") ~ "Hook 06-12m",
    COUNTRY == "FRA" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL1218") ~ "Hook 12-18m",
    COUNTRY == "FRA" & VESSEL_LENGTH %in% c("VL0006", "VL0612") ~ "Other inf 12m",
    COUNTRY == "FRA" & VESSEL_LENGTH %in% c("VL1218", "VL1824","VL2440","VL40XX") ~ "Other sup 12m",
    COUNTRY == "FRA" ~ "Other",
    TRUE ~ NA_character_))






# LandingV2 : New species aggregation----
#' at Sophie's request, we create several code-species. This allows us to differentiate 
#' between the evolution of a species as a whole and a species at a specific location. 
#' For example, MUT is the data about MUT across all locations, while MUT1 is the 
#' data of MUT from GSA, ARA67 is ARA but only at GSA's 6 and 7

landingsV2 <- landings[,c("COUNTRY","YEAR","quarter","FleetIAM","totwghtlandg",
                          "totvallandg","X3A_CODE","SUB_REGION")]

landingsV2 <- landingsV2 %>%
  mutate(X3A_CODE = replace(X3A_CODE, 
                            X3A_CODE == "MUT" & SUB_REGION == "GSA1", "MUT1"),
         X3A_CODE = replace(X3A_CODE,
                            X3A_CODE == "MUT" & SUB_REGION == "GSA5", "MUT5"),
         X3A_CODE = replace(X3A_CODE,
                            X3A_CODE == "MUT" & SUB_REGION == "GSA6", "MUT6"),
         X3A_CODE = replace(X3A_CODE,
                            X3A_CODE == "MUT" & SUB_REGION == "GSA7", "MUT7"),
         X3A_CODE = replace(X3A_CODE,
                            X3A_CODE == "ARA" & SUB_REGION %in% c("GSA1", "GSA2"), "ARA12"),
         X3A_CODE = replace(X3A_CODE,
                            X3A_CODE == "ARA" & SUB_REGION == "GSA5", "ARA5"),
         X3A_CODE = replace(X3A_CODE,
                            X3A_CODE == "ARA" & SUB_REGION %in% c("GSA6", "GSA7"), "ARA67"),
         X3A_CODE = replace(X3A_CODE,
                            X3A_CODE == "DPS" & SUB_REGION == "GSA1", "DPS1"),
         X3A_CODE = replace(X3A_CODE,
                            X3A_CODE == "DPS" & SUB_REGION %in% c("GSA5", "GSA6", "GSA7"), "DPS567"),
         X3A_CODE = replace(X3A_CODE,
                            X3A_CODE == "NEP" & SUB_REGION == "GSA1", "NEP1"),
         X3A_CODE = replace(X3A_CODE,
                            X3A_CODE == "NEP" & SUB_REGION == "GSA5", "NEP5"),
         X3A_CODE = replace(X3A_CODE,
                            X3A_CODE == "NEP" & SUB_REGION == "GSA6", "NEP6"),
         X3A_CODE = replace(X3A_CODE,
                            X3A_CODE == "NEP" & SUB_REGION == "GSA7", "NEP7"),
         X3A_CODE = replace(X3A_CODE,
                            X3A_CODE == "HKE" & SUB_REGION %in% c("GSA1", "GSA5", "GSA6", "GSA7"), "HKE1567"))





# Compute LPUE----

#' Instead of dividing the number of landings (in kg) for a specific year by the
#' sum of fishing days of the same year (which would include fishing days of fleets
#' that don't fish that specific species), we will divide by the sum of fishing
#' days of fleets that actually fish the targeted species.

# We take every "unique" value of Gregoire's taxons aggregation
species <- unique(landingsV2$X3A_CODE)

# Create a list to stock LPUE by specie, quarter, year
lpue_by_species_quarter_year <- list()

# Loop on species (Grégoire's Taxons)
for (sp in species) {
  
  # Identify the fleets that actually really fish that specie
  sp_fleets <- unique(landingsV2$FleetIAM[landingsV2$X3A_CODE == sp 
                                          #& landingsV2$totwghtlandg > 0.001
                                            ])
  
  # Create a list to stock LPUE by quarter and year of that specie
  lpue_by_quarter_year <- list()
  
  # Get the years of the data frame
  years <- unique(landingsV2$YEAR)
  
  for (yr in years) {
    # Create a vector to stock LPUE by quarters for one specific year
    lpue_by_quarter <- vector("numeric", 4)
    
    for (q in 1:4) {
      # Compute the landingsV2 of this quarter, year and specie
      landingsV2_sp <- sum(landingsV2$totwghtlandg[landingsV2$X3A_CODE == sp & 
                                                     landingsV2$YEAR == yr & 
                                                     landingsV2$quarter == q & 
                                                     landingsV2$FleetIAM %in% sp_fleets], na.rm = TRUE)
      
      # Compute the effort (fishing days) for this quarter, year and specie
      effort_sp <- sum(effort$totfishdays[effort$YEAR == yr 
                                          & effort$quarter == q 
                                          & effort$FleetIAM %in% sp_fleets], na.rm = TRUE)
      
      # Compute the LPUE and stock it in the vector
      lpue_by_quarter[q] <- landingsV2_sp / effort_sp
    }
    
    # Add the vector of LPUE by quarter, year to the list
    lpue_by_quarter_year[[as.character(yr)]] <- lpue_by_quarter
  }
  
  # Add the list of LPUE by quarter and year for this specie to the main list
  lpue_by_species_quarter_year[[sp]] <- lpue_by_quarter_year
}

# Create an empty data frame
lpue_df <- data.frame()

# Loop on species
for (sp in species) {
  if (!is.null(lpue_by_species_quarter_year[[sp]]) && length(lpue_by_species_quarter_year[[sp]]) > 0) {
    df_temp <- data.frame(
      Species = sp,
      Year = rep(as.numeric(names(lpue_by_species_quarter_year[[sp]])), times = sapply(lpue_by_species_quarter_year[[sp]], length)),
      Quarter = unlist(lapply(lpue_by_species_quarter_year[[sp]], function(x) rep(1:4, each = 1))),
      lpue = unlist(lpue_by_species_quarter_year[[sp]]))
    
    lpue_df <- rbind(lpue_df, df_temp)
  }
}

#' For more visibility, we transform the dataframe in a "wider" format, with 
#' one column per species.
lpue_df2 <- lpue_df %>%
  pivot_wider(names_from = Species, values_from = lpue)

names(lpue_df2)
sum(is.na(lpue_df2))






## Graph: LPUE's evolutions plots----
# Get the "unique" species from Grégoire's taxons group
# species <- unique(lpue_df$Species)
# plots <- list()
# 
# # Loop to create a graph for each species
# for (sp in species) {
#   sp_df <- lpue_df[lpue_df$Species == sp, ]
#   # Compute the average LPUE for each year (between the 4 quarters)
#   sp_annual <- aggregate(lpue ~ Year, data = sp_df, FUN = mean)
#   plot <- ggplot(sp_annual, aes(x = Year, y = lpue, alpha = lpue, color = lpue)) +
#     geom_line() +
#     geom_point() +
#     scale_x_continuous(breaks = seq(min(sp_annual$Year), max(sp_annual$Year), by = 3)) +
#     scale_alpha(range = c(0.7, 1), guide = "none") +
#     scale_color_gradient2(low = "darkblue", high = "darkred", mid = "red",
#                           midpoint = mean(sp_annual$lpue),guide = "none")+
#     labs(
#       title = sp,
#       x = "Année",
#       y = "lpue") +
#     theme_minimal()+
#     theme(
#       plot.title = element_text(face = "bold", size = 10),
#       axis.title.x = element_text(size = 8),
#       axis.title.y = element_text(size = 8))
# 
#   plots[[sp]] <- plot}
# 
# # Combine graphs
# combined_plot <- plots[[1]]
# 
# for (i in 2:length(plots)) {
#   combined_plot <- combined_plot + plots[[i]]}
# 
# pdf("Figures/FRA/LPUE_species.pdf", width = 20, height = 12)
# print(combined_plot)
# dev.off()


# Compute LPUEs of each species for each fleets----
# Get all species 
species <- unique(landingsV2$X3A_CODE)
# Get all fleets
all_fleets <- unique(landingsV2$FleetIAM)
# Create an empty dataframe to store the results
lpue_SF <- data.frame(Year = numeric(), 
                      Quarter = numeric())

# Loop on years
years <- unique(landingsV2$YEAR)
for (yr in years) {
  
  # Loop on quarters
  for (q in 1:4) {
    
    # Data frame to stock on year/quarters
    temp_df <- data.frame(Year = yr, Quarter = q)
    
    for (sp in species) {
      for (fleet in all_fleets) {
        
        # Compute landings for this species/fleet/year/quarter
        landingsV2_sp_fleet <- sum(landingsV2$totwghtlandg[landingsV2$X3A_CODE == sp &
                                                         landingsV2$FleetIAM == fleet &
                                                         landingsV2$YEAR == yr &
                                                         landingsV2$quarter == q],
                                 na.rm = TRUE)
        
        # Compute effort for this fleet/year/quarter
        effort_fleet <- sum(effort$totfishdays[effort$FleetIAM == fleet &
                                                 effort$YEAR == yr &
                                                 effort$quarter == q],
                            na.rm = TRUE)
        
        # Compute LPUE if effort (fishing days) >0 or else = 0
        if (effort_fleet > 0) {
          lpue <- landingsV2_sp_fleet / effort_fleet
        } else {
          lpue <- 0
        }
        
        # Add columns to the data frame
        temp_df[[paste0("LPUE_", sp, "_", fleet)]] <- lpue
        temp_df[[paste0("LAND_", sp, "_", fleet)]] <- landingsV2_sp_fleet
        
      }
    }
    lpue_SF <- rbind(lpue_SF, temp_df)
  }
}



## Graph: LPUE's evolution plots----
# Extract combo of unique species - fleets
# lpue_cols <- grep("LPUE_", names(lpue_SF), value = TRUE)
# land_cols <- grep("LAND_", names(lpue_SF), value = TRUE)
# 
# if (length(lpue_cols) != length(land_cols)) {
#   stop("Le nombre de colonnes LPUE et LAND ne correspond pas.")
# }
# 
# # Plot function
# create_plot <- function(lpue_col, land_col) {
#   plot_data <- lpue_SF %>%
#     select(Year, Quarter, LPUE = !!sym(lpue_col), LAND = !!sym(land_col))
# 
#   # Check that LPUE >0
#   if (all(plot_data$LPUE == 0)) {
#     return(NULL)
#   }
# 
#   # Get column's name without the 5 first letters
#   title_species <- gsub("^LPUE_|^LAND_", "", lpue_col)
# 
#   p <- ggplot(plot_data, aes(x = as.factor(paste(Year, Quarter, sep = "Q")))) +
#     geom_line(aes(y = LPUE, color = "LPUE", group = 1)) +
#     geom_line(aes(y = LAND / 1000, color = "LAND", group = 1)) +
#     scale_y_continuous(
#       name = "LPUE",
#       sec.axis = sec_axis(~.*1000, name = "Landings")
#     ) +
#     labs(
#       title = paste("Evolution of LPUE & Landings for", title_species),
#       x = "Year & Quarter"
#     ) +
#     theme_minimal() +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
#   return(p)
# }
# # Group plots by species code
# species_groups <- lpue_SF %>%
#   select(starts_with("LPUE_")) %>%
#   names() %>%
#   strsplit("_") %>%
#   sapply(function(x) x[2])
# 
# # PDF export directory (change it if you need to)
# output_dir <- "Figures/FRA/LPUE_species_fleets/"
# dir.create(output_dir, showWarnings = FALSE)
# 
# for (species in unique(species_groups)) {
#   # Find columns corresponding to the specie
#   species_lpue_cols <- grep(paste0("LPUE_", species, "_"), names(lpue_SF), value = TRUE)
#   species_land_cols <- grep(paste0("LAND_", species, "_"), names(lpue_SF), value = TRUE)
# 
# 
#   # Create plots next to each other
#   species_plots <- lapply(seq_along(species_lpue_cols), function(i) {
#     create_plot(species_lpue_cols[i], species_land_cols[i])
#   })
# 
#   # Delete NULL elements (LPUEs = 0 for all the period
#   species_plots <- species_plots[!sapply(species_plots, is.null)]
# 
#   if (length(species_plots) > 0) {
#     combined_plot <- wrap_plots(species_plots, ncol = 2)
# 
#     # Export PDF
#     pdf_path <- file.path(output_dir, paste0(species, ".pdf"))
#     ggsave(pdf_path, combined_plot, width = 20, height = 12, units = "in")
#     cat(paste0("Exported ", species, ".pdf\n"))
#   } else {
#     cat(paste0("No plots generated for ", species, "\n"))
#   }
# }




# Compute LPUEs for each specie-fleet-GSA----

# Get all species
species <- unique(landingsV2$X3A_CODE)

# Get all fleets
all_fleets <- unique(landingsV2$FleetIAM)

# Get all GSAs
all_gsas <- unique(landingsV2$SUB_REGION)

# Create an empty list to store the results
lpue_list <- vector("list", length = length(years) * 4)

# Create a skeleton data frame with all potential columns
all_cols <- c("Year", "Quarter")
for (sp in species) {
  for (fleet in all_fleets) {
    for (gsa in all_gsas) {
      all_cols <- c(all_cols, paste0("LAND_", sp, "_", fleet, "_", gsa),
                    paste0("LPUE_", sp, "_", fleet, "_", gsa))
    }
  }
}
skeleton_df <- data.frame(matrix(0, nrow = 1, ncol = length(all_cols)))
names(skeleton_df) <- all_cols

# Counter for list index
idx <- 1

# Loop on years
years <- unique(landingsV2$YEAR)
for (yr in years) {
  # Loop on quarters
  for (q in 1:4) {
    # Data frame to stock on year/quarters
    temp_df <- skeleton_df
    temp_df$Year <- yr
    temp_df$Quarter <- q
    
    # Subset data for current year/quarter
    landingsV2_subset <- landingsV2[landingsV2$YEAR == yr & landingsV2$quarter == q, ]
    effort_subset <- effort[effort$YEAR == yr & effort$quarter == q, ]
    
    for (sp in species) {
      for (fleet in all_fleets) {
        for (gsa in all_gsas) {
          # Compute landings for this species/fleet/gsa/year/quarter
          landings_sp_fleet_gsa <- sum(landingsV2_subset$totwghtlandg[landingsV2_subset$X3A_CODE == sp &
                                                                        landingsV2_subset$FleetIAM == fleet &
                                                                        landingsV2_subset$SUB_REGION == gsa],
                                       na.rm = TRUE)
          
          # If landings are greater than 0, compute LPUE
          if (landings_sp_fleet_gsa > 0) {
            # Compute effort for this fleet/gsa/year/quarter
            effort_fleet_gsa <- sum(effort_subset$totfishdays[effort_subset$FleetIAM == fleet &
                                                                effort_subset$sub_region == gsa],
                                    na.rm = TRUE)
            
            # Compute LPUE if effort (fishing days) > 0
            if (effort_fleet_gsa > 0) {
              lpue <- landings_sp_fleet_gsa / effort_fleet_gsa
            } else {
              lpue <- 0
            }
            
            # Add values to the data frame
            temp_df[[paste0("LAND_", sp, "_", fleet, "_", gsa)]] <- landings_sp_fleet_gsa
            temp_df[[paste0("LPUE_", sp, "_", fleet, "_", gsa)]] <- lpue
          }
        }
      }
    }
    lpue_list[[idx]] <- temp_df
    idx <- idx + 1
  }
}

# Combine all data frames in the list into one
lpue_SFG <- do.call(rbind, lpue_list)
lpue_SFG <- lpue_SFG[, colSums(lpue_SFG != 0) > 0]


## Graph: LPUE's evolution plots----
# Extract combo of unique species - fleets
# lpue_cols <- grep("LPUE_", names(lpue_SFG), value = TRUE)
# land_cols <- grep("LAND_", names(lpue_SFG), value = TRUE)
# 
# if (length(lpue_cols) != length(land_cols)) {
#   stop("Le nombre de colonnes LPUE et LAND ne correspond pas.")
# }
# 
# # Plot function
# create_plot <- function(lpue_col, land_col) {
#   plot_data <- lpue_SFG %>%
#     select(Year, Quarter, LPUE = !!sym(lpue_col), LAND = !!sym(land_col))
# 
#   # Check that LPUE >0
#   if (all(plot_data$LPUE == 0)) {
#     return(NULL)
#   }
# 
#   # Get column's name without the 5 first letters
#   title_species <- gsub("^LPUE_|^LAND_", "", lpue_col)
# 
#   p <- ggplot(plot_data, aes(x = as.factor(paste(Year, Quarter, sep = "Q")))) +
#     geom_line(aes(y = LPUE, color = "LPUE", group = 1)) +
#     geom_line(aes(y = LAND / 1000, color = "LAND", group = 1)) +
#     scale_y_continuous(
#       name = "LPUE",
#       sec.axis = sec_axis(~.*1000, name = "Landings")
#     ) +
#     labs(
#       title = paste("Evolution of LPUE & Landings for", title_species),
#       x = "Year & Quarter"
#     ) +
#     theme_minimal() +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
#   return(p)
# }
# # Group plots by species code
# species_groups <- lpue_SFG %>%
#   select(starts_with("LPUE_")) %>%
#   names() %>%
#   strsplit("_") %>%
#   sapply(function(x) x[2])
# 
# # PDF export directory (change it if you need to)
# output_dir <- "Figures/FRA/LPUE_species_fleets_GSA/"
# dir.create(output_dir, showWarnings = FALSE)
# 
# for (species in unique(species_groups)) {
#   # Find columns corresponding to the specie
#   species_lpue_cols <- grep(paste0("LPUE_", species, "_"), names(lpue_SFG), value = TRUE)
#   species_land_cols <- grep(paste0("LAND_", species, "_"), names(lpue_SFG), value = TRUE)
# 
# 
#   # Create plots next to each other
#   species_plots <- lapply(seq_along(species_lpue_cols), function(i) {
#     create_plot(species_lpue_cols[i], species_land_cols[i])
#   })
# 
#   # Delete NULL elements (LPUEs = 0 for all the period
#   species_plots <- species_plots[!sapply(species_plots, is.null)]
# 
#   if (length(species_plots) > 0) {
#     combined_plot <- wrap_plots(species_plots, ncol = 2)
# 
#     # Export PDF
#     pdf_path <- file.path(output_dir, paste0(species, ".pdf"))
#     ggsave(pdf_path, combined_plot, width = 20, height = 12, units = "in")
#     cat(paste0("Exported ", species, ".pdf\n"))
#   } else {
#     cat(paste0("No plots generated for ", species, "\n"))
#   }
# }

#' Note : having HKE LPUE's represented across different GSA's does NOT make 
#' sense, because the HKE stock is the same and only stock across all GSA's.


# Compute VPUEs----
#' Instead of dividing the number of landings (in €) for a specific year by the
#' sum of fishing days of the same year (which would include fishing days of fleets
#' that don't fish that specific species), we will divide by the sum of fishing
#' days of fleets that actually fish the targeted species.

# We take every "unique" value of Gregoire's taxons aggregation
species <- unique(landingsV2$X3A_CODE)

# Create a list to stock VPUE by specie, quarter, year
vpue_by_species_quarter_year <- list()

# Loop on species (Grégoire's Taxons)
for (sp in species) {
  
  # Identify the fleets that actually really fish that specie
  sp_fleets <- unique(landingsV2$FleetIAM[landingsV2$X3A_CODE == sp 
                                          #& landingsV2$totvallandg > 0.001
  ])
  
  # Create a list to stock VPUE by quarter and year of that specie
  vpue_by_quarter_year <- list()
  
  # Get the years of the data frame
  years <- unique(landingsV2$YEAR)
  
  for (yr in years) {
    # Create a vector to stock VPUE by quarters for one specific year
    vpue_by_quarter <- vector("numeric", 4)
    
    for (q in 1:4) {
      # Compute the landingsV2 of this quarter, year and specie
      landingsV2_sp <- sum(landingsV2$totvallandg[landingsV2$X3A_CODE == sp & landingsV2$YEAR == yr & landingsV2$quarter == q & landingsV2$FleetIAM %in% sp_fleets], na.rm = TRUE)
      
      # Compute the effort (fishing days) for this quarter, year and specie
      effort_sp <- sum(effort$totfishdays[effort$YEAR == yr & effort$quarter == q & effort$FleetIAM %in% sp_fleets], na.rm = TRUE)
      
      # Compute the VPUE and stock it in the vector
      vpue_by_quarter[q] <- landingsV2_sp / effort_sp
    }
    
    # Add the vector of VPUE by quarter, year to the list
    vpue_by_quarter_year[[as.character(yr)]] <- vpue_by_quarter
  }
  
  # Add the list of VPUE by quarter and year for this specie to the main list
  vpue_by_species_quarter_year[[sp]] <- vpue_by_quarter_year
}

# Create an empty data frame
vpue_df <- data.frame()

# Loop on species
for (sp in species) {
  if (!is.null(vpue_by_species_quarter_year[[sp]]) && length(vpue_by_species_quarter_year[[sp]]) > 0) {
    df_temp <- data.frame(
      Species = sp,
      Year = rep(as.numeric(names(vpue_by_species_quarter_year[[sp]])), times = sapply(vpue_by_species_quarter_year[[sp]], length)),
      Quarter = unlist(lapply(vpue_by_species_quarter_year[[sp]], function(x) rep(1:4, each = 1))),
      vpue = unlist(vpue_by_species_quarter_year[[sp]]))
    
    vpue_df <- rbind(vpue_df, df_temp)
  }
}

vpue_df2 <- vpue_df %>%
  pivot_wider(names_from = Species, values_from = vpue)

names(vpue_df2)
sum(is.na(vpue_df2))






### Graph: VPUE's evolutions plots----
# Get the "unique" species from Grégoire's taxons group
# species <- unique(vpue_df$Species)
# plots <- list()
# 
# # Loop to create a graph for each species
# for (sp in species) {
#   sp_df <- vpue_df[vpue_df$Species == sp, ]
#   # Compute the average VPUE for each year (between the 4 quarters)
#   sp_annual <- aggregate(vpue ~ Year, data = sp_df, FUN = mean)
#   plot <- ggplot(sp_annual, aes(x = Year, y = vpue, alpha = vpue, color = vpue)) +
#     geom_line() +
#     geom_point() +
#     scale_x_continuous(breaks = seq(min(sp_annual$Year), max(sp_annual$Year), by = 3)) +
#     scale_alpha(range = c(0.7, 1), guide = "none") +
#     scale_color_gradient2(low = "darkblue", high = "darkred", mid = "red",
#                           midpoint = mean(sp_annual$vpue),guide = "none")+
#     labs(
#       title = sp,
#       x = "Année",
#       y = "vpue") +
#     theme_minimal()+
#     theme(
#       plot.title = element_text(face = "bold", size = 10),
#       axis.title.x = element_text(size = 8),
#       axis.title.y = element_text(size = 8))
# 
#   plots[[sp]] <- plot}
# 
# # Combine graphs
# combined_plot <- plots[[1]]
# 
# for (i in 2:length(plots)) {
#   combined_plot <- combined_plot + plots[[i]]}
# 
# pdf("Figures/FRA/VPUE_species.pdf", width = 20, height = 12)
# print(combined_plot)
# dev.off()



# Compute VPUEs of each species for each fleets----
# Get all species 
species <- unique(landingsV2$X3A_CODE)
# Get all fleets
all_fleets <- unique(landingsV2$FleetIAM)
# Create an empty dataframe to store the results
vpue_SF <- data.frame(Year = numeric(), 
                      Quarter = numeric())

# Loop on years
years <- unique(landingsV2$YEAR)
for (yr in years) {
  
  # Loop on quarters
  for (q in 1:4) {
    
    # Data frame to stock on year/quarters
    temp_df <- data.frame(Year = yr, Quarter = q)
    
    for (sp in species) {
      for (fleet in all_fleets) {
        
        # Compute landings for this species/fleet/year/quarter
        landingsV2_sp_fleet <- sum(landingsV2$totvallandg[landingsV2$X3A_CODE == sp &
                                                             landingsV2$FleetIAM == fleet &
                                                             landingsV2$YEAR == yr &
                                                             landingsV2$quarter == q],
                                   na.rm = TRUE)
        
        # Compute effort for this fleet/year/quarter
        effort_fleet <- sum(effort$totfishdays[effort$FleetIAM == fleet &
                                                 effort$YEAR == yr &
                                                 effort$quarter == q],
                            na.rm = TRUE)
        
        # Compute VPUE if effort (fishing days) >0 or else = 0
        if (effort_fleet > 0) {
          vpue <- landingsV2_sp_fleet / effort_fleet
        } else {
          vpue <- 0
        }
        
        # Add columns to the data frame
        temp_df[[paste0("VPUE_", sp, "_", fleet)]] <- vpue
        temp_df[[paste0("LAND_", sp, "_", fleet)]] <- landingsV2_sp_fleet
        
      }
    }
    vpue_SF <- rbind(vpue_SF, temp_df)
  }
}



## Graph: VPUE's evolution plots----
# Extract combo of unique species - fleets
# vpue_cols <- grep("VPUE_", names(vpue_SF), value = TRUE)
# land_cols <- grep("LAND_", names(vpue_SF), value = TRUE)
# 
# if (length(vpue_cols) != length(land_cols)) {
#   stop("Le nombre de colonnes VPUE et LAND ne correspond pas.")
# }
# 
# # Plot function
# create_plot <- function(vpue_col, land_col) {
#   plot_data <- vpue_SF %>%
#     select(Year, Quarter, VPUE = !!sym(vpue_col), LAND = !!sym(land_col))
# 
#   # Check that VPUE >0
#   if (all(plot_data$VPUE == 0)) {
#     return(NULL)
#   }
# 
#   # Get column's name without the 5 first letters
#   title_species <- gsub("^VPUE_|^LAND_", "", vpue_col)
# 
#   p <- ggplot(plot_data, aes(x = as.factor(paste(Year, Quarter, sep = "Q")))) +
#     geom_line(aes(y = VPUE, color = "VPUE", group = 1)) +
#     geom_line(aes(y = LAND / 1000, color = "LAND", group = 1)) +
#     scale_y_continuous(
#       name = "VPUE",
#       sec.axis = sec_axis(~.*1000, name = "Landings")
#     ) +
#     labs(
#       title = paste("Evolution of VPUE & Landings for", title_species),
#       x = "Year & Quarter"
#     ) +
#     theme_minimal() +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
#   return(p)
# }
# # Group plots by species code
# species_groups <- vpue_SF %>%
#   select(starts_with("VPUE_")) %>%
#   names() %>%
#   strsplit("_") %>%
#   sapply(function(x) x[2])
# 
# # PDF export directory (change it if you need to)
# output_dir <- "Figures/FRA/VPUE_species_fleets/"
# dir.create(output_dir, showWarnings = FALSE)
# 
# for (species in unique(species_groups)) {
#   # Find columns corresponding to the specie
#   species_vpue_cols <- grep(paste0("VPUE_", species, "_"), names(vpue_SF), value = TRUE)
#   species_land_cols <- grep(paste0("LAND_", species, "_"), names(vpue_SF), value = TRUE)
# 
# 
#   # Create plots next to each other
#   species_plots <- lapply(seq_along(species_vpue_cols), function(i) {
#     create_plot(species_vpue_cols[i], species_land_cols[i])
#   })
# 
#   # Delete NULL elements (VPUEs = 0 for all the period
#   species_plots <- species_plots[!sapply(species_plots, is.null)]
# 
#   if (length(species_plots) > 0) {
#     combined_plot <- wrap_plots(species_plots, ncol = 2)
# 
#     # Export PDF
#     pdf_path <- file.path(output_dir, paste0(species, ".pdf"))
#     ggsave(pdf_path, combined_plot, width = 20, height = 12, units = "in")
#     cat(paste0("Exported ", species, ".pdf\n"))
#   } else {
#     cat(paste0("No plots generated for ", species, "\n"))
#   }
# }




# Compute VPUEs for each specie-fleet-GSA----

# Get all species
species <- unique(landingsV2$X3A_CODE)

# Get all fleets
all_fleets <- unique(landingsV2$FleetIAM)

# Get all GSAs
all_gsas <- unique(landingsV2$SUB_REGION)

# Create an empty list to store the results
vpue_list <- vector("list", length = length(years) * 4)

# Create a skeleton data frame with all potential columns
all_cols <- c("Year", "Quarter")
for (sp in species) {
  for (fleet in all_fleets) {
    for (gsa in all_gsas) {
      all_cols <- c(all_cols, paste0("LAND_", sp, "_", fleet, "_", gsa),
                    paste0("VPUE_", sp, "_", fleet, "_", gsa))
    }
  }
}
skeleton_df <- data.frame(matrix(0, nrow = 1, ncol = length(all_cols)))
names(skeleton_df) <- all_cols

# Counter for list index
idx <- 1

# Loop on years
years <- unique(landingsV2$YEAR)
for (yr in years) {
  # Loop on quarters
  for (q in 1:4) {
    # Data frame to stock on year/quarters
    temp_df <- skeleton_df
    temp_df$Year <- yr
    temp_df$Quarter <- q
    
    # Subset data for current year/quarter
    landingsV2_subset <- landingsV2[landingsV2$YEAR == yr & landingsV2$quarter == q, ]
    effort_subset <- effort[effort$YEAR == yr & effort$quarter == q, ]
    
    for (sp in species) {
      for (fleet in all_fleets) {
        for (gsa in all_gsas) {
          # Compute landings for this species/fleet/gsa/year/quarter
          landings_sp_fleet_gsa <- sum(landingsV2_subset$totvallandg[landingsV2_subset$X3A_CODE == sp &
                                                                        landingsV2_subset$FleetIAM == fleet &
                                                                        landingsV2_subset$SUB_REGION == gsa],
                                       na.rm = TRUE)
          
          # If landings are greater than 0, compute VPUE
          if (landings_sp_fleet_gsa > 0) {
            # Compute effort for this fleet/gsa/year/quarter
            effort_fleet_gsa <- sum(effort_subset$totfishdays[effort_subset$FleetIAM == fleet &
                                                                effort_subset$sub_region == gsa],
                                    na.rm = TRUE)
            
            # Compute VPUE if effort (fishing days) > 0
            if (effort_fleet_gsa > 0) {
              vpue <- landings_sp_fleet_gsa / effort_fleet_gsa
            } else {
              vpue <- 0
            }
            
            # Add values to the data frame
            temp_df[[paste0("LAND_", sp, "_", fleet, "_", gsa)]] <- landings_sp_fleet_gsa
            temp_df[[paste0("VPUE_", sp, "_", fleet, "_", gsa)]] <- vpue
          }
        }
      }
    }
    vpue_list[[idx]] <- temp_df
    idx <- idx + 1
  }
}

# Combine all data frames in the list into one
vpue_SFG <- do.call(rbind, vpue_list)
vpue_SFG <- vpue_SFG[, colSums(vpue_SFG != 0) > 0]


## Graph: VPUE's evolution plots----
# Extract combo of unique species - fleets
# vpue_cols <- grep("VPUE_", names(vpue_SFG), value = TRUE)
# land_cols <- grep("LAND_", names(vpue_SFG), value = TRUE)
# 
# if (length(vpue_cols) != length(land_cols)) {
#   stop("Le nombre de colonnes VPUE et LAND ne correspond pas.")
# }
# 
# # Plot function
# create_plot <- function(vpue_col, land_col) {
#   plot_data <- vpue_SFG %>%
#     select(Year, Quarter, VPUE = !!sym(vpue_col), LAND = !!sym(land_col))
# 
#   # Check that VPUE >0
#   if (all(plot_data$VPUE == 0)) {
#     return(NULL)
#   }
# 
#   # Get column's name without the 5 first letters
#   title_species <- gsub("^VPUE_|^LAND_", "", vpue_col)
# 
#   p <- ggplot(plot_data, aes(x = as.factor(paste(Year, Quarter, sep = "Q")))) +
#     geom_line(aes(y = VPUE, color = "VPUE", group = 1)) +
#     geom_line(aes(y = LAND / 1000, color = "LAND", group = 1)) +
#     scale_y_continuous(
#       name = "VPUE",
#       sec.axis = sec_axis(~.*1000, name = "Landings")
#     ) +
#     labs(
#       title = paste("Evolution of VPUE & Landings for", title_species),
#       x = "Year & Quarter"
#     ) +
#     theme_minimal() +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
#   return(p)
# }
# # Group plots by species code
# species_groups <- vpue_SFG %>%
#   select(starts_with("VPUE_")) %>%
#   names() %>%
#   strsplit("_") %>%
#   sapply(function(x) x[2])
# 
# # PDF export directory (change it if you need to)
# output_dir <- "Figures/FRA/VPUE_species_fleets_GSA/"
# dir.create(output_dir, showWarnings = FALSE)
# 
# for (species in unique(species_groups)) {
#   # Find columns corresponding to the specie
#   species_vpue_cols <- grep(paste0("VPUE_", species, "_"), names(vpue_SFG), value = TRUE)
#   species_land_cols <- grep(paste0("LAND_", species, "_"), names(vpue_SFG), value = TRUE)
# 
# 
#   # Create plots next to each other
#   species_plots <- lapply(seq_along(species_vpue_cols), function(i) {
#     create_plot(species_vpue_cols[i], species_land_cols[i])
#   })
# 
#   # Delete NULL elements (VPUEs = 0 for all the period
#   species_plots <- species_plots[!sapply(species_plots, is.null)]
# 
#   if (length(species_plots) > 0) {
#     combined_plot <- wrap_plots(species_plots, ncol = 2)
# 
#     # Export PDF
#     pdf_path <- file.path(output_dir, paste0(species, ".pdf"))
#     ggsave(pdf_path, combined_plot, width = 20, height = 12, units = "in")
#     cat(paste0("Exported ", species, ".pdf\n"))
#   } else {
#     cat(paste0("No plots generated for ", species, "\n"))
#   }
# }
# 
# 


# Compute €/Kg----

# Compute €/kg values

# We add a security, if one of the two columns = 0 then value = 0
landingsV2$value <- ifelse(landingsV2$totwghtlandg == 0 | landingsV2$totvallandg == 0,
                           0,
                           landingsV2$totvallandg / landingsV2$totwghtlandg)

species <- unique(landingsV2$X3A_CODE)
years <- unique(landingsV2$YEAR)

# Create an empty dataframe to store the results
value_landings_df <- data.frame(Year = numeric(),
                                Quarter = numeric())

for (yr in years) {
  for (q in 1:4) {
    # Data frame to stock on year/quarters
    temp_df <- data.frame(Year = yr, Quarter = q)
    for (sp in species) {
      temp_df[[paste0("LANDINGS_", sp)]] <- sum(landingsV2$totwghtlandg[landingsV2$X3A_CODE == sp &
                                                                          landingsV2$YEAR == yr &
                                                                          landingsV2$quarter == q],
                                                na.rm = TRUE)
      # Compute value for this species/year/quarter
      temp_df[[paste0("VALUE_", sp)]] <- sum(landingsV2$value[landingsV2$X3A_CODE == sp &
                                                                landingsV2$YEAR == yr &
                                                                landingsV2$quarter == q],
                                             na.rm = TRUE)
    }
    value_landings_df <- rbind(value_landings_df, temp_df)
  }
}

# Create the €/KG data frame
# Extract column names
col_names <- names(value_landings_df)

# Find species names (everything after the underscore in column names)
species <- unique(sub("^(LANDINGS|VALUE)_", "", col_names[grep("^(LANDINGS|VALUE)_", col_names)]))

# Create a new dataframe with Year and Quarter
result_df <- value_landings_df[, c("Year", "Quarter")]

for(sp in species) {
  landing_col <- paste0("LANDINGS_", sp)
  value_col <- paste0("VALUE_", sp)
  
  # Skip species with all 0 values in landings or value columns
  if (all(value_landings_df[[landing_col]] == 0) || all(value_landings_df[[value_col]] == 0)) {
    next
  }
  
  ratio <- value_landings_df[[value_col]] / value_landings_df[[landing_col]]
  
  print(paste0("Species: ", sp))
  print(ratio)
  
  result_df[[paste0(sp, "_EurPerKg")]] <- replace(ratio, is.infinite(ratio) | is.nan(ratio), NA)
}

# Print results
head(result_df)
eurkg_df <- result_df




## €/kg evolution plot----
# Convert Year and Quarter to a single factor column
#value_landings_df$YearQuarter <- factor(paste(value_landings_df$Year, value_landings_df$Quarter, sep = "Q"))

# Graph: Plot function
# create_plot <- function(data, species) {
#   # Find min and max for each variable
#   value_max <- max(data[[paste0("VALUE_", species)]], na.rm = TRUE)
#   landings_max <- max(data[[paste0("LANDINGS_", species)]], na.rm = TRUE)
# 
#   # Compute scale factor
#   scale_factor <- landings_max / value_max
# 
#   p <- ggplot(data, aes(x = YearQuarter)) +
#     geom_line(aes(y = data[[paste0("VALUE_", species)]], color = "Value", group = 1.5)) +
#     geom_line(aes(y = data[[paste0("LANDINGS_", species)]] / scale_factor, color = "Landings", group = 1)) +
#     scale_x_discrete(
#       breaks = paste(c(2013, 2017, 2022), "Q1", sep = ""),
#       labels = c("2013", "2017", "2022")
#     ) +
#     scale_y_continuous(
#       name = "Value",
#       sec.axis = sec_axis(~ . * scale_factor, name = "Landings")
#     ) +
#     labs(
#       title = paste(species)
#     ) +
#     theme_minimal() +
#     theme(axis.text.x = element_text(hjust = 1))
# 
#   return(p)
# }
# 
# # Generate plots for each species and store them in a list
# plots <- list()
# for (sp in species) {
#   species_plot <- create_plot(value_landings_df, sp)
#   plots[[sp]] <- species_plot
# }
# 
# # Nombre de graphiques par page
# plots_per_page <- 30
# 
# # Combiner les graphiques dans un seul fichier PDF avec plusieurs pages
# num_plots <- length(plots)
# num_pages <- ceiling(num_plots / plots_per_page)
# 
# output_file <- "Figures/FRA/Value_Landings_species_combined.pdf"
# pdf(output_file, width = 20, height = 12)
# 
# for (page in 1:num_pages) {
#   start_index <- (page - 1) * plots_per_page + 1
#   end_index <- min(page * plots_per_page, num_plots)
#   plot_subset <- plots[start_index:end_index]
# 
#   ncol <- 4 # Nombre de colonnes dans le graphique combiné (ajustez selon vos besoins)
#   nrow <- ceiling(length(plot_subset) / ncol)
# 
#   grid.arrange(grobs = plot_subset, ncol = ncol, nrow = nrow)
# 
#   # Ajouter une nouvelle page seulement s'il y a d'autres graphiques à afficher
#   if (end_index < num_plots) {
#     grid.newpage()
#   }
# }
# 
# dev.off()
# 
# cat(paste0("Exported combined plots to ", output_file, "\n"))


# Correlations----
## From 2013 to 2022----
#' The idea is to create a correlation's matrix to understand the strength of 
#' the relation between the evolution of each LPUE species troughs the years.
#' We want to know if the LPUE's evolution of one specie from 2013 to 2022 is 
#' highly correlated to another specie. To be more precise, we want to how the 
#' "static" species are correlated to the "dynamic" species. 

### Correlations : landings(kg) vs landings (kg)----
# Select only the columns that corresponds to species
lpue <- lpue_df2
species <- lpue[, -(1:2)] # excluding the columns "Year" and "Quarter"
matrice_corr <- cor(species)
print(matrice_corr)
# By default, we compute Pearsons's correlations coefficients 


# Correlations dataframes
corr_df <- as.data.frame(matrice_corr)
corr_df <- corr_df[,c("MUT6", "MUT7", "NEP6", "ARA67", "DPS567", "HKE1567")]

# Extract the 10th highest values (positive and negative)
extract_top_values <- function(corr_vector, species_names) {
  sorted_corr_pos <- sort(corr_vector[corr_vector > 0], decreasing = TRUE)
  sorted_corr_neg <- sort(corr_vector[corr_vector < 0], decreasing = FALSE)
  
  positive_values <- head(sorted_corr_pos, 10)
  negative_values <- head(sorted_corr_neg, 10)
  
  if(length(positive_values) < 10) {
    positive_values <- c(positive_values, rep(NA, 10 - length(positive_values)))
  }
  if(length(negative_values) < 10) {
    negative_values <- c(negative_values, rep(NA, 10 - length(negative_values)))
  }
  
  data.frame(
    Pos_Species = species_names[match(positive_values, corr_vector)],
    Pos_Values = positive_values,
    Neg_Species = species_names[match(negative_values, corr_vector)],
    Neg_Values = negative_values
  )
}

# Apply the function on each columns
corr_MUT6 <- extract_top_values(corr_df$MUT6, rownames(corr_df))
corr_MUT7 <- extract_top_values(corr_df$MUT7, rownames(corr_df))
corr_NEP6 <- extract_top_values(corr_df$NEP6, rownames(corr_df))
corr_ARA67 <- extract_top_values(corr_df$ARA67, rownames(corr_df))
corr_DPS567 <- extract_top_values(corr_df$DPS567, rownames(corr_df))

# "ww" means correlations between landings(weight) and landings(weight)
corr_MUT6_ww <- corr_MUT6 %>% mutate(across(c(Pos_Values, Neg_Values), round, 2))
corr_MUT7_ww <- corr_MUT7 %>% mutate(across(c(Pos_Values, Neg_Values), round, 2))
corr_NEP6_ww <- corr_NEP6 %>% mutate(across(c(Pos_Values, Neg_Values), round, 2))
corr_ARA67_ww <- corr_ARA67 %>% mutate(across(c(Pos_Values, Neg_Values), round, 2))
corr_DPS567_ww <- corr_DPS567 %>% mutate(across(c(Pos_Values, Neg_Values), round, 2))

# Affichage des résultats
print(corr_MUT6_ww )
print(corr_MUT7_ww)
print(corr_NEP6_ww)
print(corr_ARA67_ww)
print(corr_DPS567_ww)




### Correlations : landings(V€) vs landings (V€)----
# Select only the columns that corresponds to species
vpue <- vpue_df2
species <- vpue[, -(1:2)] # excluding the columns "Year" and "Quarter"
matrice_corr <- cor(species)
print(matrice_corr)
# By default, we compute Pearsons's correlations coefficients 


# Correlations dataframes
corr_df <- as.data.frame(matrice_corr)
corr_df <- corr_df[,c("MUT6", "MUT7", "NEP6", "ARA67", "DPS567", "HKE1567")]

# Extract the 10th highest values (positive and negative)
extract_top_values <- function(corr_vector, species_names) {
  sorted_corr_pos <- sort(corr_vector[corr_vector > 0], decreasing = TRUE)
  sorted_corr_neg <- sort(corr_vector[corr_vector < 0], decreasing = FALSE)
  
  positive_values <- head(sorted_corr_pos, 10)
  negative_values <- head(sorted_corr_neg, 10)
  
  if(length(positive_values) < 10) {
    positive_values <- c(positive_values, rep(NA, 10 - length(positive_values)))
  }
  if(length(negative_values) < 10) {
    negative_values <- c(negative_values, rep(NA, 10 - length(negative_values)))
  }
  
  data.frame(
    Pos_Species = species_names[match(positive_values, corr_vector)],
    Pos_Values = positive_values,
    Neg_Species = species_names[match(negative_values, corr_vector)],
    Neg_Values = negative_values
  )
}

# Apply the function on each columns
corr_MUT6 <- extract_top_values(corr_df$MUT6, rownames(corr_df))
corr_MUT7 <- extract_top_values(corr_df$MUT7, rownames(corr_df))
corr_NEP6 <- extract_top_values(corr_df$NEP6, rownames(corr_df))
corr_ARA67 <- extract_top_values(corr_df$ARA67, rownames(corr_df))
corr_DPS567 <- extract_top_values(corr_df$DPS567, rownames(corr_df))

# "vv" means correlations between landings(value) and landings(value)
corr_MUT6_vv <- corr_MUT6 %>% mutate(across(c(Pos_Values, Neg_Values), round, 2))
corr_MUT7_vv <- corr_MUT7 %>% mutate(across(c(Pos_Values, Neg_Values), round, 2))
corr_NEP6_vv <- corr_NEP6 %>% mutate(across(c(Pos_Values, Neg_Values), round, 2))
corr_ARA67_vv <- corr_ARA67 %>% mutate(across(c(Pos_Values, Neg_Values), round, 2))
corr_DPS567_vv <- corr_DPS567 %>% mutate(across(c(Pos_Values, Neg_Values), round, 2))

# Affichage des résultats
print(corr_MUT6_vv )
print(corr_MUT7_vv)
print(corr_NEP6_vv)
print(corr_ARA67_vv)
print(corr_DPS567_vv)




### Correlations: (€/kg) vs (€/kg) ----
# Select only the columns that correspond to species
eurkg <- eurkg_df[, -(1:2)] # excluding the columns "Year" and "Quarter"
matrice_corr <- cor(eurkg)

# Correlations dataframe
corr_df <- as.data.frame(matrice_corr)

# Extract the 10th highest values (positive and negative)
extract_top_values <- function(corr_vector, species_names) {
  sorted_corr_pos <- sort(corr_vector[corr_vector > 0], decreasing = TRUE)
  sorted_corr_neg <- sort(corr_vector[corr_vector < 0], decreasing = FALSE)
  
  positive_values <- head(sorted_corr_pos, 10)
  negative_values <- head(sorted_corr_neg, 10)
  
  if (length(positive_values) < 10) {
    positive_values <- c(positive_values, rep(NA, 10 - length(positive_values)))
  }
  if (length(negative_values) < 10) {
    negative_values <- c(negative_values, rep(NA, 10 - length(negative_values)))
  }
  
  data.frame(
    Pos_Species = species_names[match(positive_values, corr_vector)],
    Pos_Values = positive_values,
    Neg_Species = species_names[match(negative_values, corr_vector)],
    Neg_Values = negative_values
  )
}

# Apply the function on the selected columns
selected_cols <- c("MUT7_EurPerKg", "NEP7_EurPerKg", "DPS567_EurPerKg", "HKE1567_EurPerKg")
corr_results <- lapply(selected_cols, function(col_name) {
  corr_df <- extract_top_values(corr_df[, col_name], rownames(corr_df))
  corr_df %>% mutate(across(c(Pos_Values, Neg_Values), round, 2))
})

names(corr_results) <- paste0(selected_cols, "_eurkg")

# Display the results
lapply(corr_results, print)


## From 2021 to 2022----
#' Now we will do the same but only with 2021-2022 data

### Correlations : landings(kg) vs landings (kg)----
# Select only the columns that corresponds to species
lpue <- lpue_df2[lpue_df2$Year >= 2021,]
species <- lpue[, -(1:2)] # excluding the columns "Year" and "Quarter"
matrice_corr <- cor(species)
print(matrice_corr)
# By default, we compute Pearsons's correlations coefficients 


# Correlations dataframes
corr_df <- as.data.frame(matrice_corr)
corr_df <- corr_df[,c("MUT6", "MUT7", "NEP6", "ARA67", "DPS567", "HKE1567")]

# Extract the 10th highest values (positive and negative)
extract_top_values <- function(corr_vector, species_names) {
  sorted_corr_pos <- sort(corr_vector[corr_vector > 0], decreasing = TRUE)
  sorted_corr_neg <- sort(corr_vector[corr_vector < 0], decreasing = FALSE)
  
  positive_values <- head(sorted_corr_pos, 10)
  negative_values <- head(sorted_corr_neg, 10)
  
  if(length(positive_values) < 10) {
    positive_values <- c(positive_values, rep(NA, 10 - length(positive_values)))
  }
  if(length(negative_values) < 10) {
    negative_values <- c(negative_values, rep(NA, 10 - length(negative_values)))
  }
  
  data.frame(
    Pos_Species = species_names[match(positive_values, corr_vector)],
    Pos_Values = positive_values,
    Neg_Species = species_names[match(negative_values, corr_vector)],
    Neg_Values = negative_values
  )
}

# Apply the function on each columns
corr_MUT6 <- extract_top_values(corr_df$MUT6, rownames(corr_df))
corr_MUT7 <- extract_top_values(corr_df$MUT7, rownames(corr_df))
corr_NEP6 <- extract_top_values(corr_df$NEP6, rownames(corr_df))
corr_ARA67 <- extract_top_values(corr_df$ARA67, rownames(corr_df))
corr_DPS567 <- extract_top_values(corr_df$DPS567, rownames(corr_df))

# "ww" means correlations between landings(weight) and landings(weight)
corr_MUT6_ww <- corr_MUT6 %>% mutate(across(c(Pos_Values, Neg_Values), round, 2))
corr_MUT7_ww <- corr_MUT7 %>% mutate(across(c(Pos_Values, Neg_Values), round, 2))
corr_NEP6_ww <- corr_NEP6 %>% mutate(across(c(Pos_Values, Neg_Values), round, 2))
corr_ARA67_ww <- corr_ARA67 %>% mutate(across(c(Pos_Values, Neg_Values), round, 2))
corr_DPS567_ww <- corr_DPS567 %>% mutate(across(c(Pos_Values, Neg_Values), round, 2))

# Affichage des résultats
print(corr_MUT6_ww )
print(corr_MUT7_ww)
print(corr_NEP6_ww)
print(corr_ARA67_ww)
print(corr_DPS567_ww)




### Correlations : landings(V€) vs landings (V€)----
# Select only the columns that corresponds to species
vpue <- vpue_df2[vpue_df2$Year >= 2021,]
species <- vpue[, -(1:2)] # excluding the columns "Year" and "Quarter"
matrice_corr <- cor(species)
print(matrice_corr)
# By default, we compute Pearsons's correlations coefficients 


# Correlations dataframes
corr_df <- as.data.frame(matrice_corr)
corr_df <- corr_df[,c("MUT6", "MUT7", "NEP6", "ARA67", "DPS567", "HKE1567")]

# Extract the 10th highest values (positive and negative)
extract_top_values <- function(corr_vector, species_names) {
  sorted_corr_pos <- sort(corr_vector[corr_vector > 0], decreasing = TRUE)
  sorted_corr_neg <- sort(corr_vector[corr_vector < 0], decreasing = FALSE)
  
  positive_values <- head(sorted_corr_pos, 10)
  negative_values <- head(sorted_corr_neg, 10)
  
  if(length(positive_values) < 10) {
    positive_values <- c(positive_values, rep(NA, 10 - length(positive_values)))
  }
  if(length(negative_values) < 10) {
    negative_values <- c(negative_values, rep(NA, 10 - length(negative_values)))
  }
  
  data.frame(
    Pos_Species = species_names[match(positive_values, corr_vector)],
    Pos_Values = positive_values,
    Neg_Species = species_names[match(negative_values, corr_vector)],
    Neg_Values = negative_values
  )
}

# Apply the function on each columns
corr_MUT6 <- extract_top_values(corr_df$MUT6, rownames(corr_df))
corr_MUT7 <- extract_top_values(corr_df$MUT7, rownames(corr_df))
corr_NEP6 <- extract_top_values(corr_df$NEP6, rownames(corr_df))
corr_ARA67 <- extract_top_values(corr_df$ARA67, rownames(corr_df))
corr_DPS567 <- extract_top_values(corr_df$DPS567, rownames(corr_df))

# "vv" means correlations between landings(value) and landings(value)
corr_MUT6_vv <- corr_MUT6 %>% mutate(across(c(Pos_Values, Neg_Values), round, 2))
corr_MUT7_vv <- corr_MUT7 %>% mutate(across(c(Pos_Values, Neg_Values), round, 2))
corr_NEP6_vv <- corr_NEP6 %>% mutate(across(c(Pos_Values, Neg_Values), round, 2))
corr_ARA67_vv <- corr_ARA67 %>% mutate(across(c(Pos_Values, Neg_Values), round, 2))
corr_DPS567_vv <- corr_DPS567 %>% mutate(across(c(Pos_Values, Neg_Values), round, 2))

# Affichage des résultats
print(corr_MUT6_vv )
print(corr_MUT7_vv)
print(corr_NEP6_vv)
print(corr_ARA67_vv)
print(corr_DPS567_vv)




### Correlations: (€/kg) vs (€/kg) ----
# Select only the columns that correspond to species
eurkg <- eurkg_df %>% 
  filter(Year >= 2021) %>%
  select(-c(1, 2)) # excluding the columns "Year" and "Quarter"
matrice_corr <- cor(eurkg)

# Correlations dataframe
corr_df <- as.data.frame(matrice_corr)

# Extract the 10th highest values (positive and negative)
extract_top_values <- function(corr_vector, species_names) {
  sorted_corr_pos <- sort(corr_vector[corr_vector > 0], decreasing = TRUE)
  sorted_corr_neg <- sort(corr_vector[corr_vector < 0], decreasing = FALSE)
  
  positive_values <- head(sorted_corr_pos, 10)
  negative_values <- head(sorted_corr_neg, 10)
  
  if (length(positive_values) < 10) {
    positive_values <- c(positive_values, rep(NA, 10 - length(positive_values)))
  }
  if (length(negative_values) < 10) {
    negative_values <- c(negative_values, rep(NA, 10 - length(negative_values)))
  }
  
  data.frame(
    Pos_Species = species_names[match(positive_values, corr_vector)],
    Pos_Values = positive_values,
    Neg_Species = species_names[match(negative_values, corr_vector)],
    Neg_Values = negative_values
  )
}

# Apply the function on the selected columns
selected_cols <- c("MUT7_EurPerKg", "NEP7_EurPerKg", "DPS567_EurPerKg", "HKE1567_EurPerKg")
corr_results <- lapply(selected_cols, function(col_name) {
  corr_df <- extract_top_values(corr_df[, col_name], rownames(corr_df))
  corr_df %>% mutate(across(c(Pos_Values, Neg_Values), round, 2))
})

names(corr_results) <- paste0(selected_cols, "_eurkg")

# Display the results
lapply(corr_results, print)

## Correlations btw value (€/kg) & landings (kg) per species----
#' We want to compute the correlations of one specie value (€/kg) and landings
#' (in kg). So we want one coefficient per specie that illustrate the strength
#' of the relation between value & landings of each species.

species <- unique(landingsV2$X3A_CODE)

# Create "land" dataframe
land <- landingsV2[, c("YEAR", "quarter", "X3A_CODE", "totwghtlandg")]
land <- reshape2::dcast(land, YEAR + quarter ~ X3A_CODE, value.var = "totwghtlandg", fun.aggregate = sum)

# Create "value" dataframe
value <- landingsV2[, c("YEAR", "quarter", "X3A_CODE", "value")]
value <- reshape2::dcast(value, YEAR + quarter ~ X3A_CODE, value.var = "value", fun.aggregate = sum)


# Compute correlation between the two data frames for each species
# The result is one coefficient per specie (between the two data frames...)

cols <- names(land[,3:ncol(land)])

land_subset <- land[, cols, drop = FALSE]
value_subset <- value[, cols, drop = FALSE]

corr_results <- list()

# Calculate correlations for each species
for (col in cols) {
  land_col <- col
  value_col <- col
  
  if (land_col %in% names(land_subset) && value_col %in% names(value_subset)) {
    corr_test <- cor.test(land_subset[, land_col], value_subset[, value_col], method = "pearson")
    corr_results[[col]] <- corr_test
  } else {
    corr_results[[col]] <- NA
  }
}

# Transforming the results list into a dataframe
variables <- names(corr_results)
cor_values <- sapply(corr_results, function(x) if (is.list(x)) x$estimate else NA)
p_values <- sapply(corr_results, function(x) if (is.list(x)) x$p.value else NA)

corr_df <- tibble(
  Espèces = variables,
  Correlation = unlist(cor_values),
  P_value = unlist(p_values)
)

# Filter out significant correlations (p-value < 0.05)
significant_corr_df <- corr_df %>%
  filter(P_value < 0.05)



## Correlations btw value (€/kg) & landings (kg) per species and PER FLEETS----
#' We want to do the same as previously but per specie-fleet duo.

# Fleets list
fleets <- unique(landingsV2$FleetIAM)

# Initialize a list to store the names of the dataframes created
df_names <- list()

# Loop of each fleet
for (fleet in fleets) {
  # Filter data for current fleet
  fleet_data <- landingsV2 %>% filter(FleetIAM == fleet)
  
  # Create 'land' and 'value' dataframes for the current fleet
  land <- fleet_data[, c("YEAR", "quarter", "X3A_CODE", "totwghtlandg")]
  land <- dcast(land, YEAR + quarter ~ X3A_CODE, value.var = "totwghtlandg", fun.aggregate = sum)
  
  value <- fleet_data[, c("YEAR", "quarter", "X3A_CODE", "value")]
  value <- dcast(value, YEAR + quarter ~ X3A_CODE, value.var = "value", fun.aggregate = sum)
  
  # Retrieve column names (excluding 'YEAR' and 'quarter')
  cols <- names(land[,3:ncol(land)])
  
  # Subsets of the original dataframe
  land_subset <- land[, cols, drop = FALSE]
  value_subset <- value[, cols, drop = FALSE]
  
  # Initialize a list to store correlation results for this fleet
  corr_results <- list()
  
  # Calculate correlations for each species
  for (col in cols) {
    land_col <- col
    value_col <- col
    
    if (land_col %in% names(land_subset) && value_col %in% names(value_subset)) {
      corr_test <- cor.test(land_subset[, land_col], value_subset[, value_col], method = "pearson")
      corr_results[[col]] <- corr_test
    } else {
      corr_results[[col]] <- NA
    }
  }
  
  # Transform the list of results into a dataframe
  variables <- names(corr_results)
  cor_values <- sapply(corr_results, function(x) if (is.list(x)) x$estimate else NA)
  p_values <- sapply(corr_results, function(x) if (is.list(x)) x$p.value else NA)
  
  corr_df <- tibble(
    Espèces = variables,
    Correlation = unlist(cor_values),
    P_value = unlist(p_values)
  )
  
  # Filter out significant correlations (p-value < 0.05)
  significant_corr_df <- corr_df %>%
    filter(P_value < 0.05)
  
  # Filter out significant correlations (p-value < 0.05)
  df_name <- paste0("corr_df_q_", fleet)
  
  # Assign dataframe with dynamically created name
  assign(df_name, significant_corr_df)
  
  # Add dataframe name to name list
  df_names[[fleet]] <- df_name
}





# Share of each species landings (in €) for each fleet----

## For all the period (2013:2022)
prop_all <- landingsV2 %>%
  select(X3A_CODE, FleetIAM, totvallandg) %>%
  group_by(X3A_CODE, FleetIAM) %>%
  summarise(totvallandg = sum(totvallandg, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = FleetIAM, values_from = totvallandg) %>%
  mutate_all(~replace_na(., 0)) %>%
  mutate(across(where(is.numeric), ~(./sum(.))*100)) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  mutate(period = "All Years")

# For the last 2 years
prop_last2 <- landingsV2 %>%
  filter(YEAR %in% c("2021", "2022")) %>%
  select(X3A_CODE, FleetIAM, totvallandg) %>%
  group_by(X3A_CODE, FleetIAM) %>%
  summarise(totvallandg = sum(totvallandg, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = FleetIAM, values_from = totvallandg) %>%
  mutate_all(~replace_na(., 0)) %>%
  mutate(across(where(is.numeric), ~(./sum(.))*100)) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  mutate(period = "Last 2 Years")

# For last year only
prop_last1 <- landingsV2 %>%
  filter(YEAR == "2022") %>%
  select(X3A_CODE, FleetIAM, totvallandg) %>%
  group_by(X3A_CODE, FleetIAM) %>%
  summarise(totvallandg = sum(totvallandg, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = FleetIAM, values_from = totvallandg) %>%
  mutate_all(~replace_na(., 0)) %>%
  mutate(across(where(is.numeric), ~(./sum(.))*100)) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  mutate(period = "Last Year")

# Combine data
prop_combined <- bind_rows(prop_all, prop_last2, prop_last1) %>%
  pivot_longer(cols = -c(X3A_CODE, period), names_to = "FleetIAM", values_to = "proportion")

# Select the 10 most important species
top_10_species <- prop_combined %>%
  group_by(FleetIAM, period) %>%
  mutate(rank = dense_rank(desc(proportion))) %>%
  filter(rank <= 10) %>%
  ungroup()

# Order 
top_10_species <- top_10_species %>%
  group_by(FleetIAM) %>%
  arrange(proportion) %>%
  ungroup()

# Graph: plot the 10 most important species, per fleets, with 3 different periods

output_file <- "Figures/FRA/prop_species_fleet.pdf"
pdf(output_file, width = 10, height = 10)

ggplot(top_10_species, aes(x = reorder(X3A_CODE, proportion), y = proportion, fill = period)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", size = 0.2) +
  facet_wrap(~FleetIAM, scales = c("free")) +
  labs(
    title = "Top 10 species per fleet per period",
    x = "Species",
    y = "Proportion (%)",
    fill = "Periods"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

dev.off()



# Create a new data frame with the percentage change between 2013-2022 & 2021-2022 periods
prop_change <- prop_last2 %>%
  pivot_longer(cols = -c(X3A_CODE, period), names_to = "FleetIAM", values_to = "value_last2") %>%
  left_join(prop_all %>% pivot_longer(cols = -c(X3A_CODE, period), names_to = "FleetIAM", values_to = "value_all"), by = c("X3A_CODE", "FleetIAM")) %>%
  mutate(pct_change = ifelse(is.na(value_all), NA, (value_last2 - value_all) / value_all * 100)) %>%
  select(-c(value_last2, value_all)) %>%
  pivot_wider(names_from = FleetIAM, values_from = pct_change) %>%
  bind_cols(prop_all %>% select(X3A_CODE, period))%>%
  select(-c(period.x, period.y, period,X3A_CODE...13))%>%
  rename(X3A_CODE = "X3A_CODE...1")%>%
  drop_na()


# Create a new data frame with the percentage change between 2013-2022 & 022 periods

# Obtenir les valeurs uniques de X3A_CODE dans les deux data frames
codes_all <- unique(prop_all$X3A_CODE)
codes_last1 <- unique(prop_last1$X3A_CODE)

# Trouver les codes en commun
common_codes <- intersect(codes_all, codes_last1)

# Filtrer les data frames pour ne garder que les lignes avec les codes en commun
prop_all_filtered <- prop_all[prop_all$X3A_CODE %in% common_codes, ]
prop_last1_filtered <- prop_last1[prop_last1$X3A_CODE %in% common_codes, ]

prop_change2 <- prop_last1_filtered %>%
  pivot_longer(cols = -c(X3A_CODE, period), names_to = "FleetIAM", values_to = "value_last2") %>%
  left_join(prop_all_filtered %>% pivot_longer(cols = -c(X3A_CODE, period), names_to = "FleetIAM", values_to = "value_all"), by = c("X3A_CODE", "FleetIAM")) %>%
  mutate(pct_change = ifelse(is.na(value_all), NA, (value_last2 - value_all) / value_all * 100)) %>%
  select(-c(value_last2, value_all)) %>%
  pivot_wider(names_from = FleetIAM, values_from = pct_change) %>%
  bind_cols(prop_all_filtered %>% select(X3A_CODE, period))%>%
  select(-c(period.x, period.y, period,X3A_CODE...13))%>%
  rename(X3A_CODE = "X3A_CODE...1")%>%
  drop_na()


# Graph between 2013:2022 and the last 2 years
melted_data <- melt(prop_change, id.vars = "X3A_CODE")

# Create a function to generate the plots
plot_fleet <- function(fleet_col) {
  ggplot(subset(melted_data, variable == fleet_col), aes(x = X3A_CODE, y = value, fill = X3A_CODE)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    labs(title = paste("Fleet:", fleet_col), x = NULL, y = "Variation Change (%)") +
    theme_minimal()+
    guides(fill="none") # No Legend
}


output_file <- "Figures/FRA/Variations/var_2013_2022_AND_LAST2.pdf"
pdf(output_file, width = 12, height = 10)

plots <- lapply(names(prop_change)[2:ncol(prop_change)], plot_fleet)
do.call(gridExtra::grid.arrange, c(plots, ncol = 3))

dev.off()


# Graph between 2013:2022 and the last 2 years
melted_data <- melt(prop_change2, id.vars = "X3A_CODE")

# Create a function to generate the plots
plot_fleet <- function(fleet_col) {
  ggplot(subset(melted_data, variable == fleet_col), aes(x = X3A_CODE, y = value, fill = X3A_CODE)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    labs(title = paste("Fleet:", fleet_col), x = NULL, y = "Variation Change (%)") +
    theme_minimal()+
    guides(fill="none") # No Legend
}


output_file <- "Figures/FRA/Variations/var_2013_2022_AND_LAST1.pdf"
pdf(output_file, width = 12, height = 10)

plots <- lapply(names(prop_change)[2:ncol(prop_change)], plot_fleet)
do.call(gridExtra::grid.arrange, c(plots, ncol = 3))

dev.off()




# Sankey's Diagram----
#' The idea here is to create a diagram with the different vessels, size, gears
#' and the amount (in term of kg landings) they fish

# DEPENDANCE ALL----
#' This one will plot the dependance (in term of landings in values) for 5 most 
#' important fleets and the 10 most important species (excluding ZZZ) from 2013
#' to 2022. You should adjust the number of fleets and species you want to display 
#' in the values of "tentaxons" and "tenfleets", according to your data.
select <- landings %>%
  select(X3A_CODE, totvallandg) %>%
  group_by(X3A_CODE) %>%
  summarise(totvallandg = sum(totvallandg, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(totvallandg))

# Let's take the  most important species in term of landings (euros) across 2013:2022
tentaxons <- unique(head(select["X3A_CODE"], n=10))
tentaxons <- unlist(tentaxons)

select2 <- landings %>%
  select(FleetIAM, totvallandg) %>%
  group_by(FleetIAM) %>%
  summarise(totvallandg = sum(totvallandg, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(totvallandg))

# Let's take the  most important fleet in term of landings (euros) across 2013:2022
tenfleets <- unique(head(select2["FleetIAM"], n=5))
tenfleets <- unlist(tenfleets)

sankey <- landings %>%
  filter (COUNTRY == "FRA") %>%
  filter (X3A_CODE != "ZZZ") %>% # Let's get rid of ZZZ species
  filter (X3A_CODE %in% tentaxons) %>%
  filter (FleetIAM %in% tenfleets) %>%
  select (FleetIAM, X3A_CODE,totvallandg) %>%
  group_by(FleetIAM, X3A_CODE) %>%
  summarise(totvallandg = sum(totvallandg, na.rm = TRUE), .groups = "drop")


# Graph: From Fleets to Species
output_file <- "Figures/FRA/Sankey/sankey_dependALL.pdf"
pdf(output_file, width = 12, height = 10)

ggplot(data = sankey,
       aes(axis1 = FleetIAM, axis2 = X3A_CODE, y = totvallandg)) +
  geom_alluvium(aes(fill = FleetIAM),# Link goes from Fleets to Species
                curve_type = "cubic", alpha = 0.7) + # Colour links between categories
  geom_stratum()+ # Categories blocks
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) + # Categories block's names
  ggtitle("Fleets dependencies to each species",
          subtitle = "6 most important fleets and 10 most important species, both in term of landings (euros), from 2013 to 2022")+
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), # Centered & bold title
    plot.subtitle = element_text(hjust = 0.5, size = 13, face = "italic") # Centered & italic SUBtitle
  )+
  guides(fill=FALSE) # No Legend

dev.off()





# DEPENDANCE 2021:2022----
#' Same idea as previously but only for the last 2 years (2021-2022)
select <- landings %>%
  filter(YEAR == c("2021","2022")) %>%
  select(X3A_CODE, totvallandg) %>%
  group_by(X3A_CODE) %>%
  summarise(totvallandg = sum(totvallandg, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(totvallandg))

# Let's take the  most important species in term of landings (euros) across 2013:2022
tentaxons <- unique(head(select["X3A_CODE"], n=10))
tentaxons <- unlist(tentaxons)

select2 <- landings %>%
  select(FleetIAM, totvallandg) %>%
  group_by(FleetIAM) %>%
  summarise(totvallandg = sum(totvallandg, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(totvallandg))

# Let's take the  most important fleet in term of landings (euros) across 2013:2022
tenfleets <- unique(head(select2["FleetIAM"], n=5))
tenfleets <- unlist(tenfleets)

sankey <- landings %>%
  filter (COUNTRY == "FRA") %>%
  filter (X3A_CODE != "ZZZ") %>%
  filter (X3A_CODE %in% tentaxons) %>%
  filter (FleetIAM %in% tenfleets) %>%
  select (FleetIAM, X3A_CODE,totvallandg) %>%
  group_by(FleetIAM, X3A_CODE) %>%
  summarise(totvallandg = sum(totvallandg, na.rm = TRUE), .groups = "drop")


# Graph: From Fleets to Species
output_file <- "Figures/FRA/Sankey/sankey_depend2122.pdf"
pdf(output_file, width = 12, height = 10)

ggplot(data = sankey,
       aes(axis1 = FleetIAM, axis2 = X3A_CODE, y = totvallandg)) +
  geom_alluvium(aes(fill = FleetIAM),# Link goes from Fleets to Species
                curve_type = "cubic", alpha = 0.7) + # Colour links between categories
  geom_stratum()+ # Categories blocks
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) + # Categories block's names
  ggtitle("Fleets dependencies to each species",
          subtitle = "6 most important fleets and 10 most important species, both in term of landings (euros), from 2013 to 2022")+
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), # Centered & bold title
    plot.subtitle = element_text(hjust = 0.5, size = 13, face = "italic") # Centered & italic SUBtitle
  )+
  guides(fill=FALSE) # No Legend

dev.off()




# CONTRIB ALL----
#' Same idea as previously but this time it's the contribution so it's in landings
#' in KG and not in values (euros). This plot is for the 2013-2022 time period.
select <- landings %>%
  select(X3A_CODE, totwghtlandg) %>%
  group_by(X3A_CODE) %>%
  summarise(totwghtlandg = sum(totwghtlandg, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(totwghtlandg))

# Let's take the  most important species in term of landings kg across 2013:2022
tentaxons <- unique(head(select["X3A_CODE"], n=10))
tentaxons <- unlist(tentaxons)

select2 <- landings %>%
  select(FleetIAM, totwghtlandg) %>%
  group_by(FleetIAM) %>%
  summarise(totwghtlandg = sum(totwghtlandg, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(totwghtlandg))

# Let's take the  most important fleet in term of landings kg across 2013:2022
tenfleets <- unique(head(select2["FleetIAM"], n=6))
tenfleets <- unlist(tenfleets)

sankey <- landings %>%
  filter (COUNTRY == "FRA") %>%
  filter (X3A_CODE != "ZZZ") %>% # Let's get rid of ZZZ species
  filter (X3A_CODE %in% tentaxons) %>%
  filter (FleetIAM %in% tenfleets) %>%
  select (FleetIAM, X3A_CODE,totwghtlandg) %>%
  group_by(FleetIAM, X3A_CODE) %>%
  summarise(totwghtlandg = sum(totwghtlandg, na.rm = TRUE), .groups = "drop")


output_file <- "Figures/FRA/Sankey/sankey_contribALL.pdf"
pdf(output_file, width = 12, height = 10)

ggplot(data = sankey, aes(axis1 = FleetIAM, axis2 = X3A_CODE, y = totwghtlandg)) +
  geom_alluvium(aes(fill = X3A_CODE), # Link goes from Species to Fleets
                curve_type = "cubic", alpha = 0.7) + # Colour links between categories
  geom_stratum() + # Categories blocks
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) + # Categories block's names
  ggtitle("Contribution of each species to landings of each fleets",
          subtitle = "6 most important fleets and 10 most important species, both in term of landings (kg), from 2013 to 2022") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    # Centered & bold title
    plot.subtitle = element_text(hjust = 0.5, size = 13, face = "italic") # Centered & italic SUBtitle
  ) +
  guides(fill = FALSE) # No Legend

dev.off()




# CONTRIB 2021-2022----
#' Same idea as previously but only for the last 2 years (2021-2022)
select <- landings %>%
  filter(YEAR == c("2021","2022")) %>%
  select(X3A_CODE, totwghtlandg) %>%
  group_by(X3A_CODE) %>%
  summarise(totwghtlandg = sum(totwghtlandg, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(totwghtlandg))

# Let's take the  most important species in term of landings kg across 2021:2022
tentaxons <- unique(head(select["X3A_CODE"], n=10))
tentaxons <- unlist(tentaxons)

select2 <- landings %>%
  select(FleetIAM, totwghtlandg) %>%
  group_by(FleetIAM) %>%
  summarise(totwghtlandg = sum(totwghtlandg, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(totwghtlandg))

# Let's take the  most important fleet in term of landings kg across 2021:2022
tenfleets <- unique(head(select2["FleetIAM"], n=6))
tenfleets <- unlist(tenfleets)

sankey <- landings %>%
  filter (COUNTRY == "FRA") %>%
  filter (X3A_CODE != "ZZZ") %>% # Let's get rid of ZZZ species
  filter (X3A_CODE %in% tentaxons) %>%
  filter (FleetIAM %in% tenfleets) %>%
  select (FleetIAM, X3A_CODE,totwghtlandg) %>%
  group_by(FleetIAM, X3A_CODE) %>%
  summarise(totwghtlandg = sum(totwghtlandg, na.rm = TRUE), .groups = "drop")


output_file <- "Figures/FRA/Sankey/sankey_contrib2122.pdf"
pdf(output_file, width = 12, height = 10)

ggplot(data = sankey, aes(axis1 = FleetIAM, axis2 = X3A_CODE, y = totwghtlandg)) +
  geom_alluvium(aes(fill = X3A_CODE), # Link goes from Species to Fleets
                curve_type = "cubic", alpha = 0.7) + # Colour links between categories
  geom_stratum() + # Categories blocks
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) + # Categories block's names
  ggtitle("Contribution of each species to landings of each fleets",
          subtitle = "6 most important fleets and 10 most important species, both in term of landings (kg), from 2021 to 2022") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    # Centered & bold title
    plot.subtitle = element_text(hjust = 0.5, size = 13, face = "italic") # Centered & italic SUBtitle
  ) +
  guides(fill = FALSE) # No Legend

dev.off()





# Stacked barplots----
## Proportion of species landings per GSA
twentaxons <- unique(head(select["X3A_CODE"], n=20))
twentaxons <- unlist(twentaxons)

# Data frame
spgsa <- landings %>%
  select (X3A_CODE, SUB_REGION, totwghtlandg) %>%
  filter (X3A_CODE %in% twentaxons) %>%
  group_by(SUB_REGION, X3A_CODE) %>%
  summarise(totwghtlandg = sum(totwghtlandg))

# Graph
spgsa_plot <- ggplot(spgsa, aes(x = SUB_REGION, y = totwghtlandg, fill = X3A_CODE)) +
  geom_bar(position = "fill", stat="identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Sub-Regions", y = "Proportion", fill = "Species Code")+
  ggtitle("Proportion of Landings")+
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"))+
  theme(axis.text.x = element_text(hjust = 1, vjust = 1, face = "bold"),
        legend.position = "right")




## Proportion of fleets landings per GSA----
# Data frame
flgsa <- landings %>%
  select (SUB_REGION, FleetIAM, totwghtlandg) %>%
  group_by(FleetIAM, SUB_REGION) %>%
  summarise(totwghtlandg = sum(totwghtlandg))

# Graph
flgsa_plot <- ggplot(flgsa, aes(x = SUB_REGION, y = totwghtlandg, fill = FleetIAM)) +
  geom_bar(position = "fill", stat="identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Sub-Regions", y = "Proportion", fill = "Fleets Code")+
  ggtitle("Proportion of Landings")+
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"))+
  theme(axis.text.x = element_text(hjust = 1, vjust = 1, face = "bold"),
        legend.position = "right")


# Graph: Export plots
# combined_plot <- plot_grid(spgsa_plot, flgsa_plot, ncol = 1, align = "v")
# output_path <- "Figures/FRA/"
# output_file <- file.path(output_path, "proportions.pdf")
# cairo_pdf(output_file, width = 8, height = 12)
# print(combined_plot)
# dev.off()
# 





# Waffle chart----
## Waffle fleets----
waff_df <- landings %>%
  select(YEAR, FleetIAM, totwghtlandg)%>%
  group_by(YEAR,FleetIAM) %>%
  summarise(totwghtlandg = sum(totwghtlandg)) %>%
  #' We have to divide by 10 because there is one square per value and we have
  #' thousands of values
  mutate(totwghtlandg = totwghtlandg / 14)

mean(waff_df$totwghtlandg)

# Graph
# output_file <- "Figures/FRA/waffle_fleets.pdf"
# pdf(output_file, width = 12, height = 10)
# 
# ggplot(waff_df, aes(fill = FleetIAM, values = totwghtlandg)) +
#   geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
#   facet_wrap(~YEAR, nrow = 1, strip.position = "bottom") +
#   scale_x_discrete() + 
#   scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
#                      expand = c(0,0))+
#   MetBrewer::scale_fill_met_d("Hiroshige", direction=1)+
#   coord_equal()+
#   labs(title = "Proportions of french fleets in terms of landings (in kg)",
#        subtitle = "during the 2013-2022 period")+
#   theme_minimal()+
#   theme(
#     plot.title = element_text(size = 13, face = "bold"),
#     plot.background = element_rect(color="white", fill="white"),
#     plot.margin = margin(20, 40, 20, 40)
#   )
# 
# dev.off()



## Waffle species----
waff_df2 <- landings %>%
  select(YEAR, X3A_CODE, totwghtlandg)%>%
  filter (X3A_CODE %in% tentaxons) %>%
  group_by(YEAR,X3A_CODE) %>%
  summarise(totwghtlandg = sum(totwghtlandg)) %>%
  #' We have to divide by 10 because there is one square per value and we have
  #' thousands of values
  mutate(totwghtlandg = totwghtlandg / 14)

mean(waff_df2$totwghtlandg)


# Graph
# output_file <- "Figures/FRA/waffle_species.pdf"
# pdf(output_file, width = 12, height = 10)
# 
# ggplot(waff_df2, aes(fill = X3A_CODE, values = totwghtlandg,alpha = 1)) +
#   geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
#   facet_wrap(~YEAR, nrow = 1, strip.position = "bottom") +
#   scale_x_discrete() + 
#   scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
#                      expand = c(0,0))+
#   coord_equal()+
#   labs(title = "Proportions of 10 most important Species in terms of landings (in kg)",
#        subtitle = "during the 2013-2022 period")+
#   theme_minimal()+
#   theme(
#     plot.title = element_text(size = 13, face = "bold"),
#     plot.background = element_rect(color="white", fill="white"),
#     plot.margin = margin(20, 40, 20, 40)
#   )
# 
# dev.off()



# Histograms for Sophie's presentation----
#' The idea is to represent the 5 dynamic species (HKE, MUT, NEP, ARA, DPS) on 
#' a histogram for different periods, and show the difference in term of landings
#' between the French and the Spanish.
#' 
#' Because we previously filtered our data to get only French data, we have to 
#' import and clean the data again.

FDI_J <- read.csv("Data/Data_EWG2311/tableJ_EWG2311.csv")

FDI_J <- subset(FDI_J,
                fishing_tech != "INACTIVE" &
                  country_code != "ITA" &
                  supra_region == "MBS" & 
                  principal_sub_region %in% c("GSA1", "GSA2", "GSA5", "GSA6", "GSA7"))

FDI_J <- FDI_J %>%
  rename(
    COUNTRY = country_code,
    YEAR = year,
    SUPRA_REGION = supra_region,
    FISHING_TECH = fishing_tech,
    VESSEL_LENGTH = vessel_length,
    GEO_INDICATOR = geo_indicator) 

FDI_J <- FDI_J %>%
  mutate(FleetIAM = case_when(
    COUNTRY == "ESP" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL0612") ~ "ESP_DTS_<12m",
    COUNTRY == "ESP" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL1218") ~ "ESP_DTS_12-18m",
    COUNTRY == "ESP" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL1824") ~ "ESP_DTS_18-24m",
    COUNTRY == "ESP" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL2440") ~ "ESP_DTS_>=24m",
    COUNTRY == "ESP" & FISHING_TECH == "DFN" & VESSEL_LENGTH %in% c("VL0612") ~ "ESP_DFN_06-12m",
    COUNTRY == "ESP" & FISHING_TECH == "DFN" & VESSEL_LENGTH %in% c("VL1218") ~ "ESP_DFN_12-18m",
    COUNTRY == "ESP" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL0612") ~ "ESP_HOK_06-12m",
    COUNTRY == "ESP" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL1218") ~ "ESP_HOK_12-18m",
    COUNTRY == "ESP" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL1824") ~ "ESP_HOK_18-24m",
    COUNTRY == "ESP" & VESSEL_LENGTH %in% c("VL0006", "VL0612") ~ "Other_<=12m",
    COUNTRY == "ESP" & VESSEL_LENGTH %in% c("VL1218", "VL1824","VL2440","VL40XX") ~ "Other_>=12m",
    COUNTRY == "ESP" ~ "Other",
    COUNTRY == "FRA" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL1824") ~ "FRA_DTS_18-24m",
    COUNTRY == "FRA" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL2440") ~ "FRA_DTS_>24m",
    COUNTRY == "FRA" & FISHING_TECH == "DFN" & VESSEL_LENGTH %in% c("VL0006") ~ "FRA_DFN_00-06m",
    COUNTRY == "FRA" & FISHING_TECH == "DFN" & VESSEL_LENGTH %in% c("VL0612") ~ "FRA_DFN_06-12m",
    COUNTRY == "FRA" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL0006") ~ "FRA_HOK_00-06m",
    COUNTRY == "FRA" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL0612") ~ "FRA_HOK_06-12m",
    COUNTRY == "FRA" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL1218") ~ "FRA_HOK_12-18m",
    COUNTRY == "FRA" & VESSEL_LENGTH %in% c("VL0006", "VL0612") ~ "Other_<=12m",
    COUNTRY == "FRA" & VESSEL_LENGTH %in% c("VL1218", "VL1824","VL2440","VL40XX") ~ "Other_>=12m",
    COUNTRY == "FRA" ~ "Other",
    TRUE ~ NA_character_))

FDI_J <- FDI_J %>%
  mutate(Segment = case_when(
    COUNTRY == "ESP" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL0612") ~ "Spanish trawlers < 12m",
    COUNTRY == "ESP" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL1218") ~ "Spanish trawlers 12-18m",
    COUNTRY == "ESP" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL1824") ~ "Spanish trawlers 18-24m",
    COUNTRY == "ESP" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL2440") ~ "Spanish trawlers >=24m",
    COUNTRY == "ESP" & FISHING_TECH == "DFN" & VESSEL_LENGTH %in% c("VL0612") ~ "Spanish netters 06-12m",
    COUNTRY == "ESP" & FISHING_TECH == "DFN" & VESSEL_LENGTH %in% c("VL1218") ~ "Spanish netters 12-18m",
    COUNTRY == "ESP" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL0612") ~ "Spanish vessels using hooks 06-12m",
    COUNTRY == "ESP" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL1218") ~ "Spanish vessels using hooks 12-18m",
    COUNTRY == "ESP" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL1824") ~ "Spanish vessels using hooks 18-24m",
    COUNTRY == "ESP" & VESSEL_LENGTH %in% c("VL0006", "VL0612") ~ "Other inf 12m",
    COUNTRY == "ESP" & VESSEL_LENGTH %in% c("VL1218", "VL1824","VL2440","VL40XX") ~ "Other sup 12m",
    COUNTRY == "ESP" ~ "Other",
    COUNTRY == "FRA" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL1824") ~ "French demersal trawlers 18-24m",
    COUNTRY == "FRA" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL2440") ~ "French demersal trawlers 24-40m",
    COUNTRY == "FRA" & FISHING_TECH == "DFN" & VESSEL_LENGTH %in% c("VL0006") ~ "French netters < 6m",
    COUNTRY == "FRA" & FISHING_TECH == "DFN" & VESSEL_LENGTH %in% c("VL0612") ~ "French netters 06-12m",
    COUNTRY == "FRA" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL0006") ~ "Hook < 6m",
    COUNTRY == "FRA" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL0612") ~ "Hook 06-12m",
    COUNTRY == "FRA" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL1218") ~ "Hook 12-18m",
    COUNTRY == "FRA" & VESSEL_LENGTH %in% c("VL0006", "VL0612") ~ "Other inf 12m",
    COUNTRY == "FRA" & VESSEL_LENGTH %in% c("VL1218", "VL1824","VL2440","VL40XX") ~ "Other sup 12m",
    COUNTRY == "FRA" ~ "Other",
    TRUE ~ NA_character_))


FDI_G <- read.csv("Data/Data_EWG2311/tableG_EWG2311.csv")

FDI_G <- data.frame(FDI_G) 
dim(FDI_G)


FDI_G <- FDI_G %>%
  rename(
    COUNTRY = country_code,
    YEAR = year,
    SUPRA_REGION = supra_region,
    FISHING_TECH = fishing_tech,
    VESSEL_LENGTH = vessel_length,
    GEO_INDICATOR = geo_indicator)


effort <-  merge(FDI_G, unique(FDI_J[,c("COUNTRY","YEAR","VESSEL_LENGTH","FISHING_TECH","SUPRA_REGION"
                                        ,"Segment","FleetIAM")]))


taxons_select <- read_excel("Perso/Species_Focus_CodeDescription_NOA.xlsx")
FDI_A <- read.csv("Data/Data_EWG2311/tableA_EWG2311.csv")

FDI_A <- subset(FDI_A,
                fishing_tech != "INACTIVE" &
                  country_code != "ITA" &
                  supra_region == "MBS" & 
                  sub_region %in% c("GSA1", "GSA2", "GSA5", "GSA6", "GSA7") &
                  !(species %in% c("BFT"))) ### excluding Red Tunas

FDI_A <- FDI_A %>%
  rename(
    COUNTRY = country_code,
    YEAR = year,
    SUPRA_REGION = supra_region,
    FISHING_TECH = fishing_tech,
    VESSEL_LENGTH = vessel_length,
    GEO_INDICATOR = geo_indicator,
    SPECIES = species,
    SUB_REGION = sub_region)





#### Merging gregoire's taxons selection and table A
# Convert dataframes in data.table format
FDI_A <- as.data.table(FDI_A)
taxons_select <- as.data.table(taxons_select)


# Create a "key" to speed up the research
setkey(taxons_select, `Other taxonomic code included`)

# Initialize the new column
FDI_A[, New_Code := NA_character_]

# Divide FDI_A in multiple chunks 
split_size <- 100000 # Adjust this value according to the memory available
splits <- split(FDI_A, ceiling(seq_len(FDI_A[, .N]) / split_size))

# Joint by chunks 
for (chunk in splits) {
  for (code in unique(taxons_select$`Other taxonomic code included`)) {
    codes <- unlist(strsplit(code, ", "))
    chunk[SPECIES %in% codes, New_Code := taxons_select[code, x.X3A_CODE]]}}

# Gather chunks 
FDI_A <- rbindlist(splits)

length(unique(FDI_A$New_Code)) # 37 taxons "non vides", 46 au total

# Put "NA" in "ZZZ"
FDI_A <- FDI_A %>%
  rename(X3A_CODE = New_Code) %>%
  mutate(X3A_CODE = ifelse(is.na(X3A_CODE), "ZZZ", X3A_CODE))


landings <- FDI_A

length(unique(landings$X3A_CODE)) # 46 taxons
length(unique(taxons_select$X3A_CODE)) # for a total of 50
setdiff(unique(taxons_select$X3A_CODE), unique(landings$X3A_CODE)) 
# Species not present : ARI_bis, KTT, PHA and XOX

#### Creating the IAM fleets typo in table "landings"
landings <- landings %>%
  mutate(FleetIAM = case_when(
    COUNTRY == "ESP" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL0612") ~ "ESP_DTS_<12m",
    COUNTRY == "ESP" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL1218") ~ "ESP_DTS_12-18m",
    COUNTRY == "ESP" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL1824") ~ "ESP_DTS_18-24m",
    COUNTRY == "ESP" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL2440") ~ "ESP_DTS_>=24m",
    COUNTRY == "ESP" & FISHING_TECH == "DFN" & VESSEL_LENGTH %in% c("VL0612") ~ "ESP_DFN_06-12m",
    COUNTRY == "ESP" & FISHING_TECH == "DFN" & VESSEL_LENGTH %in% c("VL1218") ~ "ESP_DFN_12-18m",
    COUNTRY == "ESP" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL0612") ~ "ESP_HOK_06-12m",
    COUNTRY == "ESP" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL1218") ~ "ESP_HOK_12-18m",
    COUNTRY == "ESP" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL1824") ~ "ESP_HOK_18-24m",
    COUNTRY == "ESP" & VESSEL_LENGTH %in% c("VL0006", "VL0612") ~ "Other_<=12m",
    COUNTRY == "ESP" & VESSEL_LENGTH %in% c("VL1218", "VL1824","VL2440","VL40XX") ~ "Other_>=12m",
    COUNTRY == "ESP" ~ "Other",
    COUNTRY == "FRA" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL1824") ~ "FRA_DTS_18-24m",
    COUNTRY == "FRA" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL2440") ~ "FRA_DTS_>24m",
    COUNTRY == "FRA" & FISHING_TECH == "DFN" & VESSEL_LENGTH %in% c("VL0006") ~ "FRA_DFN_00-06m",
    COUNTRY == "FRA" & FISHING_TECH == "DFN" & VESSEL_LENGTH %in% c("VL0612") ~ "FRA_DFN_06-12m",
    COUNTRY == "FRA" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL0006") ~ "FRA_HOK_00-06m",
    COUNTRY == "FRA" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL0612") ~ "FRA_HOK_06-12m",
    COUNTRY == "FRA" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL1218") ~ "FRA_HOK_12-18m",
    COUNTRY == "FRA" & VESSEL_LENGTH %in% c("VL0006", "VL0612") ~ "Other_<=12m",
    COUNTRY == "FRA" & VESSEL_LENGTH %in% c("VL1218", "VL1824","VL2440","VL40XX") ~ "Other_>=12m",
    COUNTRY == "FRA" ~ "Other",
    TRUE ~ NA_character_))


#### Creating a segment column
landings <- landings %>%
  mutate(Segment = case_when(
    COUNTRY == "ESP" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL0612") ~ "Spanish trawlers < 12m",
    COUNTRY == "ESP" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL1218") ~ "Spanish trawlers 12-18m",
    COUNTRY == "ESP" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL1824") ~ "Spanish trawlers 18-24m",
    COUNTRY == "ESP" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL2440") ~ "Spanish trawlers >=24m",
    COUNTRY == "ESP" & FISHING_TECH == "DFN" & VESSEL_LENGTH %in% c("VL0612") ~ "Spanish netters 06-12m",
    COUNTRY == "ESP" & FISHING_TECH == "DFN" & VESSEL_LENGTH %in% c("VL1218") ~ "Spanish netters 12-18m",
    COUNTRY == "ESP" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL0612") ~ "Spanish vessels using hooks 06-12m",
    COUNTRY == "ESP" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL1218") ~ "Spanish vessels using hooks 12-18m",
    COUNTRY == "ESP" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL1824") ~ "Spanish vessels using hooks 18-24m",
    COUNTRY == "ESP" & VESSEL_LENGTH %in% c("VL0006", "VL0612") ~ "Other inf 12m",
    COUNTRY == "ESP" & VESSEL_LENGTH %in% c("VL1218", "VL1824","VL2440","VL40XX") ~ "Other sup 12m",
    COUNTRY == "ESP" ~ "Other",
    COUNTRY == "FRA" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL1824") ~ "French demersal trawlers 18-24m",
    COUNTRY == "FRA" & FISHING_TECH == "DTS" & VESSEL_LENGTH %in% c("VL2440") ~ "French demersal trawlers 24-40m",
    COUNTRY == "FRA" & FISHING_TECH == "DFN" & VESSEL_LENGTH %in% c("VL0006") ~ "French netters < 6m",
    COUNTRY == "FRA" & FISHING_TECH == "DFN" & VESSEL_LENGTH %in% c("VL0612") ~ "French netters 06-12m",
    COUNTRY == "FRA" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL0006") ~ "Hook < 6m",
    COUNTRY == "FRA" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL0612") ~ "Hook 06-12m",
    COUNTRY == "FRA" & FISHING_TECH == "HOK" & VESSEL_LENGTH %in% c("VL1218") ~ "Hook 12-18m",
    COUNTRY == "FRA" & VESSEL_LENGTH %in% c("VL0006", "VL0612") ~ "Other inf 12m",
    COUNTRY == "FRA" & VESSEL_LENGTH %in% c("VL1218", "VL1824","VL2440","VL40XX") ~ "Other sup 12m",
    COUNTRY == "FRA" ~ "Other",
    TRUE ~ NA_character_))


## ALL PERIOD----
## Plot the histogram for ALL fleets ALL period----
thefive <- landings %>%
  filter (X3A_CODE %in% c("HKE","MUT","NEP","ARA","DPS")) %>%
  filter (COUNTRY %in% c("FRA","ESP")) %>%
  select (COUNTRY, X3A_CODE, totwghtlandg) %>%
  group_by(COUNTRY, X3A_CODE) %>%
  summarise(totwghtlandg = sum(totwghtlandg, na.rm = TRUE), .groups = "drop")



ALL_2013_2022 <- ggplot(thefive, aes(x = X3A_CODE, y = totwghtlandg, fill = COUNTRY)) +
  geom_bar(stat = "identity",
           position = "stack",
           orientation = "horizontal") +
  coord_flip() +
  scale_fill_manual(values = c("ESP" = "red", "FRA" = "blue"),
                    name = "Pays") +
  labs(
    x = "Espèce",
    y = "Débarquements en tonnes",
    title = "Débarquements cumulés par espèce et par pays: toutes flottilles confondues",
    subtitle = "Données FDI sur la période 2013-2022"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.background = element_rect(color = "white", fill = "white"),
    plot.margin = margin(20, 40, 20, 40),
    axis.title.x = element_text(face = "bold", margin = margin(t = 20)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 20))
  )


## Plot the histogram for ALL fleets ALL period WITH ZZZ----
thefive <- landings %>%
  mutate(X3A_CODE = fct_other(
    X3A_CODE,
    keep = c("HKE", "MUT", "NEP", "ARA", "DPS"),
    other_level = "ZZZ"
  )) %>%
  filter (COUNTRY %in% c("FRA", "ESP")) %>%
  select (COUNTRY, X3A_CODE, totwghtlandg) %>%
  group_by(COUNTRY, X3A_CODE) %>%
  summarise(totwghtlandg = sum(totwghtlandg, na.rm = TRUE),
            .groups = "drop")



ALL_2013_2022_ZZZ <- ggplot(thefive, aes(x = X3A_CODE, y = totwghtlandg, fill = COUNTRY)) +
  geom_bar(stat = "identity",
           position = "stack",
           orientation = "horizontal") +
  coord_flip() +
  scale_fill_manual(values = c("ESP" = "red", "FRA" = "blue"),
                    name = "Pays") +
  labs(
    x = "Espèce",
    y = "Débarquements en tonnes",
    title = "Débarquements cumulés par espèce et par pays: toutes flottilles confondues",
    subtitle = "Données FDI sur la période 2013-2022"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.background = element_rect(color = "white", fill = "white"),
    plot.margin = margin(20, 40, 20, 40),
    axis.title.x = element_text(face = "bold", margin = margin(t = 20)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 20))
  )


## Plot the histogram for DTS fleets ALL period----
thefive <- landings %>%
  filter (X3A_CODE %in% c("HKE","MUT","NEP","ARA","DPS")) %>%
  filter (FISHING_TECH %in% c("DTS")) %>%
  filter (COUNTRY %in% c("FRA","ESP")) %>%
  select (COUNTRY, X3A_CODE, totwghtlandg) %>%
  group_by(COUNTRY, X3A_CODE) %>%
  summarise(totwghtlandg = sum(totwghtlandg, na.rm = TRUE), .groups = "drop")



DTS_2013_2022 <- ggplot(thefive, aes(x = X3A_CODE, y = totwghtlandg, fill = COUNTRY)) +
  geom_bar(stat = "identity",
           position = "stack",
           orientation = "horizontal") +
  coord_flip() +
  scale_fill_manual(values = c("ESP" = "red", "FRA" = "blue"),
                    name = "Pays") +
  labs(
    x = "Espèce",
    y = "Débarquements en tonnes",
    title = "Débarquements cumulés par espèce et par pays: Chalutiers (DTS)",
    subtitle = "Données FDI sur la période 2013-2022"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.background = element_rect(color = "white", fill = "white"),
    plot.margin = margin(20, 40, 20, 40),
    axis.title.x = element_text(face = "bold", margin = margin(t = 20)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 20))
  )

## Plot the histogram for DTS fleets ALL period with ZZZ----
thefive <- landings %>%
  mutate(X3A_CODE = fct_other(
    X3A_CODE,
    keep = c("HKE", "MUT", "NEP", "ARA", "DPS"),
    other_level = "ZZZ"
  )) %>%
  filter (FISHING_TECH %in% c("DTS")) %>%
  filter (COUNTRY %in% c("FRA", "ESP")) %>%
  select (COUNTRY, X3A_CODE, totwghtlandg) %>%
  group_by(COUNTRY, X3A_CODE) %>%
  summarise(totwghtlandg = sum(totwghtlandg, na.rm = TRUE),
            .groups = "drop")



DTS_2013_2022_ZZZ <- ggplot(thefive, aes(x = X3A_CODE, y = totwghtlandg, fill = COUNTRY)) +
  geom_bar(stat = "identity",
           position = "stack",
           orientation = "horizontal") +
  coord_flip() +
  scale_fill_manual(values = c("ESP" = "red", "FRA" = "blue"),
                    name = "Pays") +
  labs(
    x = "Espèce",
    y = "Débarquements en tonnes",
    title = "Débarquements cumulés par espèce et par pays: Chalutiers (DTS)",
    subtitle = "Données FDI sur la période 2013-2022"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.background = element_rect(color = "white", fill = "white"),
    plot.margin = margin(20, 40, 20, 40),
    axis.title.x = element_text(face = "bold", margin = margin(t = 20)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 20))
  )

## Plot the histogram for DFN fleets ALL period----
thefive <- landings %>%
  filter (X3A_CODE %in% c("HKE","MUT","NEP","ARA","DPS")) %>%
  filter (FISHING_TECH %in% c("DFN")) %>%
  filter (COUNTRY %in% c("FRA","ESP")) %>%
  select (COUNTRY, X3A_CODE, totwghtlandg) %>%
  group_by(COUNTRY, X3A_CODE) %>%
  summarise(totwghtlandg = sum(totwghtlandg, na.rm = TRUE), .groups = "drop")



DFN_2013_2022 <- ggplot(thefive, aes(x = X3A_CODE, y = totwghtlandg, fill = COUNTRY)) +
  geom_bar(stat = "identity",
           position = "stack",
           orientation = "horizontal") +
  coord_flip() +
  scale_fill_manual(values = c("ESP" = "red", "FRA" = "blue"),
                    name = "Pays") +
  labs(
    x = "Espèce",
    y = "Débarquements en tonnes",
    title = "Débarquements cumulés par espèce et par pays: Fileyeurs (DFN)",
    subtitle = "Données FDI sur la période 2013-2022"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.background = element_rect(color = "white", fill = "white"),
    plot.margin = margin(20, 40, 20, 40),
    axis.title.x = element_text(face = "bold", margin = margin(t = 20)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 20))
  )


## Plot the histogram for DFN fleets ALL period with ZZZ----
thefive <- landings %>%
  mutate(X3A_CODE = fct_other(
    X3A_CODE,
    keep = c("HKE", "MUT", "NEP", "ARA", "DPS"),
    other_level = "ZZZ"
  )) %>%
  filter (FISHING_TECH %in% c("DFN")) %>%
  filter (COUNTRY %in% c("FRA", "ESP")) %>%
  select (COUNTRY, X3A_CODE, totwghtlandg) %>%
  group_by(COUNTRY, X3A_CODE) %>%
  summarise(totwghtlandg = sum(totwghtlandg, na.rm = TRUE),
            .groups = "drop")



DFN_2013_2022_ZZZ <- ggplot(thefive, aes(x = X3A_CODE, y = totwghtlandg, fill = COUNTRY)) +
  geom_bar(stat = "identity",
           position = "stack",
           orientation = "horizontal") +
  coord_flip() +
  scale_fill_manual(values = c("ESP" = "red", "FRA" = "blue"),
                    name = "Pays") +
  labs(
    x = "Espèce",
    y = "Débarquements en tonnes",
    title = "Débarquements cumulés par espèce et par pays: Fileyeurs (DFN)",
    subtitle = "Données FDI sur la période 2013-2022"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.background = element_rect(color = "white", fill = "white"),
    plot.margin = margin(20, 40, 20, 40),
    axis.title.x = element_text(face = "bold", margin = margin(t = 20)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 20))
  )

## Plot the histogram for HOK fleets ALL period----
thefive <- landings %>%
  filter (X3A_CODE %in% c("HKE","MUT","NEP","ARA","DPS")) %>%
  filter (FISHING_TECH %in% c("HOK")) %>%
  filter (COUNTRY %in% c("FRA","ESP")) %>%
  select (COUNTRY, X3A_CODE, totwghtlandg) %>%
  group_by(COUNTRY, X3A_CODE) %>%
  summarise(totwghtlandg = sum(totwghtlandg, na.rm = TRUE), .groups = "drop")



HOK_2013_2022 <- ggplot(thefive, aes(x = X3A_CODE, y = totwghtlandg, fill = COUNTRY)) +
  geom_bar(stat = "identity",
           position = "stack",
           orientation = "horizontal") +
  coord_flip() +
  scale_fill_manual(values = c("ESP" = "red", "FRA" = "blue"),
                    name = "Pays") +
  labs(
    x = "Espèce",
    y = "Débarquements en tonnes",
    title = "Débarquements cumulés par espèce et par pays: ligneurs (HOK)",
    subtitle = "Données FDI sur la période 2013-2022"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.background = element_rect(color = "white", fill = "white"),
    plot.margin = margin(20, 40, 20, 40),
    axis.title.x = element_text(face = "bold", margin = margin(t = 20)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 20))
  )

## Plot the histogram for HOK fleets ALL period----
thefive <- landings %>%
  mutate(X3A_CODE = fct_other(
    X3A_CODE,
    keep = c("HKE", "MUT", "NEP", "ARA", "DPS"),
    other_level = "ZZZ"
  )) %>%
  filter (FISHING_TECH %in% c("HOK")) %>%
  filter (COUNTRY %in% c("FRA", "ESP")) %>%
  select (COUNTRY, X3A_CODE, totwghtlandg) %>%
  group_by(COUNTRY, X3A_CODE) %>%
  summarise(totwghtlandg = sum(totwghtlandg, na.rm = TRUE),
            .groups = "drop")



HOK_2013_2022_ZZZ <- ggplot(thefive, aes(x = X3A_CODE, y = totwghtlandg, fill = COUNTRY)) +
  geom_bar(stat = "identity",
           position = "stack",
           orientation = "horizontal") +
  coord_flip() +
  scale_fill_manual(values = c("ESP" = "red", "FRA" = "blue"),
                    name = "Pays") +
  labs(
    x = "Espèce",
    y = "Débarquements en tonnes",
    title = "Débarquements cumulés par espèce et par pays: ligneurs (HOK)",
    subtitle = "Données FDI sur la période 2013-2022"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.background = element_rect(color = "white", fill = "white"),
    plot.margin = margin(20, 40, 20, 40),
    axis.title.x = element_text(face = "bold", margin = margin(t = 20)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 20))
  )






## 2021-2022 PERIOD----
## Plot the histogram for ALL fleets ALL period----
thefive <- landings %>%
  filter (X3A_CODE %in% c("HKE","MUT","NEP","ARA","DPS")) %>%
  filter (YEAR %in% c(2021,2022)) %>%
  filter (COUNTRY %in% c("FRA","ESP")) %>%
  select (COUNTRY, X3A_CODE, totwghtlandg) %>%
  group_by(COUNTRY, X3A_CODE) %>%
  summarise(totwghtlandg = sum(totwghtlandg, na.rm = TRUE), .groups = "drop")



ALL_2021_2022 <- ggplot(thefive, aes(x = X3A_CODE, y = totwghtlandg, fill = COUNTRY)) +
  geom_bar(stat = "identity",
           position = "stack",
           orientation = "horizontal") +
  coord_flip() +
  scale_fill_manual(values = c("ESP" = "red", "FRA" = "blue"),
                    name = "Pays") +
  labs(
    x = "Espèce",
    y = "Débarquements en tonnes",
    title = "Débarquements cumulés par espèce et par pays: toutes flottilles confondues",
    subtitle = "Données FDI sur la période 2021-2022"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.background = element_rect(color = "white", fill = "white"),
    plot.margin = margin(20, 40, 20, 40),
    axis.title.x = element_text(face = "bold", margin = margin(t = 20)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 20))
  )


## Plot the histogram for ALL fleets 2021-2022 period WITH ZZZ----
thefive <- landings %>%
  mutate(X3A_CODE = fct_other(
    X3A_CODE,
    keep = c("HKE", "MUT", "NEP", "ARA", "DPS"),
    other_level = "ZZZ"
  )) %>%
  filter (COUNTRY %in% c("FRA", "ESP")) %>%
  filter (YEAR %in% c(2021,2022)) %>%
  select (COUNTRY, X3A_CODE, totwghtlandg) %>%
  group_by(COUNTRY, X3A_CODE) %>%
  summarise(totwghtlandg = sum(totwghtlandg, na.rm = TRUE),
            .groups = "drop")



ALL_2021_2022_ZZZ <- ggplot(thefive, aes(x = X3A_CODE, y = totwghtlandg, fill = COUNTRY)) +
  geom_bar(stat = "identity",
           position = "stack",
           orientation = "horizontal") +
  coord_flip() +
  scale_fill_manual(values = c("ESP" = "red", "FRA" = "blue"),
                    name = "Pays") +
  labs(
    x = "Espèce",
    y = "Débarquements en tonnes",
    title = "Débarquements cumulés par espèce et par pays: toutes flottilles confondues",
    subtitle = "Données FDI sur la période 2021-2022"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.background = element_rect(color = "white", fill = "white"),
    plot.margin = margin(20, 40, 20, 40),
    axis.title.x = element_text(face = "bold", margin = margin(t = 20)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 20))
  )


## Plot the histogram for DTS fleets 2021-2022 period----
thefive <- landings %>%
  filter (X3A_CODE %in% c("HKE","MUT","NEP","ARA","DPS")) %>%
  filter (FISHING_TECH %in% c("DTS")) %>%
  filter (COUNTRY %in% c("FRA","ESP")) %>%
  filter (YEAR %in% c(2021,2022)) %>%
  select (COUNTRY, X3A_CODE, totwghtlandg) %>%
  group_by(COUNTRY, X3A_CODE) %>%
  summarise(totwghtlandg = sum(totwghtlandg, na.rm = TRUE), .groups = "drop")



DTS_2021_2022 <- ggplot(thefive, aes(x = X3A_CODE, y = totwghtlandg, fill = COUNTRY)) +
  geom_bar(stat = "identity",
           position = "stack",
           orientation = "horizontal") +
  coord_flip() +
  scale_fill_manual(values = c("ESP" = "red", "FRA" = "blue"),
                    name = "Pays") +
  labs(
    x = "Espèce",
    y = "Débarquements en tonnes",
    title = "Débarquements cumulés par espèce et par pays: Chalutiers (DTS)",
    subtitle = "Données FDI sur la période 2021-2022"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.background = element_rect(color = "white", fill = "white"),
    plot.margin = margin(20, 40, 20, 40),
    axis.title.x = element_text(face = "bold", margin = margin(t = 20)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 20))
  )

## Plot the histogram for DTS fleets 2021-2022 period with ZZZ----
thefive <- landings %>%
  mutate(X3A_CODE = fct_other(
    X3A_CODE,
    keep = c("HKE", "MUT", "NEP", "ARA", "DPS"),
    other_level = "ZZZ"
  )) %>%
  filter (FISHING_TECH %in% c("DTS")) %>%
  filter (COUNTRY %in% c("FRA", "ESP")) %>%
  filter (YEAR %in% c(2021,2022)) %>%
  select (COUNTRY, X3A_CODE, totwghtlandg) %>%
  group_by(COUNTRY, X3A_CODE) %>%
  summarise(totwghtlandg = sum(totwghtlandg, na.rm = TRUE),
            .groups = "drop")



DTS_2021_2022_ZZZ <- ggplot(thefive, aes(x = X3A_CODE, y = totwghtlandg, fill = COUNTRY)) +
  geom_bar(stat = "identity",
           position = "stack",
           orientation = "horizontal") +
  coord_flip() +
  scale_fill_manual(values = c("ESP" = "red", "FRA" = "blue"),
                    name = "Pays") +
  labs(
    x = "Espèce",
    y = "Débarquements en tonnes",
    title = "Débarquements cumulés par espèce et par pays: Chalutiers (DTS)",
    subtitle = "Données FDI sur la période 2021-2022"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.background = element_rect(color = "white", fill = "white"),
    plot.margin = margin(20, 40, 20, 40),
    axis.title.x = element_text(face = "bold", margin = margin(t = 20)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 20))
  )

## Plot the histogram for DFN fleets 2021-2022 period----
thefive <- landings %>%
  filter (X3A_CODE %in% c("HKE","MUT","NEP","ARA","DPS")) %>%
  filter (FISHING_TECH %in% c("DFN")) %>%
  filter (COUNTRY %in% c("FRA","ESP")) %>%
  filter (YEAR %in% c(2021,2022)) %>%
  select (COUNTRY, X3A_CODE, totwghtlandg) %>%
  group_by(COUNTRY, X3A_CODE) %>%
  summarise(totwghtlandg = sum(totwghtlandg, na.rm = TRUE), .groups = "drop")



DFN_2021_2022 <- ggplot(thefive, aes(x = X3A_CODE, y = totwghtlandg, fill = COUNTRY)) +
  geom_bar(stat = "identity",
           position = "stack",
           orientation = "horizontal") +
  coord_flip() +
  scale_fill_manual(values = c("ESP" = "red", "FRA" = "blue"),
                    name = "Pays") +
  labs(
    x = "Espèce",
    y = "Débarquements en tonnes",
    title = "Débarquements cumulés par espèce et par pays: Fileyeurs (DFN)",
    subtitle = "Données FDI sur la période 2021-2022"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.background = element_rect(color = "white", fill = "white"),
    plot.margin = margin(20, 40, 20, 40),
    axis.title.x = element_text(face = "bold", margin = margin(t = 20)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 20))
  )


## Plot the histogram for DFN fleets ALL period with ZZZ----
thefive <- landings %>%
  mutate(X3A_CODE = fct_other(
    X3A_CODE,
    keep = c("HKE", "MUT", "NEP", "ARA", "DPS"),
    other_level = "ZZZ"
  )) %>%
  filter (FISHING_TECH %in% c("DFN")) %>%
  filter (COUNTRY %in% c("FRA", "ESP")) %>%
  filter (YEAR %in% c(2021,2022)) %>%
  select (COUNTRY, X3A_CODE, totwghtlandg) %>%
  group_by(COUNTRY, X3A_CODE) %>%
  summarise(totwghtlandg = sum(totwghtlandg, na.rm = TRUE),
            .groups = "drop")



DFN_2021_2022_ZZZ <- ggplot(thefive, aes(x = X3A_CODE, y = totwghtlandg, fill = COUNTRY)) +
  geom_bar(stat = "identity",
           position = "stack",
           orientation = "horizontal") +
  coord_flip() +
  scale_fill_manual(values = c("ESP" = "red", "FRA" = "blue"),
                    name = "Pays") +
  labs(
    x = "Espèce",
    y = "Débarquements en tonnes",
    title = "Débarquements cumulés par espèce et par pays: Fileyeurs (DFN)",
    subtitle = "Données FDI sur la période 2021-2022"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.background = element_rect(color = "white", fill = "white"),
    plot.margin = margin(20, 40, 20, 40),
    axis.title.x = element_text(face = "bold", margin = margin(t = 20)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 20))
  )

## Plot the histogram for HOK fleets 2021-2022 period----
thefive <- landings %>%
  filter (X3A_CODE %in% c("HKE","MUT","NEP","ARA","DPS")) %>%
  filter (FISHING_TECH %in% c("HOK")) %>%
  filter (COUNTRY %in% c("FRA","ESP")) %>%
  select (COUNTRY, X3A_CODE, totwghtlandg) %>%
  group_by(COUNTRY, X3A_CODE) %>%
  summarise(totwghtlandg = sum(totwghtlandg, na.rm = TRUE), .groups = "drop")



HOK_2021_2022 <- ggplot(thefive, aes(x = X3A_CODE, y = totwghtlandg, fill = COUNTRY)) +
  geom_bar(stat = "identity",
           position = "stack",
           orientation = "horizontal") +
  coord_flip() +
  scale_fill_manual(values = c("ESP" = "red", "FRA" = "blue"),
                    name = "Pays") +
  labs(
    x = "Espèce",
    y = "Débarquements en tonnes",
    title = "Débarquements cumulés par espèce et par pays: ligneurs (HOK)",
    subtitle = "Données FDI sur la période 2021-2022"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.background = element_rect(color = "white", fill = "white"),
    plot.margin = margin(20, 40, 20, 40),
    axis.title.x = element_text(face = "bold", margin = margin(t = 20)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 20))
  )

## Plot the histogram for HOK fleets 2021-2022 period----
thefive <- landings %>%
  mutate(X3A_CODE = fct_other(
    X3A_CODE,
    keep = c("HKE", "MUT", "NEP", "ARA", "DPS"),
    other_level = "ZZZ"
  )) %>%
  filter (FISHING_TECH %in% c("HOK")) %>%
  filter (COUNTRY %in% c("FRA", "ESP")) %>%
  select (COUNTRY, X3A_CODE, totwghtlandg) %>%
  group_by(COUNTRY, X3A_CODE) %>%
  summarise(totwghtlandg = sum(totwghtlandg, na.rm = TRUE),
            .groups = "drop")



HOK_2021_2022_ZZZ <- ggplot(thefive, aes(x = X3A_CODE, y = totwghtlandg, fill = COUNTRY)) +
  geom_bar(stat = "identity",
           position = "stack",
           orientation = "horizontal") +
  coord_flip() +
  scale_fill_manual(values = c("ESP" = "red", "FRA" = "blue"),
                    name = "Pays") +
  labs(
    x = "Espèce",
    y = "Débarquements en tonnes",
    title = "Débarquements cumulés par espèce et par pays: ligneurs (HOK)",
    subtitle = "Données FDI sur la période 2021-2022"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.background = element_rect(color = "white", fill = "white"),
    plot.margin = margin(20, 40, 20, 40),
    axis.title.x = element_text(face = "bold", margin = margin(t = 20)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 20))
  )


## Save the plots----
ggsave("Figures/BOTH/Hist_5_Species/ALL_2013_2022.png", plot = ALL_2013_2022, width = 3000, height = 2000, units = "px")
ggsave("Figures/BOTH/Hist_5_Species/ALL_2013_2022_ZZZ.png", plot = ALL_2013_2022_ZZZ, width = 3000, height = 2000, units = "px")
ggsave("Figures/BOTH/Hist_5_Species/DTS_2013_2022.png", plot = DTS_2013_2022, width = 3000, height = 2000, units = "px")
ggsave("Figures/BOTH/Hist_5_Species/DTS_2013_2022_ZZZ.png", plot = DTS_2013_2022_ZZZ, width = 3000, height = 2000, units = "px")
ggsave("Figures/BOTH/Hist_5_Species/DFN_2013_2022.png", plot = DFN_2013_2022, width = 3000, height = 2000, units = "px")
ggsave("Figures/BOTH/Hist_5_Species/DFN_2013_2022_ZZZ.png", plot = DFN_2013_2022_ZZZ, width = 3000, height = 2000, units = "px")
ggsave("Figures/BOTH/Hist_5_Species/HOK_2013_2022.png", plot = HOK_2013_2022, width = 3000, height = 2000, units = "px")
ggsave("Figures/BOTH/Hist_5_Species/HOK_2013_2022_ZZZ.png", plot = HOK_2013_2022_ZZZ, width = 3000, height = 2000, units = "px")
ggsave("Figures/BOTH/Hist_5_Species/ALL_2021_2022.png", plot = ALL_2021_2022, width = 3000, height = 2000, units = "px")
ggsave("Figures/BOTH/Hist_5_Species/ALL_2021_2022_ZZZ.png", plot = ALL_2021_2022_ZZZ, width = 3000, height = 2000, units = "px")
ggsave("Figures/BOTH/Hist_5_Species/DTS_2021_2022.png", plot = DTS_2021_2022, width = 3000, height = 2000, units = "px")
ggsave("Figures/BOTH/Hist_5_Species/DTS_2021_2022_ZZZ.png", plot = DTS_2021_2022_ZZZ, width = 3000, height = 2000, units = "px")
ggsave("Figures/BOTH/Hist_5_Species/DFN_2021_2022.png", plot = DFN_2021_2022, width = 3000, height = 2000, units = "px")
ggsave("Figures/BOTH/Hist_5_Species/DFN_2021_2022_ZZZ.png", plot = DFN_2021_2022_ZZZ, width = 3000, height = 2000, units = "px")
ggsave("Figures/BOTH/Hist_5_Species/HOK_2021_2022.png", plot = HOK_2021_2022, width = 3000, height = 2000, units = "px")
ggsave("Figures/BOTH/Hist_5_Species/HOK_2021_2022_ZZZ.png", plot = HOK_2021_2022_ZZZ, width = 3000, height = 2000, units = "px")






# SANKEY DYNAMIC----
library(plotly)
library(dplyr)
library(RColorBrewer)

# Préparation des données
sankey <- landings %>%
  filter(COUNTRY == "FRA", X3A_CODE != "ZZZ", X3A_CODE %in% tentaxons, FleetIAM %in% tenfleets) %>%
  group_by(FleetIAM, X3A_CODE) %>%
  summarise(totvallandg = sum(totvallandg, na.rm = TRUE), .groups = "drop")

# Création des labels uniques pour les nœuds
all_nodes <- unique(c(sankey$FleetIAM, sankey$X3A_CODE))
node_labels <- all_nodes

# Création des indices pour source et target
sankey <- sankey %>%
  mutate(
    source = match(FleetIAM, all_nodes) - 1,
    target = match(X3A_CODE, all_nodes) - 1
  )

# Création d'une palette de couleurs pour les flottilles
n_fleets <- length(unique(sankey$FleetIAM))
fleet_colors <- colorRampPalette(brewer.pal(8, "Set1"))(n_fleets)
names(fleet_colors) <- unique(sankey$FleetIAM)

# Attribution des couleurs aux liens en fonction de la flottille source
link_colors <- fleet_colors[sankey$FleetIAM]

# Normalisation des valeurs totvallandg pour ajuster l'opacité
min_val <- min(sankey$totvallandg)
max_val <- max(sankey$totvallandg)
norm_values <- (sankey$totvallandg - min_val) / (max_val - min_val)

# Fonction pour créer le diagramme Sankey
create_sankey <- function(highlight_fleet = NULL) {
  # Ajustement des couleurs et de l'opacité en fonction de la taille du lien
  link_colors_adjusted <- sapply(1:length(link_colors), function(i) {
    adjustcolor(link_colors[i], alpha.f = 0.4 + 1 * norm_values[i]) # Opacité variant entre 0.3 et 1
  })
  
  link_order <- rep(0, length(sankey$FleetIAM))
  
  if (!is.null(highlight_fleet)) {
    highlight_indices <- which(sankey$FleetIAM == highlight_fleet)
    link_colors_adjusted[highlight_indices] <- link_colors[highlight_indices]
    link_order[highlight_indices] <- 1
  }
  
  fig <- plot_ly(
    type = "sankey",
    orientation = "h",
    node = list(
      label = node_labels,
      color = c(fleet_colors, rep("#CCCCCC", length(unique(sankey$X3A_CODE)))),
      pad = 15,
      thickness = 20,
      line = list(color = "black", width = 0.5)
    ),
    link = list(
      source = sankey$source,
      target = sankey$target,
      value = sankey$totvallandg,
      color = link_colors_adjusted,
      label = paste(sankey$FleetIAM, "->", sankey$X3A_CODE),
      customdata = sankey$FleetIAM,
      ordering = link_order
    )
  )
  
  fig <- fig %>% layout(
    title = "Dépendances des flottes aux espèces",
    font = list(size = 10),
    xaxis = list(showgrid = FALSE, zeroline = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE)
  )
  
  return(fig)
}

# Création du graphique initial
fig <- create_sankey()
fig



# Landings Value contribution----
#' The idea is to understand which species contribute the most to the landings
#' in term of value (euro)

## From the last two years (2021-2022)
prop_val <- landingsV2 %>%
  filter(YEAR %in% c(2021, 2022)) %>%
  select(totvallandg, X3A_CODE) %>%
  group_by(X3A_CODE) %>%
  summarise(totvallandg = sum(totvallandg, na.rm = TRUE), .groups = "drop") %>%
  mutate(total = sum(totvallandg),
         proportion = totvallandg / total)%>%
  arrange(desc(proportion)) %>%
  select(-c(totvallandg, total))

output_file <- "Figures/FRA/landings_value_contribution_2122.pdf"
pdf(output_file, width = 12, height = 10)

ggplot(prop_val, aes(x = reorder(X3A_CODE, proportion), y = proportion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Species", y = "Proportion", title = "Proportions of each species ") +
  coord_flip() +
  labs(
    x = "Species",
    y = "Landings in euros",
    title = "Proportion of each species in landings (euros) over the 2021-2022 period"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    plot.background = element_rect(color = "white", fill = "white"),
    plot.margin = margin(20, 40, 20, 40),
    axis.title.x = element_text(face = "bold",margin = margin(t = 20)),
    axis.title.y = element_text(face = "bold",margin = margin(r = 20))
  )
dev.off()



## From all the period (2013:2022)
prop_val <- landingsV2 %>%
  select(totvallandg, X3A_CODE) %>%
  group_by(X3A_CODE) %>%
  summarise(totvallandg = sum(totvallandg, na.rm = TRUE), .groups = "drop") %>%
  mutate(total = sum(totvallandg),
         proportion = totvallandg / total)%>%
  arrange(desc(proportion)) %>%
  select(-c(totvallandg, total))

output_file <- "Figures/FRA/landings_value_contribution_ALL.pdf"
pdf(output_file, width = 12, height = 10)

ggplot(prop_val, aes(x = reorder(X3A_CODE, proportion), y = proportion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Species", y = "Proportion", title = "Proportions of each species ") +
  coord_flip() +
  labs(
    x = "Species",
    y = "Landings in euros",
    title = "Proportion of each species in landings (euros) over the 2013-2022 period"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    plot.background = element_rect(color = "white", fill = "white"),
    plot.margin = margin(20, 40, 20, 40),
    axis.title.x = element_text(face = "bold",margin = margin(t = 20)),
    axis.title.y = element_text(face = "bold",margin = margin(r = 20))
  )
dev.off()
