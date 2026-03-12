# ==============================================================================
# Bibliometric Analysis of Climate Change Literature
# Description: Data cleaning, feature engineering, and visualization of Scopus data
# Authors: Andy A. Acosta-Monterrosa, Kevin F. Montoya-Quintero, Fabriccio J. Visconti-López,
# Ivan David Lozada-Martínez
# ==============================================================================

# 1. SETUP & LIBRARIES
# ==============================================================================
# Install required packages if not already installed:
# install.packages(c("dplyr", "data.table", "stringr", "ggplot2", "RColorBrewer", "scales"))

library(dplyr) # Data manipulation
library(data.table) # Efficient processing of large datasets
library(stringr) # String manipulation
library(ggplot2) # Data visualization
library(RColorBrewer) # Color palettes for plots
library(scales) # Axis formatting

# 2. DATA LOADING & CLEANING
# ==============================================================================
cat("Loading metadata...\n")
# Assuming the refined data or raw data is in the same directory
raw_data_zip <- "ClimateChangeMetadata.zip"
raw_data_csv <- "ClimateChangeMetadata.csv"

# Check if the uncompressed file exists; if not, read from the zip file
if (file.exists(raw_data_csv)) {
  ClimateChangeMetadata <- fread(raw_data_csv)
} else {
  cat("Reading from compressed file...\n")
  # Extract the single csv file name from the zip
  unzip_file <- unzip(raw_data_zip, list = TRUE)$Name[1]
  ClimateChangeMetadata <- fread(cmd = paste0("unzip -p ", raw_data_zip, " ", unzip_file))
}

cat("Original dataset dimensions:", nrow(ClimateChangeMetadata), "rows\n")

# Remove duplicate entries based on EID
ClimateChangeMetadata <- ClimateChangeMetadata %>%
  distinct(EID, .keep_all = TRUE)

cat("Dimensions after removing duplicates:", nrow(ClimateChangeMetadata), "rows\n")

# 3. FEATURE ENGINEERING: FIRST AUTHOR COUNTRY
# ==============================================================================
# Comprehensive list of countries in English
paises <- c(
  "Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Antigua and Barbuda",
  "Argentina", "Armenia", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain",
  "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bhutan",
  "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil", "Brunei", "Bulgaria",
  "Burkina Faso", "Burundi", "Cabo Verde", "Cambodia", "Cameroon", "Canada",
  "Central African Republic", "Chad", "Chile", "China", "Colombia", "Comoros",
  "Congo", "Costa Rica", "Croatia", "Cuba", "Cyprus", "Czech Republic",
  "Democratic Republic of the Congo", "Denmark", "Djibouti", "Dominica",
  "Dominican Republic", "East Timor", "Ecuador", "Egypt", "El Salvador",
  "Equatorial Guinea", "Eritrea", "Estonia", "Eswatini", "Ethiopia", "Fiji",
  "Finland", "France", "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Greece",
  "Grenada", "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Honduras",
  "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland", "Israel",
  "Italy", "Ivory Coast", "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya",
  "Kiribati", "Kuwait", "Kyrgyzstan", "Laos", "Latvia", "Lebanon", "Lesotho",
  "Liberia", "Libya", "Liechtenstein", "Lithuania", "Luxembourg", "Madagascar",
  "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "Marshall Islands", "Mauritania",
  "Mauritius", "Mexico", "Micronesia", "Moldova", "Monaco", "Mongolia", "Montenegro",
  "Morocco", "Mozambique", "Myanmar", "Namibia", "Nauru", "Nepal", "Netherlands",
  "New Zealand", "Nicaragua", "Niger", "Nigeria", "North Korea", "North Macedonia",
  "Norway", "Oman", "Pakistan", "Palau", "Palestine", "Panama", "Papua New Guinea",
  "Paraguay", "Peru", "Philippines", "Poland", "Portugal", "Qatar", "Romania",
  "Russia", "Rwanda", "Saint Kitts and Nevis", "Saint Lucia",
  "Saint Vincent and the Grenadines", "Samoa", "San Marino", "Sao Tome and Principe",
  "Saudi Arabia", "Senegal", "Serbia", "Seychelles", "Sierra Leone", "Singapore",
  "Slovakia", "Slovenia", "Solomon Islands", "Somalia", "South Africa", "South Korea",
  "South Sudan", "Spain", "Sri Lanka", "Sudan", "Suriname", "Sweden", "Switzerland",
  "Syria", "Taiwan", "Tajikistan", "Tanzania", "Thailand", "Togo", "Tonga",
  "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan", "Tuvalu", "Uganda",
  "Ukraine", "United Arab Emirates", "United Kingdom", "United States", "Uruguay",
  "Uzbekistan", "Vanuatu", "Vatican City", "Venezuela", "Vietnam", "Yemen", "Zambia",
  "Zimbabwe", "USA", "UK", "UAE", "Republic of Korea", "Republic of China",
  "Hong Kong", "Puerto Rico", "Northern Ireland", "Scotland", "Wales"
)

# Function to extract the country of the first affiliation
extraer_pais <- function(aff) {
  if (is.na(aff) || aff == "") {
    return(NA_character_)
  }

  # Extract first affiliation (before first semicolon)
  primera_aff <- unlist(strsplit(aff, ";"))[1]
  if (is.null(primera_aff) || primera_aff == "") {
    return(NA_character_)
  }

  # Extract country (last segment after last comma)
  partes <- unlist(strsplit(primera_aff, ","))
  if (length(partes) < 1) {
    return(NA_character_)
  }

  pais <- trimws(partes[length(partes)])

  # Exact match check
  for (p in paises) {
    if (pais == p) {
      return(pais)
    }
  }

  # Partial match check
  for (p in paises) {
    if (grepl(p, pais, fixed = TRUE)) {
      return(p)
    }
  }

  return(pais)
}

# Apply country extraction
setDT(ClimateChangeMetadata)
ClimateChangeMetadata[, Pais_Primer_Autor := sapply(Affiliations, extraer_pais)]

# 4. FEATURE ENGINEERING: INCOME GROUP CLASSIFICATION
# ==============================================================================
# World Bank Income Groups 2024 (https://datahelpdesk.worldbank.org/knowledgebase/articles/906519)

high_income <- c("Andorra", "Australia", "Austria", "Bahamas", "Bahrain", "Barbados", "Belgium", "Brunei", "Canada", "Chile", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hong Kong", "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Japan", "Kuwait", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta", "Monaco", "Netherlands", "New Zealand", "Norway", "Oman", "Panama", "Poland", "Portugal", "Puerto Rico", "Qatar", "Romania", "San Marino", "Saudi Arabia", "Seychelles", "Singapore", "Slovakia", "Slovenia", "South Korea", "Republic of Korea", "Spain", "Sweden", "Switzerland", "Taiwan", "Republic of China", "Trinidad and Tobago", "United Arab Emirates", "UAE", "United Kingdom", "UK", "Northern Ireland", "Scotland", "Wales", "United States", "USA", "Uruguay", "Vatican City", "Macao", "Bermuda", "New Caledonia", "French Polynesia", "Greenland", "Guam", "Cayman Islands", "Faroe Islands", "Gibraltar", "Curaçao", "Virgin Islands (U.S.)", "Falkland Islands (Malvinas)", "Saint Kitts and Nevis")
upper_middle_income <- c("Albania", "Algeria", "Argentina", "Armenia", "Azerbaijan", "Belarus", "Belize", "Bosnia and Herzegovina", "Botswana", "Brazil", "Bulgaria", "China", "Colombia", "Costa Rica", "Cuba", "Dominica", "Dominican Republic", "Ecuador", "Equatorial Guinea", "Fiji", "Gabon", "Georgia", "Grenada", "Guatemala", "Guyana", "Iran", "Iraq", "Jamaica", "Jordan", "Kazakhstan", "Lebanon", "Libya", "Malaysia", "Maldives", "Marshall Islands", "Mauritius", "Mexico", "Moldova", "Montenegro", "Namibia", "Nauru", "North Macedonia", "Palau", "Paraguay", "Peru", "Russia", "Saint Lucia", "Saint Vincent and the Grenadines", "Samoa", "Serbia", "South Africa", "Suriname", "Thailand", "Tonga", "Turkey", "Turkmenistan", "Tuvalu")
lower_middle_income <- c("Angola", "Bangladesh", "Benin", "Bhutan", "Bolivia", "Cabo Verde", "Cambodia", "Cameroon", "Comoros", "Congo", "Ivory Coast", "Djibouti", "Egypt", "El Salvador", "Eswatini", "Ghana", "Haiti", "Honduras", "India", "Indonesia", "Kenya", "Kiribati", "Kyrgyzstan", "Laos", "Lesotho", "Mauritania", "Micronesia", "Mongolia", "Morocco", "Myanmar", "Nepal", "Nicaragua", "Nigeria", "Pakistan", "Papua New Guinea", "Philippines", "Sao Tome and Principe", "Senegal", "Solomon Islands", "Sri Lanka", "Tanzania", "Tajikistan", "East Timor", "Tunisia", "Ukraine", "Uzbekistan", "Vanuatu", "Vietnam", "Viet Nam", "Zambia", "Zimbabwe", "Cote d'Ivoire", "Palestine", "Cape Verde", "Swaziland", "Timor-Leste")
low_income <- c("Afghanistan", "Burkina Faso", "Burundi", "Central African Republic", "Chad", "Democratic Republic of the Congo", "Eritrea", "Ethiopia", "Gambia", "Guinea", "Guinea-Bissau", "Liberia", "Madagascar", "Malawi", "Mali", "Mozambique", "Niger", "North Korea", "Rwanda", "Sierra Leone", "Somalia", "South Sudan", "Sudan", "Syria", "Togo", "Uganda", "Yemen")

clasificar_ingreso <- function(pais) {
  if (is.na(pais) || pais == "") {
    return(NA_character_)
  }

  if (grepl("Venezuela", pais, ignore.case = TRUE)) {
    return("Not classified - Venezuela")
  }
  if (grepl("French Guiana|Reunion", pais, ignore.case = TRUE)) {
    return("High income")
  }
  if (grepl("Cook Islands", pais, ignore.case = TRUE)) {
    return("Upper middle income")
  }

  if (pais %in% high_income) {
    return("High income")
  }
  if (pais %in% upper_middle_income) {
    return("Upper middle income")
  }
  if (pais %in% lower_middle_income) {
    return("Lower middle income")
  }
  if (pais %in% low_income) {
    return("Low income")
  }

  return("Not classified")
}

ClimateChangeMetadata[, Income_Group := sapply(Pais_Primer_Autor, clasificar_ingreso)]

# 5. DATA EXPORT (Refined Dataset)
# ==============================================================================
output_filename <- "ClimateChangeMetadata_Refined.csv"
cat("Saving refined dataset to", output_filename, "...\n")
fwrite(ClimateChangeMetadata, output_filename)
cat("Refined data successfully saved.\n")


# 6. DESCRIPTIVE STATISTICS
# ==============================================================================
# Overview of Articles per Income Group by 2024
income_summary <- ClimateChangeMetadata %>%
  mutate(Year = as.integer(Year)) %>%
  filter(!is.na(Income_Group), !is.na(Year), Year <= 2024) %>%
  count(Income_Group, name = "count") %>%
  mutate(
    total = sum(count),
    proportion = count / total,
    percentage = round(proportion * 100, 2)
  )

print(income_summary)

# 7. DATA VISUALIZATION
# ==============================================================================
income_levels <- c("High income", "Upper middle income", "Lower middle income", "Low income")
pal_income <- brewer.pal(4, "Dark2")
names(pal_income) <- income_levels

# Prepare timeline data
prod_year_income <- ClimateChangeMetadata %>%
  filter(Income_Group %in% income_levels, !is.na(Year)) %>%
  mutate(
    Year = as.integer(Year),
    Income_Group = factor(Income_Group, levels = income_levels)
  ) %>%
  filter(Year <= 2024) %>%
  count(Year, Income_Group, name = "count")

prod_year_income_prop <- prod_year_income %>%
  group_by(Year) %>%
  mutate(total_year = sum(count), prop = count / total_year) %>%
  ungroup()

# PLOT 1: Absolute Article Count Over Time
plot1 <- ggplot(prod_year_income, aes(x = Year, y = count, group = Income_Group)) +
  geom_area(aes(fill = Income_Group), position = "stack", alpha = 0.25) +
  geom_line(aes(color = Income_Group), linewidth = 1.1) +
  scale_x_continuous(limits = c(1946, 2024), breaks = c(1946, seq(1950, 2020, by = 10), 2024), minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(0, 60000, by = 10000), labels = comma) +
  scale_color_manual(values = pal_income) +
  scale_fill_manual(values = pal_income) +
  labs(
    x = "Year",
    y = "Number of Climate Change Articles",
    fill = "Income Group",
    color = "Income Group"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.position = "right",
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# PLOT 2: Share of Articles Over Time
plot2 <- ggplot(prod_year_income_prop, aes(x = Year, y = prop, color = Income_Group)) +
  geom_line(linewidth = 1.1) +
  scale_x_continuous(limits = c(1946, 2024), breaks = c(1946, seq(1950, 2020, by = 10), 2024), minor_breaks = NULL) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_color_manual(values = pal_income) +
  labs(
    x = "Year",
    y = "Share of Articles per Year",
    color = "Income Group"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.position = "right",
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Uncomment to save plots locally
# ggsave("Articles_Over_Time.png", plot1, width = 8, height = 5, dpi = 300)
# ggsave("Share_Over_Time.png", plot2, width = 8, height = 5, dpi = 300)

cat("Analysis complete. Run `print(plot1)` or `print(plot2)` to view plots.\n")
