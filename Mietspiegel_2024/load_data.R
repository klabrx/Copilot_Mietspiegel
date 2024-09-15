# load_data.R

# Load and prepare data for the app

# Load the RData file (assuming the RData file has already been created)
load("./data/adr_2024.RData")

# Extract unique street names and sort them alphabetically
strassen <- sort(unique(sf_data$STRASSE))

# Load data for dropdown options
RefTab_Groesse <- read.csv("./data/RefTab_Groesse.csv", sep = ";")
values_list <- lapply(seq_len(nrow(RefTab_Groesse)), function(i) {
  list(
    low = RefTab_Groesse$low[i],
    med = RefTab_Groesse$med[i],
    hi = RefTab_Groesse$hi[i]
  )
})

dropdown_options <- setNames(
  seq_len(nrow(RefTab_Groesse)),
  paste(RefTab_Groesse$von, "bis unter", RefTab_Groesse$bis_unter, "m²")
)

# Define year ranges and corresponding percentages
year_ranges <- c(
  "bis 1918" = 0.00,
  "1919 - 1945" = -0.07,
  "1946 - 1977" = -0.10,
  "1978 - 1984" = -0.05,
  "1985 - 1995" = -0.01,
  "1996 - 2004" = 0.06,
  "2005 - 2012" = 0.12,
  "2013 - 2018" = 0.19,
  "2019 - 2023" = 0.24
)

display_labels <- c(
  "bis 1918" = "+-0%",
  "1919 - 1945" = "-7%",
  "1946 - 1977" = "-10%",
  "1978 - 1984" = "-5%",
  "1985 - 1995" = "-1%",
  "1996 - 2004" = "+6%",
  "2005 - 2012" = "+12%",
  "2013 - 2018" = "+19%",
  "2019 - 2023" = "+24%"
)

