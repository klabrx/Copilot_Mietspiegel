# ./script/prepApp.R

# Lade erforderliche Bibliotheken
library(dplyr)

# typo_correction: Korrigiere Schreibfehler in allen Spalten
typo_correction <- function(data) {
  # Definiere die Korrekturen
  corrections <- list(
    "Klosterwinl" = "Klosterwinkel"
    # Hier können weitere Korrekturen hinzugefügt werden
    # Zum Beispiel:
    # "Fehlerbeispiel" = "Richtig"
  )
  
  # Wende die Korrekturen auf alle Spalten an
  for (col in names(data)) {
    for (wrong in names(corrections)) {
      correct <- corrections[[wrong]]
      data[[col]] <- gsub(wrong, correct, data[[col]], fixed = TRUE)
    }
  }
  
  return(data)
}

# Laden der Referenztabelle
reference_table <- read.csv("./data/RefTab_Groesse.csv", sep = ";", stringsAsFactors = FALSE)

# Laden der Datei und Extrahieren des DataFrames sf_data
load("./data/adr_2024.RData")

# Wende Korrekturen an
sf_data <- typo_correction(sf_data)

# Erstellen einer Liste von Adressvorschlägen für das Suchfeld
adress_vorschlaege <- unique(sf_data$STRASSE_HS)

# Definieren der Lagenfaktoren
wohnlage_adjustments <- list(
  "A" = 0.00,  # No adjustment
  "B" = -0.07, # 7% decrease
  "C" = -0.10  # 10% decrease
)
