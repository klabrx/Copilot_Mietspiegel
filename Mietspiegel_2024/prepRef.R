# ./scripts/prepRef.R

# Laden der erforderlichen Bibliotheken
library(dplyr)
library(sf)
library(readr)

# Funktion zur Korrektur von Tippfehlern in allen Spalten
typo_correction <- function(data) {
  corrections <- list(
    "Klosterwinl" = "Klosterwinkel"
    # Hier können weitere Korrekturen hinzugefügt werden
  )
  
  for (col in names(data)) {
    for (wrong in names(corrections)) {
      correct <- corrections[[wrong]]
      data[[col]] <- gsub(wrong, correct, data[[col]], fixed = TRUE)
    }
  }
  
  return(data)
}

# 1. Laden der Daten aus externen Dateien

ref_adressen <- read_csv("data/adr2024.csv",
col_types = cols(PLZ = col_character()))

# Korrigiere Tippfehler in allen Spalten
ref_adressen <- typo_correction(ref_adressen)

# Erstelle die WL_FAKTOR-Spalte basierend auf der WL_2024-Spalte
ref_adressen <- ref_adressen %>%
  mutate(ADRESS_ID_KURZ = substr(ADRESS_ID, 6, nchar(ADRESS_ID))) %>%
  arrange(STRASSE, ADRESS_ID_KURZ) %>%
  select(-ADRESS_ID_KURZ) %>%
  mutate(WL_FAKTOR = case_when(
    WL_2024 == "A" ~ 0.00,
    WL_2024 == "B" ~ -0.07,
    WL_2024 == "C" ~ -0.10,
    TRUE ~ NA_real_  # Falls kein passender Wert gefunden wird
  ))

# 2. Laden und Aufbereiten der Größentabelle

# Lade die Referenztabelle für Größen
ref_groesse <- read.csv("./data/external/RefTab_Groesse.csv", sep = ";", stringsAsFactors = FALSE)

# Konvertiere die Größenwerte von Komma zu Punkt für die korrekte numerische Verarbeitung
ref_groesse <- ref_groesse %>%
  mutate(
    low = as.numeric(gsub(",", ".", low)),
    med = as.numeric(gsub(",", ".", med)),
    hi = as.numeric(gsub(",", ".", hi))
  )

#  Erstelle ref_baujahr
# Define year ranges and factors
year_ranges <- c(
  "bis 1918" = 0.00,
  "1919 - 1945" = -0.07,
  "1946 - 1977" = -0.10,
  "1978 - 1984" = -0.05,
  "1985 - 1989" = -0.01,
  "1990 - 1995" = -0.01,
  "1996 - 2004" = 0.06,
  "2005 - 2012" = 0.12,
  "2013 - 2018" = 0.19,
  "2019 - 2023" = 0.24
)

# Convert to DataFrame
ref_baujahr <- data.frame(
  Baujahresklasse = names(year_ranges),
  Faktor = as.numeric(year_ranges),
  stringsAsFactors = FALSE
)

# Print the DataFrame to check
print(ref_baujahr)

# create dataframe based on list

ref_renovation <- data.frame(
  option = c(
    "Keine Sanierung erfolgt",
    "Vollmodernisierung (nur Baujahr vor 1990)",
    "Sanitärbereich erneuert",
    "Elektroinstallation erneuert",
    "Heizanlage/Warmwasserversorgung erneuert",
    "Schallschutz eingebaut",
    "Fußböden erneuert",
    "Fenster-/Rahmenerneuerung",
    "Innen- und Wohnungstüren erneuert",
    "Treppenhaus, Eingangsbereich erneuert",
    "Barrierearme Ausstattung geschaffen",
    "Grundriss verbessert",
    "Dachsanierung",
    "Fassadensanierung"
  ),
  percentage = c(0, 0.11, rep(0.06, 12))  # Assign 0.06 to all Standardgewerke, 0 to Keine Sanierung, and 0.11 to Vollmodernisierung
)





# 3. Speichern aller Daten in einer RData-Datei

# Speichern der vorbereiteten Daten in einer .RData-Datei
save(ref_groesse, ref_baujahr, ref_renovation, ref_adressen, file = "./data/ref.RData")
