# define_options.R

# Define the renovation items
renovation_items <- c(
  "Sanitärbereich (mind. Fliesen, Wanne, WC) erneuert",
  "Elektroinstallation (zeitgemäß) erneuert",
  "Heizanlage/Warmwasserversorgung erneuert",
  "Schallschutz eingebaut",
  "Fußböden erneuert",
  "Fenster-/Rahmenerneuerung",
  "Innen- und Wohnungstüren erneuert",
  "Treppenhaus, Eingangsbereich erneuert",
  "barrierearme Ausstattung geschaffen (Mindestvoraussetzung: schwellenfrei (max. 4cm Höhe), stufenloser Zugang, bodengleiche Dusche)",
  "Grundriss verbessert",
  "Dachsanierung",
  "Fassadensanierung"
)

# Add "Keine Sanierungsmaßnahme bekannt" as the first item
renovation_items <- c("Keine Sanierungsmaßnahme bekannt", renovation_items)

# Define Sanitärausstattung items
sanitaer_items <- c(
  "zwei oder mehr abgeschlossene Badezimmer in der Wohnung vorhanden",
  "zweites WC/Gäste-WC vorhanden",
  "(separate) Einzeldusche",
  "Fußbodenheizung",
  "Belüftung(sanlage)",
  "separater WC-Raum vorhanden",
  "Handtuchheizkörper",
  "zweites Waschbecken im selben Badezimmer"
)

# Define Ausstattung items with corresponding percentages
ausstattung_items <- list(
  "Einbauküche mit mindestens zwei Elektroeinbaugeräten (z. B. Herd/Ofen, Gefrierschrank/-truhe, Kühlschrank, Geschirrspülmaschine) wird vom Vermieter ohne zusätzlichen Mietzuschlag gestellt." = 0.04,
  "Terrasse oder Dachterrasse" = 0.06,
  "Aufzug in Gebäuden mit weniger als 5 Stockwerken" = 0.07,
  "Überwiegend Parkett-, Dielen- oder Steinfußboden im überwiegenden Teil des Wohn-/Schlafbereichs, abgesehen von Flur/Bad verbaut" = 0.03,
  "Energiebedarfsklasse lt. Energiebedarfsausweis lautet F, G oder H; bzw. der Wert kWh/m2a ist größer oder gleich 200" = -0.09,
  "Teppichboden, PVC- oder Linoleum-Boden im überwiegenden Teil des Wohn-/Schlafbereichs, abgesehen von Flur/Bad verbaut, welcher seit 2013 nicht modernisiert bzw. saniert wurde" = -0.11
)

# Define consistent color codes for map markers and legend
marker_colors <- c("#FF0000", "#0000FF", "#00FF00") # Red, Blue, Green
border_colors <- c("#8B0000", "#00008B", "#006400") # Darker Red, Darker Blue, Darker Green

# Define Wohnlage adjustment values
wohnlage_adjustments <- list(
  "A" = 0.00,  # No adjustment
  "B" = -0.07, # 7% decrease
  "C" = -0.10  # 10% decrease
)
