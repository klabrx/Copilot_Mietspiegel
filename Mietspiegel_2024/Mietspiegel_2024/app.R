library(shiny)
library(dplyr)


# Create an empty result table with the columns
#  "Merkmal" (char),
#  "Angaben" (char),
#  "Anteil" (num, displayed as %),
#  "low" (num, displayed as currency (EUR)),
#  "med" (num, displayed as currency (EUR)),
#  "hi" (num, displayed as currency (EUR))
#
# to be filled with the results of a successive sequence of calculations and to
# be finally displayed as the result of the app.

result <- tibble::tibble(
  Merkmal = character(),
  Angaben = character(),
  Anteil = numeric(),
  low = numeric(),
  med = numeric(),
  hi = numeric()
)



# Create reference tables that are used to provide
# options for dropdown_inputs.

# First element: Size of the appartment, as size is the most relevant attribute
ref_groesse <- tibble::tribble(
    ~von, ~bis_unter, ~low,  ~med,   ~hi,
     25L,        26L, 9.84, 11.86, 13.88,
     26L,        27L, 9.66, 11.64, 13.61,
     27L,        28L, 9.48, 11.43, 13.37,
     28L,        29L, 9.32, 11.23, 13.14,
     29L,        30L, 9.17, 11.05, 12.93,
     30L,        31L, 9.03, 10.89, 12.74,
     31L,        32L,  8.9, 10.73, 12.55,
     32L,        33L, 8.78, 10.58, 12.38,
     33L,        34L, 8.67, 10.44, 12.22,
     34L,        35L, 8.56, 10.31, 12.06,
     35L,        36L, 8.46, 10.19, 11.92,
     36L,        37L, 8.36, 10.07, 11.79,
     37L,        38L, 8.27,  9.96, 11.66,
     38L,        39L, 8.18,  9.86, 11.54,
     39L,        40L,  8.1,  9.76, 11.42,
     40L,        41L, 8.02,  9.67, 11.31,
     41L,        42L, 7.95,  9.58, 11.21,
     42L,        43L, 7.88,  9.49, 11.11,
     43L,        44L, 7.81,  9.41, 11.01,
     44L,        45L, 7.75,  9.33, 10.92,
     45L,        46L, 7.69,  9.26, 10.83,
     46L,        47L, 7.63,  9.19, 10.75,
     47L,        48L, 7.57,  9.12, 10.67,
     48L,        49L, 7.52,  9.06,  10.6,
     49L,        50L, 7.47,     9, 10.52,
     50L,        51L, 7.42,  8.94, 10.45,
     51L,        52L, 7.37,  8.88, 10.39,
     52L,        53L, 7.32,  8.82, 10.32,
     53L,        54L, 7.28,  8.77, 10.26,
     54L,        55L, 7.24,  8.72,  10.2,
     55L,        56L,  7.2,  8.67, 10.14,
     56L,        57L, 7.16,  8.62, 10.09,
     57L,        59L,  7.1,  8.55, 10.01,
     59L,        61L, 7.03,  8.47,  9.91,
     61L,        64L, 6.95,  8.37,  9.79,
     64L,        67L, 6.86,  8.26,  9.67,
     67L,        70L, 6.77,  8.16,  9.55,
     70L,        77L, 6.65,  8.01,  9.38,
     77L,        81L, 6.53,  7.86,   9.2,
     81L,        87L, 6.44,  7.76,  9.08,
     87L,        93L, 6.35,  7.65,  8.94,
     93L,       100L, 6.25,  7.53,  8.82,
    100L,       108L, 6.16,  7.42,  8.69,
    108L,       120L, 6.06,   7.3,  8.54,
    120L,       132L, 5.96,  7.18,   8.4,
    132L,       141L, 5.88,  7.09,  8.29,
    141L,       150L, 5.82,  7.02,  8.21
    )
# Amend ref_groesse with a new first collumn "options" that contains the
# strings "von bis unter" for each row, e.g. "64 bis unter 67 m²"

ref_groesse <- ref_groesse %>%
  mutate(options = paste0(von, " bis unter ", bis_unter))

# import adress data from a csv file
ref_adressen <- read_csv("Mietspiegel_2024/data/adr2024.csv")
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


# Create a shiny app frame with fixed (non-scrolling) header and footer,
# containing two colums (width 4:8) with a sidebar and a main panel.
# In the sidebar, create a selectize dropdown menu based in ref_groesse$options.

# The dropdown will display "Pflichtangabe" as a placeholder until an option is 
# selected. "Pflichtangabe will *NOT* be just the first option, but a placeholder

# When a selection is made, react to that input and write the first line
# into results (exemplary):
# "Größe", "64 bis unter 67 m²", 100%, 6.95, 8.37, 9.79
# and display the result table in the main panel.

ui <- fluidPage(
  titlePanel("Merkmal"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("groesse", "Größe", choices = ref_groesse$options, selected = "Pflichtangabe")
    ),
    mainPanel(
      tableOutput("result")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$groesse, {
    result <<- tibble::tibble(
      Merkmal = "Größe",
      Angaben = input$groesse,
      Anteil = 100,
      low = ref_groesse$low[ref_groesse$options == input$groesse],
      med = ref_groesse$med[ref_groesse$options == input$groesse],
      hi = ref_groesse$hi[ref_groesse$options == input$groesse]
    )
    output$result <- renderTable(result)
  })
}


shinyApp(ui = ui, server = server)
