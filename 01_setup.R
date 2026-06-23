# =============================================================================
# 01_setup.R
# Zweck:       Pakete laden/pruefen, Projektpfade definieren, gemeinsame
#              Hilfsfunktionen bereitstellen.
# Inputs:      Keine (wird von 00_run_all.R gesourct)
# Outputs:     Objekte im globalen Environment: PFADE (Liste), Hilfsfunktionen
# Vorbedingung: RStudio-Projekt muss im Wurzelordner des Projekts geoeffnet sein
#
# Autor: Fabian Luehr, generiert 2026-05-06
# =============================================================================


# ---- Benoetigte Pakete (mit Installationsprueefung) -------------------------
pakete <- c(
  "here",        # robuste Pfade relativ zum Projektordner
  "data.table",  # schnelles Einlesen und Verarbeiten grosser Datensaetze
  "dplyr",       # tidyverse-Pipelines
  "tidyr",       # Daten umstrukturieren (pivot_longer etc.)
  "lubridate",   # Datums- und Zeitangaben parsen
  "lme4",        # Mixed-Effects-Modelle
  "lmerTest",    # p-Werte fuer lme4-Modelle (Satterthwaite)
  "trend",       # Mann-Kendall-Test und Sens Slope
  "ggplot2",     # Visualisierungen
  "patchwork",   # Mehrere ggplot2-Plots zusammenfuegen
  "scales",      # Achsenbeschriftungen formatieren
  "performance", # Modelldiagnostik (check_model)
  "arrow",       # Parquet-Dateien lesen/schreiben (optional, Fallback: CSV)
  "sf",               # Vektordaten fuer Karten (optional, nur wenn Koordinaten da)
  "ggspatial",        # Karten: Nordpfeil, Masssstab, OSM-Kachelhintergrund
  "rosm",             # OSM-Kacheln-Downloader (fuer annotation_map_tile)
  "prettymapr",       # interne Abhaengigkeit von ggspatial (Skalierung/Kacheln)
  "rnaturalearth",    # Laender-/Staatsgrenzen als sf (Kartenhintergrund Offline)
  "rnaturalearthdata",# Datenprovider fuer rnaturalearth (mittlerer Massstab)
  "openxlsx"          # Excel-Sammelmappe fuer Ergebnistabellen (08_export.R)
)

fehlende_pakete <- pakete[!sapply(pakete, requireNamespace, quietly = TRUE)]
if (length(fehlende_pakete) > 0) {
  cat("Folgende Pakete fehlen und werden jetzt installiert:\n")
  cat(paste(" -", fehlende_pakete, collapse = "\n"), "\n")
  install.packages(fehlende_pakete, dependencies = TRUE)
}

# Kernpakete laden (immer benoetigt)
suppressPackageStartupMessages({
  library(here)
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(lme4)
  library(lmerTest)
  library(trend)
  library(ggplot2)
  library(patchwork)
  library(scales)
})

# Optionale Pakete (nur laden wenn vorhanden)
if (requireNamespace("performance", quietly = TRUE)) library(performance)
if (requireNamespace("arrow",       quietly = TRUE)) library(arrow)
sf_verfuegbar           <- requireNamespace("sf",               quietly = TRUE)
ggspatial_verfuegbar    <- requireNamespace("ggspatial",        quietly = TRUE)
rnaturalearth_verfuegbar <- requireNamespace("rnaturalearth",   quietly = TRUE) &&
                            requireNamespace("rnaturalearthdata", quietly = TRUE)
if (sf_verfuegbar)            library(sf)
if (ggspatial_verfuegbar)     library(ggspatial)
if (rnaturalearth_verfuegbar) library(rnaturalearth)

cat("Pakete geladen.\n")


# ---- Projektpfade (relativ zum here()-Wurzelverzeichnis) --------------------
PFADE <- list(
  # Rohdaten
  roh            = here("03_Daten", "01_Roh"),
  wald_zip       = here("03_Daten", "01_Roh", "BExIS_Klimadaten",
                        "Klimadaten_BExIS_24766_Wald_AEW_HEW_SEW.zip"),
  freiland_zip   = here("03_Daten", "01_Roh", "BExIS_Klimadaten",
                        "Gruenlandplots.zip"),
  # Hinweis: Die Freilanddaten liegen unter dem Dateinamen "Gruenlandplots.zip"
  # (nicht dem urspruenglich erwarteten "Klimadaten_BExIS_24766_Freiland_...").
  # Das Skript prueft beide Varianten.

  formi_zip      = here("03_Daten", "01_Roh", "BExIS_Bewirtschaftung_ForMI",
                        "BExIS_31855_v12_ForMIX index quantifying land use for al..._20260506.zip"),
  stamm_zip      = here("03_Daten", "01_Roh", "BExIS_Bewirtschaftung_ForMI",
                        "BExIS_21426_v4_2nd forest inventory, single tree data, ..._20260506.zip"),

  # Aufbereitete Daten
  aufbereitet    = here("03_Daten", "02_Aufbereitet"),
  metadaten      = here("03_Daten", "03_Metadaten"),

  # Ergebnisse
  ergebnisse     = here("04_Analyse", "Ergebnisse"),
  abbildungen    = here("04_Analyse", "Ergebnisse", "Abbildungen"),
  tabellen       = here("04_Analyse", "Ergebnisse", "Tabellen"),
  zwischen       = here("04_Analyse", "Ergebnisse", "Zwischenstaende")
)

# Verzeichnisse anlegen falls nicht vorhanden
for (pfad in PFADE) {
  if (!dir.exists(pfad)) {
    dir.create(pfad, recursive = TRUE, showWarnings = FALSE)
  }
}

cat("Projektpfade gesetzt und Verzeichnisse geprueft.\n")


# ---- Gemeinsame Parameter ---------------------------------------------------
ANALYSE_JAHRE   <- 2006:2025   # Auswertungsfenster
MIN_STD_PRO_TAG <- 18          # Min. gueltige Stunden je Tag
MIN_TAGE_PRO_JAHR <- 300       # Min. gueltige Tage je Plot-Jahr

# Regionsabkuerzungen
REGION_CODES <- c(
  "AEW" = "Schwäbische Alb",
  "HEW" = "Hainich-Dün",
  "SEW" = "Schorfheide-Chorin",
  "AEG" = "Schwäbische Alb (Freiland)",
  "HEG" = "Hainich-Dün (Freiland)",
  "SEG" = "Schorfheide-Chorin (Freiland)"
)

# Farbpalette fuer die drei Exploratorien (farbenblindfreundlich)
REGION_FARBEN <- c(
  "Schwäbische Alb"      = "#E69F00",
  "Hainich-Dün"          = "#009E73",
  "Schorfheide-Chorin"   = "#0072B2"
)


# ---- Hilfsfunktionen --------------------------------------------------------

#' Region aus plotID ableiten (erste 3 Buchstaben)
#' @param plot_id character-Vektor mit Plot-IDs (z.B. "AEW01", "HEW51")
#' @return character-Vektor mit Regionsnamen
region_aus_id <- function(plot_id) {
  prefix <- substr(plot_id, 1, 3)
  dplyr::recode(prefix,
    "AEW" = "Schwäbische Alb",
    "HEW" = "Hainich-Dün",
    "SEW" = "Schorfheide-Chorin",
    .default = NA_character_
  )
}

#' Freiland-Region aus Freiland-plotID ableiten
region_aus_freiland_id <- function(plot_id) {
  prefix <- substr(plot_id, 1, 3)
  dplyr::recode(prefix,
    "AEG" = "Schwäbische Alb",
    "HEG" = "Hainich-Dün",
    "SEG" = "Schorfheide-Chorin",
    .default = NA_character_
  )
}

#' Prueft ob eine Datei existiert; wirft stop() mit hilfreicher Meldung wenn nicht
check_datei <- function(pfad, dateiname = NULL) {
  if (!file.exists(pfad)) {
    name <- if (!is.null(dateiname)) dateiname else basename(pfad)
    stop(paste0(
      "FEHLER: Datei nicht gefunden: ", pfad, "\n",
      "  -> Bitte sicherstellen, dass '", name, "' im richtigen Verzeichnis liegt.\n",
      "  -> Siehe Anleitung Kapitel 3 (Daten ablegen)."
    ))
  }
  invisible(TRUE)
}

#' Speichert eine data.table als CSV mit Statusmeldung
speichern_csv <- function(dt, pfad, ...) {
  data.table::fwrite(dt, pfad, ...)
  cat(sprintf("  -> Gespeichert: %s (%d Zeilen)\n", basename(pfad), nrow(dt)))
  invisible(pfad)
}

#' ggplot2-Theme fuer alle Abbildungen (einheitliches Erscheinungsbild)
theme_bpm <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title      = element_text(size = base_size + 1, face = "bold"),
      plot.subtitle   = element_text(size = base_size - 1, colour = "grey40"),
      plot.caption    = element_text(size = base_size - 2, colour = "grey60"),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}

cat("Hilfsfunktionen bereitgestellt.\n")
cat("01_setup.R abgeschlossen.\n")
