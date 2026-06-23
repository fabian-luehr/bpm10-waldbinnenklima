1# =============================================================================
# 00_run_all.R
# Master-Skript: ruft alle Teilskripte der BPM-10-Auswertungspipeline in
# der richtigen Reihenfolge auf.
#
# Inputs:  Eingangsdaten in 03_Daten/01_Roh/ (ZIPs oder bereits entpackte CSVs)
# Outputs: Aufbereitete Daten, Analyseergebnisse, Abbildungen, Tabellen,
#          RMarkdown-Bericht (optional)
#
# Systemanforderungen:
#   - R >= 4.3
#   - Mindestens 16 GB RAM (Wald-Datensatz ~5,8 GB entpackt)
#   - Ca. 10–30 Minuten Laufzeit (abhaengig von Hardware)
#
# Ausfuehren: Oeffne dieses Skript in RStudio, setze das RStudio-Projekt auf
#   den Wurzelordner des Projekts (File > Open Project > BPM10.Rproj),
#   dann: source("04_Analyse/Skripte/00_run_all.R")
#
# Autor: Fabian Luehr, generiert 2026-05-06
# =============================================================================

cat("=== BPM 10 Auswertungspipeline gestartet ===\n")
cat("Startzeit:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# ---- Startzeitpunkt merken --------------------------------------------------
t_start <- proc.time()


# ---- 1. Setup: Pakete und Pfade laden ---------------------------------------
cat("[1/8] Setup (Pakete, Pfade, Hilfsfunktionen)...\n")
source("04_Analyse/Skripte/01_setup.R")


# ---- 2. Daten einlesen und aufbereiten --------------------------------------
cat("[2/8] Daten einlesen und aufbereiten...\n")
source("04_Analyse/Skripte/02_einlesen_aufbereiten.R")


# ---- 3. Indikatoren berechnen -----------------------------------------------
cat("[3/8] Klimaindikatoren berechnen (Tagesebene -> Jahresebene)...\n")
source("04_Analyse/Skripte/03_indikatoren.R")


# ---- 4. Analyse Forschungsfrage 1: Zeitliche Entwicklung --------------------
cat("[4/8] Analyse F1: Zeitliche Entwicklung der Klimaextreme...\n")
source("04_Analyse/Skripte/04_analyse_F1.R")


# ---- 5. Analyse Forschungsfrage 2: Regionale Unterschiede ------------------
cat("[5/8] Analyse F2: Regionale Unterschiede zwischen Exploratorien...\n")
source("04_Analyse/Skripte/05_analyse_F2.R")


# ---- 6. Analyse Forschungsfrage 3: ForMI-Einfluss --------------------------
cat("[6/8] Analyse F3: Einfluss der Bewirtschaftungsintensitaet (ForMI)...\n")
source("04_Analyse/Skripte/06_analyse_F3.R")


# ---- 7. Alle Abbildungen erstellen ------------------------------------------
cat("[7/8] Abbildungen erstellen (ggplot2)...\n")
source("04_Analyse/Skripte/07_visualisierung.R")


# ---- 8. Ergebnisse exportieren ----------------------------------------------
cat("[8/8] Ergebnisse exportieren (CSV, PNG, Zusammenfassungen)...\n")
source("04_Analyse/Skripte/08_export.R")


# ---- Abschlussmeldung -------------------------------------------------------
t_end  <- proc.time()
t_diff <- (t_end - t_start)[["elapsed"]]
cat("\n=== Pipeline abgeschlossen ===\n")
cat(sprintf("Gesamtlaufzeit: %.1f Sekunden (%.1f Minuten)\n",
            t_diff, t_diff / 60))
cat("Ergebnisse unter: 04_Analyse/Ergebnisse/\n")
cat("\nOptional: RMarkdown-Bericht rendern mit:\n")
cat("  rmarkdown::render('04_Analyse/Notebooks/Auswertungsbericht_BPM10.Rmd')\n")
