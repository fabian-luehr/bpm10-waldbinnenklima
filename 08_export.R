# =============================================================================
# 08_export.R
# Zweck:       Alle finalen Tabellen und Modellzusammenfassungen in
#              04_Analyse/Ergebnisse/Tabellen/ sichern und einen
#              abschliessenden Metadatenbericht erzeugen.
#              Abbildungen wurden bereits von 07_visualisierung.R gespeichert.
#
# Inputs:  Alle Ergebnisobjekte aus F1–F3 (falls im Environment)
#          bzw. gespeicherte CSVs falls nur Nachlauf.
# Outputs: 04_Analyse/Ergebnisse/Tabellen/00_Exportprotokoll.csv
#
# Autor: Fabian Luehr, generiert 2026-05-06
# =============================================================================


# ---- Exportprotokoll initialisieren -----------------------------------------
protokoll <- data.table::data.table(
  datei          = character(),
  beschreibung   = character(),
  erstellt_am    = character(),
  n_zeilen       = integer()
)

protokoll_ergaenzen <- function(pfad, beschreibung) {
  if (file.exists(pfad)) {
    n <- tryCatch(nrow(data.table::fread(pfad, nrows = 0L, skip = 0)), error = function(e) NA)
    # Besser: Anzahl Zeilen tatsaechlich zaehlen ohne alles einzulesen
    n <- tryCatch({
      length(readLines(pfad)) - 1L  # minus Header
    }, error = function(e) NA_integer_)
    protokoll <<- rbind(protokoll, data.table::data.table(
      datei        = basename(pfad),
      beschreibung = beschreibung,
      erstellt_am  = format(file.mtime(pfad), "%Y-%m-%d %H:%M"),
      n_zeilen     = as.integer(n)
    ))
  }
}


# ---- Tabellen protokollieren ------------------------------------------------
tab_dir <- PFADE$tabellen

tab_protokoll <- list(
  list("F1_trend_lm.csv",
       "F1: Lineare Regressionskoeffizienten (Trend je Region x Indikator)"),
  list("F1_trend_mk.csv",
       "F1: Mann-Kendall tau, p-Wert und Sen's Slope je Region x Indikator"),
  list("F1_trend_delta_puffer.csv",
       "F1: Zeitlicher Trend des Wald-Freiland-Puffers (Delta_Puffer)"),
  list("F2_deskriptiv.csv",
       "F2: Deskriptive Statistiken je Region und Indikator"),
  list("F2_anova_kruskal.csv",
       "F2: ANOVA und Kruskal-Wallis-Ergebnisse je Indikator"),
  list("F2_posthoc.csv",
       "F2: Paarweise Post-hoc-Tests (Tukey, Dunn-Bonferroni)"),
  list("F2_lme_amplitude.csv",
       "F2: LME-Koeffizienten fuer Amplitude ~ year*region"),
  list("F2_lme_alle_indikatoren.csv",
       "F2: LME-Koeffizienten aller Indikatoren"),
  list("F3_deskriptiv_klassen.csv",
       "F3: Deskriptive Statistiken je ForMI-Klasse und Region"),
  list("F3_lme_amp_formi.csv",
       "F3: LME-Koeffizienten fuer Amplitude ~ year*ForMI + region"),
  list("F3_lme_alle_indikatoren_formi.csv",
       "F3: LME-Koeffizienten aller Indikatoren mit ForMI"),
  list("F3_sensitivitaet_formi_aktuell.csv",
       "F3: Sensitivitaetsanalyse mit aktuellem ForMI-Erhebungsjahr")
)

for (item in tab_protokoll) {
  protokoll_ergaenzen(file.path(tab_dir, item[[1]]), item[[2]])
}


# ---- Abbildungen protokollieren ---------------------------------------------
abb_dir   <- PFADE$abbildungen
abb_files <- list.files(abb_dir, pattern = "\\.png$", full.names = TRUE)

for (f in abb_files) {
  protokoll <- rbind(protokoll, data.table::data.table(
    datei        = basename(f),
    beschreibung = paste("Abbildung (PNG, 300 dpi):",
                         gsub("_", " ", gsub("\\.png$", "", basename(f)))),
    erstellt_am  = format(file.mtime(f), "%Y-%m-%d %H:%M"),
    n_zeilen     = NA_integer_
  ))
}


# ---- Protokoll speichern ----------------------------------------------------
speichern_csv(protokoll,
              file.path(tab_dir, "00_Exportprotokoll.csv"))

cat("\nExportprotokoll:\n")
print(protokoll[, .(datei, beschreibung)])

cat(sprintf("\nInsgesamt %d Ergebnis-Dateien erstellt.\n", nrow(protokoll)))
cat("Alle Outputs in: 04_Analyse/Ergebnisse/\n")


# ============================================================================
# Excel-Arbeitsmappe (Sammelmappe aller Ergebnistabellen)
# ============================================================================
# Hintergrund: Deutsches Excel interpretiert "," als Dezimaltrenner; die von
# data.table::fwrite erzeugten CSVs (Komma-getrennt, Punkt als Dezimaltrenner)
# erscheinen daher beim Doppelklick "durcheinander" in einer einzigen Spalte.
# Die Excel-Mappe loest das einmalig: jede Tabelle als eigenes Sheet.
cat("\nErzeuge Excel-Sammelmappe (xlsx) ...\n")

if (requireNamespace("openxlsx", quietly = TRUE)) {

  csv_dateien <- list.files(tab_dir, pattern = "\\.csv$", full.names = TRUE)

  # Sheetnamen sind in Excel auf 31 Zeichen limitiert und duerfen
  # die Sonderzeichen [ ] : * ? / \ nicht enthalten.
  sheet_name <- function(stem) {
    s <- gsub("[\\[\\]:\\*\\?/\\\\]", "", stem)
    s <- gsub("_", " ", s)
    s <- gsub("00 Exportprotokoll", "Exportprotokoll", s, fixed = TRUE)
    if (nchar(s) > 31) s <- substr(s, 1, 31)
    s
  }

  wb <- openxlsx::createWorkbook(creator = "BPM 10 Auswertungspipeline")

  # Inhalts-Sheet
  openxlsx::addWorksheet(wb, "Inhalt")
  inhalt_dt <- data.table::data.table(
    Sheet        = character(),
    Quelldatei   = character(),
    Beschreibung = character(),
    Zeilen       = integer(),
    Spalten      = integer()
  )

  hdr_style <- openxlsx::createStyle(
    fontName = "Times New Roman", fontSize = 11, textDecoration = "bold",
    fgFill = "#D9D9D9", border = "bottom", borderColour = "#999999",
    halign = "left", valign = "center"
  )
  body_style <- openxlsx::createStyle(
    fontName = "Times New Roman", fontSize = 11, valign = "center"
  )

  for (csv_file in csv_dateien) {
    stem <- tools::file_path_sans_ext(basename(csv_file))
    sn   <- sheet_name(stem)

    dt <- tryCatch(data.table::fread(csv_file), error = function(e) NULL)
    if (is.null(dt)) next

    openxlsx::addWorksheet(wb, sn)
    openxlsx::writeData(wb, sn, dt, headerStyle = hdr_style)
    openxlsx::addStyle(wb, sn, body_style,
                       rows = seq_len(nrow(dt)) + 1L,
                       cols = seq_len(ncol(dt)),
                       gridExpand = TRUE)
    openxlsx::setColWidths(wb, sn, cols = seq_len(ncol(dt)),
                            widths = "auto")
    openxlsx::freezePane(wb, sn, firstRow = TRUE)

    # Beschreibung aus tab_protokoll suchen (falls vorhanden)
    descr <- "—"
    for (item in tab_protokoll) {
      if (item[[1]] == basename(csv_file)) {
        descr <- item[[2]]; break
      }
    }
    if (basename(csv_file) == "00_Exportprotokoll.csv") {
      descr <- "Übersicht aller erzeugten Ergebnisdateien"
    }

    inhalt_dt <- rbind(inhalt_dt, data.table::data.table(
      Sheet = sn, Quelldatei = basename(csv_file),
      Beschreibung = descr, Zeilen = nrow(dt), Spalten = ncol(dt)
    ))
  }

  # Inhalts-Sheet befuellen
  title_style <- openxlsx::createStyle(
    fontName = "Times New Roman", fontSize = 14, textDecoration = "bold")
  openxlsx::writeData(wb, "Inhalt",
                      "BPM 10 – Ergebnistabellen", startRow = 1, startCol = 1)
  openxlsx::addStyle(wb, "Inhalt", title_style, rows = 1, cols = 1)
  openxlsx::writeData(wb, "Inhalt",
                      paste("Erstellt:", format(Sys.time(), "%Y-%m-%d %H:%M")),
                      startRow = 2, startCol = 1)
  openxlsx::writeData(wb, "Inhalt", inhalt_dt,
                      startRow = 4, headerStyle = hdr_style)
  openxlsx::addStyle(wb, "Inhalt", body_style,
                     rows = seq_len(nrow(inhalt_dt)) + 4L,
                     cols = seq_len(ncol(inhalt_dt)), gridExpand = TRUE)
  openxlsx::setColWidths(wb, "Inhalt", cols = 1:5,
                          widths = c(33, 38, 60, 10, 10))
  openxlsx::freezePane(wb, "Inhalt", firstActiveRow = 5)
  openxlsx::worksheetOrder(wb) <- c(
    which(names(wb) == "Inhalt"),
    setdiff(seq_along(names(wb)), which(names(wb) == "Inhalt"))
  )

  xlsx_pfad <- file.path(PFADE$ergebnisse, "BPM10_Ergebnisse_Tabellen.xlsx")
  openxlsx::saveWorkbook(wb, xlsx_pfad, overwrite = TRUE)
  cat(sprintf("  -> Excel-Mappe gespeichert: %s\n", basename(xlsx_pfad)))
} else {
  cat("  Hinweis: Paket 'openxlsx' nicht installiert – Excel-Export uebersprungen.\n")
  cat("  Installation: install.packages('openxlsx')\n")
}

cat("\n08_export.R abgeschlossen.\n")
