# =============================================================================
# 02_einlesen_aufbereiten.R
# Zweck:       ZIP-Archive entpacken (falls noetig), Stundendaten fuer Wald und
#              Freiland einlesen, aufbereiten und als CSV/Parquet speichern.
#              Daneben: ForMI- und Stammdaten einlesen und saeubern.
#
# Inputs:
#   - 03_Daten/01_Roh/BExIS_Klimadaten/Klimadaten_BExIS_24766_Wald_AEW_HEW_SEW.zip
#   - 03_Daten/01_Roh/BExIS_Klimadaten/Gruenlandplots.zip  (Freilanddaten)
#   - 03_Daten/01_Roh/BExIS_Bewirtschaftung_ForMI/BExIS_31855_v12_ForMIX...zip
#   - 03_Daten/01_Roh/BExIS_Bewirtschaftung_ForMI/BExIS_21426_v4_2nd forest inventory...zip
#
# Outputs (in 03_Daten/02_Aufbereitet/):
#   - wald_stunden.csv     (oder .parquet falls arrow verfuegbar)
#   - freiland_stunden.csv
#   - formi.csv
#   - stammdaten.csv
#
# RAM-Hinweis: Das Einlesen des Wald-Datensatzes benoetigt kurzzeitig bis zu
#   12 GB RAM. Empfehlung: Mindestens 16 GB RAM.
#
# Autor: Fabian Luehr, generiert 2026-05-06
# =============================================================================


# ---- Benoetigte Spalten (Spaltenauswahl reduziert RAM-Bedarf erheblich) -----
WALD_SPALTEN <- c(
  "plotID", "datetime",
  "Ta_200_max", "Ta_200_min",  # Fuer Amplituden- und Extremtag-Berechnung
  "Ta_200"                     # Optionales Tagesmittel (falls vorhanden)
)
# Hinweis: Weitere Spalten (rH_200, SM_10, SM_20, Ta_10, Ts_*, Niederschlag)
# werden NICHT geladen, um RAM zu sparen. Bei Bedarf WALD_SPALTEN erweitern.


# ============================================================================
# TEIL A: Wald-Klimadaten
# ============================================================================

cat("--- Teil A: Wald-Klimadaten einlesen ---\n")

# Zieldatei fuer aufbereitete Stundendaten (Wald)
ziel_wald <- file.path(PFADE$aufbereitet, "wald_stunden.csv")

if (file.exists(ziel_wald)) {
  cat("Wald-Stundendaten bereits vorhanden, ueberspringe Einlesen.\n")
  cat("  (Loeschen zum Neueinlesen:", ziel_wald, ")\n")
  wald_std <- data.table::fread(ziel_wald)
} else {

  # Wald-ZIP pruefen
  check_datei(PFADE$wald_zip, "Klimadaten_BExIS_24766_Wald_AEW_HEW_SEW.zip")

  # Entpackziel
  entpack_wald <- file.path(PFADE$roh, "BExIS_Klimadaten", "wald_entpackt")
  if (!dir.exists(entpack_wald)) {
    cat("Entpacke Wald-ZIP (kann einige Minuten dauern bei ~5,8 GB)...\n")
    dir.create(entpack_wald, recursive = TRUE)
    unzip(PFADE$wald_zip, exdir = entpack_wald)
    cat("Entpacken abgeschlossen.\n")
  }

  # CSV-Datei innerhalb des entpackten Ordners finden
  # Hinweis: Im entpackten BExIS-Archiv liegen mehrere CSVs:
  #   - plots.csv             -> die eigentlichen Klimadaten (gewuenscht)
  #   - plot_description.csv  -> Plot-Stammdaten (lat/lon/region)
  #   - sensor_description.csv-> Sensor-Metadaten
  # Wir muessen gezielt 'plots.csv' (bzw. die groesste Daten-CSV) waehlen,
  # sonst wird faelschlich die Beschreibungsdatei gelesen.
  csv_dateien <- list.files(entpack_wald, pattern = "\\.csv$",
                             recursive = TRUE, full.names = TRUE)
  if (length(csv_dateien) == 0) {
    stop(paste("Keine CSV-Datei im entpackten Wald-ZIP gefunden unter:",
               entpack_wald))
  }

  # 1. Versuch: Datei mit Namen 'plots.csv' (laut processing_settings.yaml)
  wald_csv <- csv_dateien[basename(csv_dateien) == "plots.csv"]

  # 2. Fallback: Beschreibungs-CSVs ausschliessen, groesste Datei nehmen
  if (length(wald_csv) == 0) {
    daten_csv <- csv_dateien[!grepl("description|sensor|plot_desc|metadata",
                                    basename(csv_dateien), ignore.case = TRUE)]
    if (length(daten_csv) == 0) daten_csv <- csv_dateien
    wald_csv  <- daten_csv[which.max(file.size(daten_csv))]
  } else {
    wald_csv <- wald_csv[1]
  }

  cat(sprintf("Lese Wald-CSV: %s (%.1f MB)\n",
              basename(wald_csv),
              file.size(wald_csv) / 1024^2))
  cat("RAM-Hinweis: Dies kann 8-12 GB RAM belegen. Bitte warten...\n")

  # Spalten pruefen: Welche sind tatsaechlich in der Datei?
  header_probe <- data.table::fread(wald_csv, nrows = 5)
  verfuegbare_spalten <- intersect(WALD_SPALTEN, names(header_probe))
  fehlende_spalten    <- setdiff(WALD_SPALTEN, names(header_probe))
  if (length(fehlende_spalten) > 0) {
    warning(paste("Folgende Spalten fehlen im Wald-Datensatz:",
                  paste(fehlende_spalten, collapse = ", ")))
  }

  # Einlesen mit Spaltenauswahl
  wald_std <- data.table::fread(
    wald_csv,
    select     = verfuegbare_spalten,
    na.strings = c("NA", "", "NaN", "-9999")
  )
  cat(sprintf("Eingelesen: %s Zeilen, %d Plots\n",
              format(nrow(wald_std), big.mark = ".", decimal.mark = ","),
              length(unique(wald_std$plotID))))

  # Zeitstempel parsen (Format aus BExIS: YYYY-MM-DDTHH -> ymd_h)
  wald_std[, datetime := lubridate::ymd_h(datetime)]
  wald_std[, date     := as.Date(datetime)]
  wald_std[, year     := lubridate::year(datetime)]
  wald_std[, month    := lubridate::month(datetime)]

  # Region ableiten
  wald_std[, region   := region_aus_id(plotID)]

  # Auf Auswertungsfenster einschraenken (+ 1 Pufferjahr fuer Jan-Tagesberechnungen)
  wald_std <- wald_std[year %in% (min(ANALYSE_JAHRE) - 1):(max(ANALYSE_JAHRE) + 1)]

  # Speichern
  cat("Speichere aufbereitete Wald-Stundendaten...\n")
  if (requireNamespace("arrow", quietly = TRUE)) {
    arrow::write_parquet(wald_std,
                         gsub("\\.csv$", ".parquet", ziel_wald))
    cat("  -> Als Parquet gespeichert (schnelleres Nachladen).\n")
    ziel_wald_parquet <- gsub("\\.csv$", ".parquet", ziel_wald)
  }
  data.table::fwrite(wald_std, ziel_wald)
  cat(sprintf("  -> Als CSV gespeichert: %s\n", ziel_wald))

  # RAM freigeben
  gc()
}


# ============================================================================
# TEIL B: Freiland-Klimadaten
# ============================================================================

cat("\n--- Teil B: Freiland-Klimadaten einlesen ---\n")

ziel_freiland <- file.path(PFADE$aufbereitet, "freiland_stunden.csv")

if (file.exists(ziel_freiland)) {
  cat("Freiland-Stundendaten bereits vorhanden, ueberspringe Einlesen.\n")
  freiland_std <- data.table::fread(ziel_freiland)
} else {

  # Freiland-ZIP: Robust nach moeglichen Dateinamen suchen.
  # Mehrere Schreibvarianten und auch Umlautversionen pruefen.
  # Ausserdem ausschliessen, dass ein gleichnamiger ORDNER (z.B. von einem
  # frueheren Fehllauf) als ZIP angesehen wird.
  freiland_zip_pfad <- NULL  # initialisieren!

  ist_echte_zip <- function(p) {
    file.exists(p) && !dir.exists(p) && file.size(p) > 1024
  }

  # Liste moeglicher Dateinamen (in Praeferenzreihenfolge)
  freiland_kandidaten <- c(
    PFADE$freiland_zip,                                          # Gruenlandplots.zip (Soll)
    file.path(PFADE$roh, "BExIS_Klimadaten", "Grünlandplots.zip"),# mit Umlaut
    file.path(PFADE$roh, "BExIS_Klimadaten",
              "Klimadaten_BExIS_24766_Freiland_AEG_HEG_SEG.zip")  # historischer Name
  )

  # 1. Versuch: bekannte Namen aus Kandidatenliste
  treffer <- freiland_kandidaten[sapply(freiland_kandidaten, ist_echte_zip)]
  if (length(treffer) > 0) {
    freiland_zip_pfad <- treffer[1]
  } else {
    # 2. Fallback: alle ZIP-Dateien im Klimadaten-Ordner durchsuchen,
    # die nicht das Wald-ZIP sind (Umlaute werden so zuverlaessig gefunden)
    alle_zips <- list.files(file.path(PFADE$roh, "BExIS_Klimadaten"),
                            pattern = "\\.zip$",
                            full.names = TRUE, ignore.case = TRUE)
    alle_zips <- alle_zips[sapply(alle_zips, ist_echte_zip)]
    alle_zips <- alle_zips[basename(alle_zips) != basename(PFADE$wald_zip)]
    if (length(alle_zips) > 0) {
      freiland_zip_pfad <- alle_zips[which.max(file.size(alle_zips))]
    }
  }

  if (!is.null(freiland_zip_pfad)) {
    cat(sprintf("Freiland-ZIP gefunden: %s (%.1f MB)\n",
                basename(freiland_zip_pfad),
                file.size(freiland_zip_pfad) / 1024^2))
  } else {
    warning(paste(
      "WARNUNG: Freiland-ZIP nicht gefunden. Gesucht wurde:\n",
      paste(" -", freiland_kandidaten, collapse = "\n"),
      "\nForschungsfrage 1 (Wald-Freiland-Differenz) wird uebersprungen.\n",
      "Hinweis: Falls ein gleichnamiger ORDNER (z.B. 'Gruenlandplots.zip')",
      "existiert, bitte loeschen.\n"
    ))
    freiland_std <- NULL
  }

  if (!is.null(freiland_zip_pfad) && ist_echte_zip(freiland_zip_pfad)) {
    entpack_freiland <- file.path(PFADE$roh, "BExIS_Klimadaten", "freiland_entpackt")
    if (!dir.exists(entpack_freiland)) {
      dir.create(entpack_freiland, recursive = TRUE)
    }
    # Auch entpacken, wenn Ordner zwar existiert aber leer ist (von Vorlauf)
    if (length(list.files(entpack_freiland, recursive = TRUE)) == 0) {
      cat("Entpacke Freiland-ZIP (kann mehrere Minuten dauern)...\n")
      ergebnis <- tryCatch(
        unzip(freiland_zip_pfad, exdir = entpack_freiland),
        warning = function(w) { cat("unzip-Warnung:", conditionMessage(w), "\n"); NULL },
        error   = function(e) { cat("unzip-Fehler:",  conditionMessage(e), "\n"); NULL }
      )
      cat("Entpacken abgeschlossen.\n")
    }

    csv_freiland <- list.files(entpack_freiland, pattern = "\\.csv$",
                                recursive = TRUE, full.names = TRUE)
    if (length(csv_freiland) == 0) {
      warning("Keine CSV im entpackten Freiland-ZIP gefunden.")
      freiland_std <- NULL
    } else {
      # Gleiches Problem wie bei Wald: gezielt 'plots.csv' waehlen,
      # Beschreibungs-CSVs ausschliessen.
      freiland_csv_pfad <- csv_freiland[basename(csv_freiland) == "plots.csv"]
      if (length(freiland_csv_pfad) == 0) {
        daten_csv_f <- csv_freiland[!grepl(
          "description|sensor|plot_desc|metadata",
          basename(csv_freiland), ignore.case = TRUE)]
        if (length(daten_csv_f) == 0) daten_csv_f <- csv_freiland
        freiland_csv_pfad <- daten_csv_f[which.max(file.size(daten_csv_f))]
      } else {
        freiland_csv_pfad <- freiland_csv_pfad[1]
      }
      cat(sprintf("Lese Freiland-CSV: %s (%.1f MB)\n",
                  basename(freiland_csv_pfad),
                  file.size(freiland_csv_pfad) / 1024^2))

      header_probe_f <- data.table::fread(freiland_csv_pfad, nrows = 5)
      verf_f <- intersect(WALD_SPALTEN, names(header_probe_f))

      freiland_std <- data.table::fread(
        freiland_csv_pfad,
        select     = verf_f,
        na.strings = c("NA", "", "NaN", "-9999")
      )

      freiland_std[, datetime := lubridate::ymd_h(datetime)]
      freiland_std[, date     := as.Date(datetime)]
      freiland_std[, year     := lubridate::year(datetime)]
      freiland_std[, region   := region_aus_freiland_id(plotID)]
      freiland_std <- freiland_std[
        year %in% (min(ANALYSE_JAHRE) - 1):(max(ANALYSE_JAHRE) + 1)]

      data.table::fwrite(freiland_std, ziel_freiland)
      cat(sprintf("Freiland-Daten gespeichert: %d Zeilen, %d Plots\n",
                  nrow(freiland_std), length(unique(freiland_std$plotID))))
      gc()
    }
  }
}


# ============================================================================
# TEIL C: ForMI-Daten
# ============================================================================

cat("\n--- Teil C: ForMI-Daten einlesen ---\n")

ziel_formi <- file.path(PFADE$aufbereitet, "formi.csv")

if (file.exists(ziel_formi)) {
  cat("ForMI-Daten bereits aufbereitet, lade gespeicherte Version.\n")
  formi <- data.table::fread(ziel_formi)
} else {
  check_datei(PFADE$formi_zip, "BExIS_31855_v12_ForMIX...zip")

  entpack_formi <- file.path(dirname(PFADE$formi_zip), "formi_entpackt")
  if (!dir.exists(entpack_formi)) {
    dir.create(entpack_formi, recursive = TRUE)
    unzip(PFADE$formi_zip, exdir = entpack_formi)
  }

  csv_formi_alle <- list.files(entpack_formi, pattern = "\\.csv$",
                                recursive = TRUE, full.names = TRUE)
  if (length(csv_formi_alle) == 0) {
    warning("Keine CSV im ForMI-ZIP gefunden. F3-Analyse wird uebersprungen.")
    formi <- NULL
  } else {
    cat("ForMI-CSV-Dateien gefunden:\n")
    cat(paste(" -", basename(csv_formi_alle), collapse = "\n"), "\n")

    # Strukturbeschreibungs-/Metadaten-CSVs ausschliessen.
    # BExIS legt z.B. eine 'BExIS_31855_data_structure_*.csv' bei, deren
    # Zeilen Fliesstexte mit Kommata enthalten -> bricht den Parser und
    # verschmutzt die Daten (ForMI wuerde character statt numeric).
    csv_daten <- csv_formi_alle[!grepl(
      "data_structure|structure|metadata|description|schema",
      basename(csv_formi_alle), ignore.case = TRUE)]
    if (length(csv_daten) == 0) csv_daten <- csv_formi_alle  # Fallback

    # Bevorzugt Dateien mit '_data' im Namen (BExIS-Konvention)
    bevorzugt <- csv_daten[grepl("_data\\.csv$|_data_", basename(csv_daten),
                                 ignore.case = TRUE)]
    if (length(bevorzugt) > 0) csv_daten <- bevorzugt

    cat(sprintf("ForMI-Daten-CSV(s) ausgewaehlt: %s\n",
                paste(basename(csv_daten), collapse = ", ")))

    # Daten-CSVs einlesen und zusammenfuehren (falls mehrere Jahrgaenge)
    formi_liste <- lapply(csv_daten, function(f) {
      dt <- data.table::fread(f, na.strings = c("NA", "", "NaN"))
      dt[, quelldatei := basename(f)]
      dt
    })
    formi_raw <- data.table::rbindlist(formi_liste, fill = TRUE)

    cat(sprintf("ForMI-Daten eingelesen: %d Zeilen\n", nrow(formi_raw)))
    cat("Spaltennamen:", paste(names(formi_raw), collapse = ", "), "\n")

    # Spaltennamen normalisieren (Gross-/Kleinschreibung, Varianten)
    namen <- tolower(names(formi_raw))
    names(formi_raw) <- namen

    # plotID-Spalte identifizieren
    id_spalte <- grep("^plot.?id$|^ep_plotid$", namen, value = TRUE)[1]
    if (is.na(id_spalte)) {
      id_spalte <- grep("plot", namen, value = TRUE)[1]
      if (is.na(id_spalte)) stop("Keine plotID-Spalte im ForMI-Datensatz gefunden.")
    }
    data.table::setnames(formi_raw, id_spalte, "plotID")

    # ForMI-Spalte identifizieren
    formi_spalte <- grep("^formi$|^formix$|^formi_index$|^formi_score$",
                         namen, value = TRUE)[1]
    if (is.na(formi_spalte)) {
      formi_spalte <- grep("formi", namen, value = TRUE)[1]
    }
    if (is.na(formi_spalte)) {
      stop("Keine ForMI-Spalte gefunden. Verfuegbare Spalten: ",
           paste(names(formi_raw), collapse = ", "))
    }
    data.table::setnames(formi_raw, formi_spalte, "ForMI")

    # ForMI numerisch erzwingen (Schutz gegen versehentlich gemischte Spalten)
    formi_raw[, ForMI := suppressWarnings(as.numeric(as.character(ForMI)))]
    n_na_formi <- sum(is.na(formi_raw$ForMI))
    if (n_na_formi > 0) {
      cat(sprintf("  Hinweis: %d Zeilen ohne gueltigen ForMI-Wert verworfen.\n",
                  n_na_formi))
      formi_raw <- formi_raw[!is.na(ForMI)]
    }

    # Jahres-/Inventur-Spalte identifizieren (falls vorhanden)
    # BExIS-Datensatz nutzt z.B. 'Inventory' mit Werten "1st"/"2nd"
    jahr_spalte <- grep("^year$|^jahr$|^erhebungs.*jahr$|^survey.*year$|^inventory$",
                        namen, value = TRUE)[1]

    if (!is.na(jahr_spalte)) {
      data.table::setnames(formi_raw, jahr_spalte, "jahr_erhebung")
      jahre_vorhanden <- sort(unique(formi_raw$jahr_erhebung[
        !is.na(formi_raw$jahr_erhebung)]))
      cat(sprintf("Erhebungsjahre im ForMI-Datensatz: %s\n",
                  paste(jahre_vorhanden, collapse = ", ")))

      # Hauptanalyse: mittleres bzw. fortlaufend letztes Erhebungsjahr.
      # Funktioniert auch bei character-Werten (z.B. "1st", "2nd").
      if (length(jahre_vorhanden) >= 2) {
        idx_mitte    <- ceiling(length(jahre_vorhanden) / 2)
        jahr_haupt   <- jahre_vorhanden[idx_mitte]
        jahr_aktuell <- jahre_vorhanden[length(jahre_vorhanden)]
        cat(sprintf("Hauptanalyse: Erhebung '%s' (Mitte), Sensitivitaet: Erhebung '%s' (aktuellst)\n",
                    as.character(jahr_haupt), as.character(jahr_aktuell)))

        formi_haupt   <- formi_raw[jahr_erhebung == jahr_haupt]
        formi_aktuell <- formi_raw[jahr_erhebung == jahr_aktuell]

        speichern_csv(formi_aktuell,
                      file.path(PFADE$aufbereitet, "formi_sensitivitaet_aktuell.csv"))
        formi <- formi_haupt
      } else {
        formi <- formi_raw
      }
    } else {
      cat("Kein Erhebungsjahr im ForMI-Datensatz, nutze alle Zeilen.\n")
      formi <- formi_raw
    }

    # Region ableiten
    formi[, region := region_aus_id(plotID)]

    # Nur Wald-Plots behalten
    formi <- formi[!is.na(region)]

    # ForMI-Klassen erstellen (3 gleichmaessige Terzile)
    formi[, formi_klasse := cut(ForMI,
      breaks = quantile(ForMI, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
      labels = c("gering", "mittel", "hoch"),
      include.lowest = TRUE
    )]

    speichern_csv(formi, ziel_formi)
    cat(sprintf("ForMI: %d Plots, ForMI-Bereich: %.3f – %.3f\n",
                nrow(formi), min(formi$ForMI, na.rm = TRUE),
                max(formi$ForMI, na.rm = TRUE)))
  }
}


# ============================================================================
# TEIL D: Plot-Stammdaten (2. Forstinventur)
# ============================================================================

cat("\n--- Teil D: Plot-Stammdaten einlesen ---\n")

ziel_stamm <- file.path(PFADE$aufbereitet, "stammdaten.csv")

if (file.exists(ziel_stamm)) {
  cat("Stammdaten bereits aufbereitet.\n")
  stamm <- data.table::fread(ziel_stamm)
} else {
  check_datei(PFADE$stamm_zip, "BExIS_21426_v4_2nd forest inventory...zip")

  entpack_stamm <- file.path(dirname(PFADE$stamm_zip), "stamm_entpackt")
  if (!dir.exists(entpack_stamm)) {
    dir.create(entpack_stamm, recursive = TRUE)
    unzip(PFADE$stamm_zip, exdir = entpack_stamm)
  }

  csv_stamm_alle <- list.files(entpack_stamm, pattern = "\\.csv$",
                                recursive = TRUE, full.names = TRUE)
  if (length(csv_stamm_alle) == 0) {
    warning("Keine CSV in Stammdaten-ZIP. Karte ohne Stammdaten-Layer.")
    stamm <- NULL
  } else {
    # Strukturbeschreibungs-/Metadaten-CSVs ausschliessen (BExIS legt z.B.
    # 'BExIS_21426_data_structure_*.csv' bei, deren Fliesstexte den Parser
    # brechen). Bevorzugt '_data.csv' lesen.
    csv_daten_st <- csv_stamm_alle[!grepl(
      "data_structure|structure|metadata|description|schema",
      basename(csv_stamm_alle), ignore.case = TRUE)]
    if (length(csv_daten_st) == 0) csv_daten_st <- csv_stamm_alle
    bevorzugt_st <- csv_daten_st[grepl("_data\\.csv$|_data_",
                                       basename(csv_daten_st),
                                       ignore.case = TRUE)]
    if (length(bevorzugt_st) > 0) csv_daten_st <- bevorzugt_st

    cat("Stammdaten-CSV ausgewaehlt:\n")
    cat(paste(" -", basename(csv_daten_st), collapse = "\n"), "\n")

    stamm_liste <- lapply(csv_daten_st, function(f) {
      data.table::fread(f, na.strings = c("NA", "", "NaN"))
    })
    stamm_raw <- data.table::rbindlist(stamm_liste, fill = TRUE)
    cat("Spaltennamen Stammdaten:", paste(names(stamm_raw), collapse = ", "), "\n")

    speichern_csv(stamm_raw, ziel_stamm)
    stamm <- stamm_raw
  }
}


# ============================================================================
# TEIL E: Plot-Koordinaten und Nearest-Neighbor-Zuordnung (Wald -> Freiland)
# ============================================================================
# Zweck: Jeder Wald-Plot wird seinem geografisch naechsten Freiland-Plot
#        innerhalb der gleichen Region (Exploratorium) zugeordnet.
#        Das Ergebnis (naechstes_freiland.csv) wird in 03_indikatoren.R
#        fuer die Plot-genaue Delta_Puffer-Berechnung genutzt.
#        Quelle der Koordinaten: plot_description.csv im jeweiligen BExIS-ZIP.

cat("\n--- Teil E: Koordinatenbasierte Wald-Freiland-Zuordnung ---\n")

ziel_koord  <- file.path(PFADE$aufbereitet, "plot_koordinaten.csv")
ziel_nn_map <- file.path(PFADE$aufbereitet, "naechstes_freiland.csv")

# Hilfsfunktion: Koordinaten aus entpacktem BExIS-Ordner lesen
# Sucht zuerst nach plot_description.csv, dann nach der kleinsten CSV.
lese_plot_koordinaten <- function(entpack_dir, typ = "wald") {
  if (!dir.exists(entpack_dir)) {
    cat(sprintf("  Entpackordner nicht gefunden: %s\n", entpack_dir))
    return(NULL)
  }
  csv_alle <- list.files(entpack_dir, pattern = "\\.csv$",
                          recursive = TRUE, full.names = TRUE)
  if (length(csv_alle) == 0) { cat("  Keine CSV gefunden.\n"); return(NULL) }

  # Bevorzugt: Datei mit 'description' oder 'meta' im Namen
  desc_csv <- csv_alle[grepl("plot_description|plotdescription|plot_desc|meta",
                              basename(csv_alle), ignore.case = TRUE)]
  if (length(desc_csv) == 0) {
    # Fallback: kleinste CSV (Beschreibungsdateien kleiner als Massendaten)
    desc_csv <- csv_alle[which.min(file.size(csv_alle))]
  }
  cat(sprintf("  Koordinaten-CSV (%s): %s\n", typ, basename(desc_csv[1])))

  dt <- tryCatch(
    data.table::fread(desc_csv[1], na.strings = c("NA", "", "-9999", "NULL")),
    error = function(e) { cat("  Lesefehler:", e$message, "\n"); NULL }
  )
  if (is.null(dt) || nrow(dt) == 0) return(NULL)

  # Spaltennamen normalisieren
  names(dt) <- tolower(trimws(names(dt)))

  # plotID-Spalte
  # Hinweis: BExIS plot_description.csv nutzt "plot" (nicht "plotID").
  # Daher ^plot$ explizit als Muster aufnehmen.
  id_s <- grep("^plot$|^plotid$|^ep_plotid$|^plot.?id$", names(dt), value = TRUE)[1]
  if (is.na(id_s)) id_s <- grep("plot", names(dt), value = TRUE)[1]
  if (is.na(id_s)) {
    cat(sprintf("  Keine plotID-Spalte in %s. Spalten: %s\n",
                typ, paste(names(dt)[1:min(8, ncol(dt))], collapse = ", ")))
    return(NULL)
  }
  data.table::setnames(dt, id_s, "plotID")

  # Breitengrad
  lat_s <- grep("^lat$|^latitude$|^lat_wgs|^decimallatitude$",
                names(dt), value = TRUE)[1]
  if (is.na(lat_s)) lat_s <- grep("lat", names(dt), value = TRUE)[1]

  # Laengengrad
  lon_s <- grep("^lon$|^long$|^longitude$|^lon_wgs|^decimallongitude$|^lng$",
                names(dt), value = TRUE)[1]
  if (is.na(lon_s)) lon_s <- grep("lon|lng", names(dt), value = TRUE)[1]

  if (is.na(lat_s) || is.na(lon_s)) {
    cat(sprintf("  Keine lat/lon-Spalten in %s. Verfuegbare Spalten: %s\n",
                typ, paste(names(dt)[1:min(10, ncol(dt))], collapse = ", ")))
    return(NULL)
  }
  data.table::setnames(dt, c(lat_s, lon_s), c("lat", "lon"))
  dt[, .(plotID, lat = suppressWarnings(as.numeric(lat)),
                  lon = suppressWarnings(as.numeric(lon)))]
}

if (!file.exists(ziel_nn_map)) {

  # Entpackpfade (aus Teil A und B gesetzt)
  entpack_wald     <- file.path(PFADE$roh, "BExIS_Klimadaten", "wald_entpackt")
  entpack_freiland <- file.path(PFADE$roh, "BExIS_Klimadaten", "freiland_entpackt")

  koord_wald  <- lese_plot_koordinaten(entpack_wald,     "Wald")
  koord_freil <- lese_plot_koordinaten(entpack_freiland, "Freiland")

  if (!is.null(koord_wald)  && nrow(koord_wald)  > 0 &&
      !is.null(koord_freil) && nrow(koord_freil) > 0) {

    # Region ableiten
    koord_wald[,  region := region_aus_id(plotID)]
    koord_freil[, region := region_aus_freiland_id(plotID)]

    # Zeilen ohne Koordinaten oder Region entfernen
    koord_wald  <- koord_wald[ !is.na(lat) & !is.na(lon) & !is.na(region)]
    koord_freil <- koord_freil[!is.na(lat) & !is.na(lon) & !is.na(region)]

    # Alle Koordinaten speichern (fuer Karte in 07_visualisierung.R)
    koord_wald[,  typ := "wald"]
    koord_freil[, typ := "freiland"]
    koord_alle <- data.table::rbindlist(list(koord_wald, koord_freil), fill = TRUE)
    speichern_csv(koord_alle, ziel_koord)
    cat(sprintf("Koordinaten gespeichert: %d Wald-Plots, %d Freiland-Plots\n",
                nrow(koord_wald), nrow(koord_freil)))

    # Nearest-Neighbor-Zuordnung innerhalb jeder Region
    # Naeherungsformel: 1 Grad lat ~ 111 km; lon wird mit cos(mittlerer Breite)
    # skaliert. Ausreichend genau fuer Intra-Exploratorium-Distanzen (<50 km).
    nn_map <- data.table::rbindlist(lapply(unique(koord_wald$region), function(reg) {
      wr <- koord_wald[ region == reg]
      fr <- koord_freil[region == reg]
      if (nrow(wr) == 0 || nrow(fr) == 0) return(NULL)

      lat_mid <- mean(c(wr$lat, fr$lat), na.rm = TRUE)
      cos_lat <- cos(lat_mid * pi / 180)

      data.table::rbindlist(lapply(seq_len(nrow(wr)), function(i) {
        dlat    <- fr$lat - wr$lat[i]
        dlon    <- (fr$lon - wr$lon[i]) * cos_lat
        dist_km <- sqrt(dlat^2 + dlon^2) * 111
        j       <- which.min(dist_km)
        data.table::data.table(
          plotID_wald  = wr$plotID[i],
          plotID_freil = fr$plotID[j],
          region       = reg,
          dist_km      = round(dist_km[j], 2)
        )
      }))
    }))

    if (!is.null(nn_map) && nrow(nn_map) > 0) {
      speichern_csv(nn_map, ziel_nn_map)
      cat(sprintf("Nearest-Neighbor-Zuordnung: %d Plot-Paare\n", nrow(nn_map)))
      cat(sprintf("  Distanz: Mittel %.1f km, Max %.1f km, Min %.1f km\n",
                  mean(nn_map$dist_km), max(nn_map$dist_km), min(nn_map$dist_km)))
    }

  } else {
    cat("  Koordinaten konnten nicht gelesen werden.\n")
    cat("  -> 03_indikatoren.R verwendet als Fallback den Regionsmittelwert.\n")
  }
} else {
  cat("Nearest-Neighbor-Zuordnung bereits vorhanden, ueberspringe.\n")
  cat(sprintf("  (%s)\n", ziel_nn_map))
}


# ============================================================================
# ABSCHLUSS
# ============================================================================

cat("\n--- Einlesen und Aufbereiten abgeschlossen ---\n")
cat("Verfuegbare Datensaetze:\n")
cat(sprintf("  wald_std:    %s Zeilen\n",
            if (exists("wald_std") && !is.null(wald_std))
              format(nrow(wald_std), big.mark = ".", decimal.mark = ",") else "NICHT GELADEN"))
cat(sprintf("  freiland_std: %s Zeilen\n",
            if (exists("freiland_std") && !is.null(freiland_std))
              format(nrow(freiland_std), big.mark = ".", decimal.mark = ",") else "NICHT GELADEN"))
cat(sprintf("  formi:       %s Zeilen\n",
            if (exists("formi") && !is.null(formi))
              format(nrow(formi), big.mark = ".", decimal.mark = ",") else "NICHT GELADEN"))
cat(sprintf("  stamm:       %s Zeilen\n",
            if (exists("stamm") && !is.null(stamm))
              format(nrow(stamm), big.mark = ".", decimal.mark = ",") else "NICHT GELADEN"))
