# =============================================================================
# 06_analyse_F3.R
# Zweck:       Forschungsfrage 3 – Einfluss der Bewirtschaftungsintensitaet
#              (ForMI) auf die Temperaturamplitude und Extremtage.
#
#              (a) Deskriptive Analyse: Boxplots und Kennwerte je ForMI-Klasse
#              (b) Mixed-Effects-Modell: amp ~ year * ForMI + region + (1|plotID)
#              (c) Sensitivitaetsanalyse mit aktuellem ForMI (falls mehrere Jahre)
#
# Inputs:
#   - wald_jahres  (aus 03_indikatoren.R)
#   - formi        (aus 02_einlesen_aufbereiten.R)
#
# Outputs (in 04_Analyse/Ergebnisse/Tabellen/):
#   - F3_deskriptiv_klassen.csv
#   - F3_lme_amp_formi.csv
#   - F3_lme_alle_indikatoren_formi.csv
#   - F3_sensitivitaet_formi_aktuell.csv  (falls vorhanden)
#
# Autor: Fabian Luehr, generiert 2026-05-06
# =============================================================================


# ---- Daten laden ------------------------------------------------------------
if (!exists("wald_jahres") || is.null(wald_jahres)) {
  f <- file.path(PFADE$aufbereitet, "wald_jahres.csv")
  if (!file.exists(f)) stop("wald_jahres.csv fehlt.")
  wald_jahres <- data.table::fread(f)
}

if (!exists("formi") || is.null(formi)) {
  f <- file.path(PFADE$aufbereitet, "formi.csv")
  if (!file.exists(f)) {
    warning("formi.csv fehlt. F3-Analyse wird uebersprungen.")
    formi <- NULL
  } else {
    formi <- data.table::fread(f)
  }
}

if (is.null(formi)) {
  cat("ForMI-Daten fehlen. 06_analyse_F3.R uebersprungen.\n")
  erg_f3_lme  <- NULL
  deskr_f3    <- NULL
} else {

  INDIKATOREN <- c("amp_mean", "n_sommer", "n_hitze", "n_tropen", "n_frost", "gdd_sum")

  # ---- Zusammenfuehren wald_jahres + formi -----------------------------------
  # ForMI ist auf Plotebene (kein Jahreswert), daher: jede Plot-Jahreskombination
  # erhaelt den ForMI-Wert des Plots.
  formi_merge <- formi[, .(plotID, ForMI, formi_klasse)]
  # Duplikate entfernen (falls mehrere Zeilen pro Plot)
  formi_merge <- unique(formi_merge, by = "plotID")

  wald_formi <- merge(wald_jahres, formi_merge, by = "plotID", all.x = FALSE)
  cat(sprintf("Plots mit ForMI: %d (von %d Wald-Plots insgesamt)\n",
              length(unique(wald_formi$plotID)),
              length(unique(wald_jahres$plotID))))

  if (nrow(wald_formi) == 0) {
    warning("Kein Merge moeglich zwischen Wald-Jahresdaten und ForMI. Pruefe plotID-Format.")
    wald_formi <- NULL
  }
}

if (!is.null(wald_formi)) {

  # ============================================================================
  # A) DESKRIPTIVE ANALYSE je ForMI-Klasse
  # ============================================================================
  cat("F3-A: Deskriptive Statistiken je ForMI-Klasse...\n")

  deskr_f3 <- data.table::rbindlist(lapply(INDIKATOREN, function(ind) {
    if (!ind %in% names(wald_formi)) return(NULL)
    wald_formi[!is.na(get(ind)) & !is.na(formi_klasse), .(
      indikator   = ind,
      n_obs       = .N,
      n_plots     = data.table::uniqueN(plotID),
      formi_mw    = round(mean(ForMI, na.rm = TRUE), 3),
      mw          = round(mean(get(ind), na.rm = TRUE), 2),
      sd          = round(sd(get(ind), na.rm = TRUE), 2),
      median      = round(median(get(ind), na.rm = TRUE), 2)
    ), by = .(formi_klasse, region)]
  }))

  speichern_csv(deskr_f3, file.path(PFADE$tabellen, "F3_deskriptiv_klassen.csv"))
  cat("Deskriptive F3-Statistiken gespeichert.\n")


  # ============================================================================
  # B) MIXED-EFFECTS-MODELL: amp ~ year * ForMI + region + (1|plotID) + (1|year)
  # ============================================================================
  cat("F3-B: Mixed-Effects-Modelle mit ForMI...\n")

  erg_f3_lme_alle <- list()

  for (ind in INDIKATOREN) {
    if (!ind %in% names(wald_formi)) next
    sub <- wald_formi[!is.na(get(ind)) & !is.na(ForMI)]
    if (nrow(sub) < 30) {
      warning(paste("Zu wenige Beobachtungen fuer", ind))
      next
    }
    cat(sprintf("  Modell fuer: %s (%d Beobachtungen)...\n", ind, nrow(sub)))

    # ForMI zentrieren (erleichtert Interpretation der Interaktionskoeffizienten)
    sub[, ForMI_z := scale(ForMI)]
    sub[, year_z  := scale(year)]

    formel <- as.formula(
      paste(ind, "~ year_z * ForMI_z + region + (1|plotID) + (1|year)")
    )

    fit <- tryCatch(
      lmerTest::lmer(formel, data = sub, REML = TRUE),
      warning = function(w) {
        message(paste("  WARNUNG bei", ind, ":", conditionMessage(w)))
        suppressWarnings(lmerTest::lmer(formel, data = sub, REML = TRUE))
      },
      error = function(e) {
        warning(paste("  FEHLER bei", ind, ":", e$message))
        NULL
      }
    )

    if (!is.null(fit)) {
      koeff <- as.data.table(summary(fit)$coefficients, keep.rownames = "term")
      data.table::setnames(koeff,
        c("Estimate", "Std. Error", "df", "t value", "Pr(>|t|)"),
        c("estimate", "se", "df", "t_val", "p_val"),
        skip_absent = TRUE)
      # Defensive Absicherung: fehlende Spalten als NA ergaenzen,
      # damit nachgelagerte Aufrufe (print/plot) nicht abbrechen
      for (sp in c("estimate", "se", "df", "t_val", "p_val")) {
        if (!sp %in% names(koeff)) koeff[, (sp) := NA_real_]
      }
      koeff[, indikator := ind]
      koeff[, n_obs := nrow(sub)]
      erg_f3_lme_alle[[ind]] <- list(fit = fit, koeff = koeff)

      speichern_csv(koeff,
        file.path(PFADE$tabellen, paste0("F3_lme_", ind, "_formi.csv")))
    }
  }

  # Zusammenfassende Ausgabe fuer Amplitude
  if (!is.null(erg_f3_lme_alle[["amp_mean"]])) {
    cat("\nHauptergebnis F3 – Amplitude vs. ForMI:\n")
    print(erg_f3_lme_alle[["amp_mean"]]$koeff[
      grepl("ForMI_z|year_z", term),
      .(term, estimate, se, p_val)])

    speichern_csv(
      erg_f3_lme_alle[["amp_mean"]]$koeff,
      file.path(PFADE$tabellen, "F3_lme_amp_formi.csv"))
  }

  # Alle Indikatoren zusammen
  if (length(erg_f3_lme_alle) > 0) {
    alle_f3 <- data.table::rbindlist(
      lapply(erg_f3_lme_alle, `[[`, "koeff"), fill = TRUE)
    speichern_csv(alle_f3,
                  file.path(PFADE$tabellen, "F3_lme_alle_indikatoren_formi.csv"))
  }


  # ============================================================================
  # C) SENSITIVITAETSANALYSE mit aktuellem ForMI
  # ============================================================================
  formi_sens_pfad <- file.path(PFADE$aufbereitet, "formi_sensitivitaet_aktuell.csv")

  if (file.exists(formi_sens_pfad)) {
    cat("\nF3-C: Sensitivitaetsanalyse mit aktuellem ForMI-Zeitpunkt...\n")

    formi_sens <- data.table::fread(formi_sens_pfad)
    namen_s    <- tolower(names(formi_sens))
    names(formi_sens) <- namen_s

    # plotID-Spalte robust auf "plotID" (CamelCase) setzen.
    # Hinweis: setnames immer ausfuehren, auch wenn der Quellname schon
    # "plotid" lautet -- sonst bliebe die Spalte lowercase und der spaetere
    # Zugriff per .(plotID, ...) wuerde fehlschlagen.
    id_s <- grep("^plotid$|^ep_plotid$", namen_s, value = TRUE)[1]
    if (is.na(id_s)) id_s <- grep("plot", namen_s, value = TRUE)[1]
    if (!is.na(id_s)) {
      data.table::setnames(formi_sens, id_s, "plotID")
    }

    # ForMI-Spalte robust auf "ForMI" setzen (gleiche Logik wie oben)
    formi_s <- grep("^formi$|^formix$|^formi_index$", namen_s, value = TRUE)[1]
    if (is.na(formi_s)) formi_s <- grep("formi", namen_s, value = TRUE)[1]
    if (!is.na(formi_s)) {
      data.table::setnames(formi_sens, formi_s, "ForMI")
    }

    # ForMI numerisch erzwingen
    formi_sens[, ForMI := suppressWarnings(as.numeric(as.character(ForMI)))]
    formi_sens <- formi_sens[!is.na(ForMI)]

    formi_sens_merge <- unique(formi_sens[, .(plotID, ForMI)], by = "plotID")
    wald_formi_sens  <- merge(wald_jahres, formi_sens_merge,
                               by = "plotID", all.x = FALSE)

    if (nrow(wald_formi_sens) > 30) {
      wald_formi_sens[, ForMI_z := scale(ForMI)]
      wald_formi_sens[, year_z  := scale(year)]
      wald_formi_sens[, region  := region_aus_id(plotID)]

      fit_sens <- tryCatch(
        lmerTest::lmer(amp_mean ~ year_z * ForMI_z + region + (1|plotID) + (1|year),
                   data = wald_formi_sens, REML = TRUE),
        error = function(e) NULL
      )
      if (!is.null(fit_sens)) {
        koeff_sens <- as.data.table(
          summary(fit_sens)$coefficients, keep.rownames = "term")
        data.table::setnames(koeff_sens,
          c("Estimate", "Std. Error", "df", "t value", "Pr(>|t|)"),
          c("estimate", "se", "df", "t_val", "p_val"),
          skip_absent = TRUE)
        for (sp in c("estimate", "se", "df", "t_val", "p_val")) {
          if (!sp %in% names(koeff_sens)) koeff_sens[, (sp) := NA_real_]
        }
        koeff_sens[, datensatz := "aktueller ForMI-Zeitpunkt"]
        speichern_csv(koeff_sens,
          file.path(PFADE$tabellen, "F3_sensitivitaet_formi_aktuell.csv"))
        cat("Sensitivitaetsanalyse abgeschlossen.\n")
        cat("Vergleich Interaktion year_z:ForMI_z:\n")
        print(koeff_sens[term == "year_z:ForMI_z",
                         .(term, estimate, se, p_val)])
      }
    }
  } else {
    cat("Keine aktuellen ForMI-Daten fuer Sensitivitaetsanalyse.\n")
  }

  erg_f3_lme <- erg_f3_lme_alle
}

cat("\n06_analyse_F3.R abgeschlossen.\n")
