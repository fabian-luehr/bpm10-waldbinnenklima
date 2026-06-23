# =============================================================================
# 03_indikatoren.R
# Zweck:       Berechnung der Klimaindikatoren aus Stundendaten:
#              - Tagestemperaturamplitude (Ta_200_max - Ta_200_min)
#              - Sommertage       (Tmax >= 25 degC)
#              - Hitzetage        (Tmax >= 30 degC)
#              - Tropennaechte    (Tmin >= 20 degC)
#              - Frosttage        (Tmin < 0 degC)
#              - Wachstumsgradtage (GDD10: Mittel(Tmax,Tmin) - 10, gekap. 0-20)
#              Aggregation auf Plot-Jahres-Ebene mit Qualitaetsfilter.
#
# Inputs:
#   - wald_std     (aus 02_einlesen_aufbereiten.R oder CSV)
#   - freiland_std (optional, fuer Wald-Freiland-Differenz)
#
# Outputs (in 03_Daten/02_Aufbereitet/):
#   - wald_jahres.csv     (Jahres-Indikatoren je Plot, Wald)
#   - freiland_jahres.csv (Jahres-Indikatoren je Plot, Freiland, optional)
#   - delta_puffer.csv    (Wald-Freiland-Differenz Δ_Puffer, optional)
#
# Autor: Fabian Luehr, generiert 2026-05-06
# =============================================================================


# ---- Hilfsfunktion: Tages- -> Jahresaggregation ----------------------------

#' Berechnet Tages- und Jahres-Indikatoren aus Stundendaten
#' @param std_dt  data.table mit Spalten plotID, date, year, region,
#'                Ta_200_max, Ta_200_min
#' @return data.table auf Jahresebene
berechne_jahresindikatoren <- function(std_dt) {

  if (is.null(std_dt) || nrow(std_dt) == 0) {
    warning("Leere Eingabe, keine Indikatoren berechnet.")
    return(NULL)
  }

  # -- Schritt 1: Tagesaggregation ------------------------------------------
  # Pro Plot und Tag: Tagesmaximum, Tagesminimum, Anzahl gueltiger Stunden
  cat("  Tagesaggregation...\n")
  tage <- std_dt[, .(
    ta_max    = suppressWarnings(max(Ta_200_max, na.rm = TRUE)),
    ta_min    = suppressWarnings(min(Ta_200_min, na.rm = TRUE)),
    n_std_max = sum(!is.na(Ta_200_max)),
    n_std_min = sum(!is.na(Ta_200_min))
  ), by = .(plotID, region, date, year)]

  # -Inf/+Inf -> NA (entsteht wenn alle Stunden NA)
  tage[is.infinite(ta_max), ta_max := NA_real_]
  tage[is.infinite(ta_min), ta_min := NA_real_]

  # Qualitaetsfilter: Tag braucht >= MIN_STD_PRO_TAG gueltige Stunden
  # (beide Werte, da Amplitude beide benoetigt)
  n_vor <- nrow(tage)
  tage  <- tage[n_std_max >= MIN_STD_PRO_TAG &
                n_std_min >= MIN_STD_PRO_TAG &
                !is.na(ta_max) & !is.na(ta_min)]
  cat(sprintf("  Tage nach Qualitaetsfilter: %s (von %s, %d%% behalten)\n",
              format(nrow(tage), big.mark = ".", decimal.mark = ","),
              format(n_vor,      big.mark = ".", decimal.mark = ","),
              round(100 * nrow(tage) / n_vor)))

  # -- Schritt 2: Indikatoren pro Tag ----------------------------------------
  # Amplitude (Tagesschwankung)
  tage[, amplitude    := ta_max - ta_min]

  # Extremtag-Indikatoren (binarisiert, 0/1)
  tage[, sommer_tag   := as.integer(ta_max >= 25)]   # Sommertag   >= 25 degC
  tage[, hitze_tag    := as.integer(ta_max >= 30)]   # Hitzetag    >= 30 degC
  tage[, tropen_nacht := as.integer(ta_min >= 20)]   # Tropennacht >= 20 degC
  tage[, frost_tag    := as.integer(ta_min <   0)]   # Frosttag    <  0 degC

  # Wachstumsgradtage (GDD10): max(0, Tagesmittel - 10)
  # Tagesmittel = (Tmax + Tmin) / 2; GDD gekappt auf [0, 20] (kein Hitzemalus)
  tage[, ta_mittel := (ta_max + ta_min) / 2]
  tage[, gdd10     := pmax(0, pmin(ta_mittel - 10, 20))]

  # -- Schritt 3: Jahresaggregation ------------------------------------------
  cat("  Jahresaggregation...\n")
  jahre <- tage[, .(
    # Amplitude
    amp_mean     = mean(amplitude,    na.rm = TRUE),  # mittl. Tagesampl.
    amp_sd       = sd(amplitude,      na.rm = TRUE),
    amp_p90      = quantile(amplitude, 0.9, na.rm = TRUE),

    # Extremtage (Summen pro Jahr)
    n_sommer     = sum(sommer_tag),
    n_hitze      = sum(hitze_tag),
    n_tropen     = sum(tropen_nacht),
    n_frost      = sum(frost_tag),
    gdd_sum      = sum(gdd10, na.rm = TRUE),  # Jahres-GDD10

    # Qualitaet
    n_tage_gut   = .N
  ), by = .(plotID, region, year)]

  # Qualitaetsfilter: Plot-Jahr mit >= MIN_TAGE_PRO_JAHR gueltigen Tagen
  n_vor2 <- nrow(jahre)
  jahre  <- jahre[n_tage_gut >= MIN_TAGE_PRO_JAHR]
  cat(sprintf("  Plot-Jahre nach Qualitaetsfilter: %d (von %d, %d%% behalten)\n",
              nrow(jahre), n_vor2, round(100 * nrow(jahre) / n_vor2)))

  # Auf Auswertungsfenster einschraenken
  jahre <- jahre[year %in% ANALYSE_JAHRE]
  cat(sprintf("  Plot-Jahre im Auswertungsfenster %d-%d: %d\n",
              min(ANALYSE_JAHRE), max(ANALYSE_JAHRE), nrow(jahre)))

  return(jahre)
}


# ============================================================================
# Wald-Jahresindikatoren
# ============================================================================

cat("Berechne Wald-Jahresindikatoren...\n")

ziel_wald_j <- file.path(PFADE$aufbereitet, "wald_jahres.csv")

if (file.exists(ziel_wald_j)) {
  cat("Wald-Jahresindikatoren bereits vorhanden, lade CSV.\n")
  wald_jahres <- data.table::fread(ziel_wald_j)
} else {
  if (!exists("wald_std") || is.null(wald_std)) {
    ziel_std <- file.path(PFADE$aufbereitet, "wald_stunden.csv")
    if (file.exists(ziel_std)) {
      cat("Lade Wald-Stundendaten aus CSV...\n")
      wald_std <- data.table::fread(ziel_std)
    } else {
      stop("wald_std nicht verfuegbar. Bitte zuerst 02_einlesen_aufbereiten.R laufen lassen.")
    }
  }

  wald_jahres <- berechne_jahresindikatoren(wald_std)
  speichern_csv(wald_jahres, ziel_wald_j)

  # RAM freigeben (Stundendaten nicht mehr benoetigt)
  rm(wald_std); gc()
}

cat(sprintf("Wald-Jahresindikatoren: %d Plot-Jahre, %d Plots, %d Jahre\n",
            nrow(wald_jahres),
            length(unique(wald_jahres$plotID)),
            length(unique(wald_jahres$year))))


# ============================================================================
# Freiland-Jahresindikatoren (optional)
# ============================================================================

cat("\nBerechne Freiland-Jahresindikatoren (fuer Wald-Freiland-Differenz)...\n")

ziel_freil_j <- file.path(PFADE$aufbereitet, "freiland_jahres.csv")

if (file.exists(ziel_freil_j)) {
  cat("Freiland-Jahresindikatoren bereits vorhanden.\n")
  freiland_jahres <- data.table::fread(ziel_freil_j)
} else if (exists("freiland_std") && !is.null(freiland_std)) {
  freiland_jahres <- berechne_jahresindikatoren(freiland_std)
  speichern_csv(freiland_jahres, ziel_freil_j)
  rm(freiland_std); gc()
} else {
  freiland_csv <- file.path(PFADE$aufbereitet, "freiland_stunden.csv")
  if (file.exists(freiland_csv)) {
    cat("Lade Freiland-Stundendaten aus CSV...\n")
    freiland_std <- data.table::fread(freiland_csv)
    freiland_jahres <- berechne_jahresindikatoren(freiland_std)
    speichern_csv(freiland_jahres, ziel_freil_j)
    rm(freiland_std); gc()
  } else {
    warning("Freiland-Stundendaten fehlen. Delta-Puffer-Analyse wird uebersprungen.")
    freiland_jahres <- NULL
  }
}


# ============================================================================
# Delta-Puffer: Freiland-Amplitude minus Wald-Amplitude
# ============================================================================
# Δ_Puffer > 0 bedeutet: Freiland hat groessere Tagesamplitude als Wald.
# Mit steigender Pufferwirkung nimmt Δ_Puffer zu.

if (!is.null(freiland_jahres)) {
  cat("\nBerechne Wald-Freiland-Differenz (Delta_Puffer)...\n")

  # Koordinatenbasierter Ansatz: jeder Wald-Plot ist seinem geografisch
  # naechsten Freiland-Plot innerhalb der gleichen Region zugeordnet
  # (erstellt von Teil E in 02_einlesen_aufbereiten.R).
  nn_pfad <- file.path(PFADE$aufbereitet, "naechstes_freiland.csv")

  if (!file.exists(nn_pfad)) {
    stop(paste0(
      "FEHLER: naechstes_freiland.csv nicht gefunden.\n",
      "  -> Bitte zuerst 02_einlesen_aufbereiten.R erneut laufen lassen,\n",
      "     damit Teil E die Koordinaten-Zuordnung erstellt.\n",
      "  -> Pfad: ", nn_pfad
    ))
  }

  nn_map <- data.table::fread(nn_pfad)
  cat(sprintf("  Nearest-Neighbor-Zuordnung geladen: %d Plot-Paare\n", nrow(nn_map)))

  # Freiland-Jahresindikatoren auf benoetigte Spalten reduzieren
  freil_ind <- freiland_jahres[, .(
    plotID_freil = plotID,
    year,
    amp_freil    = amp_mean
  )]

  # Schritt 1: Wald-Plots mit ihrem zugeordneten Freiland-Plot verbinden
  wald_mit_freil <- merge(
    wald_jahres,
    nn_map[, .(plotID_wald, plotID_freil, dist_km)],
    by.x  = "plotID",
    by.y  = "plotID_wald",
    all.x = TRUE
  )

  # Schritt 2: Freiland-Jahreswert des zugeordneten Plots anhaengen
  wald_mit_delta <- merge(
    wald_mit_freil,
    freil_ind,
    by    = c("plotID_freil", "year"),
    all.x = TRUE
  )

  # Delta_Puffer = Freiland-Amplitude - Wald-Amplitude
  # Positiver Wert: Freiland hat groessere Tagesamplitude -> Wald puffert
  wald_mit_delta[, delta_puffer := amp_freil - amp_mean]

  n_na_delta <- sum(is.na(wald_mit_delta$delta_puffer))
  if (n_na_delta > 0) {
    cat(sprintf("  Hinweis: %d Wald-Plot-Jahre ohne Freiland-Gegenstueck in diesem Jahr (NA).\n",
                n_na_delta))
  }

  speichern_csv(wald_mit_delta,
                file.path(PFADE$aufbereitet, "wald_jahres_mit_delta.csv"))

  # Regionsmittel fuer F1-Trendanalyse (aggregiert aus Plot-Paaren)
  delta <- wald_mit_delta[!is.na(delta_puffer), .(
    delta_puffer = mean(delta_puffer, na.rm = TRUE),
    n_wald_pl    = data.table::uniqueN(plotID),
    n_freil_pl   = data.table::uniqueN(plotID_freil)
  ), by = .(region, year)]

  speichern_csv(delta, file.path(PFADE$aufbereitet, "delta_puffer.csv"))

  cat(sprintf("  Delta-Puffer: %d Region-Jahre\n", nrow(delta)))
  cat(sprintf("  Mittlerer Delta_Puffer: %.2f degC\n",
              mean(delta$delta_puffer, na.rm = TRUE)))

} else {
  cat("Freilanddaten nicht verfuegbar, Delta-Puffer wird ausgelassen.\n")
  delta <- NULL
}

cat("\n03_indikatoren.R abgeschlossen.\n")
