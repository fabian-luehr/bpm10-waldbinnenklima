# =============================================================================
# 04_analyse_F1.R
# Zweck:       Forschungsfrage 1 – Zeitliche Entwicklung der Klimaextreme.
#              (a) Lineare Regression je Region und Indikator (Trend ueber Zeit)
#              (b) Mann-Kendall-Test + Sen's-Slope auf Regionsmittelwerten
#              (c) Zeitlicher Trend des Wald-Freiland-Puffers (Delta_Puffer)
#
# Inputs:
#   - wald_jahres       (aus 03_indikatoren.R oder CSV)
#   - delta (optional)  (aus 03_indikatoren.R oder delta_puffer.csv)
#
# Outputs (in 04_Analyse/Ergebnisse/Tabellen/):
#   - F1_trend_lm.csv          (lineare Trendschaetzer je Region x Indikator)
#   - F1_trend_mk.csv          (Mann-Kendall tau + p + Sen's Slope)
#   - F1_trend_delta_puffer.csv (Trend des Wald-Freiland-Puffers)
#
# Objekte im Global Environment nach Lauf:
#   - erg_lm_F1, erg_mk_F1, erg_delta_F1
#
# Autor: Fabian Luehr, generiert 2026-05-06
# =============================================================================


# ---- Indikatoren-Definitionen -----------------------------------------------
INDIKATOREN <- c(
  "amp_mean"   = "Mittlere Tagesamplitude (°C)",
  "n_sommer"   = "Sommertage (Tmax >= 25°C)",
  "n_hitze"    = "Hitzetage (Tmax >= 30°C)",
  "n_tropen"   = "Tropennächte (Tmin >= 20°C)",
  "n_frost"    = "Frosttage (Tmin < 0°C)",
  "gdd_sum"    = "Wachstumsgradtage (GDD10)"
)


# ---- Daten laden (falls nicht im Environment) --------------------------------
if (!exists("wald_jahres") || is.null(wald_jahres)) {
  f <- file.path(PFADE$aufbereitet, "wald_jahres.csv")
  if (!file.exists(f)) stop("wald_jahres.csv fehlt. Bitte 03_indikatoren.R ausfuehren.")
  wald_jahres <- data.table::fread(f)
}


# ============================================================================
# A) LINEARE REGRESSION je Region x Indikator
# ============================================================================
cat("F1-A: Lineare Regressionen (Trend je Region x Indikator)...\n")

erg_lm_F1 <- data.table::rbindlist(lapply(names(INDIKATOREN), function(ind) {
  if (!ind %in% names(wald_jahres)) {
    warning(paste("Indikator nicht gefunden:", ind))
    return(NULL)
  }
  data.table::rbindlist(lapply(unique(wald_jahres$region), function(reg) {
    sub <- wald_jahres[region == reg & !is.na(get(ind))]
    if (nrow(sub) < 10) {
      warning(paste("Zu wenige Beobachtungen fuer", ind, "in", reg))
      return(NULL)
    }
    fit <- lm(as.formula(paste(ind, "~ year")), data = sub)
    s   <- summary(fit)
    r2  <- s$r.squared
    # Werte vorab berechnen, damit sie im data.table()-Aufruf
    # mehrfach verwendet werden koennen (Argumente werden unabhaengig
    # ausgewertet, daher kein Zugriff auf Schwester-Spalten moeglich).
    p_value <- s$coefficients["year", "Pr(>|t|)"]
    data.table::data.table(
      region      = reg,
      indikator   = ind,
      n_obs       = nrow(sub),
      n_plots     = data.table::uniqueN(sub$plotID),
      n_jahre     = data.table::uniqueN(sub$year),
      slope       = unname(coef(fit)["year"]),
      se          = s$coefficients["year", "Std. Error"],
      t_val       = s$coefficients["year", "t value"],
      p_val       = p_value,
      r_squared   = r2,
      signifikant = p_value < 0.05
    )
  }))
}))

# Bonferroni-Korrektur (multiple Vergleiche)
n_tests <- nrow(erg_lm_F1)
erg_lm_F1[, p_bonf := pmin(p_val * n_tests, 1)]
erg_lm_F1[, sig_bonf := p_bonf < 0.05]

# Lesehinweis: slope = Veraenderung pro Jahr (z.B. +0.3 Tage/Jahr = +3 Tage/Dekade)
erg_lm_F1[, slope_pro_dekade := slope * 10]

speichern_csv(erg_lm_F1, file.path(PFADE$tabellen, "F1_trend_lm.csv"))
cat("Lineare Regressionen abgeschlossen.\n")
print(erg_lm_F1[, .(region, indikator, slope_pro_dekade, p_val, signifikant)])


# ============================================================================
# B) MANN-KENDALL + SEN'S SLOPE auf Regionsmittelwerten
# ============================================================================
cat("\nF1-B: Mann-Kendall + Sen's Slope...\n")

# Regionsmittelwerte ueber alle Plots je Jahr
region_mittel <- wald_jahres[, lapply(.SD, mean, na.rm = TRUE),
                              .SDcols = names(INDIKATOREN),
                              by = .(region, year)]
data.table::setorder(region_mittel, region, year)

erg_mk_F1 <- data.table::rbindlist(lapply(names(INDIKATOREN), function(ind) {
  if (!ind %in% names(region_mittel)) return(NULL)
  data.table::rbindlist(lapply(unique(region_mittel$region), function(reg) {
    ts_werte <- region_mittel[region == reg, get(ind)]
    ts_jahre <- region_mittel[region == reg, year]
    if (length(ts_werte) < 5 || all(is.na(ts_werte))) return(NULL)
    ts_obj <- ts(ts_werte,
                 start = min(ts_jahre), frequency = 1)
    mk  <- trend::mk.test(ts_obj)
    ss  <- trend::sens.slope(ts_obj)
    data.table::data.table(
      region      = reg,
      indikator   = ind,
      n_jahre     = length(ts_werte),
      mk_tau      = unname(mk$estimates["tau"]),
      mk_p        = mk$p.value,
      sens_slope  = unname(ss$estimates),  # Veraenderung pro Jahr
      sens_per_10 = unname(ss$estimates) * 10,
      richtung    = dplyr::case_when(
        mk$p.value < 0.05 & unname(mk$estimates["tau"]) > 0 ~ "signif. Zunahme",
        mk$p.value < 0.05 & unname(mk$estimates["tau"]) < 0 ~ "signif. Abnahme",
        TRUE ~ "kein signif. Trend"
      )
    )
  }))
}))

speichern_csv(erg_mk_F1, file.path(PFADE$tabellen, "F1_trend_mk.csv"))
cat("Mann-Kendall abgeschlossen.\n")
print(erg_mk_F1[, .(region, indikator, sens_per_10, mk_p, richtung)])

# Zwischenspeichern des Regionsmittels fuer Visualisierung
speichern_csv(region_mittel,
              file.path(PFADE$zwischen, "region_mittelwerte_jahres.csv"))


# ============================================================================
# C) ZEITLICHER TREND DES WALD-FREILAND-PUFFERS (Delta_Puffer)
# ============================================================================
cat("\nF1-C: Trend des Wald-Freiland-Puffers...\n")

delta_pfad <- file.path(PFADE$aufbereitet, "delta_puffer.csv")
if (exists("delta") && !is.null(delta)) {
  delta_dt <- as.data.table(delta)
} else if (file.exists(delta_pfad)) {
  delta_dt <- data.table::fread(delta_pfad)
} else {
  warning("delta_puffer.csv nicht gefunden, F1-C wird uebersprungen.")
  delta_dt <- NULL
}

if (!is.null(delta_dt) && "delta_puffer" %in% names(delta_dt)) {
  erg_delta_F1 <- data.table::rbindlist(lapply(unique(delta_dt$region), function(reg) {
    sub <- delta_dt[region == reg & !is.na(delta_puffer)]
    if (nrow(sub) < 5) return(NULL)
    data.table::setorder(sub, year)
    fit <- lm(delta_puffer ~ year, data = sub)
    s   <- summary(fit)
    mk  <- trend::mk.test(ts(sub$delta_puffer, start = min(sub$year)))
    ss  <- trend::sens.slope(ts(sub$delta_puffer, start = min(sub$year)))
    data.table::data.table(
      region         = reg,
      n_jahre        = nrow(sub),
      lm_slope_yr    = unname(coef(fit)["year"]),
      lm_slope_10yr  = unname(coef(fit)["year"]) * 10,
      lm_p           = s$coefficients["year", "Pr(>|t|)"],
      mk_tau         = unname(mk$estimates["tau"]),
      mk_p           = mk$p.value,
      sens_slope_10yr = unname(ss$estimates) * 10,
      interpretation = paste0(
        "Δ_Puffer aendert sich um ",
        round(unname(coef(fit)["year"]) * 10, 2),
        "°C/Dekade (p=", round(s$coefficients["year", "Pr(>|t|)"], 3), ")"
      )
    )
  }))

  speichern_csv(erg_delta_F1,
                file.path(PFADE$tabellen, "F1_trend_delta_puffer.csv"))
  cat("Delta-Puffer-Trend:\n")
  print(erg_delta_F1[, .(region, lm_slope_10yr, lm_p, mk_p)])
} else {
  cat("Delta-Puffer nicht verfuegbar.\n")
  erg_delta_F1 <- NULL
}

cat("\n04_analyse_F1.R abgeschlossen.\n")
