# =============================================================================
# 05_analyse_F2.R
# Zweck:       Forschungsfrage 2 – Raeumliche Unterschiede zwischen den drei
#              Exploratorien Schwaebische Alb, Hainich-Duen, Schorfheide-Chorin.
#
#              (a) Deskriptive Statistiken je Region
#              (b) ANOVA / Kruskal-Wallis je Indikator (omnibus-Test)
#              (c) Paarweise Post-hoc-Tests (Tukey, Dunn)
#              (d) Mixed-Effects-Modell: amp ~ year * region + (1|plotID) + (1|year)
#
# Inputs:  wald_jahres  (aus 03_indikatoren.R)
# Outputs (in 04_Analyse/Ergebnisse/Tabellen/):
#   - F2_deskriptiv.csv
#   - F2_anova.csv
#   - F2_kruskal.csv
#   - F2_posthoc.csv
#   - F2_lme_amplitude.csv
#   - F2_lme_indikatoren.csv
#
# Autor: Fabian Luehr, generiert 2026-05-06
# =============================================================================

# Optionale Pakete fuer Post-hoc-Tests
if (!requireNamespace("emmeans", quietly = TRUE)) {
  message("Paket 'emmeans' nicht installiert. Post-hoc-Tests werden uebersprungen.")
  message("  -> install.packages('emmeans')")
}
if (!requireNamespace("dunn.test", quietly = TRUE)) {
  message("Paket 'dunn.test' nicht installiert. Dunn-Test wird uebersprungen.")
  message("  -> install.packages('dunn.test')")
}

# ---- Daten laden ------------------------------------------------------------
if (!exists("wald_jahres") || is.null(wald_jahres)) {
  f <- file.path(PFADE$aufbereitet, "wald_jahres.csv")
  if (!file.exists(f)) stop("wald_jahres.csv fehlt.")
  wald_jahres <- data.table::fread(f)
}

INDIKATOREN <- c("amp_mean", "n_sommer", "n_hitze", "n_tropen", "n_frost", "gdd_sum")


# ============================================================================
# A) DESKRIPTIVE STATISTIKEN je Region
# ============================================================================
cat("F2-A: Deskriptive Statistiken je Region...\n")

deskr_F2 <- data.table::rbindlist(lapply(INDIKATOREN, function(ind) {
  if (!ind %in% names(wald_jahres)) return(NULL)
  wald_jahres[!is.na(get(ind)), .(
    indikator = ind,
    n_obs     = .N,
    n_plots   = data.table::uniqueN(plotID),
    n_jahre   = data.table::uniqueN(year),
    mw        = round(mean(get(ind)), 2),
    sd        = round(sd(get(ind)),   2),
    median    = round(median(get(ind)), 2),
    q25       = round(quantile(get(ind), 0.25), 2),
    q75       = round(quantile(get(ind), 0.75), 2),
    min       = round(min(get(ind)), 2),
    max       = round(max(get(ind)), 2)
  ), by = region]
}))

speichern_csv(deskr_F2, file.path(PFADE$tabellen, "F2_deskriptiv.csv"))


# ============================================================================
# B) ANOVA + KRUSKAL-WALLIS je Indikator
# ============================================================================
cat("F2-B: ANOVA / Kruskal-Wallis...\n")

erg_anova <- data.table::rbindlist(lapply(INDIKATOREN, function(ind) {
  if (!ind %in% names(wald_jahres)) return(NULL)
  sub <- wald_jahres[!is.na(get(ind))]
  formel <- as.formula(paste(ind, "~ region"))

  # Shapiro-Wilk-Test (Normalverteilung) nur wenn n <= 5000
  if (nrow(sub) <= 5000) {
    sw <- tryCatch(shapiro.test(sub[[ind]]), error = function(e) NULL)
    normal_p <- if (!is.null(sw)) sw$p.value else NA
  } else {
    normal_p <- NA
  }

  # Levene-Test (Varianzhomogenitaet)
  levene_p <- tryCatch({
    lev <- car::leveneTest(formel, data = sub)
    lev[1, "Pr(>F)"]
  }, error = function(e) NA)

  # ANOVA
  fit_anova <- tryCatch(aov(formel, data = sub), error = function(e) NULL)
  if (!is.null(fit_anova)) {
    sa <- summary(fit_anova)[[1]]
    anova_F <- sa["region", "F value"]
    anova_p <- sa["region", "Pr(>F)"]
  } else {
    anova_F <- anova_p <- NA
  }

  # Kruskal-Wallis
  kw <- tryCatch(kruskal.test(formel, data = sub), error = function(e) NULL)

  data.table::data.table(
    indikator  = ind,
    normal_p   = round(normal_p, 4),
    levene_p   = round(levene_p, 4),
    anova_F    = round(anova_F, 3),
    anova_p    = round(anova_p, 5),
    kw_chi2    = if (!is.null(kw)) round(kw$statistic, 3) else NA,
    kw_p       = if (!is.null(kw)) round(kw$p.value, 5) else NA,
    empfehlung = dplyr::case_when(
      is.na(anova_p) ~ "keine Berechnung",
      !is.na(normal_p) & normal_p < 0.05 ~ "Kruskal-Wallis bevorzugen",
      !is.na(levene_p) & levene_p < 0.05 ~ "Welch-ANOVA bevorzugen",
      TRUE ~ "ANOVA geeignet"
    )
  )
}))

speichern_csv(erg_anova, file.path(PFADE$tabellen, "F2_anova_kruskal.csv"))
print(erg_anova[, .(indikator, anova_p, kw_p, empfehlung)])


# ============================================================================
# C) PAARWEISE POST-HOC-TESTS (nur Amplitude als Hauptzielgroesse)
# ============================================================================
cat("F2-C: Post-hoc-Tests fuer Amplitude...\n")

posthoc_liste <- list()

if (requireNamespace("emmeans", quietly = TRUE)) {
  fit_amp <- tryCatch(
    aov(amp_mean ~ region, data = wald_jahres[!is.na(amp_mean)]),
    error = function(e) NULL
  )
  if (!is.null(fit_amp)) {
    em      <- emmeans::emmeans(fit_amp, "region")
    tukey   <- as.data.table(pairs(em, adjust = "tukey"))
    tukey$test <- "Tukey"
    posthoc_liste[["tukey"]] <- tukey
    cat("Tukey-Test (Amplitude) abgeschlossen.\n")
  }
}

if (requireNamespace("dunn.test", quietly = TRUE)) {
  dt_amp <- wald_jahres[!is.na(amp_mean)]
  dunn_res <- tryCatch(
    dunn.test::dunn.test(dt_amp$amp_mean, dt_amp$region, method = "bonferroni"),
    error = function(e) NULL
  )
  if (!is.null(dunn_res)) {
    dunn_dt <- data.table::data.table(
      contrast = dunn_res$comparisons,
      Z        = dunn_res$Z,
      p_adj    = dunn_res$P.adjusted,
      test     = "Dunn-Bonferroni"
    )
    posthoc_liste[["dunn"]] <- dunn_dt
    cat("Dunn-Test (Amplitude) abgeschlossen.\n")
  }
}

if (length(posthoc_liste) > 0) {
  posthoc_gesamt <- data.table::rbindlist(posthoc_liste, fill = TRUE)
  speichern_csv(posthoc_gesamt, file.path(PFADE$tabellen, "F2_posthoc.csv"))
} else {
  cat("Keine Post-hoc-Tests verfuegbar (emmeans/dunn.test installieren).\n")
}


# ============================================================================
# D) MIXED-EFFECTS-MODELL: amp ~ year * region + (1|plotID) + (1|year)
# ============================================================================
cat("F2-D: Mixed-Effects-Modell (lmer)...\n")

lmer_ergebnisse <- list()

for (ind in INDIKATOREN) {
  if (!ind %in% names(wald_jahres)) next
  sub <- wald_jahres[!is.na(get(ind))]
  if (nrow(sub) < 50) next

  formel <- as.formula(paste(ind, "~ year * region + (1|plotID) + (1|year)"))
  cat(sprintf("  Modell fuer: %s...\n", ind))

  fit_lme <- tryCatch(
    lmerTest::lmer(formel, data = sub, REML = TRUE),
    error = function(e) {
      warning(paste("lmer fehlgeschlagen fuer", ind, ":", e$message))
      NULL
    }
  )

  if (!is.null(fit_lme)) {
    # Diagnostik speichern
    if (requireNamespace("performance", quietly = TRUE)) {
      tryCatch(
        performance::check_model(fit_lme),
        error = function(e) NULL
      )
    }

    # Koeffizienten-Tabelle (mit lmerTest p-Werten)
    koeff <- as.data.table(summary(fit_lme)$coefficients, keep.rownames = "term")
    data.table::setnames(koeff,
      c("Estimate", "Std. Error", "df", "t value", "Pr(>|t|)"),
      c("estimate", "se", "df", "t_val", "p_val"),
      skip_absent = TRUE
    )
    # Defensive Absicherung: fehlende Spalten als NA ergaenzen
    for (sp in c("estimate", "se", "df", "t_val", "p_val")) {
      if (!sp %in% names(koeff)) koeff[, (sp) := NA_real_]
    }
    koeff[, indikator := ind]

    # Intraklassen-Korrelation
    vc <- as.data.table(lme4::VarCorr(fit_lme))
    lmer_ergebnisse[[ind]] <- list(
      modell    = fit_lme,
      koeff     = koeff,
      varianz   = vc
    )

    # Koeffizienten speichern
    speichern_csv(koeff,
      file.path(PFADE$tabellen, paste0("F2_lme_", ind, "_koeff.csv")))
  }
}

# Zusammenfassung aller Modelle fuer Hauptindikator Amplitude
if (!is.null(lmer_ergebnisse[["amp_mean"]])) {
  erg_lme_amp <- lmer_ergebnisse[["amp_mean"]]$koeff
  speichern_csv(erg_lme_amp, file.path(PFADE$tabellen, "F2_lme_amplitude.csv"))
  cat("LME-Koeffizienten (Amplitude):\n")
  print(erg_lme_amp)
}

# Alle Indikatoren zusammen
if (length(lmer_ergebnisse) > 0) {
  alle_koeff <- data.table::rbindlist(
    lapply(lmer_ergebnisse, `[[`, "koeff"), fill = TRUE)
  speichern_csv(alle_koeff, file.path(PFADE$tabellen, "F2_lme_alle_indikatoren.csv"))
}

cat("\n05_analyse_F2.R abgeschlossen.\n")
