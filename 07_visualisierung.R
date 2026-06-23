# =============================================================================
# 07_visualisierung.R
# Zweck:       Alle Abbildungen fuer BPM-10 erstellen.
#              Abbildungen 1-4: Zeitreihen (F1)
#              Abbildungen 5-6: Regionale Boxplots (F2)
#              Abbildungen 7-8: ForMI-Boxplots (F3)
#              Abbildung 9:     Forest-Plot der Modellkoeffizienten
#              Abbildung 10:    (Optional) Karte der Plots
#
# Inputs:  wald_jahres, region_mittel (aus Zwischenspeicher), erg_lm_F1,
#          erg_mk_F1, lmer_ergebnisse (aus F2), wald_formi (aus F3)
# Outputs: PNG-Dateien (300 dpi) in 04_Analyse/Ergebnisse/Abbildungen/
#
# Autor: Fabian Luehr, generiert 2026-05-06
# =============================================================================

abb <- PFADE$abbildungen  # Kurzkuerzel

# ---- Hilfsfunktion: Abbildung speichern ------------------------------------
save_abb <- function(plot_obj, name, w = 9, h = 6, dpi = 300) {
  pfad <- file.path(abb, paste0(name, ".png"))
  ggplot2::ggsave(pfad, plot_obj, width = w, height = h, dpi = dpi,
                  bg = "white")
  cat(sprintf("  Abbildung gespeichert: %s\n", basename(pfad)))
  invisible(pfad)
}

# ---- Globaler Schalter: OSM-Kachelhintergrund erzwungen ausschalten ----------
# Setze KARTE_OHNE_OSM <- TRUE vor dem Aufruf von 07_visualisierung.R,
# wenn die OSM-Karte Memory-Probleme verursacht (s. Anleitung Kap. 8).
if (!exists("KARTE_OHNE_OSM")) KARTE_OHNE_OSM <- FALSE

# ---- Daten laden (falls nicht im Environment) --------------------------------
if (!exists("wald_jahres") || is.null(wald_jahres)) {
  wald_jahres <- data.table::fread(file.path(PFADE$aufbereitet, "wald_jahres.csv"))
}

region_mittel_pfad <- file.path(PFADE$zwischen, "region_mittelwerte_jahres.csv")
if (!exists("region_mittel") && file.exists(region_mittel_pfad)) {
  region_mittel <- data.table::fread(region_mittel_pfad)
}

delta_pfad <- file.path(PFADE$aufbereitet, "delta_puffer.csv")
if (file.exists(delta_pfad)) {
  delta_dt <- data.table::fread(delta_pfad)
} else {
  delta_dt <- NULL
}

# Farben und Theme
cols <- REGION_FARBEN


# ============================================================================
# ABB 1: Zeitreihe Mittlere Tagesamplitude (Regionsmittelwerte)
# ============================================================================
cat("Abbildung 1: Zeitreihe Amplitude...\n")

if (exists("region_mittel") && !is.null(region_mittel)) {
  p_amp <- ggplot2::ggplot(region_mittel,
              ggplot2::aes(x = year, y = amp_mean,
                           colour = region, group = region)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 1.5) +
    ggplot2::geom_smooth(method = "lm", se = TRUE, alpha = 0.15,
                         linewidth = 0.6, linetype = "dashed") +
    ggplot2::scale_colour_manual(values = cols, name = "Exploratorium") +
    ggplot2::scale_x_continuous(breaks = seq(2006, 2025, by = 4)) +
    ggplot2::labs(
      title    = "Mittlere jährliche Tagestemperaturamplitude 2006–2025",
      subtitle = "Regionsmittelwert aus je 50–51 Waldplots; gestrichelt = lin. Trend",
      x        = "Jahr",
      y        = "Tagestemperaturamplitude (°C)",
      caption  = "Datenquelle: BExIS 24766 (Wald; stündliche Auflösung). Eigene Berechnung."
    ) +
    theme_bpm()

  save_abb(p_amp, "Abb01_Zeitreihe_Amplitude")
}


# ============================================================================
# ABB 2–4: Zeitreihen Extremtage (Sommertage, Frosttage, Tropennächte)
# ============================================================================
cat("Abbildungen 2–4: Zeitreihen Extremtage...\n")

indikatoren_abb <- list(
  list(var = "n_sommer", title = "Sommertage (Tmax ≥ 25 °C) 2006–2025",
       ylab = "Anzahl Sommertage je Plot", name = "Abb02_Zeitreihe_Sommertage"),
  list(var = "n_frost",  title = "Frosttage (Tmin < 0 °C) 2006–2025",
       ylab = "Anzahl Frosttage je Plot",   name = "Abb03_Zeitreihe_Frosttage"),
  list(var = "n_tropen", title = "Tropennächte (Tmin ≥ 20 °C) 2006–2025",
       ylab = "Anzahl Tropennächte je Plot", name = "Abb04_Zeitreihe_Tropennaechte")
)

for (ia in indikatoren_abb) {
  if (!ia$var %in% names(wald_jahres)) next
  p <- ggplot2::ggplot(wald_jahres,
         ggplot2::aes(x = year, y = .data[[ia$var]],
                       colour = region, fill = region)) +
    ggplot2::geom_point(alpha = 0.2, size = 0.8) +
    ggplot2::geom_smooth(method = "lm", se = TRUE, alpha = 0.15,
                         linewidth = 0.8) +
    ggplot2::scale_colour_manual(values = cols, name = "Exploratorium") +
    ggplot2::scale_fill_manual(values = cols,   name = "Exploratorium") +
    ggplot2::scale_x_continuous(breaks = seq(2006, 2025, by = 4)) +
    ggplot2::labs(
      title   = ia$title,
      x       = "Jahr",
      y       = ia$ylab,
      caption = "Datenquelle: BExIS 24766 (Wald). Eigene Berechnung."
    ) +
    theme_bpm()
  save_abb(p, ia$name)
}


# ============================================================================
# ABB 5: Wald-Freiland-Differenz (Delta_Puffer) ueber Zeit
# ============================================================================
cat("Abbildung 5: Delta_Puffer Zeitreihe...\n")

if (!is.null(delta_dt) && "delta_puffer" %in% names(delta_dt)) {
  p_delta <- ggplot2::ggplot(delta_dt,
               ggplot2::aes(x = year, y = delta_puffer,
                            colour = region, group = region)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_smooth(method = "lm", se = FALSE, linetype = "dashed",
                         linewidth = 0.6) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted", colour = "grey40") +
    ggplot2::scale_colour_manual(values = cols, name = "Exploratorium") +
    ggplot2::labs(
      title    = "Pufferwirkung des Waldes (Δ_Puffer) 2006–2025",
      subtitle = "Δ_Puffer = Freiland-Amplitude − Wald-Amplitude (Regionsmittelwerte)",
      x        = "Jahr",
      y        = "Δ_Puffer (°C)",
      caption  = "Positiv = Freiland hat größere Tagesamplitude als Wald."
    ) +
    theme_bpm()
  save_abb(p_delta, "Abb05_Delta_Puffer_Zeitreihe")
}


# ============================================================================
# ABB 6: Regionale Boxplots (Amplitude und Extremtage nebeneinander)
# ============================================================================
cat("Abbildung 6: Regionale Boxplots...\n")

if ("amp_mean" %in% names(wald_jahres)) {
  p_box_amp <- ggplot2::ggplot(wald_jahres,
                 ggplot2::aes(x = region, y = amp_mean,
                               fill = region, colour = region)) +
    ggplot2::geom_violin(alpha = 0.3, trim = TRUE) +
    ggplot2::geom_boxplot(width = 0.2, outlier.size = 0.5, alpha = 0.7) +
    ggplot2::scale_fill_manual(values   = cols, guide = "none") +
    ggplot2::scale_colour_manual(values = cols, guide = "none") +
    ggplot2::labs(
      title = "Verteilung der Tagestemperaturamplitude nach Exploratorium",
      x     = NULL,
      y     = "Tagestemperaturamplitude (°C/Jahr)"
    ) +
    theme_bpm() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 15, hjust = 1))

  p_box_sommer <- ggplot2::ggplot(wald_jahres,
                   ggplot2::aes(x = region, y = n_sommer,
                                 fill = region, colour = region)) +
    ggplot2::geom_violin(alpha = 0.3, trim = TRUE) +
    ggplot2::geom_boxplot(width = 0.2, outlier.size = 0.5, alpha = 0.7) +
    ggplot2::scale_fill_manual(values   = cols, guide = "none") +
    ggplot2::scale_colour_manual(values = cols, guide = "none") +
    ggplot2::labs(
      title = "Verteilung der Sommertage nach Exploratorium",
      x     = NULL,
      y     = "Anzahl Sommertage/Jahr"
    ) +
    theme_bpm() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 15, hjust = 1))

  p_region_kombi <- p_box_amp + p_box_sommer +
    patchwork::plot_annotation(
      caption = "Datenquelle: BExIS 24766. Alle Jahre 2006–2025."
    )
  save_abb(p_region_kombi, "Abb06_Regionale_Boxplots", w = 12, h = 6)
}


# ============================================================================
# ABB 7: ForMI-Boxplots (Amplitude je ForMI-Klasse, getrennt nach Region)
# ============================================================================
cat("Abbildung 7: ForMI-Boxplots...\n")

formi_pfad <- file.path(PFADE$aufbereitet, "formi.csv")
wald_delta_pfad <- file.path(PFADE$aufbereitet, "wald_jahres_mit_delta.csv")

if (file.exists(formi_pfad)) {
  formi_dt <- data.table::fread(formi_pfad)
  formi_m  <- unique(formi_dt[, .(plotID, ForMI, formi_klasse)], by = "plotID")
  wald_fm  <- merge(wald_jahres, formi_m, by = "plotID", all.x = FALSE)

  if (nrow(wald_fm) > 0 && "formi_klasse" %in% names(wald_fm)) {
    # Klassen als geordneten Faktor
    wald_fm[, formi_klasse := factor(formi_klasse,
                                      levels = c("gering", "mittel", "hoch"))]

    p_formi_amp <- ggplot2::ggplot(wald_fm[!is.na(formi_klasse)],
                     ggplot2::aes(x = formi_klasse, y = amp_mean,
                                   fill = region, colour = region)) +
      ggplot2::geom_boxplot(alpha = 0.5, outlier.size = 0.4) +
      ggplot2::facet_wrap(~region, scales = "free_y") +
      ggplot2::scale_fill_manual(values   = cols, guide = "none") +
      ggplot2::scale_colour_manual(values = cols, guide = "none") +
      ggplot2::labs(
        title    = "Tagestemperaturamplitude nach Bewirtschaftungsintensität (ForMI)",
        subtitle = "gering / mittel / hoch = Terzile des ForMI-Index",
        x        = "ForMI-Klasse",
        y        = "Mittlere Tagestemperaturamplitude (°C/Jahr)",
        caption  = "Datenquelle: BExIS 24766 + ForMIX (BExIS 31855)."
      ) +
      theme_bpm()
    save_abb(p_formi_amp, "Abb07_ForMI_Boxplot_Amplitude", w = 11, h = 5)
  }
}


# ============================================================================
# ABB 8: Forest-Plot der LME-Koeffizienten (F2 und F3)
# ============================================================================
cat("Abbildung 8: Forest-Plot der Modellkoeffizienten...\n")

fp_pfad <- file.path(PFADE$tabellen, "F2_lme_alle_indikatoren.csv")
if (file.exists(fp_pfad)) {
  fp_dt <- data.table::fread(fp_pfad)

  # Nur Steigung (year) und Interaktion (year:region) anzeigen
  fp_sub <- fp_dt[grepl("year|Year", term, ignore.case = TRUE) &
                  !grepl("Intercept", term)]

  if (nrow(fp_sub) > 0 && "estimate" %in% names(fp_sub)) {
    fp_sub[, ci_lo := estimate - 1.96 * se]
    fp_sub[, ci_hi := estimate + 1.96 * se]
    fp_sub[, signif := ifelse(!is.na(p_val) & p_val < 0.05, "p < 0.05", "n.s.")]

    p_forest <- ggplot2::ggplot(fp_sub,
                  ggplot2::aes(x = estimate, y = paste(indikator, term, sep = "\n"),
                                colour = signif)) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
      ggplot2::geom_errorbar(ggplot2::aes(xmin = ci_lo, xmax = ci_hi),
                              orientation = "y", width = 0.25, linewidth = 0.7) +
      ggplot2::geom_point(size = 2.5) +
      ggplot2::scale_colour_manual(values = c("p < 0.05" = "#0072B2", "n.s." = "grey60"),
                                    name = "Signifikanz") +
      ggplot2::labs(
        title    = "Modellkoeffizienten: Mixed-Effects-Modelle (F2)",
        subtitle = "Jahr-Effekte und Interaktionen mit Region; Balken = 95%-KI",
        x        = "Koeffizient (standardisiert)",
        y        = NULL,
        caption  = "Modell: Indikator ~ year * region + (1|plotID) + (1|year); lme4/lmerTest"
      ) +
      theme_bpm()
    save_abb(p_forest, "Abb08_ForestPlot_Koeffizienten", w = 9, h = 7)
  }
}


# ============================================================================
# ABB 9: Karte der Wald- und Freiland-Plots mit Kartenhintergrund
# ============================================================================
# Koordinatenquellen (in Prioritaetsreihenfolge):
#   1. plot_koordinaten.csv (03_Daten/02_Aufbereitet/) -- erzeugt von Teil E
#      in 02_einlesen_aufbereiten.R, enthaelt Wald- UND Freiland-Plots
#   2. Fallback: plot_description.csv im entpackten Wald-ZIP
# Kartenhintergrund (in Prioritaetsreihenfolge):
#   A. ggspatial::annotation_map_tile() -- OSM-Kacheln (benoetigt Internet
#      beim ersten Aufruf, danach gecacht); am detailliertesten
#   B. rnaturalearth -- Deutschland mit Bundeslaendergrenzen (offline, vektoriell)
#   C. ggplot2::borders() -- Weltkarte-Umriss (immer verfuegbar)
cat("Abbildung 9: Karte der Monitoring-Plots...\n")

# --- 1. Koordinaten laden ---------------------------------------------------
koord_pfad   <- file.path(PFADE$aufbereitet, "plot_koordinaten.csv")
plot_desc_pfad <- file.path(PFADE$roh, "BExIS_Klimadaten", "wald_entpackt",
                             "plot_description.csv")

sf_plot_dt <- NULL  # wird befuellt, falls Koordinaten gefunden

if (sf_verfuegbar) {

  if (file.exists(koord_pfad)) {
    # Bevorzugt: plot_koordinaten.csv aus Teil E (Wald + Freiland)
    kdt <- data.table::fread(koord_pfad, na.strings = c("NA","","-9999"))
    kdt <- kdt[!is.na(lat) & !is.na(lon)]
    kdt[, region := dplyr::coalesce(
      region_aus_id(plotID),
      region_aus_freiland_id(plotID)
    )]
    kdt <- kdt[!is.na(region)]
    sf_plot_dt <- kdt
    cat(sprintf("  Koordinaten aus plot_koordinaten.csv: %d Plots (%s)\n",
                nrow(kdt),
                paste(sort(table(kdt$typ)), names(sort(table(kdt$typ))),
                      sep = " ", collapse = ", ")))

  } else if (file.exists(plot_desc_pfad)) {
    # Fallback: plot_description.csv aus Wald-ZIP (nur Wald-Plots)
    kdt <- data.table::fread(plot_desc_pfad, na.strings = c("NA","","-9999"))
    names(kdt) <- tolower(trimws(names(kdt)))
    lon_s  <- grep("^lon$|^long$|^longitude$", names(kdt), value = TRUE)[1]
    lat_s  <- grep("^lat$|^latitude$",         names(kdt), value = TRUE)[1]
    plot_s <- grep("^plot$|^plotid$",          names(kdt), value = TRUE)[1]
    if (!is.na(lon_s) && !is.na(lat_s) && !is.na(plot_s)) {
      kdt[, (lon_s) := suppressWarnings(as.numeric(get(lon_s)))]
      kdt[, (lat_s) := suppressWarnings(as.numeric(get(lat_s)))]
      kdt <- kdt[!is.na(get(lon_s)) & !is.na(get(lat_s))]
      data.table::setnames(kdt, c(plot_s, lon_s, lat_s), c("plotID", "lon", "lat"))
      kdt[, region := region_aus_id(plotID)]
      kdt[, typ    := "wald"]
      kdt <- kdt[!is.na(region)]
      sf_plot_dt <- kdt
      cat(sprintf("  Koordinaten aus plot_description.csv: %d Wald-Plots\n",
                  nrow(kdt)))
    }
  }
}

if (sf_verfuegbar && !is.null(sf_plot_dt) && nrow(sf_plot_dt) > 0) {

  # Farben: Regionen fuer Wald, Freiland mit Transparenz
  cols_karte <- REGION_FARBEN

  # Formsymbol je Typ (Wald: gefuellter Kreis, Freiland: Dreieck)
  typ_shapes <- c("wald" = 16, "freiland" = 17)
  typ_labels <- c("wald" = "Wald-Plot", "freiland" = "Freiland-Plot")

  # sf-Objekt erzeugen
  sf_plots <- sf::st_as_sf(as.data.frame(sf_plot_dt),
                            coords = c("lon", "lat"), crs = 4326)

  # --- 2. Basiskartenobjekt aufbauen ----------------------------------------
  # Bounding Box mit Puffer fuer Beschriftungsraum
  bbox   <- sf::st_bbox(sf_plots)
  puffer <- 0.8  # Grad
  xlim   <- c(bbox["xmin"] - puffer, bbox["xmax"] + puffer)
  ylim   <- c(bbox["ymin"] - puffer, bbox["ymax"] + puffer)

  # --- A: OSM-Kacheln (ggspatial + Internetverbindung) ----------------------
  # annotation_map_tile() benoetigt sowohl 'rosm' (Kachel-Download) als auch
  # 'prettymapr' (interne Skalierung). Beide muessen vorhanden sein, sonst
  # bricht die Karte mit "es gibt kein Paket namens 'prettymapr'".
  # Zusaetzlich: KARTE_OHNE_OSM kann OSM erzwungen ausschalten (s. Header).
  osm_ok <- ggspatial_verfuegbar &&
             !isTRUE(KARTE_OHNE_OSM) &&
             requireNamespace("rosm",       quietly = TRUE) &&
             requireNamespace("prettymapr", quietly = TRUE)

  # --- B: rnaturalearth (offline Vektor-Karte) --------------------------------
  ne_ok <- rnaturalearth_verfuegbar && sf_verfuegbar

  # ---- Hilfsfunktion: Karte mit optionalem OSM-Hintergrund aufbauen ---------
  # Wird ggf. zweimal aufgerufen: einmal mit OSM und (bei Render-Fehler beim
  # Speichern) einmal ohne OSM, dann mit rnaturalearth/borders als Hintergrund.
  bau_karte <- function(mit_osm) {
    p <- ggplot2::ggplot()

    if (mit_osm) {
      cat("  Kartenhintergrund: OpenStreetMap-Kacheln (ggspatial, Zoom 6)\n")
      # Zoom 6 (statt 7) erzeugt kleinere/weniger Kacheln und vermeidet den
      # bekannten Integer-Overflow beim Raster-Rendering.
      p <- p +
        ggspatial::annotation_map_tile(type = "osm", zoom = 6, quiet = TRUE)
    } else if (ne_ok) {
      cat("  Kartenhintergrund: rnaturalearth (Bundeslaendergrenzen)\n")
      de_states <- tryCatch(
        rnaturalearth::ne_states(country = "Germany", returnclass = "sf"),
        error = function(e) NULL)
      de_country <- tryCatch(
        rnaturalearth::ne_countries(country = "Germany", scale = "medium",
                                     returnclass = "sf"),
        error = function(e) NULL)
      nachbarn <- tryCatch(
        rnaturalearth::ne_countries(
          country = c("Austria","Switzerland","France","Czech Republic",
                      "Poland","Netherlands","Belgium","Denmark"),
          scale = "medium", returnclass = "sf"),
        error = function(e) NULL)
      if (!is.null(nachbarn)) {
        p <- p + ggplot2::geom_sf(data = nachbarn, fill = "grey92",
                                    colour = "grey70", linewidth = 0.3)
      }
      if (!is.null(de_states)) {
        p <- p + ggplot2::geom_sf(data = de_states, fill = "grey97",
                                    colour = "grey60", linewidth = 0.4)
      } else if (!is.null(de_country)) {
        p <- p + ggplot2::geom_sf(data = de_country, fill = "grey97",
                                    colour = "grey60", linewidth = 0.5)
      }
    } else {
      cat("  Kartenhintergrund: ggplot2::borders() (Fallback)\n")
      welt_dt <- tryCatch(ggplot2::map_data("world"), error = function(e) NULL)
      if (!is.null(welt_dt)) {
        p <- p + ggplot2::geom_polygon(
          data = welt_dt[welt_dt$region %in%
                           c("Germany","Austria","Switzerland","France",
                             "Czech Republic","Poland","Netherlands",
                             "Belgium","Denmark","Luxembourg"), ],
          ggplot2::aes(x = long, y = lat, group = group),
          fill = "grey95", colour = "grey60", linewidth = 0.3)
      }
    }

    # Plots aufzeichnen
    if ("typ" %in% names(sf_plot_dt)) {
      p <- p +
        ggplot2::geom_sf(data = sf_plots,
                          ggplot2::aes(colour = region, shape = typ),
                          size = 2, alpha = 0.85) +
        ggplot2::scale_shape_manual(values = typ_shapes,
                                     labels = typ_labels,
                                     name   = "Plot-Typ")
    } else {
      p <- p +
        ggplot2::geom_sf(data = sf_plots,
                          ggplot2::aes(colour = region),
                          size = 2, alpha = 0.85)
    }

    p <- p +
      ggplot2::scale_colour_manual(values = cols_karte, name = "Exploratorium") +
      ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
      ggplot2::labs(
        title    = "Lage der Monitoring-Plots in Deutschland",
        subtitle = paste(
          nrow(sf_plot_dt), "Plots in den drei Biodiversitaets-Exploratorien;",
          if ("typ" %in% names(sf_plot_dt))
            sprintf("%d Wald-, %d Freiland-Plots",
                    sum(sf_plot_dt$typ == "wald",     na.rm = TRUE),
                    sum(sf_plot_dt$typ == "freiland", na.rm = TRUE))
          else ""
        ),
        x = NULL, y = NULL,
        caption = "Datenquelle: BExIS 24766 (plot_description.csv). WGS84."
      ) +
      theme_bpm() +
      ggplot2::theme(
        legend.position  = "right",
        panel.background = ggplot2::element_rect(fill = "aliceblue"),
        panel.grid.major = ggplot2::element_line(colour = "white",
                                                  linewidth = 0.3)
      )

    # Kartografische Elemente (Nordpfeil, Massstab)
    if (ggspatial_verfuegbar) {
      p <- p +
        ggspatial::annotation_scale(
          location = "bl", width_hint = 0.25,
          pad_x = ggplot2::unit(0.4, "cm"),
          pad_y = ggplot2::unit(0.4, "cm")) +
        ggspatial::annotation_north_arrow(
          location = "tr",
          style    = ggspatial::north_arrow_fancy_orienteering(
            fill = c("grey30","white"),
            line_col = "grey30", text_col = "grey30"),
          height = ggplot2::unit(1.2, "cm"),
          width  = ggplot2::unit(1.2, "cm"))
    }
    p
  }

  # ---- Speichern mit Fallback bei OSM-Render-Fehler ------------------------
  karte_gespeichert <- FALSE
  if (osm_ok) {
    karte_gespeichert <- tryCatch({
      p_karte <- bau_karte(mit_osm = TRUE)
      # dpi = 150 (statt 300) reduziert die Rastergroesse drastisch und ist
      # fuer eine Uebersichtskarte voellig ausreichend.
      save_abb(p_karte, "Abb09_Karte_Plots", w = 8, h = 9, dpi = 150)
      TRUE
    }, error = function(e) {
      cat(sprintf("  WARNUNG: OSM-Karte konnte nicht gespeichert werden:\n    %s\n",
                  conditionMessage(e)))
      cat("  -> Wechsle automatisch auf Vektor-Hintergrund (ohne OSM).\n")
      FALSE
    })
  }
  if (!karte_gespeichert) {
    p_karte <- bau_karte(mit_osm = FALSE)
    save_abb(p_karte, "Abb09_Karte_Plots", w = 8, h = 9, dpi = 300)
  }
  cat(sprintf("  Karte gespeichert mit %d Plots.\n", nrow(sf_plot_dt)))

} else {
  if (!sf_verfuegbar) {
    cat("  sf nicht verfuegbar -> install.packages('sf'). Karte entfaellt.\n")
  } else {
    cat("  Keine Koordinaten gefunden. Zuerst 02_einlesen_aufbereiten.R neu laufen lassen.\n")
  }
}

cat("\n07_visualisierung.R abgeschlossen.\n")
