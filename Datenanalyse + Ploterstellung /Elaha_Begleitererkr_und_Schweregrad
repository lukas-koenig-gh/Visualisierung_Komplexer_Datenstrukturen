library(tidyverse)

### 1. Komorbiditäten (Begleiterkrankungen) ###

to01 <- function(x) {
  if (is.numeric(x)) return(ifelse(is.na(x), NA_real_, ifelse(x > 0, 1, 0)))
  if (is.logical(x)) return(ifelse(is.na(x), NA_real_, as.numeric(x)))
  
  x_chr <- tolower(trimws(as.character(x)))
  
  yes_vals <- c("1", "yes", "y", "true", "ja", "j", "t", "pos", "positiv", "present")
  no_vals  <- c("0", "no", "n", "false", "nein", "f", "neg", "negativ", "absent")
  
  ifelse(x_chr %in% yes_vals, 1,
         ifelse(x_chr %in% no_vals, 0, NA_real_))
}

# Labels (Präsentation: verständlich statt Abkürzungen)
comorb_labels <- c(
  Cardiovascular = "Kardiovaskulär",
  Hypertension   = "Hypertonie",
  CHF            = "Chron. Herzinsuffizienz (CHF)",
  CKD            = "Chron. Nierenversagen (CKD)",
  COPD           = "COPD",
  DM             = "Diabetes mellitus (DM)",
  Malignancy     = "Malignität"
)

comorbidity_long <- dat %>%
  select(Cardiovascular, Hypertension, CHF, CKD, COPD, DM, Malignancy) %>%
  mutate(across(everything(), to01)) %>%
  pivot_longer(everything(), names_to = "Komorbidität", values_to = "Vorhanden") %>%
  group_by(Komorbidität) %>%
  summarise(
    n_total = n(),
    n_valid = sum(!is.na(Vorhanden)),
    Anteil  = mean(Vorhanden == 1, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    Komorbidität = recode(Komorbidität, !!!comorb_labels)
  )

# Optional: Missingness-Info als Untertitel (nur wenn wirklich Missing da ist)
missing_info <- comorbidity_long %>%
  summarise(miss = sum(n_total - n_valid)) %>%
  pull(miss)

subt <- if (missing_info > 0) {
  paste0("Hinweis: ", missing_info, " fehlende Werte über alle Komorbiditäten")
} else {
  NULL
}

ggplot(comorbidity_long, aes(x = reorder(Komorbidität, Anteil), y = Anteil)) +
  geom_col(fill = "#2C7BB6") +
  geom_text(aes(label = sprintf("%.1f", Anteil)),
            hjust = -0.15, size = 4) +
  coord_flip() +
  labs(
    title = "Prävalenz der Komorbiditäten",
    subtitle = subt,
    x = "Komorbidität",
    y = "Anteil der Patienten (%)"
  ) +
  theme_minimal(base_size = 14) +
  scale_y_continuous(limits = c(0, max(comorbidity_long$Anteil) + 6),
                     expand = expansion(mult = c(0, 0.02)))


### 2. Schweregrad der Erkrankung ###

severity_long <- dat %>%
  select(SAPS, SOFA, Leukocytes, CRP) %>%
  pivot_longer(everything(), names_to = "Parameter", values_to = "Wert") %>%
  mutate(
    # Reihenfolge (erst Scores, dann Labore)
    Parameter = factor(Parameter, levels = c("SAPS", "SOFA", "Leukocytes", "CRP")),
    # Schönere Namen + Einheiten
    Parameter_label = recode(Parameter,
                             "SAPS" = "SAPS (Score)",
                             "SOFA" = "SOFA (Score)",
                             "Leukocytes" = "Leukozyten (/nL)",
                             "CRP" = "CRP (mg/dL)"
    )
  )

# Mediane pro Panel berechnen (für Label)
meds <- severity_long %>%
  group_by(Parameter_label) %>%
  summarise(median = median(Wert, na.rm = TRUE), .groups = "drop")

ggplot(severity_long, aes(x = "", y = Wert)) +
  geom_boxplot(fill = "#FDAE61", alpha = 0.85, width = 0.6, outlier.alpha = 0.6) +
  facet_wrap(~ Parameter_label, scales = "free_y") +
  geom_text(
    data = meds,
    aes(x = 1, y = median, label = paste0("Median: ", round(median, 1))),
    inherit.aes = FALSE,
    vjust = -0.8, size = 4
  ) +
  labs(
    title = "Schweregradparameter zu Therapiebeginn",
    x = NULL,
    y = "Wert"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
