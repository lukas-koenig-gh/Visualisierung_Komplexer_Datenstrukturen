#Code von Elaha 

#Setup 

#Pakete Laden 
library(tidyverse)

#Datensatz laden
load("vancomycin.RData")

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
  
  #Ausswahl aller Werte dessen Namen wir oben angepasst haben 
  select(all_of(names(comorb_labels))) %>%
  
  #Datensatz langziehen für ggplot
  pivot_longer(everything(), names_to = "Komorbiditaet", values_to = "Vorhanden") %>%
  
  #Fehlende Werte Entfernen 
  drop_na(Vorhanden) %>%
  
  #Gruppieren nach Komorbidität
  group_by(Komorbiditaet) %>%
  
  #Summen und Durchschnitte Berechnen 
  summarise(
    #   n_total = n(),
    #   n_valid = sum(!is.na(Vorhanden)),
    Anteil  = mean(Vorhanden == "yes", na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  
  #Anpassung der Labels für besseres Verständnis im Plot 
  mutate(
    Komorbidität = recode(Komorbiditaet, !!!comorb_labels)
  )

ggplot(comorbidity_long, aes(x = reorder(Komorbiditaet, Anteil), y = Anteil, fill = Komorbiditaet)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f", Anteil)), hjust = -0.15, size = 4) +
  coord_flip() +
  scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.8) +
  labs(
    title = "Prävalenz der Komorbiditäten",
    x = "Komorbidität",
    y = "Anteil der Patienten (%)"
  ) +
  #Theme Auswahl
  theme_minimal(base_size = 14) +
  
  #Entfernen der Legende
  theme(
    legend.position = "none"
  ) +

scale_y_continuous(limits = c(0, max(comorbidity_long$Anteil) + 6),
                   expand = expansion(mult = c(0, 0.02)))

#Speichern des Plots
ggsave(
  filename = "Barplot_Elaha.png",
  width = 30,
  height = 13,
  units = "cm",
  device = "png"
)

### 2. Schweregrad der Erkrankung ###

severity_long <- dat %>%
  
  #Auswahl der Daten
  select(SAPS, SOFA, Leukocytes, CRP) %>%
  
  #Langziehen der Daten für ggplot
  pivot_longer(everything(), names_to = "Parameter", values_to = "Wert") %>%
  
  #Entfernen der NA Werte
  drop_na(Wert) %>%
  
  #Modifizeren der Reinfolge und Labels 
  mutate(
    # Reihenfolge (erst Scores, dann Labore)
    Parameter_label = factor(Parameter,
                       levels = c("SAPS",
                                  "SOFA",
                                  "Leukocytes",
                                  "CRP"),
                       labels = c("SAPS" = "SAPS (Score)",
                                  "SOFA" = "SOFA (Score)",
                                  "Leukocytes" = "Leukozyten (/nL)",
                                  "CRP" = "CRP (mg/dL)"))
    
  )

# Mediane pro Panel berechnen (für Label)
meds <- severity_long %>%
  
  #Gruppierung nach Parametern
  group_by(Parameter_label) %>%
  
  #Berechnung des Medians für den Text 
  summarise(median = median(Wert, na.rm = TRUE), .groups = "drop")


#Erstellung des Plots
ggplot(severity_long, aes(x = "", y = Wert, fill = Parameter)) +
  
  #Boxplot 
  geom_boxplot(alpha = 0.85, width = 0.6, outlier.alpha = 0.6) +
  
  #Erstellen mehrerer Grafiken
  facet_wrap(~ Parameter_label, scales = "free_y") +
  
  #Überlegen des Median Textes
  geom_text(
    data = meds,
    aes(x = 1, y = median, label = paste0("Median: ", round(median, 1))),
    inherit.aes = FALSE,
    vjust = -0.8, size = 4
  ) +
  
  #Farbauswahl 
  scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.8) +
  
  #Beschriftungen 
  labs(
    title = "Schweregradparameter zu Therapiebeginn",
    x = NULL,
    y = "Wert"
  ) +
  
  #Theme Auswahl und Einstellungen 
  theme_minimal(base_size = 14) +
  
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  )

#Speichern des Plots
ggsave(
  filename = "Boxplot_John.png",
  width = 30,
  height = 13,
  units = "cm",
  device = "png"
)
