#Erstellung der Plots - Lukas 

load("vancomycin.RData")

#Setup/Vorbereitung 
data <- dat

library(tidyverse)
library(grid)
library(svglite)
library(lubridate) 

#Divergierende Farbpalette 
color <- c("#5B50A1", "#536872", "#40B7AD")




#Plot 1 - Scatterplot mit Polynomieller Regressionsanalyse 

plot.1.data <- data %>%
  
  #Wir nehemen uns die Spalten Gewicht,
  #Initialdosis sowie die Konzentration
  #des Antibiotikums nach  24h heraus 
  select("C24", "Weight", "LD") %>%
  
  #Wir Kategorisieren unsere Konzentrationen (Angestrebter Wert liegt bei 15-20 mg/L)
  mutate(
    dose.class = case_when(
      C24 < 15 ~ "Unterdosierte Patienten",
      C24 <= 20 ~ "Normaldosierte Patienten",
      C24 > 20  ~ "Überdosierte Patienten"
    )
  ) %>%
  
  #Wir filtern alle Patienten die keine Initialdosis bekommen haben 
  filter(LD > 0) %>%
  
  #Wir Sortieren die Klassen damit sie in der Richtigen Reinfolge im GGplot gezeigt werden 
  mutate(
    dose.class = factor(dose.class,
                        levels = c("Unterdosierte Patienten", "Normaldosierte Patienten", "Überdosierte Patienten"))
  )

dose.class.null <- plot.1.data %>%
  select(-dose.class) 

#Wir erstellen unseren ggplot mit den erstellten daten
ggplot(plot.1.data, aes(x = Weight, y = LD)) +
  
  geom_jitter(data = dose.class.null, color = "grey90" ) +
  
  #Wir zeichnen die Patienten als Punkte ein 
  geom_point(alpha = 0.9, aes(color = dose.class, fill = dose.class)) +
  
  #Wir spalten unsere Daten nach der vorher gennanten Dosierungsklasse auf 
  facet_wrap(~ dose.class) +
  
  #Wir bestimmen unsere Farbgebung 
  scale_color_manual(values = color) +
  
  #Wir fügen Achsenbeschriftungen hinzu
  labs(
    title ="Verteilung der Dosis abhängig vom Gewicht",
    subtitle = "Gruppiert nach Konzentration von Vancomycin in mg/L nach 24 Stunden",
    x = "Gewicht (in kg)",
    y = "Initialdosis (in mg/Kg)"
    
    #Wir entfernen die Legende 
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none"
  ) 

#Wir Speichern den Plot als PNG für einfache Übergabe in powerpoint 
ggsave(
  filename = "Regressionplot_Scatterplot_Lukas.svg",
  width = 30,
  height = 13,
  units = "cm",
  device = "svg"
)

plot.absolute.dosis <- plot.1.data %>%
  # DEINE IDEE: Wir berechnen die absolute Dosis
  mutate(
    Absolute_LD = LD * Weight
  )

ggplot(plot.absolute.dosis, aes(x = Weight, y = Absolute_LD, color = dose.class)) +
  
  # Wir nutzen Jitter statt Point, um Overplotting bei Standard-Dosen aufzulösen
  # height = 50 sorgt dafür, dass die Punkte auf der Y-Achse leicht (um 50mg) streuen
  geom_jitter(alpha = 0.7, size = 2.5, width = 0, height = 50) +
  
  # Wir zeichnen typische Ampullen-Größen als Referenzlinien ein
  geom_hline(yintercept = c(1000, 1500, 2000), linetype = "dashed", color = "gray50", alpha = 0.5) +
  
  scale_color_manual(values = c("Unterdosiert" = "#5B50A1", 
                                "Normaldosiert" = "#87969E", 
                                "Überdosiert" = "#40B7AD")) +
  
  labs(
    title = "Die Illusion der Präzision: Standarddosen statt Gewichts-Adaption",
    subtitle = "Die Linien bei z.B. 1000mg oder 1500mg zeigen: Ärzte dosieren oft in festen Blöcken.\nDie farbliche Durchmischung beweist, dass dies zu unvorhersehbaren Ergebnissen führt.",
    x = "Gewicht (in kg)",
    y = "Tatsächliche absolute Initialdosis (in mg)",
    color = "Resultat (C24)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

#Erstellung von Plot 2, Boxplot des Verlaufs vom Antibiotikumsspiegel

#Wir erstellen einen neuen Datensatz
plot.2.data <- data %>%
  
  #Wir wählen uns alle Spalten aus welche die Konzentration des 
  #Antibiotikums nach 24/48/72 Stunden zeigen 
  select("C24", "C48", "C72") %>%
  
  #Wir Ziehen unsere Daten Lang damit ggplot sie Korrekt darstellen kann
  pivot_longer(cols = everything(),
               names_to = "Time",
               values_to = "Werte"
  ) %>%
  
  #Wir entfernen alle Fehlenden Werte
  drop_na(Werte)%>%
  
  #Wir Sortieren wieder unsere Rangordnung damit die Boxplots Chronologisch Sinn Ergeben 
  #Außerdem Bennen wir sie um damit man weiß was sie Bedeuteten 
  mutate(Time = factor(Time,
                       levels = c("C24", "C48", "C72"),
                       labels = c("24 Stunden", "48 Stunden", "72 Stunden")))


#Wir erstellen unseren ggplot 
ggplot(plot.2.data, aes(y = Werte, x = Time, fill = Time)) +
  
  #Wir erschaffen einen Boxplot 
  geom_boxplot(alpha = 0.6, width = 0.5) +
  
  #Wir geben unsere Standard Farbpalette hinzu 
  scale_fill_manual(option = "mako", begin = 0.3, end = 0.8) +
  
  #Wir fügen Aachsenbeschriftungen hinzu
  labs(
    title = "Konzentration von Vancomycin über den Behandelungsverlauf",
    subtitle = "Beobachtung der Konzentration alle 24 Stunden",
    y = "Vancomycin Konzentration",
    x = ""
  ) +
  
  #Hier eine Themeauswahl für einen "cleaneren" look 
  theme_minimal(base_size = 12) +
  
  #Und wir entfernen wieder die Legende damit wir keine Doppelnden Informationen in der Grafik haben
  theme(
    legend.position = "none"
  )

#Wir speichern den Plot ab 
ggsave(
  filename = "Boxplot_Lukas.svg",
  width = 30,
  height = 13,
  units = "cm",
  device = "svg"
)


#Plot 3 - Zusammenhang zwischen Nierenfunktion und der Abbaurate von Vancomyicin 

#Datenaufbereitung 
plot.3.data <- data %>%
  
  #Wir nehmen uns den Startwert für die Nierenfunktion, die Konzentration 
  #nach 24h und Initialdosis
  select(eGFRStart, C24, LD) %>%
  
  #Entfernen von NAs in Numerischen Faktoren
  drop_na(eGFRStart, C24) %>%
  
  #Wir Filtern alle Patienten ohne Initialdosis
  filter(LD > 0) %>%
  
  #Wir Kategorisieren wie für Plot 1 unsere Dosierung der Patienten
  mutate(
    dose.class = case_when(
      C24 < 15 ~ "Unterdosiert",
      C24 <= 20 ~ "Normaldosiert",
      C24 > 20  ~ "Überdosiert"
    )
    ,
    #Wir geben den Klassen eine Ordnung sodass sie im Plot Intuitiv dargestellt werden 
    dose.class = factor(dose.class, 
                        levels = c("Unterdosiert",
                                   "Normaldosiert",
                                   "Überdosiert"))
  )

ggplot(plot.3.data, aes(x = eGFRStart, y = C24)) +
  geom_point(aes(color = dose.class), alpha = 0.6, size = 2) +
  
  geom_smooth(method = "lm", color = "#0B0405", se = FALSE) +
  
  #scale_color_viridis_d(option = "mako", begin = 0.2, end = 0.8) +
  scale_color_manual(values = color) +
  
  # geom_hline(yintercept = 15, linetype = "dashed", color = "#87969E", linewidth = 1) +
  # geom_hline(yintercept = 20, linetype = "dashed", color = "#87969E", linewidth = 1) +
  
  labs(
    title = "Nierenfunktion und Abbau von Vancomyicin",
    subtitle = "Zusammenhang zwischen initialer Nierenfunktion und der
    Konzentration von Vancomyicin im Blut nach 24 Stunden",
    x = "Nierenfunktion (eGFR)",
    y = "Vanconycin-Konzentration nach 24h (mg/L)",
    color = "Klasse"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top"
  )
  
#Wir speichern den Plot ab 
ggsave(
  filename = "Scatterplot_Lukas.svg",
  width = 30,
  height = 13,
  units = "cm",
  device = "svg"
)

plot.3.zeitverlauf <- data %>%
  select(C24, C48, C72) %>%
  drop_na(C24, C48, C72) %>%
  pivot_longer(
    cols = c(C24, C48, C72),
         names_to = "Zeitpunkt",
         values_to = "Konzentration") %>%
  mutate(
    dose.class = case_when(
      Konzentration < 15 ~ "Unterdosiert",
      Konzentration <= 20 ~ "Normaldosiert",
      Konzentration > 20 ~ "Überdosiert"
    ),
    dose.class = factor(dose.class, levels = c("Unterdosiert", "Normaldosiert", "Überdosiert")),
    Zeitpunkt = case_when(
      Zeitpunkt == "C24" ~ "Tag 1 (24h)",
      Zeitpunkt == "C48" ~ "Tag 2 (48h)",
      Zeitpunkt == "C72" ~ "Tag 3 (72h)"
    )
  )

ggplot(plot.3.zeitverlauf, aes(x = Zeitpunkt, fill = dose.class)) +
  geom_bar(position = "fill", width = 0.6, color = "white", linewidth = 0.5) +
  scale_fill_manual(values = c("Unterdosiert" = "#5B50A1", 
                               "Normaldosiert" = "#87969E", 
                               "Überdosiert" = "#40B7AD")) +
  scale_y_continuous(labels = scales::percent) 
  
#Plot 4 - Fertigstellung der Geschichte - Zusammenhang zwischen einer Vancomycin Überdosis und einer Nierenschädigung


# 1. Datenaufbereitung für Plot 4
plot.4.data <- data %>%
  # (Für heute generieren wir die Klasse noch einmal schnell hier, 
  # später wandert das ganz nach oben ins Skript!)
  mutate(
    dose.class = case_when(
      C24 < 15  ~ "Unterdosiert",
      C24 <= 20 ~ "Normaldosiert",
      C24 > 20  ~ "Überdosiert"
    ),
    dose.class = factor(dose.class, levels = c("Unterdosiert", "Normaldosiert", "Überdosiert"))
  ) %>%
  drop_na(dose.class) %>% # Wir betrachten nur Patienten mit bekanntem C24-Wert
  
  # Wir behalten unsere Gruppe und alle Nieren-Werte
  select(dose.class, eGFRStart, eGFR24, eGFR48, eGFR72, eGFREnd) %>%
  
  # Wide zu Long: Alle eGFR-Spalten in zwei neue Spalten packen
  pivot_longer(
    cols = starts_with("eGFR"), # Praktischer Shortcut: nimmt alle Spalten, die mit "eGFR" beginnen
    names_to = "Zeitpunkt",
    values_to = "eGFR_Wert"
  ) %>%
  
  # WICHTIG: Den Zeitpunkt als Factor sortieren, sonst ordnet R die x-Achse alphabetisch!
  mutate(
    Zeitpunkt = factor(Zeitpunkt, 
                       levels = c("eGFRStart", "eGFR24", "eGFR48", "eGFR72", "eGFREnd"),
                       labels = c("Start", "24h", "48h", "72h", "Ende"))
  ) %>%
  
  # Jetzt kommt die Magie: Gruppieren und Mittelwerte berechnen
  group_by(dose.class, Zeitpunkt) %>%
  summarise(
    mean_eGFR = mean(eGFR_Wert, na.rm = TRUE),
    # Den Standardfehler berechnen für diese schönen Fehlerbalken in der Grafik
    se_eGFR = sd(eGFR_Wert, na.rm = TRUE) / sqrt(n()), 
    .groups = "drop"
  )

# 2. Plotting: Das große Finale
ggplot(plot.4.data, aes(x = Zeitpunkt, y = mean_eGFR, color = dose.class, group = dose.class)) +
  
  # Linien und Punkte
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  
  # Fehlerbalken (Errorbars) für akademische Seriosität
  geom_errorbar(aes(ymin = mean_eGFR - se_eGFR, ymax = mean_eGFR + se_eGFR), 
                width = 0.1, linewidth = 1) +
  
  # Unsere konsistente Farbpalette
  scale_color_manual(
    values = c("Unterdosiert" = "#5B50A1", 
               "Normaldosiert" = "#87969E", 
               "Überdosiert" = "#40B7AD")
  ) +
  
  labs(
    title = "Der Preis der Überdosierung: Nierenfunktion im Zeitverlauf",
    subtitle = "Durchschnittliche eGFR (mit Standardfehler), gruppiert nach initialer Dosierung an Tag 1",
    x = "Zeitpunkt im Therapieverlauf",
    y = "Mittlere Nierenfunktion (eGFR in ml/min)",
    color = "Initiale Dosierung"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# --- 1. Datenaufbereitung ---
plot.multi.data <- data %>%
  
  # Wir berechnen das Alter des Patienten (Start-Datum minus Geburtsdatum)
  mutate(
    Start = as.POSIXct(Start),
    Birthdate = as.Date(Birthdate),
    Age = as.numeric(difftime(Start, Birthdate, units = "days")) / 365.25
  ) %>%
  
  # Wir wählen alle interessanten Faktoren + C24 und LD
  select(C24, LD, Weight, eGFRStart, Age, SAPS, SOFA) %>%
  
  # Entfernen von Zeilen mit fehlenden Werten in diesen Spalten
  drop_na() %>%
  
  # Wir Filtern alle Patienten ohne Initialdosis
  filter(LD > 0) %>%
  
  # Kategorisierung der Dosierung
  mutate(
    dose.class = case_when(
      C24 < 15  ~ "Unterdosiert",
      C24 <= 20 ~ "Normaldosiert",
      C24 > 20  ~ "Überdosiert"
    ),
    dose.class = factor(dose.class, levels = c("Unterdosiert", "Normaldosiert", "Überdosiert"))
  ) %>%
  
  # MAGIE: Wir wandeln die 5 Faktoren ins Long-Format um, damit facet_wrap sie trennen kann!
  pivot_longer(
    cols = c(Weight, eGFRStart, Age, SAPS, SOFA),
    names_to = "Faktor",
    values_to = "Wert"
  ) %>%
  
  # Wir geben den Faktoren schöne deutsche Namen für die Plot-Überschriften
  mutate(
    Faktor = case_when(
      Faktor == "eGFRStart" ~ "1. Nierenfunktion (eGFR)",
      Faktor == "Weight"    ~ "2. Gewicht (kg)",
      Faktor == "Age"       ~ "3. Alter (Jahre)",
      Faktor == "SAPS"      ~ "4. SAPS Score",
      Faktor == "SOFA"      ~ "5. SOFA Score"
    )
  )

# --- 2. Plot Erstellung ---

# Unsere etablierte Farbpalette
color <- c("Unterdosiert" = "#5B50A1", "Normaldosiert" = "#87969E", "Überdosiert" = "#40B7AD")

ggplot(plot.multi.data, aes(x = Wert, y = C24)) +
  
  # Wir zeichnen den "Sicheren Hafen" leicht im Hintergrund ein (Hilft dem Auge extrem!)
  geom_hline(yintercept = 15, linetype = "dashed", color = "gray60", linewidth = 0.8) +
  geom_hline(yintercept = 20, linetype = "dashed", color = "gray60", linewidth = 0.8) +
  
  # Punkte zeichnen
  geom_point(aes(color = dose.class), alpha = 0.5, size = 1.5) +
  
  # Die Regressionslinie zeigt uns, wie stark der jeweilige Faktor C24 beeinflusst
  geom_smooth(method = "lm", color = "#0B0405", se = FALSE, linewidth = 1) +
  
  # Das wichtigste: Aufteilen nach Faktor, JEDER bekommt seine eigene X-Achse (free_x)
  facet_wrap(~ Faktor, scales = "free_x", ncol = 3) +
  
  scale_color_manual(values = color) +
  
  labs(
    title = "Einflussfaktoren auf die Vancomycin-Konzentration",
    subtitle = "Zusammenhang zwischen Patientenmerkmalen und der C24-Konzentration (gestrichelt = Zielbereich)",
    x = "Wert des jeweiligen Faktors",
    y = "Konzentration nach 24h (mg/L)",
    color = "Dosierungs-Status"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    # Mache die Titel der einzelnen Facets etwas präsenter
    strip.text = element_text(face = "bold", size = 11, margin = margin(b = 5))
  )

# Speichern
ggsave(
  filename = "Multifaktor_Scatter_Lukas.svg",
  width = 30,
  height = 16, # Etwas höher machen wegen der zwei Reihen Facets
  units = "cm",
  device = "svg"
)

# --- Daten für Plot 4: Das Nomogramm ---
plot.4.nomogramm <- data %>%
  select(C24, LD, Weight, eGFRStart) %>%
  drop_na() %>%
  filter(LD > 0) %>%
  mutate(
    dose.class = case_when(
      C24 < 15  ~ "Unterdosiert",
      C24 <= 20 ~ "Normaldosiert",
      C24 > 20  ~ "Überdosiert"
    ),
    dose.class = factor(dose.class, levels = c("Unterdosiert", "Normaldosiert", "Überdosiert"))
  )

# --- Plot 4 Erstellung ---
ggplot(plot.4.nomogramm, aes(x = eGFRStart, y = Weight, color = dose.class)) +
  
  # Punkte (leicht transparent, damit Überlappungen sichtbar sind)
  geom_point(alpha = 0.5, size = 2) +
  
  # Topografische Linien, um die "Gravitationszentren" der 3 Gruppen zu zeigen
  geom_density_2d(aes(color = dose.class), linewidth = 1, alpha = 0.8) +
  
  # Unsere konsistente Mako-Farbpalette
  scale_color_manual(
    values = c("Unterdosiert" = "#5B50A1", 
               "Normaldosiert" = "#87969E", 
               "Überdosiert" = "#40B7AD")
  ) +
  
  labs(
    title = "Der Weg zu einer besseren Leitlinie",
    subtitle = "Zusammenspiel aus Gewicht und Nierenfunktion (Zentren markiert durch Höhenlinien)",
    x = "Initiale Nierenfunktion (eGFR in ml/min)",
    y = "Körpergewicht (in kg)",
    color = "Resultierender Dosierungs-Status nach 24h"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# Speichern
ggsave(
  filename = "Nomogramm_Zonen_Lukas.svg",
  width = 25,
  height = 15,
  units = "cm",
  device = "svg"
)

the_formula <- function(LD, Weight, eGFRStart, Age, SAPS, SOFA){
  return(63.36 + 0.0126 * LD - 0.202 * Weight - 0.219 * eGFRStart - 0.097 * Age + 0.037 * SAPS + 0.038 * SOFA)
}

test.pipline.model <- data %>%
  select(LD, Weight, eGFRStart, SAPS, SOFA, C24) %>%
  mutate(
    Age = as.numeric(difftime(Start, Birthdate, units = "days")) / 365.25
  ) %>%
  drop_na() %>%
  mutate(pred.C24 = the_formula(LD, Weight, eGFRStart, Age, SAPS, SOFA)) 
    

library(tidyverse)
library(lubridate)

# 1. Daten laden
df <- read_csv("dat.csv")

# 2. Funktion definieren (wie von dir vorgegeben)
the_formula <- function(LD, Weight, eGFRStart, Age, SAPS, SOFA){
  return(63.36 + 0.0126 * LD - 0.202 * Weight - 0.219 * eGFRStart - 0.097 * Age + 0.037 * SAPS + 0.038 * SOFA)
}

# 3. Pipeline: Alter berechnen, NAs entfernen und Vorhersage treffen
eval_df <- df %>%
  # Alter berechnen (Differenz zwischen Start und Geburtsdatum in Jahren)
  mutate(Age = as.numeric(difftime(as.Date(Start), as.Date(Birthdate), units = "days")) / 365.25) %>%
  # Nur benötigte Spalten wählen und Zeilen mit NAs (z.B. in SOFA) entfernen
  select(C24, LD, Weight, eGFRStart, Age, SAPS, SOFA) %>%
  drop_na() %>%
  # Deine Formel anwenden
  mutate(pred_vanco = the_formula(LD, Weight, eGFRStart, Age, SAPS, SOFA))

# 4. Modell-Güte berechnen
metrics <- eval_df %>%
  summarise(
    RMSE = sqrt(mean((C24 - pred_vanco)^2)),
    MAE  = mean(abs(C24 - pred_vanco)),
    R2   = cor(C24, pred_vanco)^2
  )

print(metrics)

# 5. Visualisierung: Vorhersage vs. Tatsächlicher Wert
ggplot(eval_df, aes(x = C24, y = pred_vanco)) +
  geom_point(alpha = 0.6, color = "#2c3e50") +
  geom_abline(slope = 1, intercept = 0, color = "#e74c3c", linetype = "dashed", size = 1) +
  labs(
    title = "Evaluation: Vancomycin-Modell",
    subtitle = paste("R² =", round(metrics$R2, 3), "| RMSE =", round(metrics$RMSE, 2)),
    x = "Tatsächliche Konzentration (C24) [mg/L]",
    y = "Vorhergesagte Konzentration [mg/L]"
  ) +
  theme_minimal()

  