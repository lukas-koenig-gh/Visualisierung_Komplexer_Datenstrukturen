#Erstellung der Plots - Lukas 

load("vancomycin.RData")

#Setup/Vorbereitung 
data <- dat

library(tidyverse)
library(grid)
library(svglite)

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

#Wir erstellen unseren ggplot mit den erstellten daten
ggplot(plot.1.data, aes(x = Weight, y = LD, color = dose.class, fill = dose.class)) +
  
  #Wir zeichnen die Patienten als Punkte ein 
  geom_point(alpha = 0.9) +
  
  #Wir ziehen eine Polynomielle Regressionskurve durch
  #Die Kurve wird etwas abgeweicht damit man klar den Trend erkennen kann
  geom_smooth(span = 1.2) +
  
  #Wir spalten unsere Daten nach der vorher gennanten Dosierungsklasse auf 
  facet_wrap(~ dose.class) +
  
  #Wir bestimmen unsere Farbgebung 
  scale_color_viridis_d(option = "mako", begin = 0.3, end = 0.8) +
  scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.8) +
  
  #Wir fügen Achsenbeschriftungen hinzu
  labs(
    title ="Verteilung der Dosis abhängig vom Gewicht",
    subtitle = "Gruppiert nach Konzentration von Vancomycin in mg/L nach 24 Stunden",
    x = "Gewicht (in kg)",
    y = "Initialdosis (in mg/Kg)"
    
    #Wir entfernen die Legende 
  ) +
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
  scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.8) +
  
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
  
  geom_smooth(method = "lm", color = "#0B0405") +
  
  scale_color_viridis_d(option = "mako", begin = 0.2, end = 0.8) +
  
  labs(
    title = "Nierenfunktion und Abbau von Vancomyicin",
    subtitle = "Zusammenhang zwischen initialer Nierenfunktion und der
    Konzentration von Vancomyicin im Blut nach 24 Stunden",
    x = "Nierenfunktion (eGFR)",
    y = "Vanconycin-Konzentration nach 24h (mg/L)",
    color = "Klasse"
  ) +
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
