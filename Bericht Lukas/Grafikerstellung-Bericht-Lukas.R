#Erstellung der Grafiken für den Bericht - Visualisierung Komplexer Datenstrukturen - Lukas König

#Setup 
load("vancomycin.Rdata")

#Librarys 
library(tidyverse)
library(rsample)

#Plot 1 - Marginal Plot 

#Datenaufbereitung Plot 1 

plot.1.data <- dat %>%
  
  #Wir wählen unsere Notwendigen Spalten aus 
  select(Weight, C24, LD) %>%
  
  #Wir Entfernen unnötige Werte 
  drop_na() %>%
  
  #Wir Filtern Patienten ohne eine Initialdosis Vancomyicin 
  filter(LD > 0) %>%
  
  #Wir Kategorisieren die Patienten anhand des C24 Werts auf unter/überdosierung
  mutate(
    dose.class = case_when(
      C24 > 20 ~ "Überdosiert",
      C24 >= 15 ~ "Normaldosiert",
      C24 < 15 ~ "Unterdosiert"
    ),
    
    #Wir refactorn unsere Dosierungsklassen damit sie nicht Alphabetisch Sortiert sind
    dose.class = factor(dose.class, levels = c("Unterdosiert", "Normaldosiert", "Überdosiert")),
    Absolute_LD = LD * Weight
  )

#Wir hinterlegen später Jitter da wir ja mit Facet-wrap die Dossierungklassen trennen
#Allerding wollen wir trotzdem zeigen wie ähnlich sich die Gruppen sind #


dose.class.null <- plot.1.data %>%
  select(-dose.class) 
#Datenvisualisierung Plot 2 


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



#Plot 2 - Zusammenhang zwischen Nierenfunktion und der Abbaurate von Vancomyicin 

#Datenaufbereitung 
plot.2.data <- data %>%
  
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

ggplot(plot.2.data, aes(x = eGFRStart, y = C24)) +
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

#Plot 3/4 wLineare Modelle entwickeln und Testen

#Wir werden 2 Modelle Testen eins was die auf Nierenfunktion und Gewicht basiert 
#sowie eins was nur auf Gewicht basiert 

#Datenaufbereitung 

model.data <- dat %>%
  
  #Wir nehmen uns die Spalten auf denen die Modelle Trainiert und getestet werden sollen 
  select(Weight, eGFRStart, LD, C24) %>%
  
  #Wir entfernen alle Fehlenden Werte
  drop_na() %>%
  
  #wir spalten unsere Testdaten 70/30 damit wir das Modell nicht overfitten auf die Testdaten 
  #Und Akkurate Resultate bekommen 
  initial_split(prop = 0.7)

#Wir holen uns aus dem Split Objekt unsere Datensätze heraus 
training.data <- training(model.data)
testing.data <- testing(model.data)

