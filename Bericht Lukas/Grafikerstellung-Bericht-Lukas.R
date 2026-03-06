#Erstellung der Grafiken für den Bericht - Visualisierung Komplexer Datenstrukturen - Lukas König

#Setup 
load("vancomycin.Rdata")

#Librarys 
library(tidyverse)
library(rsample)
library(yardstick)
library(ranger)
library(mrgsolve)

#Um Codewiederholungen zu vermeiden 
dat <- dat %>%
  mutate(
    dose.class = case_when(
      C24 > 20 ~ "Überdosiert",
      C24 >= 15 ~ "Normaldosiert",
      C24 < 15 ~ "Unterdosiert"
    ),
    dose.class = factor(dose.class,
                        levels = c("Unterdosiert",
                                   "Normaldosiert",
                                   "Überdosiert"))
    )

#Plot 1 - Marginal Plot 

#Datenaufbereitung Plot 1 

plot.1.data <- dat %>%
  
  #Wir wählen unsere Notwendigen Spalten aus 
  select(dose.class, Weight, C24, LD) %>%
  
  #Wir Entfernen unnötige Werte 
  drop_na() %>%
  
  #Wir Filtern Patienten ohne eine Initialdosis Vancomyicin 
  filter(LD > 0)
  

#Wir hinterlegen später Jitter da wir ja mit Facet-wrap die Dossierungklassen trennen
#Allerding wollen wir trotzdem zeigen wie ähnlich sich die Gruppen sind #
dose.class.null <- plot.1.data %>%
  select(-dose.class) 


#Datenvisualisierung Plot 1

#Wir erstellen unseren ggplot mit den erstellten daten
ggplot(plot.1.data, aes(x = Weight, y = LD)) +
  
  geom_jitter(data = dose.class.null, color = "grey90" ) +
  
  #Wir zeichnen die Patienten als Punkte ein 
  geom_point(alpha = 0.9, aes(color = dose.class, fill = dose.class)) +
  
  #Wir spalten unsere Daten nach der vorher gennanten Dosierungsklasse auf 
  facet_wrap(~ dose.class) +
  
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


#Plot 2 - Zusammenhang zwischen Nierenfunktion/Gewicht/SAPS und anderen Faktoren und der Abbaurate von Vancomyicin 

#Datenaufbereitung 
plot.2.data <- dat %>%
  
  #Wir nehmen uns den Startwert für die Nierenfunktion, die Konzentration 
  #nach 24h und Initialdosis
  select(eGFRStart, C24, LD, SAPS, Weight) %>%
  
  #Entfernen von NAs in Numerischen Faktoren
  drop_na() %>%
  
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
  ) %>% 
  
  pivot_longer(
    cols = c(eGFRStart, LD, SAPS, Weight),
    names_to = "param",
    values_to = "vals"
  )

ggplot(plot.2.data, aes(x = vals, y = C24)) +
  geom_point(aes(color = dose.class), alpha = 0.6, size = 2) +
  
  geom_smooth(method = "lm", color = "#0B0405", se = FALSE) +
  
  facet_wrap(~ param) +

  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top"
  )

#Plot3 - Kategorische Risikofaktoren und Dosierungsreinlfluss 

#Datenaufbereitung 

plot.3.data <- dat %>%
  select(dose.class, C24, LD, Hypertension, CKD, Sepsis, Vasopressors) %>%
  drop_na() %>%
  filter(LD > 0) %>%
  pivot_longer(
    cols = c(Hypertension, CKD, Sepsis, Vasopressors),
    names_to = "Risikofaktoren",
    values_to = "Status"
  )

ggplot(plot.3.data, aes(x = Status, fill = dose.class)) +
  geom_bar(position = "fill", alpha = 0.85, width = 0.6) +
  facet_wrap(~ Risikofaktoren) +
  scale_y_continuous(labels = scales::percent_format())


#Plot 4 KI-Modelle entwickeln und Testen

#Reproduzierbarkeit gewährleisten 
set.seed(123)

#Datenaufbereitung 

model.data <- dat %>%
  
  #Wir entfernen alle Fehlenden Werte
  drop_na() %>%
  
  #wir spalten unsere Testdaten 70/30 damit wir das Modell nicht overfitten auf die Testdaten 
  #Und Akkurate Resultate bekommen 
  initial_split(prop = 0.7)

#Wir holen uns aus dem Split Objekt unsere Datensätze heraus 
training.data <- training(model.data)
testing.data <- testing(model.data)

#Basierend auf den Daten erschaffen wir ein Modell mit Realistischen Parametern 
#die schnell Erhoben werden können
final.model <- ranger(
  formula = C24 ~ Weight + LD + SAPS + Hypertension + Vasopressors + CKD + Sepsis,
  data = training.data,
  num.trees = 1000,
  importance = "impurity"
)

#Wir erstellen Code mittles mrgsolve welcher die Vancomycin Konzentration präzise 
#vorhersagt

goti_code <- "
$PROB
Goti 2018 - Vancomycin 2-Kompartment-Modell

$CMT
blood tissue 

$PARAM
//Typische Populationswerte welche wir brauchen werdne 
TVCL = 3.5 //Typische Clearance (L/h)
TVV1 = 40.0 // TYpisches Volumen Blut (L)
TVQ = 2.0 //Typische interkompartimentelle Clearance (L/h)
TVV2 = 50.0 // Typisches Volumen Gewebe (L)

//Patienten-Kovariaten
WT = 70 //Gewicht in Kg
CRCL = 100 //Kreatininclearance in ml/min

$MAIN 


"
