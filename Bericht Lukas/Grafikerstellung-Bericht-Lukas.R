#Erstellung der Grafiken für den Bericht - Visualisierung Komplexer Datenstrukturen - Lukas König

#Setup 
load("vancomycin.Rdata")

#Librarys 
library(tidyverse)
library(rpart)
library(rpart.plot)

#Farbpalette
color <- c("#0072B2", "grey40", "#D55E00")

#Um Codewiederholungen zu vermeiden 
dat <- dat %>%
  
  #Wir klassifizieren die Dosierungsklassen nach der 2009 Leitlinie
  mutate(
    dose.class = case_when(
      C24 > 20 ~ "Überdosiert",
      C24 >= 15 ~ "Normaldosiert",
      C24 < 15 ~ "Unterdosiert"
    ),
    #Wir factorn das ganze damit es nicht Alphabetisch sortiert wird 
    dose.class = factor(dose.class,
                        levels = c("Unterdosiert",
                                   "Normaldosiert",
                                   "Überdosiert"))
    )

#Signifikanztests 

#Wir filtern die untersuchenden Faktoren raus um zu schauen wie signifikant diese sind
sig.test <- dat %>%
  filter(LD > 0) %>%
  drop_na(LD, Weight, C24, eGFRStart, SAPS, CKD, Vasopressors, Hypertension, Sepsis)

#Wir bauen ein Modell
model.sig <- lm(C24 ~ Weight + eGFRStart + Weight + LD + CKD + Vasopressors + Hypertension + Sepsis, data = sig.test)

#Wir geben das Modell aus 
summary(model.sig)

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
  
  #Wir zeichnen die Leitlinie ein damit man sehen kann wie weit die Ärzte abweichen
  geom_hline(yintercept = 15, linetype = "dashed", color = "black", linewidth = 0.8) +
  
  #Wir spalten unsere Daten nach der vorher gennanten Dosierungsklasse auf 
  facet_wrap(~ dose.class) +
  
  #Wir fügen unsere Farbpalette hinzu
  scale_color_manual(values = color) +
  
  #Wir fügen Achsenbeschriftungen hinzu
  labs(
    subtitle = "Die schwarze Linie makiert die offizielle Leitline zur Dosierung - (15 mg/kg)",
    x = "Gewicht (in kg)",
    y = "Initialdosis (in mg/Kg)"
    
    #Wir entfernen die Legende und sorgen dafür das die Grafiken abstand zueinander haben
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.spacing = unit(1.5, "lines"),
    strip.background = element_rect(fill = "white")
  ) 

#Wir Speichern den Plot als SVG für einfache Übergabe in Latex
ggsave(
  filename = "plot1.pdf",
  width = 16,
  height = 10.6,
  units = "cm",
  device = "pdf"
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
  
  #Wir nennen die Werte um damit sie im Plot verständlicher sind 
  rename(
    "Nierenfunktion (eGFR in ml/min)" = eGFRStart,
    "Initialdosis (LD in mg/kg)" = LD,
    "Schweregrad der Erkrankung (SAPS Score)" = SAPS,
    "Körpergewicht (in kg)" = Weight 
  ) %>%
  
  #Wir gruppieren die neuen werte und ziehen sie lang
  pivot_longer(
    cols = -c(C24, dose.class),
    names_to = "param",
    values_to = "vals"
  )

#Datenvisualisierung 
ggplot(plot.2.data, aes(x = vals, y = C24)) +
  
  #Erst legen wir unseren Scatterplot
  geom_point(aes(color = dose.class), alpha = 0.6, size = 2) +
  
  #Dann unsere Lineare Regressionsgrade
  geom_smooth(method = "lm", color = "#0B0405", se = FALSE) +
  
  #Dann trennen wir die Bilder voneinander
  facet_wrap(~ param, ncol = 2, scales = "free_x") +
  
  #Fügen unsere Klassische Farbpalette hinzu
  scale_color_manual(values = color) +
  
  #Anpssung des Themes
  theme_minimal(base_size = 12) +
  
  #Wir fügen Aachsenbeschriftungen hinzu
  labs(
    x = "",
    y = "Vancomycin Talspiegel nach 24h in mg/L",
    color = "Dosierungsklasse"
  ) +
  
  #Wir passen die Position der Legende an so das wir den Platz möglichst Optimal nutzen
  theme(
    legend.position = "bottom"
  )

#Wir Speichern den Plot als SVG für einfache Übergabe in Latex
ggsave(
  filename = "plot2.pdf",
  width = 18,
  height = 15,
  units = "cm",
  device = "pdf"
)

#Plot3 - Kategorische Risikofaktoren und Dosierungsreinlfluss 

#Datenaufbereitung 

plot.3.data <- dat %>%
  
  #Wir wählen unsere Relevanten Spalten aus 
  select(dose.class, C24, LD, Hypertension, CKD, Sepsis, Vasopressors) %>%
  
  #Wir entfernen unsere Fehlenden Werte
  drop_na() %>%
  
  #Dann Enternen wir noch Patienten deren Initialdosis nicht Dokumentiert ist
  #bzw. nicht verschrieben wurde 
  filter(LD > 0) %>%
  
  #Wir grupppieren die Spalten und ziehen sie lang für Facet_wrap
  pivot_longer(
    cols = c(Hypertension, CKD, Sepsis, Vasopressors),
    names_to = "Risikofaktoren",
    values_to = "Status"
  ) %>%
  
  #Wir recodieren die Binären Variablen damit es im Plot besser aussieht 
  mutate(Status = ifelse(Status == "yes", "Ja", "Nein")) %>%
  
  #Wir gruppieren das ganze damit wir später die Prozente hinzufügen können
  group_by(Risikofaktoren, Status, dose.class) %>%
  
  #Wir holen uns die n jeder gruppe
  summarise(n = n(), .groups = "drop") %>%
  
  #Wir gruppieren nach Ja Nein und Binärgruppen
  group_by(Risikofaktoren, Status) %>%
  
  #Wir berechnen den Prozentsatz 
  mutate(prozent = n / sum(n))

#Datenvisualisierung 
ggplot(plot.3.data, aes(x = Status, y = prozent, fill = dose.class)) +
  
  #Wir fügen unseren colplot hinzu
  geom_col(position = "fill", alpha = 0.8, width = 0.6) +
  
  #Text Overlay für die Prozentualen Anteile
  geom_text(aes(label = scales::percent(prozent, accuracy = 1)),
            position = position_fill(vjust = 0.5),
            color = "white", size = 4) +

  #Wir gruppieren die Plots wieder
  facet_wrap(~ Risikofaktoren) +
  
  #Wir packen auf unsere Y Achse Prozente
  scale_y_continuous(labels = scales::percent_format()) +
  
  #Wir fügen unsere Farbpalette hinzu
  scale_fill_manual(values = color) +
  
  #Wir fügen Aachsenbeschriftungen hinzu
  labs(
    x = "",
    y = "Anteil der Patienten (%)",
    fill = "Dosierungsklasse"
  ) +
  
  #Wir passen das Theme an 
  theme_minimal(base_size = 12) +
  
  #wir entfernen gridlines und passen die Postition der Legende an 
  theme(
    legend.position = "right",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )


#Wir Speichern den Plot als SVG für einfache Übergabe in Latex
ggsave(
  filename = "plot3.pdf",
  width = 16,
  height = 16,
  units = "cm",
  device = "pdf"
)

#Plot 4 - Komplexität der Situation darstellen 

#Wir bauen ein Baummodell zur vorhersage
tree.model <- rpart(
  dose.class ~ Weight + LD + SAPS + Hypertension + Vasopressors + CKD + Sepsis + eGFRStart,
  data = dat %>% drop_na() %>% filter(LD > 0),
  method = "class",
  control = rpart.control(cp = 0.02, maxdepth = 4)
)

#Wir bauen einen pdf Befehl zum Speichern des Plots
pdf("plot4.pdf", width = 6.29, height = 6)

rpart.plot(
  tree.model,
  type = 4,
  extra = 104,
  under = TRUE,
  faclen = 0,
  tweak = 1,
  compress = TRUE, 
  ycompress = TRUE,
  box.palette = as.list(color)
)

dev.off()