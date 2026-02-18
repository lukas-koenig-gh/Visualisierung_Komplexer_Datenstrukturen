load("vancomycin.RData")
library(tidyverse)
library(plotly)

plot.1.data <- dat %>%
  
  #Wir wählen die Spalten aus die die Werte enthalten 
  select(starts_with("SCr"), starts_with("eGFR"), "Gender") %>%
  
  #Wir entfernen alle Fehlenden Werte 
  drop_na() %>%
  
  pivot_longer(
    cols = -Gender,
    names_to = c("Messwert", "Zeitpunkt"),
    names_pattern = "(SCr|eGFR)(.*)",
    values_to = "Wert",
  ) %>%
  
  mutate(Zeitpunkt = factor(Zeitpunkt, levels = c("Start", "24", "48", "72", "End")))

plot.1.summary <- plot.1.data %>%
  group_by(Gender, Messwert, Zeitpunkt) %>%
  summarise(Mittelwert = mean(Wert), .groups = "drop")

ggplot(plot.1.summary, aes(x = Zeitpunkt, y = Mittelwert, color = Gender)) +
  facet_wrap(~ Messwert, scales = "free_y") +
  geom_point(size = 3) +
  geom_line(co) +
  scale_colour_viridis_d(option = "mako", begin = 0.3, end = 0.8) +
  theme_minimal(base_size = 12) +
  labs(title = "Verlauf der Konzentration von SCr und eGFR alle 24 Stunden",
       x = "Messpunkt",
       y = "Konzentration")

#Grafik 2 - Scatterplot mit Plotly

plot.2.data <- dat %>%
  select(LD, eGFRStart, C24) %>%
  drop_na() 

#Wir erstellen unser Lineares Regressionsmodell
modell <- lm(C24 ~ LD + eGFRStart, data = plot.2.data)

x.grid <- seq

fig <- plot_ly(
  data = plot.2.data,
  x = ~LD,
  y = ~eGFRStart,
  z = ~C24,
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 3, opacity = 0.7)
)

fig

# 1. Ihr bestehendes Grundgerüst
plot.2.data <- dat %>%
  select(LD, eGFRStart, C24) %>%
  filter(LD > 0) %>%
  drop_na() 

# 2. Lineares Regressionsmodell berechnen
# Wir modellieren: C24 wird erklärt durch LD und eGFRStart
modell <- lm(C24 ~ LD + eGFRStart, data = plot.2.data)

# 3. Ein "Raster" (Grid) für die Fläche berechnen
# Wir erzeugen eine Spanne von den kleinsten bis zu den größten X- und Y-Werten
x_grid <- seq(min(plot.2.data$LD), max(plot.2.data$LD), length.out = 50)
y_grid <- seq(min(plot.2.data$eGFRStart), max(plot.2.data$eGFRStart), length.out = 50)

# 4. Vorhersagen des Modells für jeden Punkt auf diesem Raster berechnen (Z-Achse)
# outer() kombiniert jeden X-Wert mit jedem Y-Wert
z_grid <- outer(
  x_grid, 
  y_grid, 
  function(x, y) {
    predict(modell, newdata = data.frame(LD = x, eGFRStart = y))
  }
)

# 5. Den Plot mit den Punkten UND der berechneten Ebene bauen
fig <- plot_ly() %>%
  # A) Die echten Patientendaten als Punkte (wie in Ihrem Code)
  add_trace(
    data = plot.2.data,
    x = ~LD,
    y = ~eGFRStart,
    z = ~C24,
    type = "scatter3d",
    mode = "markers",
    marker = list(size = 3, opacity = 0.5, color = "blue"),
    name = "Patienten"
  ) %>%
  # B) Die berechnete Hyperebene als "surface" hinzufügen
  add_surface(
    x = ~x_grid, 
    y = ~y_grid, 
    z = ~t(z_grid), # Die Matrix muss für Plotly transponiert werden (mit t())
    opacity = 0.8,
    colorscale = "Viridis",
    name = "Regressions-Ebene"
  ) %>%
  # C) Achsenbeschriftungen anpassen
  layout(
    title = "Einfluss von Initialdosis und eGFR auf den Vancomycin-Spiegel (24h)",
    scene = list(
      xaxis = list(title = 'Initialdosis (LD) [mg/kg]'),
      yaxis = list(title = 'Nierenfunktion (eGFRStart)'),
      zaxis = list(title = 'Spiegel nach 24h (C24) [mg/L]')
    )
  )

fig

# Daten vorbereiten
cor.data <- dat %>%
  mutate(
    # Alter berechnen (Geburtsdatum bis Therapiestart)
    Age = as.numeric(difftime(Start, Birthdate, units = "weeks")) / 52.25
  ) %>%
  # Nur numerische Spalten auswählen, die uns interessieren
  select(Age, Weight, Height, SCrStart, eGFRStart, LD, C24, SAPS, SOFA) %>%
  drop_na()

# Korrelationsmatrix berechnen
cor_matrix <- cor(cor.data)

# Wir schauen uns die Zahlen kurz an (optional)
# round(cor_matrix, 2)

# Matrix ins Long-Format für ggplot umwandeln
cor.data.long <- cor_matrix %>%
  as.data.frame() %>%
  rownames_to_column(var = "Var1") %>%
  pivot_longer(cols = -Var1, names_to = "Var2", values_to = "Correlation")

# Heatmap zeichnen
ggplot(cor.data.long, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, limit = c(-1, 1)) +
  geom_text(aes(label = round(Correlation, 2)), color = "black", size = 3) +
  theme_minimal() +
  labs(title = "Korrelationsmatrix der klinischen Parameter", 
       x = "", y = "", fill = "Korrelation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


