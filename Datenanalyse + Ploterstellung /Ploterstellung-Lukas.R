#Erstellung der Plots - Lukas 

#Setup/Vorbereitung 
data <- read.csv("dat.csv")

library(tidyverse)
library(grid)

#Plot 1 - Scatterplot mit Spezieller Regressionsanalyse 

plot.1.data <- data %>%
  select("C24", "Weight", "LD") %>%
  mutate(
    dose.class = case_when(
      C24 < 15 ~ "Unterdosiert",
      C24 <= 20 ~ "Normaldosiert",
      C24 > 20  ~ "Überdosiert"
    )
  )
  
ggplot(plot.1.data, aes(x = Weight, y = LD, color = dose.class, fill = dose.class)) +
  geom_point(alpha = 0.9) +
  geom_smooth() +
  facet_wrap(~ dose.class) +
  
  scale_color_viridis_d(option = "mako", begin = 0.3, end = 0.8) +
  scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.8) +
  labs(
    title ="Verteilung der Dosis abhängig vom Gewicht",
    subtitle = "Gruppiert nach Konzentration von Vancomycin in mg/L nach 24 Stunden",
    x = "Gewicht (in kg)",
    y = "Initialdosis"
    
  )
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top"
  )

plot.2.data <- data %>%
  select(starts_with("eGFR")) 
  
  
  