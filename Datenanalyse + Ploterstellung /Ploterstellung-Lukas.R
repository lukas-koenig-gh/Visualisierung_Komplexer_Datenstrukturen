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
      C24 < 15 ~ "Unterdosierte Patienten",
      C24 <= 20 ~ "Normaldosierte Patienten",
      C24 > 20  ~ "Überdosierte Patienten"
    )
  ) %>%
  filter(LD > 0) %>%
  mutate(
    dose.class = factor(dose.class,
                        levels = c("Unterdosierte Patienten", "Normaldosierte Patienten", "Überdosierte Patienten"))
  )

ggplot(plot.1.data, aes(x = Weight, y = LD, color = dose.class, fill = dose.class)) +
  geom_point(alpha = 0.9) +
  geom_smooth(span = 1.2) +
  facet_wrap(~ dose.class) +
  scale_color_viridis_d(option = "mako", begin = 0.3, end = 0.8) +
  scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.8) +
  labs(
    title ="Verteilung der Dosis abhängig vom Gewicht",
    subtitle = "Gruppiert nach Konzentration von Vancomycin in mg/L nach 24 Stunden",
    x = "Gewicht (in kg)",
    y = "Initialdosis"
    
  ) +
  theme(
    legend.position = "none"
  ) 

ggsave(
  filename = "Regressionplot.pdf", # Den Namen kannst du natürlich anpassen
  width = 30,
  height = 13,
  units = "cm",
  device = "pdf"
)

#Erstellung von Plot 2 

plot.2.data <- data %>%
  select("C24", "C48", "C72") %>%
  pivot_longer(cols = everything(),
               names_to = "Time",
               values_to = "Werte"
  ) %>%
  drop_na(Werte)%>%
  mutate(Time = factor(Time,
                       levels = c("C24", "C48", "C72"),
                       labels = c("24 Stunden", "48 Stunden", "72 Stunden")))

ggplot(plot.2.data, aes(y = Werte, x = Time, fill = Time)) +
  geom_boxplot(alpha = 0.6, width = 0.5) +
  scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.8) +
  labs(
    title = "Konzentration von Vancomycin über den Behandelungsverlauf",
    subtitle = "Beobachtung der Konzentration alle 24 Stunden",
    y = "Vancomycin Konzentration",
    x = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none"
  )

ggsave(
  filename = "Boxplot.pdf", # Den Namen kannst du natürlich anpassen
  width = 30,
  height = 13,
  units = "cm",
  device = "pdf"
)

