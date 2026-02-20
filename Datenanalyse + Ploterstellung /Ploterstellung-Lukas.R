#Erstellung der Plots - Lukas 

#Setup/Vorbereitung 
data <- read.csv("dat.csv")

library(tidyverse)

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
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top"
  )

#Plot2 - Spagetti Line Graph 

plot.2.data <- data %>%
  select("ID" = "X", "eGFR24", "eGFR48", "eGFR72", "C24", "C48", "C72") %>%
  pivot_longer(
    cols = -ID,
    names_to = c(".value", "Time"),
    names_pattern = "([A-Za-z]+)(\\d+)"
  )

plot.2.avg.data <- plot.2.data %>%
  group_by(Time) %>%
  summarise(
    eGFR = mean(eGFR , na.rm = TRUE),
    C = mean(C, na.rm = TRUE),
    ID = "Durchschnitt"
  )

plot.2.final.data <- plot.2.data %>%
  filter(ID %in% c(7, 11)) %>%
  bind_rows(plot.2.avg.data)
  
ggplot(plot.2.final.data, aes(x = eGFR, y = C, color = ID)) +
  # 1. Die "Todeszone" markieren (Leitlinie: Toxisch über 20 mg/L)
  geom_hline(yintercept = 20, linetype = "dashed", color = "red", alpha = 0.6, linewidth = 1) +
  annotate("text", x = 140, y = 21, label = "Toxizitäts-Grenze (>20)", color = "red", vjust = -0.5) +
  
  # 2. Die Trajektorie (der Pfad) MIT Pfeilen, um die Zeitrichtung (24h -> 48h -> 72h) zu zeigen
  geom_path(arrow = arrow(length = unit(0.3, "cm"), type = "closed"), linewidth = 1.2, alpha = 0.8) +
  
  # 3. Die eigentlichen Messpunkte (als dicke Punkte auf der Linie)
  geom_point(size = 3) +
  
  # 4. Optik: Farben und Beschriftungen
  scale_color_viridis_d(option = "turbo", name = "Patienten-Verlauf") +
  labs(
    title = "Das hämodynamische Paradoxon (Phasenraum)",
    subtitle = "Dynamik der Nierenfunktion vs. Medikamentenspiegel über 72 Stunden",
    x = "Nierenfunktion (eGFR in mL/min)",
    y = "Vancomycin-Spiegel (C in mg/L)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold")
  )
  
