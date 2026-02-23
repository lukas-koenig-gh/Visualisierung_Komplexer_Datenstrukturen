#################################################
# 1. SETUP & DATEN (Einheitliche Farben definieren)
#################################################
library(tidyverse)
library(viridis)
library(lubridate)
library(ggbeeswarm)

# Falls die Datei nicht im Ordner gefunden wird, nutzt du: load(file.choose())
load("vancomycin.RData")
vancomycin <- dat

# EINHEITLICHE FARBEN DEFINIEREN
# Blau-Ton aus dem Beeswarm für die Heatmap-Spitze
main_blue <- "#0072B2" 
my_colors <- c("female" = "#E69F00", "male" = main_blue)

ind_cols <- c("Sepsis", "Schock", "Bacteraemia", "Catheter", "BJI", 
              "Endocarditis", "CNS", "Gastrointestinal", "Genitourinary", 
              "Pulmonary", "SSTI")

mik_cols <- c("Culture", "Polymicrobial", "MSSA", "MRSA", "CoNS", 
              "Streptococcus", "Enterococcus", "Enterobacterales", "PA", "Fungi")

#################################################
# 2. GECLUSTERTE HEATMAP (Blau-Schema)
#################################################
df_mik_ind <- vancomycin %>%
  select(all_of(ind_cols), all_of(mik_cols)) %>%
  mutate(across(everything(), ~ as.numeric(. == "yes"))) 

co_occur <- t(as.matrix(df_mik_ind[, ind_cols])) %*% as.matrix(df_mik_ind[, mik_cols])

# Clustering berechnen
hc_ind <- hclust(dist(co_occur))
hc_bef <- hclust(dist(t(co_occur)))

co_occur_df <- as.data.frame(as.table(co_occur)) %>%
  rename(Indikation = Var1, Befund = Var2, Haeufigkeit = Freq) %>%
  mutate(Indikation = factor(Indikation, levels = rownames(co_occur)[hc_ind$order]),
         Befund = factor(Befund, levels = colnames(co_occur)[hc_bef$order]))

plot_heatmap <- ggplot(co_occur_df, aes(x = Indikation, y = Befund, fill = Haeufigkeit)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = Haeufigkeit, color = Haeufigkeit > max(Haeufigkeit)/2), 
            size = 3.5, show.legend = FALSE) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "white")) +
  # Ein schöner Blau-Verlauf passend zum Beeswarm-Blau
  scale_fill_gradient(low = "#F0F7FF", high = main_blue, name = "Fälle") +
  theme_minimal(base_size = 14) +
  labs(title = "Zusammenhang: Indikationen & Befunde",
       x = "Indikation", y = "Befund") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

#################################################
# 3. BEESWARM PLOT (DEUTSCH & OPTIMIERT)
#################################################

# Vorbereitung der Daten
df_violin <- vancomycin %>%
  mutate(Age = as.numeric(difftime(as.Date(Start), as.Date(Birthdate), units = "days")) / 365.25) %>%
  select(Gender, Age, all_of(ind_cols)) %>%
  pivot_longer(cols = all_of(ind_cols), names_to = "Indikation", values_to = "Vorhanden") %>%
  filter(Vorhanden == "yes") %>%
  drop_na(Age, Gender) %>%
  mutate(Indikation = reorder(Indikation, Age, FUN = median))

# Der Plot
plot_beeswarm <- ggplot(df_violin, aes(x = Indikation, y = Age, color = Gender)) +
  geom_quasirandom(dodge.width = 0.8, alpha = 0.6, size = 1.5) +
  geom_boxplot(aes(fill = Gender), 
               position = position_dodge(width = 0.8), 
               width = 0.25, 
               outlier.shape = NA, 
               alpha = 0.4, 
               color = "gray20") +
  # Hier werden die Farben zugewiesen und die Legende beschriftet
  scale_color_manual(
    values = my_colors,
    name = NULL, # Entfernt das Wort "Gender" über der Legende
    labels = c("female" = "Weiblich", "male" = "Männlich")
  ) +
  scale_fill_manual(
    values = my_colors,
    name = NULL, # Entfernt das Wort "Gender" für die Füllung
    labels = c("female" = "Weiblich", "male" = "Männlich")
  ) +
  theme_minimal(base_size = 14) +
  labs(title = "Altersverteilung nach Indikation",
       subtitle = "Vergleich zwischen weiblichen (orange) und männlichen (blau) Patienten",
       x = "Indikation", 
       y = "Alter (Jahre)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "top",
        legend.text = element_text(size = 12), # Schriftgröße der Legende
        panel.grid.minor = element_blank())

#################################################
# 4. ANZEIGE
#################################################
print(plot_heatmap)
print(plot_beeswarm)