################################################################################
#            Grafiken erstellen
################################################################################

# Pakete laden
library(tidyverse)

# Datei laden
vancomycin <- read.csv("vancomycin.csv")

################################################################################
# Grafik 1


# Die Liste der 7 Nephrotoxine (nierenschädigende Arzneien)
nephrotoxins <- c("ACEI", "ARB", "Aminoglycosides", "Loop", "NSAID", "PipTaz", "Vasopressors")

## Datenaufbereitung
# Berechnung der Veränderung des Serumkreatinins von Start bis Ende
vancomycin$Delta_SCr <- vancomycin$SCrEnd - vancomycin$SCrStart
# Unbrauchbare Zeilen entfernen
vancomycin <- vancomycin[!is.na(vancomycin$Delta_SCr), ]

# Die Gruppe ohne Nephrotoxine extrahieren
no_nephrotoxins <- data.frame(
  Gruppe = "Keine Nephrotoxine",
  Delta_SCr = vancomycin$Delta_SCr[rowSums(vancomycin[, nephrotoxins] == "yes", na.rm = TRUE) == 0]
)

# Die 7 Nephrotoxin-Gruppen extrahieren
list_nephrotoxins <- lapply(nephrotoxins, function(nephro) {
  # Wähle nur die Zeilen, wo das jeweilige Nephrotoxin "yes" ist
  nephrotoxin_used <- vancomycin[vancomycin[[nephro]] == "yes", ]
  # Erstelle einen kleinen Dataframe für dieses Nephrotoxin
  data.frame(Gruppe = nephro, Delta_SCr = nephrotoxin_used$Delta_SCr)
})

# Die Liste von Dataframes zu einem einzigen zusammenfügen 
df_nephrotoxins <- do.call(rbind, list_nephrotoxins)

# Alles zusammenfügen und als Faktor ordnen 
df_final <- rbind(no_nephrotoxins, df_nephrotoxins)
df_final$Gruppe <- factor(df_final$Gruppe, levels = c("Keine Nephrotoxine", sort(nephrotoxins)))

## Plot erstellen (Fokus auf Data-to-Ink-Ratio und präattentive Wahrnehmung)
ggplot(df_final, aes(x = Gruppe, y = Delta_SCr, fill = Gruppe == "Keine Nephrotoxine")) +
  
  stat_boxplot(geom = "errorbar", width = 0.7, alpha = 0.7) +
  
  geom_boxplot(outlier.shape = NA, alpha = 0.7, width = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, color = "gray60") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  # Manuelle, diskrete Farben für kategoriale Daten (Blau vs. Orange)
  scale_fill_manual(values = c("TRUE" = "#0072B2", "FALSE" = "#D55E00")) +
  
  labs(
    title = "Einfluss von Nephrotoxinen auf die Nierenfunktion",
    x = NULL, # "Patientengruppe" ist selbsterklärend
    y = expression(paste(Delta, " Serumkreatinin (mg/dL)")),
    caption = "Jeder Punkt entspricht einem Patienten (Jitter-Plot). 
     Negative Werte = Verbesserung | Positive Werte = Verschlechterung"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold")
  )

################################################################################
# Grafik 2

## Datenaufbereitung
# Alle Patienten herausfiltern, bei denen der eGFR-Wert (Nierenfunktion) am Ende fehlt
mortality <- vancomycin[!is.na(vancomycin$eGFREnd), ]

# Neue Spalte erstellen: Wenn ein Sterbedatum existiert -> "Verstorben", sonst -> "Überlebt"
mortality$Verstorben <- ifelse(!is.na(mortality$Mortalitydate), "Verstorben", "Überlebt")

# Fallzahlen für die Legende ermitteln
n_ueberlebt <- sum(mortality$Verstorben == "Überlebt")
n_verstorben <- sum(mortality$Verstorben == "Verstorben")

# dynamische Labels (z.B. "Überlebt (n = 700)")
label_ueberlebt <- paste0("Überlebt (n = ", n_ueberlebt, ")")
label_verstorben <- paste0("Verstorben (n = ", n_verstorben, ")")

# Als Faktor umwandeln und die Labels zuweisen 
mortality$Verstorben <- factor(
  mortality$Verstorben, 
  levels = c("Überlebt", "Verstorben"),
  labels = c(label_ueberlebt, label_verstorben)
)

## Plot erstellen
ggplot(mortality, aes(x = eGFREnd, fill = Verstorben)) +
  
  geom_histogram(position = "identity", alpha = 0.5, color = "white", bins = 20) +
  
  # Manuelle, farbblindenfreundliche HCL-Farben (aus der Vorlesung)
  scale_fill_manual(values = setNames(
    c("#0072B2", "#D55E00"), 
    c(label_ueberlebt, label_verstorben)
  )) +
  
  # Beschriftungen
  labs(
    title = "Verteilung der Nierenfunktion am Therapieende",
    subtitle = "Histogramm der absoluten Fallzahlen",
    x = expression("eGFR"["End"] ~ "(mL/min/1.73" ~ m^2 ~ ")"),
    y = "Absolute Häufigkeit (Anzahl Patienten)", 
    fill = ""
  ) +
  theme_minimal() +
  theme(legend.position = "top") 
