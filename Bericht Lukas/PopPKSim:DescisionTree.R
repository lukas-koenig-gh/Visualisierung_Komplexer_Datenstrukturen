
#Librarys 
library(tidyverse)
library(rsample)
library(yardstick)
library(ranger)
library(mrgsolve)
library(ggbeeswarm)


#Plot 4 KI-Modelle entwickeln und Testen

#Reproduzierbarkeit gewährleisten 
set.seed(123)

#Datenaufbereitung 
model.data <- dat %>%
  drop_na() 

#Spalten in Test und Training (70/30)
model_split <- initial_split(model.data, prop = 0.7)
training.data <- training(model_split)
testing.data <- testing(model_split)


# --- 1. DAS NEUE KI-MODELL ---
# Achtung Änderung: Wir trainieren die KI jetzt darauf, die IDEALE DOSIS (LD)
# zu finden, basierend auf einer Ziel-Konzentration (C24) und den Patientendaten!
dose.model <- ranger(
  formula = C24 ~ LD + Weight + SAPS + Hypertension + Vasopressors + CKD + Sepsis,
  data = training.data,
  num.trees = 1000,
  importance = "impurity"
)

# Wir sagen der KI: "Was hättest du diesen Test-Patienten gespritzt, 
# wenn dein Ziel exakt 17.5 mg/L (die Mitte des grünen Bereichs) gewesen wäre?"
target.data <- testing.data %>% mutate(C24 = 17.5)

# Die KI spuckt uns die mg/kg Empfehlung aus und wir speichern sie im Test-Datensatz
testing.data <- testing.data %>%
  mutate(KI_LD_Empfehlung = predict(dose.model, data = target.data)$predictions)


# --- 2. DAS POP-PK MODELL (GOTI) IN C++ ---
goti.code <- "
$PROB
Goti 2018 - Vancomycin 2-Kompartment-Modell

$CMT
blood tissue 

$PARAM
TVCL = 3.5 
TVV1 = 40.0 
TVQ = 2.0 
TVV2 = 50.0 
WT = 70 
CRCL = 100 

$MAIN 
double CL = TVCL * (CRCL / 100) * pow(WT / 70, 0.75);
double V1 = TVV1 * (WT / 70);
double V2 = TVV2 * (WT / 70);
double Q  = TVQ; 

double k10 = CL / V1;
double k12 = Q / V1;
double k21 = Q / V2;

$ODE
dxdt_blood = -k10 * blood - k12 * blood + k21 * tissue; 
dxdt_tissue = k12 * blood - k21 * tissue; 

$TABLE 
// Hier war der Tippfehler: double statt douple!
double CP = blood / V1;

$CAPTURE
CP
"

# Modell kompilieren
prediction.model <- mcode("goti.predictions", goti.code)


# --- 3. DIE SIMULATION (DER FAIRE VERGLEICH) ---
testing.data <- testing.data %>% mutate(ID = row_number())
idata <- testing.data %>% select(ID, WT = Weight, CRCL = eGFRStart)

# A) Die Leitlinie gibt stur 15 mg/kg
ev_guideline <- testing.data %>%
  mutate(amt = 15 * Weight, cmt = 1, evid = 1, time = 0) %>%
  select(ID, time, amt, cmt, evid)

# B) Die KI gibt ihre maßgeschneiderte Dosis (KI_LD_Empfehlung * Gewicht)
ev_ki <- testing.data %>%
  mutate(amt = KI_LD_Empfehlung * Weight, cmt = 1, evid = 1, time = 0) %>%
  select(ID, time, amt, cmt, evid)

# Simulation A laufen lassen
sim_guideline <- prediction.model %>%
  idata_set(idata) %>% data_set(ev_guideline) %>%
  mrgsim(end = 24, delta = 24) %>% as_tibble() %>% filter(time == 24) %>%
  select(ID, C24_Guideline = CP)

# Simulation B laufen lassen
sim_ki <- prediction.model %>%
  idata_set(idata) %>% data_set(ev_ki) %>%
  mrgsim(end = 24, delta = 24) %>% as_tibble() %>% filter(time == 24) %>%
  select(ID, C24_KI = CP)


# --- 4. DATEN FÜR DEN PLOT ZUSAMMENFÜGEN ---
plot.4.data <- testing.data %>%
  left_join(sim_guideline, by = "ID") %>%
  left_join(sim_ki, by = "ID") %>%
  select(ID, C24_Guideline, C24_KI) %>%
  pivot_longer(
    cols = c(C24_Guideline, C24_KI),
    names_to = "Szenario",
    values_to = "Simulierte_Konzentration"
  ) %>%
  mutate(
    Szenario = if_else(Szenario == "C24_Guideline", 
                       "1. Starre Leitlinie (Immer 15 mg/kg)", 
                       "2. KI-Empfehlung (Ziel: 17.5 mg/L)")
  )


# --- 5. DER FINALE BEESWARM PLOT ---
ggplot(plot.4.data, aes(x = Szenario, y = Simulierte_Konzentration, color = Szenario)) +
  
  # Zielbereich (15-20 mg/L) in Grün
  annotate("rect", ymin = 15, ymax = 20, xmin = -Inf, xmax = Inf, fill = "#009E73", alpha = 0.2) +
  geom_hline(yintercept = 17.5, color = "darkgreen", linetype = "dashed", linewidth = 1) +
  
  # Boxplot + Bienenschwarm
  geom_boxplot(width = 0.2, fill = NA, color = "black", outlier.shape = NA, alpha = 0.5, linewidth = 0.5) +
  geom_quasirandom(alpha = 0.75, size = 2, width = 0.25) +
  
  coord_cartesian(ylim = c(0, 50)) +
  scale_color_brewer(palette = "Set1") +
  
  labs(
    title = "Simulierter Therapieerfolg: Leitlinie vs. KI-System",
    subtitle = "Beide Ansätze wurden durch dasselbe pharmakokinetische Modell (Goti) simuliert.\nDer grüne Bereich ist der sichere Zielkorridor (15-20 mg/L).",
    x = "",
    y = "Simulierte C24-Konzentration (mg/L)"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(face = "bold", size = 12),
    panel.grid.major.x = element_blank()
  )