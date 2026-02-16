
# 1. Erstelle ein leeres Environment und lade die Datei dorthin
my_data <- new.env()
load("vancomycin.RData", envir = my_data)

# 2. Schau nach, wie die Objekte in der RData-Datei eigentlich heißen
ls(envir = my_data)

# -> Sagen wir mal, der Befehl oben zeigt dir, dass das Objekt "vancomycin_df" heißt.

# 3. Speichere genau dieses Objekt als CSV ab
write.csv(my_data$vancomycin_df, "vancomycin_export.csv", row.names = FALSE)

