PatientenV1 <- read.csv("Daten\\PatientInnendaten mit Visiten-20250423\\Patients_V1_10-03-2025.csv",header=TRUE, sep = ";")
PatientenV2 <- read.csv("Daten\\PatientInnendaten mit Visiten-20250423\\Patients_V2_10-03-2025.csv",header=TRUE, sep = ";")
PatientenV3 <- read.csv("Daten\\PatientInnendaten mit Visiten-20250423\\Patients_V3_10-03-2025.csv",header=TRUE, sep = ";")
PatientenV4 <- read.csv("Daten\\PatientInnendaten mit Visiten-20250423\\Patients_V4_10-03-2025.csv",header=TRUE, sep = ";")
PatientenV5 <- read.csv("Daten\\PatientInnendaten mit Visiten-20250423\\Patients_V5_10-03-2025.csv",header=TRUE, sep = ";")
PatientenV6 <- read.csv("Daten\\PatientInnendaten mit Visiten-20250423\\Patients_V6_10-03-2025.csv",header=TRUE, sep = ";")
PatientenV7 <- read.csv("Daten\\PatientInnendaten mit Visiten-20250423\\Patients_V7_10-03-2025.csv",header=TRUE, sep = ";")
PatientenV8 <- read.csv("Daten\\PatientInnendaten mit Visiten-20250423\\Patients_V8_10-03-2025.csv",header=TRUE, sep = ";")
PatientenV9 <- read.csv("Daten\\PatientInnendaten mit Visiten-20250423\\Patients_V9_10-03-2025.csv",header=TRUE, sep = ";")
PatientenV10 <- read.csv("Daten\\PatientInnendaten mit Visiten-20250423\\Patients_V10_10-03-2025.csv",header=TRUE, sep = ";")
PatientenV11 <- read.csv("Daten\\PatientInnendaten mit Visiten-20250423\\Patients_V11_10-03-2025.csv",header=TRUE, sep = ";")
PatientenV12 <- read.csv("Daten\\PatientInnendaten mit Visiten-20250423\\Patients_V12_10-03-2025.csv",header=TRUE, sep = ";")
PatientenV13 <- read.csv("Daten\\PatientInnendaten mit Visiten-20250423\\Patients_V13_10-03-2025.csv",header=TRUE, sep = ";")
PatientenV14 <- read.csv("Daten\\PatientInnendaten mit Visiten-20250423\\Patients_V14_10-03-2025.csv",header=TRUE, sep = ";")
PatientenV15 <- read.csv("Daten\\PatientInnendaten mit Visiten-20250423\\Patients_V15_10-03-2025.csv",header=TRUE, sep = ";")

## Ausgabe der Spalten der Patientendaten
ncol(PatientenV1)

output <- data.frame(
  Nummer = seq_along(names(enhancedPatients)),
  Spalte = names(enhancedPatients),
  Beispielwert = sapply(enhancedPatients, function(col) {
    # Umwandlung in character zur einheitlichen Prüfung
    col_values <- as.character(col)
    
    # Filtere NA, "", und "0"
    valid_values <- col_values[!is.na(col_values) & col_values != "" & col_values != "0"]
    
    if (length(valid_values) == 0) {
      return(NA)
    } else {
      return(valid_values[1])
    }
  }),
  row.names = NULL
)
write.csv(output, file = "Spaltenübersicht_mit_Beispielen.csv", row.names = FALSE)

# Patientenliste
curAllPatients <- PatientenV15

#Methode zur Ausgabe der Patientenverteilung, welche Präventivmedikation einnehmen
enhanceData <- function(patients) {
  prophylaxeMedNameColumns <- grep("preventive_name_p", names(patients), value=TRUE)
  prophylaxeAbortedMedNameColumns <- grep("preventive_name_fp", names(patients), value=TRUE)
  prophylaxeMedEffectColumns <- grep("effect_p", names(patients), value=TRUE) 
  prophylaxeMedDosageColumns <- grep("dosage_p", names(patients), value=TRUE) 
  prophylaxeMedTolerabilityColumns <- grep("tolerability_p", names(patients), value=TRUE) 

  patients[, prophylaxeMedNameColumns] <- lapply(patients[,prophylaxeMedNameColumns], as.character)
  patients[, prophylaxeAbortedMedNameColumns] <- lapply(patients[,prophylaxeAbortedMedNameColumns], as.character)
  patients[, prophylaxeMedEffectColumns] <- lapply(patients[,prophylaxeMedEffectColumns], as.numeric)
  patients[, prophylaxeMedDosageColumns] <- lapply(patients[,prophylaxeMedDosageColumns], as.numeric)
  patients[, prophylaxeMedTolerabilityColumns] <- lapply(patients[,prophylaxeMedTolerabilityColumns], as.numeric)

  patients$birthyear <- as.numeric(patients$birthyear)
  patients$age <- as.numeric(format(as.Date(patients[, grep("^date_pc_K\\d{1,2}$", names(patients))]), "%Y")) - patients$birthyear
  patients$menopause <- patients$age <55 & patients$age>45
  patients$prophylaxeMed <- patients[prophylaxeMedNameColumns[1]] != '0'
  patients$nProphylaxeMed <- rowSums(patients[,prophylaxeMedNameColumns] != '0')
  patients$abortedProphylaxeMed <- patients[prophylaxeAbortedMedNameColumns[1]] != '0'
  patients$nAbortedProphylaxeMed <- rowSums(patients[,prophylaxeAbortedMedNameColumns] != '0')
  
  # Durchschnitt der prophylaktischen Effekt-Werte (≠ 0)
  patients$avgProphylaxisEffect <- apply(patients[, prophylaxeMedEffectColumns], 1, function(x) {
    x_nonzero <- x[x != 0 & !is.na(x)]
    if (length(x_nonzero) == 0) return(NA)
    mean(x_nonzero)
  })

  # Durchschnitt der prophylaktischen Dosage-Werte (≠ 0)
  patients$avgProphylaxisDosage <- apply(patients[, prophylaxeMedDosageColumns], 1, function(x) {
    x_nonzero <- x[x != 0 & !is.na(x)]
    if (length(x_nonzero) == 0) return(NA)
    mean(x_nonzero)
  })

  # Durchschnitt der prophylaktischen Tolerability-Werte (≠ 0)
  patients$avgProphylaxisTolerability <- apply(patients[, prophylaxeMedTolerabilityColumns], 1, function(x) {
    x_nonzero <- x[x != 0 & !is.na(x)]
    if (length(x_nonzero) == 0) return(NA)
    mean(x_nonzero)
  })

  patients
}

enhancedPatients <- enhanceData(curAllPatients)

num_male_patients <- sum(enhancedPatients$gender == "männlich")
num_female_patients <- sum(enhancedPatients$gender == "weiblich")
num_menopause <- sum(enhancedPatients$menopause == TRUE)

print(paste("Gesamtanzahl Patienten in Visite:", nrow(enhancedPatients)))
print(paste("Anzahl männliche Patienten in Visite:",num_male_patients))
print(paste("Anzahl weibliche Patienten in Visite:",num_female_patients))
print(paste("Anzahl Patienten in Wechseljahren:", num_menopause))

## Visualisierung der Daten
library(ggplot2)

## Visualisierung der Altersverteilung
# binwidth 1
ggplot(enhancedPatients, aes(x = age, fill=menopause)) +
  geom_histogram(binwidth = 1, color = "white") +
  scale_fill_manual(values= c("steelblue","darkgreen"))+
  labs(title = "Altersverteilung", x = "Alter", y = "Anzahl Personen", fill="Wechseljahre") +
  theme_minimal()

# binwidth 5
ggplot(enhancedPatients, aes(x = age, fill=menopause)) +
  geom_histogram(binwidth = 5, color = "white") +
  scale_fill_manual(values= c("steelblue","darkgreen"))+
  labs(title = "Altersverteilung", x = "Alter", y = "Anzahl Personen", fill="Wechseljahre") +
  geom_text(aes(label=after_stat(count)),stat = "bin",binwidth=5, color="black",position ="stack",vjust=-0.5, size=3.5)+
  theme_minimal()

## Visualisierung der Geschlechterverteilung
  ggplot(enhancedPatients, aes(x = gender)) +
  geom_bar(stat = "count", fill = "steelblue", color = "white") +
  labs(title = "Geschlechterverteilung", x = "Geschlecht", y = "Anzahl")+
  geom_text(aes(label=after_stat(count)),stat = "count", position = position_stack(vjust = 0.5), color="black")+
  theme_minimal()

## Visualisierung der Patienten in Wechseljahren
ggplot(enhancedPatients, aes(x = menopause)) +
  geom_bar(stat = "count", fill = "steelblue", color = "white") +
  labs(title = "Anzahl Patienten in Wechseljahre", x = "Wechseljahre", y = "Anzahl")+
  geom_text(aes(label=after_stat(count)),stat = "count", position = position_stack(vjust = 0.5), color="black")+
  scale_x_discrete(labels = c("FALSE" = "Nein", "TRUE" = "Ja"))+ 
  theme_minimal()

## Visualisierung der Anzahl an Patienten in Wechseljahren nach Geschlecht
ggplot(enhancedPatients, aes(x = menopause)) +
  geom_bar(stat = "count", fill = "steelblue", color = "white") +
  facet_wrap(~ gender) +
  labs(title = "Verteilung von Wechseljahren nach Geschlecht",
       x = "Wechseljahre",
       y = "Anzahl") +
  geom_text(aes(label=after_stat(count)),stat = "count", position = position_stack(vjust = 0.5), color="black")+
  scale_x_discrete(labels = c("FALSE" = "Nein", "TRUE" = "Ja"))+ 
  theme_minimal()

## Visualisierung der Einnahme von Prophylaxemedikamenten
ggplot(enhancedPatients, aes(x = prophylaxeMed)) +
  geom_bar(stat = "count", fill = "steelblue", color = "white") +
  geom_text(aes(label=after_stat(count)),stat = "count", position = position_stack(vjust = 0.5))+
  labs(title = "Verteilung Prophylaxemedikamente", x = "Nimmt Prophylaxemedikamente", y = "Anzahl")+
  theme_minimal()

# nach Wechelsjahren
ggplot(enhancedPatients, aes(x = prophylaxeMed, fill=prophylaxeMed)) +
  geom_bar(stat = "count", color = "white") +
  scale_fill_manual(values= c("steelblue","darkgreen"))+
  facet_wrap(~menopause) +
  labs(title = "Verteilung von Prophlaxemedikamenten nach Wechseljahren",
       x = "Prophylaxemedikation",
       y = "Anzahl") +
  geom_text(aes(label=after_stat(count)),stat = "count", position = position_stack(vjust = 0.5))+
  scale_x_discrete(labels = c("TRUE" = "Ja", "FALSE" = "Nein"))+
  theme_minimal()

# nach Geschlecht
ggplot(enhancedPatients, aes(x = prophylaxeMed, fill=prophylaxeMed)) +
  geom_bar(stat = "count", color = "white") +
  scale_fill_manual(values= c("steelblue","darkgreen"))+
  facet_wrap(~gender) +
  labs(title = "Verteilung von Prophlaxemedikamenten nach Geschlecht",
       x = "Prophylaxemedikation",
       y = "Anzahl") +
  geom_text(aes(label=after_stat(count)),stat = "count", position = position_stack(vjust = 0.5))+
  scale_x_discrete(labels = c("TRUE" = "Ja", "FALSE" = "Nein"))+
  theme_minimal()

## Visualisierung der Verteilung der Anzahl an eingenommenen Prophylaxe Medikamente
ggplot(enhancedPatients, aes(x = nProphylaxeMed)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  geom_text(aes(label=after_stat(count)),stat = "count", position = position_stack(vjust = 0.5),angle=90)+
  labs(title = "Verteilung Anzahl Prophylaxemedikamente", x = "Anzahl Prophylaxe Medikamente", y = "Anzahl Personen") +
  theme_minimal()

# Nach Wechseljahren %
ggplot(enhancedPatients, aes(x=nProphylaxeMed)) +
  facet_wrap(~menopause)+
  geom_histogram(aes(y=after_stat(density)),binwidth = 1, fill = "steelblue", color = "white") +
  labs(title = "Verteilung Anzahl Prophylaxemedikamente", x = "Anzahl Prophylaxe Medikamente", y = "Prozent der Patienten") +
  theme_minimal()

# Nach Wechseljahren absolut
ggplot(enhancedPatients, aes(x=nProphylaxeMed)) +
  facet_wrap(~menopause)+
  geom_histogram(aes(y=after_stat(count)),binwidth = 1, fill = "steelblue", color = "white") +
  labs(title = "Verteilung Anzahl Prophylaxemedikamente", x = "Anzahl Prophylaxe Medikamente", y = "Prozent der Patienten") +
  geom_text(aes(label=after_stat(count)),stat = "count",position = position_stack(vjust = 0.5),size=3.0, angle=90)+
  theme_minimal()

# Nach Geschlecht %
ggplot(enhancedPatients, aes(x=nProphylaxeMed)) +
  facet_wrap(~gender)+
  geom_histogram(aes(y=after_stat(density)),binwidth = 1, fill = "steelblue", color = "white") +
  labs(title = "Verteilung Anzahl Prophylaxemedikamente", x = "Anzahl Prophylaxe Medikamente", y = "Prozent der Patienten") +
  theme_minimal()

# Nach Geschlecht absolut
ggplot(enhancedPatients, aes(x=nProphylaxeMed),) +
  facet_wrap(~gender)+
  geom_histogram(aes(y=after_stat(count)),binwidth = 1, fill = "steelblue", color = "white") +
  labs(title = "Verteilung Anzahl Prophylaxemedikamente", x = "Anzahl Prophylaxe Medikamente", y = "Prozent der Patienten") +
  geom_text(aes(label=after_stat(count)),stat = "count",position = position_stack(vjust = 0.5),size=3.0)+
  theme_minimal()

##In 10er Altersgruppen aufteilen
enhancedPatients$ageGroup <- cut(
  enhancedPatients$age,
  breaks = seq(0, max(enhancedPatients$age, na.rm = TRUE) + 10, by = 10),
  right = FALSE,
  include.lowest = TRUE,
  labels = paste(seq(0, max(enhancedPatients$age, na.rm = TRUE), by = 10),
                 seq(9, max(enhancedPatients$age, na.rm = TRUE) + 9, by = 10),
                 sep = "-")
)

## Visualisierung der Prophylaxe Effekt
# Durschnitts Phrophylaxe Effekt pro 10 Jahre Altersgruppe
library(dplyr)

avgEffectByAgeGroup <- enhancedPatients %>%
  group_by(ageGroup) %>%
  summarise(
    meanEffect = mean(avgProphylaxisEffect, na.rm = TRUE),
    count = n()
  )

ggplot(avgEffectByAgeGroup, aes(x = ageGroup, y = meanEffect)) +
  geom_col(fill = "darkgreen") +
  geom_text(aes(label = round(meanEffect, 2)), vjust = -0.5) +
  labs(title = "Durchschnittlicher Prophylaxe-Effekt pro Altersgruppe",
       x = "Altersgruppe (Jahre)",
       y = "Ø Prophylaxe-Effekt") +
  theme_minimal()

# Durchschnittliche Dosierung pro Wechseljahres-Gruppe berechnen
avgEffectByMenopause <- enhancedPatients %>%
  group_by(menopause) %>%
  summarise(
    meanEffect = mean(avgProphylaxisEffect, na.rm = TRUE),
    count = n()
  )

ggplot(avgEffectByMenopause, aes(x = as.factor(menopause), y = meanEffect)) +
  geom_col(fill = "darkgreen") +
  geom_text(aes(label = meanEffect), vjust = -0.5) +
  labs(title = "Durchschnittlicher Prophylaxe-Effekt nach Wechseljahresstatus",
       x = "Wechseljahre (FALSE = Nein, TRUE = Ja)",
       y = "Ø Prophylaxe-Effekt") +
  theme_minimal()

# Durschnitts Phrophylaxe Effekt pro Alter
library(dplyr)

avgEffectByAge <- enhancedPatients %>%
  group_by(age) %>%
  summarise(
    meanEffect = mean(avgProphylaxisEffect, na.rm = TRUE),
    count = n()
  )

ggplot(avgEffectByAge, aes(x = age, y = meanEffect)) +
  geom_col(fill = "darkgreen") +
  geom_text(aes(label = round(meanEffect, 2)), vjust = -0.5) +
  labs(title = "Durchschnittliche Prophylaxe-Effekt pro Alter",
       x = "Alter (Jahre)",
       y = "Ø Prophylaxe-Effekt") +
  theme_minimal()

## Visualisierung der Prophylaxe Dosierung
# Durschnitts Phrophylaxe Dosierung pro 10 Jahre Altersgruppe
library(dplyr)

avgDosageByAgeGroup <- enhancedPatients %>%
  group_by(ageGroup) %>%
  summarise(
    meanDosage = mean(avgProphylaxisDosage, na.rm = TRUE),
    count = n()
  )

ggplot(avgDosageByAgeGroup, aes(x = ageGroup, y = meanDosage)) +
  geom_col(fill = "darkgreen") +
  geom_text(aes(label = round(meanDosage, 2)), vjust = -0.5) +
  labs(title = "Durchschnittliche Prophylaxe-Dosierung pro Altersgruppe",
       x = "Altersgruppe (Jahre)",
       y = "Ø Prophylaxe-Dosierung") +
  theme_minimal()

# Durchschnittliche Dosierung pro Wechseljahres-Gruppe berechnen
avgDosageByMenopause <- enhancedPatients %>%
  group_by(menopause) %>%
  summarise(
    meanDosage = mean(avgProphylaxisDosage, na.rm = TRUE),
    count = n()
  )

ggplot(avgDosageByMenopause, aes(x = as.factor(menopause), y = meanDosage)) +
  geom_col(fill = "darkgreen") +
  geom_text(aes(label = meanDosage), vjust = -0.5) +
  labs(title = "Durchschnittliche Dosierung nach Wechseljahresstatus",
       x = "Wechseljahre (FALSE = Nein, TRUE = Ja)",
       y = "Ø Prophylaxe-Dosierung") +
  theme_minimal()

# Durschnitts Phrophylaxe Dosierung pro Alter
library(dplyr)

avgDosageByAge <- enhancedPatients %>%
  group_by(age) %>%
  summarise(
    meanDosage = mean(avgProphylaxisDosage, na.rm = TRUE),
    count = n()
  )

ggplot(avgDosageByAge, aes(x = age, y = meanDosage)) +
  geom_col(fill = "darkgreen") +
  geom_text(aes(label = round(meanDosage, 0)), vjust = -0.5) +
  labs(title = "Durchschnittliche Prophylaxe-Dosierung pro Alter",
       x = "Alter (Jahre)",
       y = "Ø Prophylaxe-Dosierung") +
  theme_minimal()

## Visualisierung der Prophylaxe Tolerability
# Durschnitts Prophylaxe Tolerability pro 10 Jahre Altersgruppe
library(dplyr)

avgTolerabilityByAgeGroup <- enhancedPatients %>%
  group_by(ageGroup) %>%
  summarise(
    meanTolerability = mean(avgProphylaxisTolerability, na.rm = TRUE),
    count = n()
  )

ggplot(avgTolerabilityByAgeGroup, aes(x = ageGroup, y = meanTolerability)) +
  geom_col(fill = "darkgreen") +
  geom_text(aes(label = round(meanTolerability, 2)), vjust = -0.5) +
  labs(title = "Durchschnittlicher Prophylaxe-Tolerability-Wert pro Altersgruppe",
       x = "Altersgruppe (Jahre)",
       y = "Ø Prophylaxe-Tolerability") +
  theme_minimal()

# Durchschnittliche Tolerability pro Wechseljahres-Gruppe berechnen
avgTolerabilityByMenopause <- enhancedPatients %>%
  group_by(menopause) %>%
  summarise(
    meanTolerability = mean(avgProphylaxisTolerability, na.rm = TRUE),
    count = n()
  )

ggplot(avgTolerabilityByMenopause, aes(x = as.factor(menopause), y = meanTolerability)) +
  geom_col(fill = "darkgreen") +
  geom_text(aes(label = meanTolerability), vjust = -0.5) +
  labs(title = "Durchschnittliche Tolerability nach Wechseljahresstatus",
       x = "Wechseljahre (FALSE = Nein, TRUE = Ja)",
       y = "Ø Prophylaxe-Tolerability") +
  theme_minimal()

# Durschnitts Phrophylaxe Tolerability pro Alter
library(dplyr)

avgTolerabilityByAge <- enhancedPatients %>%
  group_by(age) %>%
  summarise(
    meanTolerability = mean(avgProphylaxisEffect, na.rm = TRUE),
    count = n()
  )

ggplot(avgTolerabilityByAge, aes(x = age, y = meanTolerability)) +
  geom_col(fill = "darkgreen") +
  geom_text(aes(label = round(meanTolerability, 1)), vjust = -0.5) +
  labs(title = "Durchschnittliche Prophylaxe-Tolerability pro Alter",
       x = "Alter (Jahre)",
       y = "Ø Prophylaxe-Tolerability") +
  theme_minimal()

## Visualisierung ob Prophylaxemedikamenten abgesetzt
ggplot(enhancedPatients, aes(x = abortedProphylaxeMed)) +
  geom_bar(stat = "count", fill = "steelblue", color = "white") +
  geom_text(aes(label=after_stat(count)),stat = "count", position = position_stack(vjust = 0.5))+
  labs(title = "Verteilung abgesetzter Prophylaxemedikamente", x = "Prophylaxemedikamente abgesetzt", y = "Anzahl")+
  theme_minimal()

# nach Wechelsjahren
ggplot(enhancedPatients, aes(x = abortedProphylaxeMed, fill=abortedProphylaxeMed)) +
  geom_bar(stat = "count", color = "white") +
  scale_fill_manual(values= c("steelblue","darkgreen"))+
  facet_wrap(~menopause) +
  labs(title = "Verteilung von abgesetzten Prophlaxemedikamenten nach Wechseljahren",
       x = "Prophylaxemedikation abgesetzt",
       y = "Anzahl") +
  geom_text(aes(label=after_stat(count)),stat = "count", position = position_stack(vjust = 0.5))+
  scale_x_discrete(labels = c("TRUE" = "Ja", "FALSE" = "Nein"))+
  theme_minimal()

# nach Geschlecht
ggplot(enhancedPatients, aes(x = abortedProphylaxeMed, fill=abortedProphylaxeMed)) +
  geom_bar(stat = "count", color = "white") +
  scale_fill_manual(values= c("steelblue","darkgreen"))+
  facet_wrap(~gender) +
  labs(title = "Verteilung von abgesetzten Prophlaxemedikamenten nach Geschlecht",
       x = "Prophylaxemedikation abgesetzt",
       y = "Anzahl") +
  geom_text(aes(label=after_stat(count)),stat = "count", position = position_stack(vjust = 0.5))+
  scale_x_discrete(labels = c("TRUE" = "Ja", "FALSE" = "Nein"))+
  theme_minimal()

## Visualisierung der Verteilung der Anzahl an abgesetzten Prophylaxe Medikamente
ggplot(enhancedPatients, aes(x = nAbortedProphylaxeMed)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  geom_text(aes(label=after_stat(count)),stat = "count", position = position_stack(vjust = 0.5),angle=90)+
  labs(title = "Verteilung Anzahl abgesetzter Prophylaxemedikamente", x = "Anzahl abgesetzter Prophylaxe Medikamente", y = "Anzahl Personen") +
  theme_minimal()

# Nach Wechseljahren %
ggplot(enhancedPatients, aes(x=nAbortedProphylaxeMed)) +
  facet_wrap(~menopause)+
  geom_histogram(aes(y=after_stat(density)),binwidth = 1, fill = "steelblue", color = "white") +
  labs(title = "Verteilung Anzahl abgesetzter Prophylaxemedikamente nach Wechseljahren", x = "Anzahl abgesetzter Prophylaxe Medikamente", y = "Prozent der Patienten") +
  theme_minimal()

# Nach Wechseljahren absolut
ggplot(enhancedPatients, aes(x=nAbortedProphylaxeMed)) +
  facet_wrap(~menopause)+
  geom_histogram(aes(y=after_stat(count)),binwidth = 1, fill = "steelblue", color = "white") +
  labs(title = "Verteilung Anzahl abgesetzter Prophylaxemedikamente nach Wecheljahren", x = "Anzahl abgesetzter Prophylaxe Medikamente", y = "Prozent der Patienten") +
  geom_text(aes(label=after_stat(count)),stat = "count",position = position_stack(vjust = 0.5),size=3.0, angle=90)+
  theme_minimal()

# Nach Geschlecht %
ggplot(enhancedPatients, aes(x=nAbortedProphylaxeMed)) +
  facet_wrap(~gender)+
  geom_histogram(aes(y=after_stat(density)),binwidth = 1, fill = "steelblue", color = "white") +
  labs(title = "Verteilung Anzahl abgesetzter Prophylaxemedikamente nach Geschlecht", x = "Anzahl abgesetzter Prophylaxe Medikamente", y = "Prozent der Patienten") +
  theme_minimal()

# Nach Geschlecht absolut
ggplot(enhancedPatients, aes(x=nAbortedProphylaxeMed),) +
  facet_wrap(~gender)+
  geom_histogram(aes(y=after_stat(count)),binwidth = 1, fill = "steelblue", color = "white") +
  labs(title = "Verteilung Anzahl abgesetzter Prophylaxemedikamente nach Geschlecht", x = "Anzahl abgesetzter Prophylaxe Medikamente", y = "Prozent der Patienten") +
  geom_text(aes(label=after_stat(count)),stat = "count",position = position_stack(vjust = 0.5),size=3.0)+
  theme_minimal()