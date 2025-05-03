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

# Patientenliste
curAllPatients <- PatientenV1

#Methode zur Ausgabe der Patientenverteilung, welche Präventivmedikation einnehmen
enhanceData <- function(patients) {
  
  # Nur patienten mir Präventivmedikation
  spalten <- grep("preventive_name_", names(patients), value=TRUE)
  patients[, spalten] <- lapply(patients[,spalten], as.character)
  
  patients$birthyear <- as.numeric(patients$birthyear)
  patients$alter <- 2025 - patients$birthyear
  patients$wechseljahre <- patients$alter <55 & patients$alter>45
  patients$prophylaxeMed <- patients[spalten[1]] != '0'
  patients$nProphylaxeMed <- rowSums(patients[,spalten] != '0')
  
  patients
}

enhancedPatients <- enhanceData(curAllPatients)

num_male_patients <- sum(enhancedPatients$gender == "männlich")
num_female_patients <- sum(enhancedPatients$gender == "weiblich")
num_wechseljahre <- sum(enhancedPatients$wechseljahre == TRUE)

print(paste("Gesamtanzahl Patienten in Visite:", nrow(enhancedPatients)))
print(paste("Anzahl männliche Patienten in Visite:",num_male_patients))
print(paste("Anzahl weibliche Patienten in Visite:",num_female_patients))
print(paste("Anzahl Patienten in Wechseljahren:", num_wechseljahre))

## Visualisierung der Daten
library(ggplot2)

## Visualisierung der Altersverteilung
ggplot(enhancedPatients, aes(x = alter)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  labs(title = "Altersverteilung", x = "Alter", y = "Anzahl Personen") +
  theme_minimal()

## Visualisierung der Geschlechterverteilung
ggplot(enhancedPatients, aes(x = gender)) +
  geom_bar(stat = "count", fill = "steelblue", color = "white") +
  labs(title = "Geschlechterverteilung", x = "Geschlecht", y = "Anzahl")+
  theme_minimal()

## Visualisierung der Einnahme von Prophylaxemedikamenten
ggplot(enhancedPatients, aes(x = prophylaxeMed)) +
  geom_bar(stat = "count", fill = "steelblue", color = "white") +
  geom_text(aes(label=after_stat(count)),stat = "count", position = position_stack(vjust = 0.5))
  labs(title = "Verteilung Prophylaxemedikamente", x = "Nimmt Prophylaxemedikamente", y = "Anzahl")+
  theme_minimal()

## Visualisierung der Verteilung der Anzahlt an eingenommenen Prophylaxe Medikamente
ggplot(enhancedPatients, aes(x = nProphylaxeMed)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  geom_text(aes(label=after_stat(count)),stat = "count", position = position_stack(vjust = 0.5))
  labs(title = "Verteilung Anzahl Prophylaxemedikamente", x = "Anzahl Prophylaxe Medikamente", y = "Anzahl Personen") +
  theme_minimal()
