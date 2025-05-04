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
curAllPatients <- PatientenV1

#Methode zur Ausgabe der Patientenverteilung, welche Präventivmedikation einnehmen
enhanceData <- function(patients) {
  
  # Nur patienten mir Präventivmedikation
  spalten <- grep("preventive_name_p", names(patients), value=TRUE)
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
# binwidth 1
ggplot(enhancedPatients, aes(x = alter, fill=wechseljahre)) +
  geom_histogram(binwidth = 1, color = "white") +
  scale_fill_manual(values= c("steelblue","darkgreen"))+
  labs(title = "Altersverteilung", x = "Alter", y = "Anzahl Personen", fill="Wechseljahre") +
  theme_minimal()

# binwidth 5
ggplot(enhancedPatients, aes(x = alter, fill=wechseljahre)) +
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
ggplot(enhancedPatients, aes(x = wechseljahre)) +
  geom_bar(stat = "count", fill = "steelblue", color = "white") +
  labs(title = "Anzahl Patienten in Wechseljahre", x = "Wechseljahre", y = "Anzahl")+
  geom_text(aes(label=after_stat(count)),stat = "count", position = position_stack(vjust = 0.5), color="black")+
  scale_x_discrete(labels = c("FALSE" = "Nein", "TRUE" = "Ja"))+ 
  theme_minimal()

## Visualisierung der Anzahl an Patienten in Wechseljahren nach Geschlecht
ggplot(enhancedPatients, aes(x = wechseljahre)) +
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
  facet_wrap(~wechseljahre) +
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
  facet_wrap(~wechseljahre)+
  geom_histogram(aes(y=after_stat(density)),binwidth = 1, fill = "steelblue", color = "white") +
  labs(title = "Verteilung Anzahl Prophylaxemedikamente", x = "Anzahl Prophylaxe Medikamente", y = "Prozent der Patienten") +
  theme_minimal()

# Nach Wechseljahren absolut
ggplot(enhancedPatients, aes(x=nProphylaxeMed)) +
  facet_wrap(~wechseljahre)+
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
