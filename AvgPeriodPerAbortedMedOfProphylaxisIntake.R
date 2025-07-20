# nolint start
### Einlesen der Daten
PatientenV1 <- read.csv("Daten\\PatientInnendaten mit Visiten-20250423\\Patients_V1_10-03-2025.csv", header = TRUE, sep = ";")
PatientenV2 <- read.csv("Daten\\PatientInnendaten mit Visiten-20250423\\Patients_V2_10-03-2025.csv", header = TRUE, sep = ";")
PatientenV3 <- read.csv("Daten\\PatientInnendaten mit Visiten-20250423\\Patients_V3_10-03-2025.csv", header = TRUE, sep = ";")
PatientenV4 <- read.csv("Daten\\PatientInnendaten mit Visiten-20250423\\Patients_V4_10-03-2025.csv", header = TRUE, sep = ";")
PatientenV5 <- read.csv("Daten\\PatientInnendaten mit Visiten-20250423\\Patients_V5_10-03-2025.csv", header = TRUE, sep = ";")
PatientenV6 <- read.csv("Daten\\PatientInnendaten mit Visiten-20250423\\Patients_V6_10-03-2025.csv", header = TRUE, sep = ";")
PatientenV7 <- read.csv("Daten\\PatientInnendaten mit Visiten-20250423\\Patients_V7_10-03-2025.csv", header = TRUE, sep = ";")
PatientenV8 <- read.csv("Daten\\PatientInnendaten mit Visiten-20250423\\Patients_V8_10-03-2025.csv", header = TRUE, sep = ";")
PatientenV9 <- read.csv("Daten\\PatientInnendaten mit Visiten-20250423\\Patients_V9_10-03-2025.csv", header = TRUE, sep = ";")
PatientenV10 <- read.csv("Daten\\PatientInnendaten mit Visiten-20250423\\Patients_V10_10-03-2025.csv", header = TRUE, sep = ";")
PatientenV11 <- read.csv("Daten\\PatientInnendaten mit Visiten-20250423\\Patients_V11_10-03-2025.csv", header = TRUE, sep = ";")
PatientenV12 <- read.csv("Daten\\PatientInnendaten mit Visiten-20250423\\Patients_V12_10-03-2025.csv", header = TRUE, sep = ";")
PatientenV13 <- read.csv("Daten\\PatientInnendaten mit Visiten-20250423\\Patients_V13_10-03-2025.csv", header = TRUE, sep = ";")
PatientenV14 <- read.csv("Daten\\PatientInnendaten mit Visiten-20250423\\Patients_V14_10-03-2025.csv", header = TRUE, sep = ";")
PatientenV15 <- read.csv("Daten\\PatientInnendaten mit Visiten-20250423\\Patients_V15_10-03-2025.csv", header = TRUE, sep = ";")


### Nötigen Librarys
library(ggplot2)
library(dplyr)
library(glue)
library(patchwork)

### Initialisiere Variablen
## Festlegen welche Visite
curAllPatients <- PatientenV1

## Daten um weitere berechnete Felder ergänzen
enhanceData <- function(patients) {
  prophylaxeMedStartDateColumns <- grep("begin_p", names(patients), value = TRUE)
  prophylaxeAbortedMedEndDateColumns <- grep("end_date_fp_", names(patients), value = TRUE)
  prophylaxeAbortedMedStartDateColumns <- grep("begin_date_fp_", names(patients), value = TRUE)

  patients$birthyear <- as.numeric(patients$birthyear)
  patients$age <- as.numeric(format(as.Date(patients[, grep("^date_dc_K\\d{1,2}$", names(patients))]), "%Y")) - patients$birthyear

    # Alter beim Start jeder aktuellen Medikation berechnen
  for (col in prophylaxeMedStartDateColumns) {
    new_col_name <- paste0("startAge_", sub(".*_p(\\d+)_K\\d+", "\\1", col))
    
    date_values <- patients[[col]]    
    date_values[date_values == 0 | date_values == "0"] <- NA
    
    parsed_dates <- as.Date(date_values, format = "%Y-%m-%d")
    start_year <- as.numeric(format(as.Date(date_values), "%Y"))
    startAge <- ifelse(is.na(start_year), NA, start_year - patients$birthyear)
    
    patients[[new_col_name]] <- startAge
  }
  
  # Alter beim Absetzen jeder abgebrochenen Medikation berechnen
  for (col in prophylaxeAbortedMedEndDateColumns) {
    new_col_name <- paste0("abortedEndAge_", sub("end_date_fp_(\\d+)_K\\d+", "\\1", col))
    
    date_values <- patients[[col]]    
    date_values[date_values == 0 | date_values == "0"] <- NA
    
    parsed_dates <- as.Date(date_values, format = "%Y-%m-%d")
    end_year <- as.numeric(format(as.Date(date_values), "%Y"))
    abortedAge <- ifelse(is.na(end_year), NA, end_year - patients$birthyear)
    
    patients[[new_col_name]] <- abortedAge
  }

  # Alter beim Start jeder abgebrochenen Medikation berechnen
  for (col in prophylaxeAbortedMedStartDateColumns) {
    new_col_name <- paste0("abortedStartAge_", sub("begin_date_fp_(\\d+)_K\\d+", "\\1", col))
    
    date_values <- patients[[col]]    
    date_values[date_values == 0 | date_values == "0"] <- NA
    
    parsed_dates <- as.Date(date_values, format = "%Y-%m-%d")
    start_year <- as.numeric(format(as.Date(date_values), "%Y"))
    abortedStartAge <- ifelse(is.na(start_year), NA, start_year - patients$birthyear)
    
    patients[[new_col_name]] <- abortedStartAge
  }

  # Drei felder: jeweils durschnittliche Dauer an Prophylaxis Medikation in Jahren pro Altersphase, 
  # berechnet über die Felder: für aborted abortedStartAge und abortedEndAge, für aktuelle startAge und age
  # Kein Medikament in der Phase: durschnitt gleich 0
  # Ist Patient nicht älter als die Obergrenze der Altersphase: durschnitt gleich NA
  # Geht die Zeitspanne über Grenze einer Altersphase: z.B. 42-48 -> Aufteilung jeweils in die Phasen (nehme als Endalter oder Startalter jeweils entsprechend die Grenze)
  # Define age phases
  age_phases <- list(
    phase1 = c(15, 25),
    phase2 = c(25, 35),
    phase3 = c(35, 45),
    phase4 = c(45, 55),
    phase5 = c(55, 65),
    phase6 = c(65, 75)
  )

  # Initialize average duration columns
  patients$avgDurationPhase1 <- NA
  patients$avgDurationPhase2 <- NA
  patients$avgDurationPhase3 <- NA
  patients$avgDurationPhase4 <- NA
  patients$avgDurationPhase5 <- NA
  patients$avgDurationPhase6 <- NA

  for (i in 1:nrow(patients)) {
    total_durations <- c(0, 0, 0, 0, 0, 0)
    count_durations <- c(0, 0, 0, 0, 0, 0)
    current_age <- patients$age[i]

    # Helper function to add durations to the right phases
    add_aborted_duration_to_phases <- function(startAge, endAge) {
      if (is.na(startAge) | is.na(endAge)) return(NULL)
      for (p in 1:length(age_phases)) {
        phase_start <- age_phases[[p]][1]
        phase_end <- age_phases[[p]][2]
        
        overlap_start <- max(startAge, phase_start)
        overlap_end <- min(endAge, phase_end)
        
        if (overlap_start <= overlap_end) {
          duration <- overlap_end - overlap_start
          total_durations[p] <<- total_durations[p] + duration
          count_durations[p] <<- count_durations[p] + 1
        }
      }
    }

    # Aborted medications
    abortedStartCols <- grep("^abortedStartAge_", names(patients), value = TRUE)
    abortedEndCols <- grep("^abortedEndAge_", names(patients), value = TRUE)
    for (j in seq_along(abortedStartCols)) {
      startAge <- patients[[abortedStartCols[j]]][i]
      endAge <- patients[[abortedEndCols[j]]][i]
      add_aborted_duration_to_phases(startAge, endAge)
    }

    # Finalize: compute averages or assign NA
    for (p in 1:6) {
      if (count_durations[p] == 0 || current_age >= age_phases[[p]][2] || current_age <= age_phases[[p]][1]) {
        patients[i, paste0("avgDurationPhase", p)] <- NA
      } else {
        patients[i, paste0("avgDurationPhase", p)] <- total_durations[p] / count_durations[p]
      }
    }
  }


  patients
}

enhancedPatients <- enhanceData(curAllPatients)

# Berechne Durschnittliche Dauer in jahre für die Einnahme von Prophylaxe Medikamenten für jede Phase über alle Patienten
# Ignoriere alle NA Werte und nehme nur Werte >= 0

durchschnittPhase1 <- mean(enhancedPatients$avgDurationPhase1[!is.na(enhancedPatients$avgDurationPhase1) & enhancedPatients$avgDurationPhase1 >= 0])
durchschnittPhase2 <- mean(enhancedPatients$avgDurationPhase2[!is.na(enhancedPatients$avgDurationPhase2) & enhancedPatients$avgDurationPhase2 >= 0])
durchschnittPhase3 <- mean(enhancedPatients$avgDurationPhase3[!is.na(enhancedPatients$avgDurationPhase3) & enhancedPatients$avgDurationPhase3 >= 0])
durchschnittPhase4 <- mean(enhancedPatients$avgDurationPhase4[!is.na(enhancedPatients$avgDurationPhase4) & enhancedPatients$avgDurationPhase4 >= 0])
durchschnittPhase5 <- mean(enhancedPatients$avgDurationPhase5[!is.na(enhancedPatients$avgDurationPhase5) & enhancedPatients$avgDurationPhase5 >= 0])
durchschnittPhase6 <- mean(enhancedPatients$avgDurationPhase6[!is.na(enhancedPatients$avgDurationPhase6) & enhancedPatients$avgDurationPhase6 >= 0])



# Optional: Ausgabe zur Kontrolle
cat(glue::glue("Durchschnittliche Einnahmedauer von abgesetzten Prophylaxe-Meds (in Jahren):\n  
Phase 15 - 25: {round(durchschnittPhase1, 2)}\n
Phase 25 - 35: {round(durchschnittPhase2, 2)}\n
Phase 35 - 45: {round(durchschnittPhase3, 2)}\n
Phase 45 - 55: {round(durchschnittPhase4, 2)}\n
Phase 55 - 65: {round(durchschnittPhase5, 2)}\n
Phase 65 - 75: {round(durchschnittPhase6, 2)}\n\n"))

install.packages("tidyr")
library(tidyr)
library(dplyr)

# Beispiel: Wandle deine Spalten in ein langes Format
long_data <- enhancedPatients %>%
  select(avgDurationPhase3,
         avgDurationPhase4, avgDurationPhase5) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Phase",
    values_to = "Einnahmedauer"
  ) %>%
  filter(!is.na(Einnahmedauer) & Einnahmedauer >= 0)

# ANOVA testen
anova_result <- aov(Einnahmedauer ~ Phase, data = long_data)
summary(anova_result)

TukeyHSD(anova_result)

# nolint end