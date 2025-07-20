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
## Festlegen von wann bis wann Wechseljahre
menopauseStart <- 45
menopauseEnd <- 55

## Daten um weitere berechnete Felder ergänzen
enhanceData <- function(patients) {
  prophylaxeMedNameColumns <- grep("preventive_name_p", names(patients), value = TRUE)
  prophylaxeAbortedMedNameColumns <- grep("preventive_name_fp", names(patients), value = TRUE)
  prophylaxeAbortedMedEndDateColumns <- grep("end_date_fp_", names(patients), value = TRUE)
  prophylaxeMedEffectColumns <- grep("effect_p", names(patients), value = TRUE)
  prophylaxeMedDosageColumns <- grep("dosage_p", names(patients), value = TRUE)
  prophylaxeMedTolerabilityColumns <- grep("tolerability_p", names(patients), value = TRUE)

  patients[, prophylaxeMedNameColumns] <- lapply(patients[, prophylaxeMedNameColumns], as.character)
  patients[, prophylaxeAbortedMedNameColumns] <- lapply(patients[, prophylaxeAbortedMedNameColumns], as.character)
  patients[, prophylaxeMedEffectColumns] <- lapply(patients[, prophylaxeMedEffectColumns], as.numeric)
  patients[, prophylaxeMedDosageColumns] <- lapply(patients[, prophylaxeMedDosageColumns], as.numeric)
  patients[, prophylaxeMedTolerabilityColumns] <- lapply(patients[, prophylaxeMedTolerabilityColumns], as.numeric)

  patients$birthyear <- as.numeric(patients$birthyear)
  patients$age <- as.numeric(format(as.Date(patients[, grep("^date_dc_K\\d{1,2}$", names(patients))]), "%Y")) - patients$birthyear
  patients$menopause <- patients$age < menopauseEnd & patients$age > menopauseStart
  patients$prophylaxeMed <- patients[prophylaxeMedNameColumns[1]] != "0"
  patients$nProphylaxeMed <- rowSums(patients[, prophylaxeMedNameColumns] != "0")
  patients$abortedProphylaxeMed <- patients[prophylaxeAbortedMedNameColumns[1]] != "0"
  patients$nAbortedProphylaxeMed <- rowSums(patients[, prophylaxeAbortedMedNameColumns] != "0")

  # Durchschnitt der prophylaktischen Effekt-Werte (≠ 0)
  patients$avgProphylaxisEffect <- apply(patients[, prophylaxeMedEffectColumns], 1, function(x) {
    x_nonzero <- x[x != 0 & !is.na(x)]
    if (length(x_nonzero) == 0) {
      return(NA)
    }
    mean(x_nonzero)
  })

  # Durchschnitt der prophylaktischen Dosage-Werte (≠ 0)
  patients$avgProphylaxisDosage <- apply(patients[, prophylaxeMedDosageColumns], 1, function(x) {
    x_nonzero <- x[x != 0 & !is.na(x)]
    if (length(x_nonzero) == 0) {
      return(NA)
    }
    mean(x_nonzero)
  })

  # Durchschnitt der prophylaktischen Tolerability-Werte (≠ 0)
  patients$avgProphylaxisTolerability <- apply(patients[, prophylaxeMedTolerabilityColumns], 1, function(x) {
    x_nonzero <- x[x != 0 & !is.na(x)]
    if (length(x_nonzero) == 0) {
      return(NA)
    }
    mean(x_nonzero)
  })

  # Alter beim Absetzen jeder abgebrochenen Medikation berechnen
  for (col in prophylaxeAbortedMedEndDateColumns) {
    new_col_name <- paste0("abortedAge_", sub("end_date_fp_(\\d+)_K\\d+", "\\1", col))
    
    date_values <- patients[[col]]    
    date_values[date_values == 0 | date_values == "0"] <- NA
    
    parsed_dates <- as.Date(date_values, format = "%Y-%m-%d")
    end_year <- as.numeric(format(as.Date(date_values), "%Y"))
    abortedAge <- ifelse(is.na(end_year), NA, end_year - patients$birthyear)
    
    patients[[new_col_name]] <- abortedAge
  }

  patients
}

enhancedPatients <- enhanceData(curAllPatients)


## In 10er Altersgruppen vor und nach Menopause aufteilen
enhancedPatients$menopausePhase <- cut(
  enhancedPatients$age,
  breaks = c(menopauseStart-10, menopauseStart, menopauseEnd, menopauseEnd+10),
  right = FALSE,
  labels = c(glue("{menopauseStart-10}–{menopauseStart-1}"), glue("{menopauseStart}–{menopauseEnd-1}"), glue("{menopauseEnd}-{menopauseEnd+9}"))
)


### Untersuchungen auf statistische Signigikanz
## Prophylaxis Effekt vor und nach Menopause
filteredPatients <- enhancedPatients %>%
  filter(!is.na(menopausePhase), !is.na(avgProphylaxisEffect)) %>%
  mutate(effectGroup = ifelse(avgProphylaxisEffect < 3, "niedrig", "hoch"))
 
tableEffectMenopause <- table(filteredPatients$menopausePhase, filteredPatients$effectGroup)
print(tableEffectMenopause)

testResult <- chisq.test(tableEffectMenopause)
print(testResult)

## Prophylaxis Tolerability vor und nach Menopause 
filteredPatients <- enhancedPatients %>%
  filter(!is.na(menopausePhase), !is.na(avgProphylaxisTolerability)) %>%
  mutate(tolearbilityGroup = ifelse(avgProphylaxisTolerability < 3, "niedrig", "hoch"))
 
tableTolerabilityMenopause <- table(filteredPatients$menopausePhase, filteredPatients$tolearbilityGroup)
print(tableTolerabilityMenopause)

testResult <- chisq.test(tableTolerabilityMenopause)
print(testResult)

## Plotten des Durschnitts

filteredPatients <- enhancedPatients %>%
  filter(!is.na(menopausePhase))
  avgEffectByAgeGroup <- filteredPatients %>%
    group_by(menopausePhase) %>%
    summarise(
      meanEffect = mean(avgProphylaxisEffect, na.rm = TRUE),
      count = n()
    )

filteredPatients <- enhancedPatients %>%
  filter(!is.na(menopausePhase))
  avgTolerabilityByAgeGroup <- filteredPatients %>%
    group_by(menopausePhase) %>%
    summarise(
      meanTolerability = mean(avgProphylaxisTolerability, na.rm = TRUE),
      count = n()
    )

p1 <- ggplot(avgEffectByAgeGroup, aes(x = menopausePhase, y = meanEffect)) +
  geom_col(fill = "darkgreen", width = 0.5) +
  geom_text(aes(label = round(meanEffect, 2)), vjust = -0.5, size = 5) +
  labs(
    title = "Durchschnittlicher Prophylaxe-Effekt-Wert pro Altersgruppe",
    x = "Altersgruppe (Jahre)",
    y = "Ø Prophylaxe-Effekt"
  ) +
  coord_cartesian(ylim = c(0, 3.4)) +
  theme_minimal()

ggsave("effect_value_v1.png", plot = p1, width = 8, height = 4, dpi = 300)

p2 <- ggplot(avgTolerabilityByAgeGroup, aes(x = menopausePhase, y = meanTolerability)) +
  geom_col(fill = "darkgreen", width = 0.5) +
  geom_text(aes(label = round(meanTolerability, 2)), vjust = -0.5, size = 5) +
  labs(
    title = "Durchschnittlicher Prophylaxe-Tolerability-Wert pro Altersgruppe",
    x = "Altersgruppe (Jahre)",
    y = "Ø Prophylaxe-Tolerability"
  ) +
  coord_cartesian(ylim = c(0, 2.4)) +
  theme_minimal()


ggsave("tolera_value_v1.png", plot = p2, width = 8, height = 4, dpi = 300)

# nolint end
