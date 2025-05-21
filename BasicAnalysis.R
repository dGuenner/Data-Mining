### Main (angeben welche Plot-Funktion ausgeführt wwrden soll):
# agedistributionBinwidth1()
# agedistributionBinwidth5()
# genderDistribution()
# menopauseDistribution()
# menopauseDistributionByGender()
# prophylaxisMedicationDistribution()
# prophylaxisMedicationDistributionByMenopause()
# prophylaxisMedicationDistributionByGender()
# prophylaxisMedicationAmountDistribution()
# prophylaxisMedicationAmountDistributionByMenopauseRelativ()
# prophylaxisMedicationAmountDistributionByMenopauseAbsolute()
# prophylaxisMedicationAmountDistributionByGenderRelativ()
# prophylaxisMedicationAmountDistributionByGenderAbsolute()
# averageProphylaxisEffectBy10YearAgeGroup()
# averageProphylaxisEffectByMenopause()
# averageProphylaxisEffectByAge1()
# averageProphylaxisEffectByAge2()
# averageProphylaxisDosageBy10YearAgeGroup()
# averageProphylaxisDosageByMenopause()
# averageProphylaxisDosageByAge1()
# averageProphylaxisDosageByAge2()
# averageProphylaxisTolerabilityBy10YearAgeGroup()
# averageProphylaxisTolerabilityByMenopause()
# averageProphylaxisTolerabilityByAge()
# abortedProphylaxisMedDistribution()
# abortedProphylaxisMedDistributionByMenopause()
# abortedProphylaxisMedDistributionByGender()
# abortedProphylaxisMedAmountDistribution()
# abortedProphylaxisMedAmountDistributionByMenopauseRelativ()
# abortedProphylaxisMedAmountDistributionByMenopauseAbsolute()
# abortedProphylaxisMedAmountDistributionByGenderRelativ()
# abortedProphylaxisMedAmountDistributionByGenderAbsolute()

### Plots zur Medikamentenverteilung

# plotMedicationUsageGeneral()
# plotMedicationUsageMenopause()
# plotMedicationUsageNotMenopause()
# plotMedicationUsageComparison()
# plotMedicationGroupsGeneral()
# plotMedicationGroupsMenopause()
# plotMedicationGroupsNotMenopause()
# plotMedicationGroupsComparison()
# plotMedicationGroupsComparisonPercent()
# plotMedicationGroupsComparisonAbsAndPercent()

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

### Untersuchung der Spalten
## Ausgabe der Spalten der Patientendaten
getColumnNamesInTerminal <- function() {
  ncol(PatientenV1)
}

## Ausgabe der Spaltennamen mit Beispielwerten in csv
getColumnNamesExampleValueInCsv <- function() {
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
}

### Nötigen Librarys
library(ggplot2)
library(dplyr)
library(patchwork)

### Initialisiere Variablen
## Festlegen welche Visite
curAllPatients <- PatientenV1

## Daten um weitere berechnete Felder ergänzen
enhanceData <- function(patients) {
  prophylaxeMedNameColumns <- grep("preventive_name_p", names(patients), value = TRUE)
  prophylaxeAbortedMedNameColumns <- grep("preventive_name_fp", names(patients), value = TRUE)
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
  patients$menopause <- patients$age < 55 & patients$age > 45
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

  patients
}

enhancedPatients <- enhanceData(curAllPatients)
patientsPredMeds <- enhancedPatients$prophylaxeMed == TRUE

getUniqueMedicationGroupsUsage <- function(data, med_groups) {
  library(magrittr)
  columns <- grep("preventive_name_p", names(data), value = TRUE)
  allMeds <- unlist(lapply(data[, columns], function(x) as.character(x)))
  allMeds <- sort(unique(allMeds))
  allMeds <- allMeds[allMeds != "0"] # Entferne '0' Werte
  
  # Erstelle eine Zuordnung von Medikament zu Gruppe
  med_to_group <- data.frame(
    medication = character(),
    group = character(),
    stringsAsFactors = FALSE
  )
  
  for (group_name in names(med_groups)) {
    for (med in med_groups[[group_name]]) {
      med_to_group <- rbind(med_to_group, data.frame(
        medication = med,
        group = group_name,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Berechne die Medikamentenhäufigkeiten wie vorher
  meds_each_column <- list()
  for (column in columns) {
    freqMeds <- plyr::count(data, column)
    meds_each_column <- append(meds_each_column, freqMeds)
  }
  
  meds_each_column_menopause <- list()
  menopause_only <- data %>% dplyr::filter(menopause == TRUE)
  for (column in columns) {
    freqMeds <- plyr::count(menopause_only, column)
    meds_each_column_menopause <- append(meds_each_column_menopause, freqMeds)
  }
  
  menopause_excluded <- data %>% dplyr::filter(menopause == FALSE)
  meds_each_column_not_menopause <- list()
  for (column in columns) {
    freqMeds <- plyr::count(menopause_excluded, column)
    meds_each_column_not_menopause <- append(meds_each_column_not_menopause, freqMeds)
  }
  
  # Transformiere Daten für alle drei Kategorien
  resultGeneral <- as.data.frame(transformMedicationData(meds_each_column))
  resultGeneral <- resultGeneral[resultGeneral$medication != "0", ]
  
  resultMenopause <- as.data.frame(transformMedicationData(meds_each_column_menopause))
  resultMenopause <- resultMenopause[resultMenopause$medication != "0", ]
  
  resultNotMenopause <- as.data.frame(transformMedicationData(meds_each_column_not_menopause))
  resultNotMenopause <- resultNotMenopause[resultNotMenopause$medication != "0", ]
  
  # Füge Gruppenzugehörigkeit hinzu
  addGroupInfo <- function(result_df) {
    result_df$group <- "Sonstige"
    for (i in 1:nrow(result_df)) {
      idx <- which(med_to_group$medication == result_df$medication[i])
      if (length(idx) > 0) {
        result_df$group[i] <- med_to_group$group[idx[1]]
      }
    }
    return(result_df)
  }
  
  resultGeneral <- addGroupInfo(resultGeneral)
  resultMenopause <- addGroupInfo(resultMenopause)
  resultNotMenopause <- addGroupInfo(resultNotMenopause)
  
  # Aggregiere nach Gruppen
  resultGeneralByGroup <- resultGeneral %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(usage = sum(usage))
  
  resultMenopauseByGroup <- resultMenopause %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(usage = sum(usage))
  
  resultNotMenopauseByGroup <- resultNotMenopause %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(usage = sum(usage))
  
  # Rückgabe der ursprünglichen und der nach Gruppen aggregierten Daten
  return(list(
    individual = list(resultGeneral, resultMenopause, resultNotMenopause),
    byGroup = list(resultGeneralByGroup, resultMenopauseByGroup, resultNotMenopauseByGroup)
  ))
}

getUniqueMedicationUsage <- function(data) {
  library(magrittr)
  columns <- grep("preventive_name_p", names(data), value = TRUE)
  allMeds <- unlist(lapply(data[, columns], function(x) as.character(x)))
  allMeds <- sort(unique(allMeds))
  meds_each_column <- list()
  for (column in columns) {
    freqMeds <- plyr::count(data, column)
    meds_each_column <- append(meds_each_column, freqMeds)
  }
  meds_each_column_menopause <- list()
  menopause_only <- data %>% dplyr::filter(menopause == TRUE)
  for (column in columns) {
    freqMeds <- plyr::count(menopause_only, column)
    meds_each_column_menopause <- append(meds_each_column_menopause, freqMeds)
  }
  menopause_excluded <- data %>% dplyr::filter(menopause == FALSE)
  meds_each_column_not_menopause <- list()
  for (column in columns) {
    freqMeds <- plyr::count(menopause_excluded, column)
    meds_each_column_not_menopause <- append(meds_each_column_not_menopause, freqMeds)
  }
  
  resultGeneral <- as.data.frame(transformMedicationData(meds_each_column))
  missing_medication <- setdiff(allMeds, resultGeneral$medication)
  if (length(missing_medication) > 0) {
    missing_df <- data.frame(
      medication = missing_medication,
      usage = rep(0, length(missing_medication))
    )
    resultGeneral <- rbind(resultGeneral, missing_df)
  }
  
  resultMenopause <- as.data.frame(transformMedicationData(meds_each_column_menopause))
  missing_medication <- setdiff(allMeds, resultMenopause$medication)
  if (length(missing_medication) > 0) {
    missing_df <- data.frame(
      medication = missing_medication,
      usage = rep(0, length(missing_medication))
    )
    resultMenopause <- rbind(resultMenopause, missing_df)
  }
  
  resultNotMenopause <- as.data.frame(transformMedicationData(meds_each_column_not_menopause))
  missing_medication <- setdiff(allMeds, resultNotMenopause$medication)
  if (length(missing_medication) > 0) {
    missing_df <- data.frame(
      medication = missing_medication,
      usage = rep(0, length(missing_medication))
    )
    resultNotMenopause <- rbind(uniqueMedsGeneral, missing_df)
  }
  return(list(resultGeneral, resultMenopause, resultNotMenopause))
}

# Initialisierung der Variablen
# Verwende die Funktion zur Berechnung der Medikamentennutzung nach Gruppen
initializeVariables <- function() {
  # Definiere die Medikamentengruppen global
  med_groups <<- list(
    "Betablocker" = c("Propranolol", "Metoprolol", "Bisoprolol"),
    "Flunarizin" = "Flunarizin",
    "Topiramat" = "Topiramat",
    "Amitriptylin" = "Amitriptylin",
    "OnabotulinumtoxinA" = "OnabotulinumtoxinA",
    "CGRP(R)-Ak" = c("Eptinezumab", "Erenumab", "Fremanezumab", "Galcanezumab")
  )
  
  # Berechne die Medikamentendaten
  grouped_results <<- getUniqueMedicationGroupsUsage(enhancedPatients, med_groups)
  
  # Extrahiere die nach Gruppen aggregierten Daten global
  groupedMedsGeneral <<- grouped_results$byGroup[[1]]
  groupedMedsMenopause <<- grouped_results$byGroup[[2]]
  groupedMedsNotMenopause <<- grouped_results$byGroup[[3]]
  
  # Berechne die Medikamentendaten ohne Gruppierung
  result <<- getUniqueMedicationUsage(enhancedPatients)
  uniqueMedsGeneral <<- result[[1]][result[[1]]$medication != "0", ]
  uniqueMedsMenopause <<- result[[2]][result[[2]]$medication != "0", ]
  uniqueMedsNotMenopasue <<- result[[3]][result[[3]]$medication != "0", ]
  
  # Erstelle ein kombiniertes Dataframe für Vergleiche und entferne die "Sonstige" Gruppe
  grouped_comparison <<- rbind(
    transform(groupedMedsMenopause, status = "Menopause"),
    transform(groupedMedsNotMenopause, status = "Nicht-Menopause")
  )
  grouped_comparison <<- grouped_comparison[grouped_comparison$group != "Sonstige", ]
  
  # Berechne prozentuale Verteilungen
  grouped_comparison_percent <<- grouped_comparison %>%
    group_by(status) %>%
    mutate(percentage = usage / sum(usage) * 100)
}

getUniqueMedicationNames <- function(data) {
  columns <- grep("preventive_name_p", names(data), value = TRUE)
  
  unique_values <- lapply(data[, columns], unique)
  
  uniqueMeds <- unique(unlist(unique_values))
  uniqueMeds[uniqueMeds != "0"]
}
medicationNames <- getUniqueMedicationNames(enhancedPatients)
print(medicationNames)

transformMedicationData <- function(meds_each_column) {
  result <- data.frame(medication = character(), usage = numeric())
  for (i in seq(1, length(meds_each_column), by = 2)) {
    for (k in 1:(length(meds_each_column[[i]]))) {
      if (as.character(meds_each_column[[i]][[k]]) %in% result$medication) {
        idx <- which(result$medication == as.character(meds_each_column[[i]][k]))
        result[idx, "usage"] <- (result[idx, "usage"] + meds_each_column[[i + 1]][k])
      } else {
        new_row <- data.frame(
          medication = as.character(meds_each_column[[i]][[k]]),
          usage = as.numeric(meds_each_column[[i + 1]][[k]])
        )
        result <- rbind(result, new_row)
      }
    }
    return(result)
  }
}

# Führe die Initialisierung aus
initializeVariables()


## Verwendung von bestimmten Medikamenten

## In 10er Altersgruppen aufteilen
enhancedPatients$ageGroup10 <- cut(
  enhancedPatients$age,
  breaks = seq(0, max(enhancedPatients$age, na.rm = TRUE) + 10, by = 10),
  right = FALSE,
  include.lowest = TRUE,
  labels = paste(seq(0, max(enhancedPatients$age, na.rm = TRUE), by = 10),
    seq(9, max(enhancedPatients$age, na.rm = TRUE) + 9, by = 10),
    sep = "-"
  )
)

### Plots zur Datenvisualisierung
## Visualisierung der Altersverteilung
# agedistribution binwidth 1
agedistributionBinwidth1 <- function() {
  ggplot(enhancedPatients, aes(x = age, fill = menopause)) +
    geom_histogram(binwidth = 1, color = "white") +
    scale_fill_manual(values = c("steelblue", "darkgreen")) +
    labs(title = "Altersverteilung", x = "Alter", y = "Anzahl Personen", fill = "Wechseljahre") +
    theme_minimal()
}

# agedistribution binwidth 5
agedistributionBinwidth5 <- function() {
  ggplot(enhancedPatients, aes(x = age, fill = menopause)) +
    geom_histogram(binwidth = 5, color = "white") +
    scale_fill_manual(values = c("steelblue", "darkgreen")) +
    labs(title = "Altersverteilung", x = "Alter", y = "Anzahl Personen", fill = "Wechseljahre") +
    geom_text(aes(label = after_stat(count)), stat = "bin", binwidth = 5, color = "black", position = "stack", vjust = -0.5, size = 3.5) +
    theme_minimal()
}

## Visualisierung der Geschlechterverteilung
genderDistribution <- function() {
  ggplot(enhancedPatients, aes(x = gender)) +
    geom_bar(stat = "count", fill = "steelblue", color = "white") +
    labs(title = "Geschlechterverteilung", x = "Geschlecht", y = "Anzahl") +
    geom_text(aes(label = after_stat(count)), stat = "count", position = position_stack(vjust = 0.5), color = "black") +
    theme_minimal()
}

## Visualisierung der Patienten in Wechseljahren
menopauseDistribution <- function() {
  ggplot(enhancedPatients, aes(x = menopause)) +
    geom_bar(stat = "count", fill = "steelblue", color = "white") +
    labs(title = "Anzahl Patienten in Wechseljahre", x = "Wechseljahre", y = "Anzahl") +
    geom_text(aes(label = after_stat(count)), stat = "count", position = position_stack(vjust = 0.5), color = "black") +
    scale_x_discrete(labels = c("FALSE" = "Nein", "TRUE" = "Ja")) +
    theme_minimal()
}

# Visualisierung der Anzahl an Patienten in Wechseljahren nach Geschlecht
menopauseDistributionByGender <- function() {
  ggplot(enhancedPatients, aes(x = menopause)) +
    geom_bar(stat = "count", fill = "steelblue", color = "white") +
    facet_wrap(~gender) +
    labs(
      title = "Verteilung von Wechseljahren nach Geschlecht",
      x = "Wechseljahre",
      y = "Anzahl"
    ) +
    geom_text(aes(label = after_stat(count)), stat = "count", position = position_stack(vjust = 0.5), color = "black") +
    scale_x_discrete(labels = c("FALSE" = "Nein", "TRUE" = "Ja")) +
    theme_minimal()
}

## Visualisierung der Einnahme von Prophylaxemedikamenten
prophylaxisMedicationDistribution <- function() {
  ggplot(enhancedPatients, aes(x = prophylaxeMed)) +
    geom_bar(stat = "count", fill = "steelblue", color = "white") +
    geom_text(aes(label = after_stat(count)), stat = "count", position = position_stack(vjust = 0.5)) +
    labs(title = "Verteilung Prophylaxemedikamente", x = "Nimmt Prophylaxemedikamente", y = "Anzahl") +
    theme_minimal()
}

# nach Wechelsjahren
prophylaxisMedicationDistributionByMenopause <- function() {
  ggplot(enhancedPatients, aes(x = prophylaxeMed, fill = prophylaxeMed)) +
    geom_bar(stat = "count", color = "white") +
    scale_fill_manual(values = c("steelblue", "darkgreen")) +
    facet_wrap(~menopause) +
    labs(
      title = "Verteilung von Prophlaxemedikamenten nach Wechseljahren",
      x = "Prophylaxemedikation",
      y = "Anzahl"
    ) +
    geom_text(aes(label = after_stat(count)), stat = "count", position = position_stack(vjust = 0.5)) +
    scale_x_discrete(labels = c("TRUE" = "Ja", "FALSE" = "Nein")) +
    theme_minimal()
}

# nach Geschlecht
prophylaxisMedicationDistributionByGender <- function() {
  ggplot(enhancedPatients, aes(x = prophylaxeMed, fill = prophylaxeMed)) +
    geom_bar(stat = "count", color = "white") +
    scale_fill_manual(values = c("steelblue", "darkgreen")) +
    facet_wrap(~gender) +
    labs(
      title = "Verteilung von Prophlaxemedikamenten nach Geschlecht",
      x = "Prophylaxemedikation",
      y = "Anzahl"
    ) +
    geom_text(aes(label = after_stat(count)), stat = "count", position = position_stack(vjust = 0.5)) +
    scale_x_discrete(labels = c("TRUE" = "Ja", "FALSE" = "Nein")) +
    theme_minimal()
}

## Visualisierung der Verteilung der Anzahl an eingenommenen Prophylaxe Medikamente
prophylaxisMedicationAmountDistribution <- function() {
  ggplot(enhancedPatients, aes(x = nProphylaxeMed)) +
    geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
    geom_text(aes(label = after_stat(count)), stat = "count", position = position_stack(vjust = 0.5), angle = 90) +
    labs(title = "Verteilung Anzahl Prophylaxemedikamente", x = "Anzahl Prophylaxe Medikamente", y = "Anzahl Personen") +
    theme_minimal()
}

# Nach Wechseljahren %
prophylaxisMedicationAmountDistributionByMenopauseRelativ <- function() {
  ggplot(enhancedPatients, aes(x = nProphylaxeMed)) +
    facet_wrap(~menopause) +
    geom_histogram(aes(y = after_stat(density)), binwidth = 1, fill = "steelblue", color = "white") +
    labs(title = "Verteilung Anzahl Prophylaxemedikamente", x = "Anzahl Prophylaxe Medikamente", y = "Prozent der Patienten") +
    theme_minimal()
}

# Nach Wechseljahren absolut
prophylaxisMedicationAmountDistributionByMenopauseAbsolute <- function() {
  ggplot(enhancedPatients, aes(x = nProphylaxeMed)) +
    facet_wrap(~menopause) +
    geom_histogram(aes(y = after_stat(count)), binwidth = 1, fill = "steelblue", color = "white") +
    labs(title = "Verteilung Anzahl Prophylaxemedikamente", x = "Anzahl Prophylaxe Medikamente", y = "Prozent der Patienten") +
    geom_text(aes(label = after_stat(count)), stat = "count", position = position_stack(vjust = 0.5), size = 3.0, angle = 90) +
    theme_minimal()
}

# Nach Geschlecht %
prophylaxisMedicationAmountDistributionByGenderRelativ <- function() {
  ggplot(enhancedPatients, aes(x = nProphylaxeMed)) +
    facet_wrap(~gender) +
    geom_histogram(aes(y = after_stat(density)), binwidth = 1, fill = "steelblue", color = "white") +
    labs(title = "Verteilung Anzahl Prophylaxemedikamente", x = "Anzahl Prophylaxe Medikamente", y = "Prozent der Patienten") +
    theme_minimal()
}

# Nach Geschlecht absolut
prophylaxisMedicationAmountDistributionByGenderAbsolute <- function() {
  ggplot(enhancedPatients, aes(x = nProphylaxeMed), ) +
    facet_wrap(~gender) +
    geom_histogram(aes(y = after_stat(count)), binwidth = 1, fill = "steelblue", color = "white") +
    labs(title = "Verteilung Anzahl Prophylaxemedikamente", x = "Anzahl Prophylaxe Medikamente", y = "Prozent der Patienten") +
    geom_text(aes(label = after_stat(count)), stat = "count", position = position_stack(vjust = 0.5), size = 3.0) +
    theme_minimal()
}

## Visualisierung der Prophylaxe Effekt
# Durschnitts Phrophylaxe Effekt pro 10 Jahre Altersgruppe
averageProphylaxisEffectBy10YearAgeGroup <- function() {
  avgEffectByAgeGroup <- enhancedPatients %>%
    group_by(ageGroup10) %>%
    summarise(
      meanEffect = mean(avgProphylaxisEffect, na.rm = TRUE),
      count = n()
    )

  ggplot(avgEffectByAgeGroup, aes(x = ageGroup10, y = meanEffect)) +
    geom_col(fill = "darkgreen") +
    geom_text(aes(label = round(meanEffect, 2)), vjust = -0.5) +
    labs(
      title = "Durchschnittlicher Prophylaxe-Effekt pro Altersgruppe",
      x = "Altersgruppe (Jahre)",
      y = "Ø Prophylaxe-Effekt"
    ) +
    theme_minimal()
}

# Durchschnittliche Dosierung pro Wechseljahres-Gruppe berechnen
averageProphylaxisEffectByMenopause <- function() {
  avgEffectByMenopause <- enhancedPatients %>%
    group_by(menopause) %>%
    summarise(
      meanEffect = mean(avgProphylaxisEffect, na.rm = TRUE),
      count = n()
    )

  ggplot(avgEffectByMenopause, aes(x = as.factor(menopause), y = meanEffect)) +
    geom_col(fill = "darkgreen") +
    geom_text(aes(label = meanEffect), vjust = -0.5) +
    labs(
      title = "Durchschnittlicher Prophylaxe-Effekt nach Wechseljahresstatus",
      x = "Wechseljahre (FALSE = Nein, TRUE = Ja)",
      y = "Ø Prophylaxe-Effekt"
    ) +
    theme_minimal()
}

# Durschnitts Phrophylaxe Effekt pro Alter 1
averageProphylaxisEffectByAge1 <- function() {
  avgEffectByAge <- enhancedPatients %>%
    group_by(age) %>%
    summarise(
      meanEffect = mean(avgProphylaxisEffect, na.rm = TRUE),
      count = n()
    )

  ggplot(avgEffectByAge, aes(x = age, y = meanEffect)) +
    geom_col(fill = "darkgreen") +
    geom_text(aes(label = round(meanEffect, 2)), vjust = -0.5) +
    labs(
      title = "Durchschnittliche Prophylaxe-Effekt pro Alter",
      x = "Alter (Jahre)",
      y = "Ø Prophylaxe-Effekt"
    ) +
    theme_minimal()
}

# Durschnitts Phrophylaxe Effekt pro Alter Visualisierung 2
averageProphylaxisEffectByAge2 <- function() {
  avgEffectByAge <- enhancedPatients %>%
    group_by(age) %>%
    summarise(
      meanEffect = mean(avgProphylaxisEffect, na.rm = TRUE),
      count = n()
    )

  # Plot
  ggplot() +
    # Alle Datenpunkte als Punkte mit geringer Opazität
    geom_point(
      data = enhancedPatients, aes(x = age, y = avgProphylaxisEffect),
      color = "darkgreen", alpha = 0.2
    ) +
    # Linie für Durchschnittswerte mit höherer Opazität
    geom_line(
      data = avgEffectByAge, aes(x = age, y = meanEffect),
      color = "red", size = 1, alpha = 0.9
    ) +
    labs(
      title = "Durchschnittlicher Prophylaxe-Effektpro Alter",
      x = "Alter (Jahre)",
      y = "Ø Prophylaxe-Effekt"
    ) +
    theme_minimal()
}

## Visualisierung der Prophylaxe Dosierung
# Durschnitts Phrophylaxe Dosierung pro 10 Jahre Altersgruppe
averageProphylaxisDosageBy10YearAgeGroup <- function() {
  avgDosageByAgeGroup <- enhancedPatients %>%
    group_by(ageGroup10) %>%
    summarise(
      meanDosage = mean(avgProphylaxisDosage, na.rm = TRUE),
      count = n()
    )

  ggplot(avgDosageByAgeGroup, aes(x = ageGroup10, y = meanDosage)) +
    geom_col(fill = "darkgreen") +
    geom_text(aes(label = round(meanDosage, 2)), vjust = -0.5) +
    labs(
      title = "Durchschnittliche Prophylaxe-Dosierung pro Altersgruppe",
      x = "Altersgruppe (Jahre)",
      y = "Ø Prophylaxe-Dosierung"
    ) +
    theme_minimal()
}

# Durchschnittliche Dosierung pro Wechseljahres-Gruppe berechnen
averageProphylaxisDosageByMenopause <- function() {
  avgDosageByMenopause <- enhancedPatients %>%
    group_by(menopause) %>%
    summarise(
      meanDosage = mean(avgProphylaxisDosage, na.rm = TRUE),
      count = n()
    )

  ggplot(avgDosageByMenopause, aes(x = as.factor(menopause), y = meanDosage)) +
    geom_col(fill = "darkgreen") +
    geom_text(aes(label = meanDosage), vjust = -0.5) +
    labs(
      title = "Durchschnittliche Dosierung nach Wechseljahresstatus",
      x = "Wechseljahre (FALSE = Nein, TRUE = Ja)",
      y = "Ø Prophylaxe-Dosierung"
    ) +
    theme_minimal()
}

# Durschnitts Phrophylaxe Dosierung pro Alter 1
averageProphylaxisDosageByAge1 <- function() {
  avgDosageByAge <- enhancedPatients %>%
    group_by(age) %>%
    summarise(
      meanDosage = mean(avgProphylaxisDosage, na.rm = TRUE),
      count = n()
    )

  ggplot(avgDosageByAge, aes(x = age, y = meanDosage)) +
    geom_col(fill = "darkgreen") +
    geom_text(aes(label = round(meanDosage, 0)), vjust = -0.5) +
    labs(
      title = "Durchschnittliche Prophylaxe-Dosierung pro Alter",
      x = "Alter (Jahre)",
      y = "Ø Prophylaxe-Dosierung"
    ) +
    theme_minimal()
}

# Durschnitts Phrophylaxe Dosierung pro Alter Visualisierung 2
averageProphylaxisDosageByAge2 <- function() {
  avgDosageByAge <- enhancedPatients %>%
    group_by(age) %>%
    summarise(
      meanDosage = mean(avgProphylaxisDosage, na.rm = TRUE),
      count = n()
    )

  # Plot
  ggplot() +
    # Alle Datenpunkte als Punkte mit geringer Opazität
    geom_point(
      data = enhancedPatients, aes(x = age, y = avgProphylaxisDosage),
      color = "darkgreen", alpha = 0.2
    ) +
    # Linie für Durchschnittswerte mit höherer Opazität
    geom_line(
      data = avgDosageByAge, aes(x = age, y = meanDosage),
      color = "red", size = 1, alpha = 0.9
    ) +
    labs(
      title = "Durchschnittliche Prophylaxe-Dosierung pro Alter",
      x = "Alter (Jahre)",
      y = "Ø Prophylaxe-Dosierung"
    ) +
    theme_minimal()
}

## Visualisierung der Prophylaxe Tolerability
# Durschnitts Prophylaxe Tolerability pro 10 Jahre Altersgruppe
averageProphylaxisTolerabilityBy10YearAgeGroup <- function() {
  avgTolerabilityByAgeGroup <- enhancedPatients %>%
    group_by(ageGroup10) %>%
    summarise(
      meanTolerability = mean(avgProphylaxisTolerability, na.rm = TRUE),
      count = n()
    )

  ggplot(avgTolerabilityByAgeGroup, aes(x = ageGroup10, y = meanTolerability)) +
    geom_col(fill = "darkgreen") +
    geom_text(aes(label = round(meanTolerability, 2)), vjust = -0.5) +
    labs(
      title = "Durchschnittlicher Prophylaxe-Tolerability-Wert pro Altersgruppe",
      x = "Altersgruppe (Jahre)",
      y = "Ø Prophylaxe-Tolerability"
    ) +
    theme_minimal()
}

# Durchschnittliche Tolerability pro Wechseljahres-Gruppe berechnen
averageProphylaxisTolerabilityByMenopause <- function() {
  avgTolerabilityByMenopause <- enhancedPatients %>%
    group_by(menopause) %>%
    summarise(
      meanTolerability = mean(avgProphylaxisTolerability, na.rm = TRUE),
      count = n()
    )

  ggplot(avgTolerabilityByMenopause, aes(x = as.factor(menopause), y = meanTolerability)) +
    geom_col(fill = "darkgreen") +
    geom_text(aes(label = meanTolerability), vjust = -0.5) +
    labs(
      title = "Durchschnittliche Tolerability nach Wechseljahresstatus",
      x = "Wechseljahre (FALSE = Nein, TRUE = Ja)",
      y = "Ø Prophylaxe-Tolerability"
    ) +
    theme_minimal()
}

# Durschnitts Phrophylaxe Tolerability pro Alter
averageProphylaxisTolerabilityByAge <- function() {
  avgTolerabilityByAge <- enhancedPatients %>%
    group_by(age) %>%
    summarise(
      meanTolerability = mean(avgProphylaxisEffect, na.rm = TRUE),
      count = n()
    )

  ggplot(avgTolerabilityByAge, aes(x = age, y = meanTolerability)) +
    geom_col(fill = "darkgreen") +
    geom_text(aes(label = round(meanTolerability, 1)), vjust = -0.5) +
    labs(
      title = "Durchschnittliche Prophylaxe-Tolerability pro Alter",
      x = "Alter (Jahre)",
      y = "Ø Prophylaxe-Tolerability"
    ) +
    theme_minimal()
}

## Visualisierung ob Prophylaxemedikamenten abgesetzt
abortedProphylaxisMedDistribution <- function() {
  ggplot(enhancedPatients, aes(x = abortedProphylaxeMed)) +
    geom_bar(stat = "count", fill = "steelblue", color = "white") +
    geom_text(aes(label = after_stat(count)), stat = "count", position = position_stack(vjust = 0.5)) +
    labs(title = "Verteilung abgesetzter Prophylaxemedikamente", x = "Prophylaxemedikamente abgesetzt", y = "Anzahl") +
    theme_minimal()
}

# nach Wechelsjahren
abortedProphylaxisMedDistributionByMenopause <- function() {
  ggplot(enhancedPatients, aes(x = abortedProphylaxeMed, fill = abortedProphylaxeMed)) +
    geom_bar(stat = "count", color = "white") +
    scale_fill_manual(values = c("steelblue", "darkgreen")) +
    facet_wrap(~menopause) +
    labs(
      title = "Verteilung von abgesetzten Prophlaxemedikamenten nach Wechseljahren",
      x = "Prophylaxemedikation abgesetzt",
      y = "Anzahl"
    ) +
    geom_text(aes(label = after_stat(count)), stat = "count", position = position_stack(vjust = 0.5)) +
    scale_x_discrete(labels = c("TRUE" = "Ja", "FALSE" = "Nein")) +
    theme_minimal()
}

# nach Geschlecht
abortedProphylaxisMedDistributionByGender <- function() {
  ggplot(enhancedPatients, aes(x = abortedProphylaxeMed, fill = abortedProphylaxeMed)) +
    geom_bar(stat = "count", color = "white") +
    scale_fill_manual(values = c("steelblue", "darkgreen")) +
    facet_wrap(~gender) +
    labs(
      title = "Verteilung von abgesetzten Prophlaxemedikamenten nach Geschlecht",
      x = "Prophylaxemedikation abgesetzt",
      y = "Anzahl"
    ) +
    geom_text(aes(label = after_stat(count)), stat = "count", position = position_stack(vjust = 0.5)) +
    scale_x_discrete(labels = c("TRUE" = "Ja", "FALSE" = "Nein")) +
    theme_minimal()
}

## Visualisierung der Verteilung der Anzahl an abgesetzten Prophylaxe Medikamente
abortedProphylaxisMedAmountDistribution <- function() {
  ggplot(enhancedPatients, aes(x = nAbortedProphylaxeMed)) +
    geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
    geom_text(aes(label = after_stat(count)), stat = "count", position = position_stack(vjust = 0.5), angle = 90) +
    labs(title = "Verteilung Anzahl abgesetzter Prophylaxemedikamente", x = "Anzahl abgesetzter Prophylaxe Medikamente", y = "Anzahl Personen") +
    theme_minimal()
}

# Nach Wechseljahren %
abortedProphylaxisMedAmountDistributionByMenopauseRelativ <- function() {
  ggplot(enhancedPatients, aes(x = nAbortedProphylaxeMed)) +
    facet_wrap(~menopause) +
    geom_histogram(aes(y = after_stat(density)), binwidth = 1, fill = "steelblue", color = "white") +
    labs(title = "Verteilung Anzahl abgesetzter Prophylaxemedikamente nach Wechseljahren", x = "Anzahl abgesetzter Prophylaxe Medikamente", y = "Prozent der Patienten") +
    theme_minimal()
}

# Nach Wechseljahren absolut
abortedProphylaxisMedAmountDistributionByMenopauseAbsolute <- function() {
  ggplot(enhancedPatients, aes(x = nAbortedProphylaxeMed)) +
    facet_wrap(~menopause) +
    geom_histogram(aes(y = after_stat(count)), binwidth = 1, fill = "steelblue", color = "white") +
    labs(title = "Verteilung Anzahl abgesetzter Prophylaxemedikamente nach Wecheljahren", x = "Anzahl abgesetzter Prophylaxe Medikamente", y = "Prozent der Patienten") +
    geom_text(aes(label = after_stat(count)), stat = "count", position = position_stack(vjust = 0.5), size = 3.0, angle = 90) +
    theme_minimal()
}

# Nach Geschlecht %
abortedProphylaxisMedAmountDistributionByGenderRelativ <- function() {
  ggplot(enhancedPatients, aes(x = nAbortedProphylaxeMed)) +
    facet_wrap(~gender) +
    geom_histogram(aes(y = after_stat(density)), binwidth = 1, fill = "steelblue", color = "white") +
    labs(title = "Verteilung Anzahl abgesetzter Prophylaxemedikamente nach Geschlecht", x = "Anzahl abgesetzter Prophylaxe Medikamente", y = "Prozent der Patienten") +
    theme_minimal()
}

# Nach Geschlecht absolut
abortedProphylaxisMedAmountDistributionByGenderAbsolute <- function() {
  ggplot(enhancedPatients, aes(x = nAbortedProphylaxeMed), ) +
    facet_wrap(~gender) +
    geom_histogram(aes(y = after_stat(count)), binwidth = 1, fill = "steelblue", color = "white") +
    labs(title = "Verteilung Anzahl abgesetzter Prophylaxemedikamente nach Geschlecht", x = "Anzahl abgesetzter Prophylaxe Medikamente", y = "Prozent der Patienten") +
    geom_text(aes(label = after_stat(count)), stat = "count", position = position_stack(vjust = 0.5), size = 3.0) +
    theme_minimal()
}

# Plot für allgemeine Medikamentennutzung
plotMedicationUsageGeneral <- function() {
  ggplot(uniqueMedsGeneral, aes(x = medication, y = usage)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(title = "Medikamentenverwendung allgemein", x = "Medikament", y = "Verwendung") +
    geom_text(aes(label = usage), size = 3.0, position = position_stack(), hjust = -0.5) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2)))
}

# Plot für Medikamentennutzung in Wechseljahren
plotMedicationUsageMenopause <- function() {
  ggplot(uniqueMedsMenopause, aes(x = medication, y = usage)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(title = "Medikamentenverwendung in Wechseljahren", x = "Medikament", y = "Verwendung") +
    geom_text(aes(label = usage), size = 3.0, position = position_stack(), hjust = -0.5) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2)))
}

# Plot für Medikamentennutzung außerhalb der Wechseljahre
plotMedicationUsageNotMenopause <- function() {
  ggplot(uniqueMedsNotMenopasue, aes(x = medication, y = usage)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(title = "Medikamentenverwendung außerhalb Wechseljahre", x = "Medikament", y = "Verwendung") +
    geom_text(aes(label = usage), size = 3.0, position = position_stack(), hjust = -0.5) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2)))
}

# Vergleich der Medikamentennutzung zwischen Wechseljahren und nicht Wechseljahren
plotMedicationUsageComparison <- function() {
  medUsageMenopause <- plotMedicationUsageMenopause()
  medUsageNotMenopause <- plotMedicationUsageNotMenopause()
  medUsageMenopause + medUsageNotMenopause + plot_layout(ncol = 2)
}

# Plot für allgemeine Medikamentengruppen-Verwendung
plotMedicationGroupsGeneral <- function() {
  ggplot(groupedMedsGeneral, aes(x = group, y = usage)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(title = "Medikamentengruppen - Allgemeine Verwendung", x = "Gruppe", y = "Anzahl") +
    geom_text(aes(label = usage), size = 3.0, position = position_stack(), hjust = -0.5) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2)))
}

# Plot für Medikamentengruppen-Verwendung in Wechseljahren
plotMedicationGroupsMenopause <- function() {
  ggplot(groupedMedsMenopause, aes(x = group, y = usage)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(title = "Medikamentengruppen - Wechseljahre", x = "Gruppe", y = "Anzahl") +
    geom_text(aes(label = usage), size = 3.0, position = position_stack(), hjust = -0.5) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2)))
}

# Plot für Medikamentengruppen-Verwendung außerhalb der Wechseljahre
plotMedicationGroupsNotMenopause <- function() {
  ggplot(groupedMedsNotMenopause, aes(x = group, y = usage)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(title = "Medikamentengruppen - Außerhalb Wechseljahre", x = "Gruppe", y = "Anzahl") +
    geom_text(aes(label = usage), size = 3.0, position = position_stack(), hjust = -0.5) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2)))
}

# Vergleich der Medikamentengruppen-Verwendung
plotMedicationGroupsComparison <- function() {
  medGroupsMenopause <- plotMedicationGroupsMenopause()
  medGroupsNotMenopause <- plotMedicationGroupsNotMenopause()
  medGroupsMenopause + medGroupsNotMenopause + plot_layout(ncol = 2)
}

# Plot für Vergleich der Medikamentengruppen-Nutzung (absolute Zahlen)
plotMedicationGroupsComparisonAbs <- function() {
  ggplot(grouped_comparison, aes(x = group, y = usage, fill = status)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      title = "Vergleich der Medikamentengruppen-Nutzung (absolute Zahlen)",
      x = "Medikamentengruppe", y = "Anzahl", fill = "Status"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("Menopause" = "darkred", "Nicht-Menopause" = "darkgreen"))
}

# Plot für Vergleich der Medikamentengruppen-Nutzung (prozentual)
plotMedicationGroupsComparisonPercent <- function() {
  ggplot(grouped_comparison_percent, aes(x = group, y = percentage, fill = status)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      title = "Vergleich der Medikamentengruppen-Nutzung (prozentual)",
      x = "Medikamentengruppe", y = "Prozent (%)", fill = "Status"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("Menopause" = "darkred", "Nicht-Menopause" = "darkgreen")) +
    geom_text(aes(label = sprintf("%.1f%%", percentage)),
              position = position_dodge(width = 0.9),
              vjust = -0.5, size = 3
    )
}

# Plot für Vergleich der Medikamentengruppen-Nutzung (absolute und prozentuale Werte)
plotMedicationGroupsComparisonAbsAndPercent <- function() {
  plotAbs <- plotMedicationGroupsComparisonAbs()
  plotPercent <- plotMedicationGroupsComparisonPercent()
  plotAbs / plotPercent + plot_layout(ncol = 1)
}
