# nolint start
### Main (angeben welche Plot-Funktion ausgeführt werden soll):
# agedistributionBinwidth1()
# agedistributionBinwidth5()
# genderDistribution()
# menopauseDistribution()
# menopauseDistributionByGender()
# prophylaxisMedicationDistribution()
# prophylaxisMedicationDistributionByMenopause()
# prophylaxisMedicationDistributionByAge()
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

### Statistische Tests
# testProphylaxisMedicationByMenopause()
# testProphylaxisMedicationByMenopauseWilcoxon()
### Plots zur Medikamentenverteilung

# plotMedicationUsageGeneral()
# plotMedicationUsageComparison()
# plotMedicationUsageAgeGroups()
# plotMedicationUsageAgeGroup1()
# plotMedicationUsageAgeGroup2()
# plotMedicationUsageAgeGroup3()
# plotMedicationUsageAllAgeGroups()
# plotMedicationGroupsGeneral()
# plotMedicationGroupsComparison()
# plotMedicationGroupsAllAgeGroups()
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
library(glue)
library(patchwork)

### Initialisiere Variablen
## Festlegen welche Visite
curAllPatients <- PatientenV2

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
  patients$menopause <- patients$age < menopauseEnd & patients$age >= menopauseStart
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

  # Remove unused columns
  # columns_to_remove <- c(
  #   grep("_a\\d{1,2}_K", names(patients), value = TRUE),
  #   grep("_fa_", names(patients), value = TRUE)
  # )
  # if(length(columns_to_remove) > 0) {
  #   patients <- patients %>% select(-all_of(columns_to_remove))
  # }

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
  uniqueMedsNotMenopause <<- result[[3]][result[[3]]$medication != "0", ]

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
  breaks = c(menopauseStart - 10, menopauseStart, menopauseEnd, menopauseEnd + 10),
  right = FALSE,
  labels = c(glue("{menopauseStart-10}–{menopauseStart-1}"), glue("{menopauseStart}–{menopauseEnd-1}"), glue("{menopauseEnd}-{menopauseEnd+9}"))
)

### Plots zur Datenvisualisierung
## Visualisierung der Altersverteilung
# agedistribution binwidth 1
agedistributionBinwidth1 <- function() {
  # Erstelle eine neue Variable für die Füllfarbe:
  enhancedPatients$menopause_status <- ifelse(
    enhancedPatients$menopause,
    "Wechseljahre",
    ifelse(!is.na(enhancedPatients$ageGroup10), "Nicht-Wechseljahre (in ageGroup10)", "Nicht-Wechseljahre (außerhalb ageGroup10)")
  )

  ggplot(enhancedPatients, aes(x = age, fill = menopause_status)) +
    geom_histogram(binwidth = 1, color = "white") +
    scale_fill_manual(
      values = c(
        "Wechseljahre" = "darkgreen",
        "Nicht-Wechseljahre (in ageGroup10)" = "orange",
        "Nicht-Wechseljahre (außerhalb ageGroup10)" = "steelblue"
      ),
      breaks = c("Wechseljahre", "Nicht-Wechseljahre (in ageGroup10)", "Nicht-Wechseljahre (außerhalb ageGroup10)"),
      labels = c("Wechseljahre", "Nicht-Wechseljahre (in ageGroup10)", "Nicht-Wechseljahre (außerhalb ageGroup10)")
    ) +
    labs(
      title = "Altersverteilung",
      x = "Alter",
      y = "Anzahl Personen",
      fill = "Status"
    ) +
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
  # Filtere NA Werte
  filtered_data <- enhancedPatients[!is.na(enhancedPatients$ageGroup10), ]

  # Erstelle numerische Variable für Prophylaxemedikation (0 = nein, 1 = ja)
  filtered_data$prophylaxeMed_numeric <- as.numeric(filtered_data$prophylaxeMed)

  # Extrahiere die drei Gruppen
  groups <- levels(as.factor(filtered_data$ageGroup10))

  # Führe paarweise Mann-Whitney-U-Tests durch
  group_combinations <- combn(groups, 2, simplify = FALSE)
  p_values <- c()
  comparison_names <- c()

  for (i in seq_along(group_combinations)) {
    group1 <- group_combinations[[i]][1]
    group2 <- group_combinations[[i]][2]

    # Extrahiere Daten für beide Gruppen
    data1 <- filtered_data[filtered_data$ageGroup10 == group1, "prophylaxeMed_numeric"]
    data2 <- filtered_data[filtered_data$ageGroup10 == group2, "prophylaxeMed_numeric"]

    # Entferne NA Werte
    data1 <- data1[!is.na(data1)]
    data2 <- data2[!is.na(data2)]

    # Führe Mann-Whitney-U-Test durch
    if (length(data1) > 0 && length(data2) > 0) {
      wilcox_test <- wilcox.test(data1, data2, exact = FALSE)
      p_values <- c(p_values, wilcox_test$p.value)
      comparison_names <- c(comparison_names, paste(group1, "vs", group2))
    }
  }

  # Bonferroni-Korrektur
  adjusted_p_values <- p.adjust(p_values, method = "bonferroni")

  # Erstelle Signifikanz-Notationen für jede Gruppe
  # Für jede Gruppe sammeln wir alle relevanten p-Werte
  group_significance <- list()

  for (group in groups) {
    # Finde alle Vergleiche, die diese Gruppe betreffen
    group_p_values <- c()
    for (i in seq_along(group_combinations)) {
      if (group %in% group_combinations[[i]]) {
        group_p_values <- c(group_p_values, adjusted_p_values[i])
      }
    }

    # Nimm den niedrigsten (bedeutsamsten) p-Wert für diese Gruppe
    min_p <- min(group_p_values, na.rm = TRUE)

    # Bestimme Signifikanz-Notation
    if (min_p < 0.001) {
      group_significance[[group]] <- "***"
    } else if (min_p < 0.01) {
      group_significance[[group]] <- "**"
    } else if (min_p < 0.05) {
      group_significance[[group]] <- "*"
    } else {
      group_significance[[group]] <- "n.s."
    }
  }

  # Berechne maximale Balkenhöhe für jede Gruppe für Annotation-Positionierung
  count_data <- filtered_data %>%
    group_by(ageGroup10, prophylaxeMed) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(ageGroup10) %>%
    summarise(max_count = max(count), .groups = "drop")

  # Erstelle Annotations-Dataframe
  annotations <- data.frame(
    ageGroup10 = groups,
    significance = sapply(groups, function(g) group_significance[[g]]),
    y_pos = sapply(groups, function(g) {
      max_count <- count_data$max_count[count_data$ageGroup10 == g]
      if (length(max_count) > 0) max_count + max_count * 0.1 else 1
    }),
    stringsAsFactors = FALSE
  )

  ggplot(filtered_data, aes(x = prophylaxeMed, fill = prophylaxeMed)) +
    geom_bar(stat = "count", color = "white") +
    scale_fill_manual(
      values = c("steelblue", "darkgreen"),
      labels = c("FALSE" = "Nein", "TRUE" = "Ja"),
      name = "Prophylaxemedikation"
    ) +
    facet_wrap(~ageGroup10) +
    # Füge Signifikanz-Annotationen hinzu
    geom_text(
      data = annotations,
      aes(x = 1.5, y = y_pos, label = significance),
      inherit.aes = FALSE,
      size = 4,
      fontface = "bold",
      color = "red"
    ) +
    labs(
      title = "Verteilung von Prophylaxemedikamenten nach Altersgruppen",
      x = "Prophylaxemedikation",
      y = "Anzahl"
    ) +
    geom_text(aes(label = after_stat(count)), stat = "count", position = position_stack(vjust = 0.5)) +
    scale_x_discrete(labels = c("TRUE" = "Ja", "FALSE" = "Nein")) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 11, hjust = 0.5),
      strip.text = element_text(size = 10)
    )
}

prophylaxisMedicationDistributionByAge <- function() {
  # Create age groups for better visualization
  enhancedPatients$ageGroup <- cut(
    enhancedPatients$age,
    breaks = seq(min(enhancedPatients$age, na.rm = TRUE),
      max(enhancedPatients$age, na.rm = TRUE) + 5,
      by = 5
    ),
    right = FALSE
  )

  ggplot(enhancedPatients, aes(x = ageGroup, fill = prophylaxeMed)) +
    geom_bar(position = "dodge", color = "white") +
    scale_fill_manual(
      values = c("steelblue", "darkgreen"),
      labels = c("FALSE" = "Nein", "TRUE" = "Ja"),
      name = "Prophylaxemedikation"
    ) +
    labs(
      title = "Verteilung von Prophylaxemedikamenten nach Alter",
      x = "Altersgruppe",
      y = "Anzahl"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
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

# Nach Altersgruppe %
prophylaxisMedicationAmountDistributionByMenopauseRelativ <- function() {
  # Filter patients with valid ageGroup10 data
  filteredPatients <- enhancedPatients %>%
    filter(!is.na(ageGroup10))

  ggplot(filteredPatients, aes(x = nProphylaxeMed)) +
    facet_wrap(~ageGroup10, scales = "free") +
    geom_histogram(aes(y = after_stat(density)), binwidth = 1, fill = "steelblue", color = "white") +
    labs(
      title = "Verteilung Anzahl Prophylaxemedikamente nach Altersgruppe",
      x = "Anzahl Prophylaxe Medikamente",
      y = "Prozent der Patienten"
    ) +
    scale_x_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 1)) +
    theme_minimal()
}

# Nach Altersgruppe absolut
prophylaxisMedicationAmountDistributionByMenopauseAbsolute <- function() {
  # Filter patients with valid ageGroup10 data
  filteredPatients <- enhancedPatients %>%
    filter(!is.na(ageGroup10))

  ggplot(filteredPatients, aes(x = nProphylaxeMed)) +
    facet_wrap(~ageGroup10, scales = "free") +
    geom_histogram(aes(y = after_stat(count)), binwidth = 1, fill = "steelblue", color = "white") +
    labs(
      title = "Verteilung Anzahl Prophylaxemedikamente nach Altersgruppe",
      x = "Anzahl Prophylaxe Medikamente",
      y = "Anzahl der Patienten"
    ) +
    geom_text(aes(label = after_stat(count)), stat = "count", position = position_stack(vjust = 0.5), size = 3.0, angle = 90) +
    scale_x_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 1)) +
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
  filteredPatients <- enhancedPatients %>%
    filter(!is.na(ageGroup10))
  avgEffectByAgeGroup <- filteredPatients %>%
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
  filteredPatients <- enhancedPatients %>%
    filter(!is.na(ageGroup10))
  avgDosageByAgeGroup <- filteredPatients %>%
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
  filteredPatients <- enhancedPatients %>%
    filter(!is.na(ageGroup10))
  avgTolerabilityByAgeGroup <- filteredPatients %>%
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

## Visualisierung der Verteilung des Alters bei Absetzten eines Prophylaxe Medikaments
# Nach Alter absolut
abortedAgeProphylaxisMedDistributionByAgeAbsolute <- function() {
  aborted_cols <- grep("^abortedAge_", names(enhancedPatients), value = TRUE)
  aborted_all <- as.numeric(unlist(enhancedPatients[, aborted_cols]))
  aborted_all <- aborted_all[!is.na(aborted_all)]
  aborted_df <- data.frame(abortedAge = aborted_all)

  print(summary(aborted_df$abortedAge))
  ggplot(data = aborted_df, aes(x = abortedAge)) +
    geom_histogram(binwidth = 1, fill = "firebrick", color = "white", alpha = 0.8) +
    labs(
      title = "Alter beim Absetzen von Prophylaxemedikamenten",
      x = "Abgesetzt-Alter (Jahre)",
      y = "Anzahl abgesetzter Medikamente"
    ) +
    theme_minimal()
}

# Nach Wechseljahren absolut
abortedAgeProphylaxisMedDistributionByMenopauseAbsolute <- function() {
  # Relevante Spalten extrahieren
  aborted_cols <- grep("^abortedAge_", names(enhancedPatients), value = TRUE)

  # Alle Werte in einen Vektor umwandeln
  aborted_all <- as.numeric(unlist(enhancedPatients[, aborted_cols]))
  aborted_all <- aborted_all[!is.na(aborted_all)]

  # Klassifizieren, ob innerhalb oder außerhalb der Wechseljahre
  menopauseStatus <- ifelse(
    aborted_all >= menopauseStart & aborted_all <= menopauseEnd,
    "innerhalb Wechseljahre",
    "außerhalb Wechseljahre"
  )

  # Dataframe erstellen
  aborted_df <- data.frame(
    abortedAge = aborted_all,
    menopauseStatus = menopauseStatus
  )

  # Plot
  ggplot(aborted_df, aes(x = menopauseStatus)) +
    geom_bar(fill = "steelblue", color = "white", alpha = 0.9) +
    geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
    labs(
      title = "Abgesetzte Prophylaxemedikamente in/außerhalb der Wechseljahre",
      x = "Menopause-Status",
      y = "Anzahl abgesetzter Medikamente"
    ) +
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

# Plot für Medikamentennutzung nach Altersgruppen
plotMedicationUsageAgeGroups <- function() {
  # Erstelle Daten für alle Altersgruppen
  ageGroups <- unique(enhancedPatients$ageGroup10[!is.na(enhancedPatients$ageGroup10)])

  # Ermittle alle verfügbaren Medikamente aus allen Daten
  columns <- grep("preventive_name_p", names(enhancedPatients), value = TRUE)
  allMeds <- unlist(lapply(enhancedPatients[, columns], function(x) as.character(x)))
  allMeds <- sort(unique(allMeds))
  allMeds <- allMeds[allMeds != "0"] # Entferne "0" aus der Liste

  plots <- list()

  for (ageGroup in ageGroups) {
    # Filtere Daten für diese Altersgruppe
    ageGroupData <- enhancedPatients %>% filter(ageGroup10 == ageGroup)

    # Berechne Medikamentennutzung für diese Altersgruppe
    meds_each_column <- list()
    for (column in columns) {
      freqMeds <- plyr::count(ageGroupData, column)
      meds_each_column <- append(meds_each_column, freqMeds)
    }

    resultAgeGroup <- as.data.frame(transformMedicationData(meds_each_column))
    resultAgeGroup <- resultAgeGroup[resultAgeGroup$medication != "0", ]

    # Füge fehlende Medikamente mit 0 Verwendung hinzu
    missing_medications <- setdiff(allMeds, resultAgeGroup$medication)
    if (length(missing_medications) > 0) {
      missing_df <- data.frame(
        medication = missing_medications,
        usage = rep(0, length(missing_medications))
      )
      resultAgeGroup <- rbind(resultAgeGroup, missing_df)
    }

    # Sortiere nach Medikamentenname für konsistente Darstellung
    resultAgeGroup <- resultAgeGroup[order(resultAgeGroup$medication), ]

    # Umbreche lange Medikamentennamen
    resultAgeGroup$medication <- stringr::str_wrap(resultAgeGroup$medication, width = 20)

    # Erstelle Plot für diese Altersgruppe
    plot <- ggplot(resultAgeGroup, aes(x = medication, y = usage)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(
        title = stringr::str_wrap(paste("Medikamentenverwendung Altersgruppe", ageGroup), width = 30),
        x = "Medikament", y = "Verwendung"
      ) +
      geom_text(aes(label = usage), size = 3.0, position = position_stack(), hjust = -0.5) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12, hjust = 0.5, margin = margin(b = 10)),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9),
        axis.title = element_text(size = 10)
      )

    plots[[ageGroup]] <- plot
  }

  return(plots)
}

# Vergleich der Medikamentennutzung zwischen allen Altersgruppen
plotMedicationUsageComparison <- function() {
  plots <- plotMedicationUsageAgeGroups()

  if (length(plots) == 0) {
    return(ggplot() +
      labs(title = "Keine Daten verfügbar"))
  }

  # Kombiniere alle Plots in einem Layout
  if (length(plots) == 1) {
    return(plots[[1]])
  } else if (length(plots) == 2) {
    return(plots[[1]] + plots[[2]] + plot_layout(ncol = 2))
  } else if (length(plots) == 3) {
    return(plots[[1]] + plots[[2]] + plots[[3]] + plot_layout(ncol = 3))
  } else if (length(plots) == 4) {
    return(plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]] + plot_layout(ncol = 2, nrow = 2))
  } else {
    # Für mehr als 4 Plots, verwende wrap_plots
    return(wrap_plots(plots, ncol = 3))
  }
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

# Plot für Medikamentengruppen-Verwendung nach Altersgruppen
plotMedicationGroupsAgeGroups <- function() {
  # Erstelle Daten für alle Altersgruppen
  ageGroups <- unique(enhancedPatients$ageGroup10[!is.na(enhancedPatients$ageGroup10)])

  # Ermittle alle verfügbaren Medikamentengruppen
  allGroups <- names(med_groups)

  plots <- list()

  for (ageGroup in ageGroups) {
    # Filtere Daten für diese Altersgruppe
    ageGroupData <- enhancedPatients %>% filter(ageGroup10 == ageGroup)

    # Überprüfe ob genügend Daten vorhanden sind
    if (nrow(ageGroupData) == 0) {
      warning(paste("Keine Daten für Altersgruppe", ageGroup))
      next
    }

    # Berechne Medikamentengruppen-Nutzung für diese Altersgruppe (vereinfachter Ansatz)
    tryCatch(
      {
        # Hole alle Medikamenten-Spalten
        columns <- grep("preventive_name_p", names(ageGroupData), value = TRUE)

        # Sammle alle Medikamente aus allen Spalten
        allMedsInAge <- c()
        for (column in columns) {
          meds <- as.character(ageGroupData[[column]])
          meds <- meds[meds != "0" & !is.na(meds)]
          allMedsInAge <- c(allMedsInAge, meds)
        }

        # Zähle die Häufigkeit jedes Medikaments
        medCounts <- table(allMedsInAge)

        # Erstelle Ergebnis-Dataframe für Gruppen
        resultAgeGroup <- data.frame(
          group = allGroups,
          usage = rep(0, length(allGroups)),
          stringsAsFactors = FALSE
        )

        # Zuordnung der Medikamente zu Gruppen
        for (i in 1:length(allGroups)) {
          groupName <- allGroups[i]
          groupMeds <- med_groups[[groupName]]

          # Zähle Verwendung für diese Gruppe
          groupUsage <- 0
          for (med in groupMeds) {
            if (med %in% names(medCounts)) {
              groupUsage <- groupUsage + medCounts[[med]]
            }
          }
          resultAgeGroup$usage[i] <- groupUsage
        }

        # Sortiere nach Gruppenname für konsistente Darstellung
        resultAgeGroup <- resultAgeGroup[order(resultAgeGroup$group), ]

        # Umbreche lange Gruppennamen
        resultAgeGroup$group <- stringr::str_wrap(resultAgeGroup$group, width = 15)

        # Erstelle Plot für diese Altersgruppe
        plot <- ggplot(resultAgeGroup, aes(x = group, y = usage)) +
          geom_bar(stat = "identity", fill = "steelblue") +
          coord_flip() +
          labs(
            title = stringr::str_wrap(paste("Medikamentengruppen Altersgruppe", ageGroup), width = 25),
            x = "Gruppe", y = "Anzahl"
          ) +
          geom_text(aes(label = usage), size = 3.0, position = position_stack(), hjust = -0.5) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 12, hjust = 0.5, margin = margin(b = 10)),
            axis.text.y = element_text(size = 9),
            axis.text.x = element_text(size = 9),
            axis.title = element_text(size = 10)
          )

        plots[[ageGroup]] <- plot
      },
      error = function(e) {
        warning(paste("Fehler bei Altersgruppe", ageGroup, ":", e$message))
      }
    )
  }

  return(plots)
}

# Vergleich der Medikamentengruppen-Verwendung zwischen allen Altersgruppen
plotMedicationGroupsComparison <- function() {
  plots <- plotMedicationGroupsAgeGroups()

  if (length(plots) == 0) {
    return(ggplot() +
      labs(title = "Keine Daten verfügbar"))
  }

  # Kombiniere alle Plots in einem Layout
  if (length(plots) == 1) {
    return(plots[[1]])
  } else if (length(plots) == 2) {
    return(plots[[1]] + plots[[2]] + plot_layout(ncol = 2))
  } else if (length(plots) == 3) {
    return(plots[[1]] + plots[[2]] + plots[[3]] + plot_layout(ncol = 3))
  } else if (length(plots) == 4) {
    return(plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]] + plot_layout(ncol = 2, nrow = 2))
  } else {
    # Für mehr als 4 Plots, verwende wrap_plots
    return(wrap_plots(plots, ncol = 3))
  }
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

## Statistische Tests
# Chi-Quadrat-Test für Prophylaxemedikamentenverteilung nach Menopause
testProphylaxisMedicationByMenopause <- function() {
  # Erstelle Kontingenztabelle
  contingency_table <- table(enhancedPatients$ageGroup10, enhancedPatients$prophylaxeMed)

  # Zeige die Kontingenztabelle an
  cat("Kontingenztabelle - Menopause vs. Prophylaxemedikation:\n")
  print(contingency_table)
  cat("\n")

  # Führe Chi-Quadrat-Test durch
  chi_test <- chisq.test(contingency_table)

  # Zeige Testergebnisse
  cat("Chi-Quadrat-Test Ergebnisse:\n")
  cat("Chi-Quadrat-Statistik:", chi_test$statistic, "\n")
  cat("Freiheitsgrade:", chi_test$parameter, "\n")
  cat("p-Wert:", chi_test$p.value, "\n")
  cat("Signifikanzniveau: α = 0.05\n")

  if (chi_test$p.value < 0.05) {
    cat("Ergebnis: STATISTISCH SIGNIFIKANT (p < 0.05)\n")
    cat("Es gibt einen signifikanten Zusammenhang zwischen Menopause-Status und Prophylaxemedikamenteneinnahme.\n")
  } else {
    cat("Ergebnis: NICHT STATISTISCH SIGNIFIKANT (p ≥ 0.05)\n")
    cat("Es gibt keinen signifikanten Zusammenhang zwischen Menopause-Status und Prophylaxemedikamenteneinnahme.\n")
  }

  # Zusätzliche Informationen
  cat("\nZusätzliche Informationen:\n")

  # Erwartete Häufigkeiten
  cat("Erwartete Häufigkeiten:\n")
  print(chi_test$expected)

  # Cramér's V (Effektgröße)
  n <- sum(contingency_table)
  cramers_v <- sqrt(chi_test$statistic / (n * (min(dim(contingency_table)) - 1)))
  cat("\nCramér's V (Effektgröße):", cramers_v, "\n")

  if (cramers_v < 0.1) {
    cat("Effektgröße: KLEIN\n")
  } else if (cramers_v < 0.3) {
    cat("Effektgröße: MITTEL\n")
  } else {
    cat("Effektgröße: GROSS\n")
  }

  # Proportionen berechnen und anzeigen
  cat("\nProportionen:\n")
  prop_table <- prop.table(contingency_table, margin = 1) * 100
  print(round(prop_table, 1))

  return(list(
    contingency_table = contingency_table,
    chi_test = chi_test,
    cramers_v = cramers_v,
    proportions = prop_table
  ))
}

# Mann-Whitney-U-Tests (Wilcoxon) für paarweise Vergleiche zwischen Altersgruppen
testProphylaxisMedicationByMenopauseWilcoxon <- function() {
  cat("=== MANN-WHITNEY-U-TESTS (WILCOXON) FÜR ALTERSGRUPPEN ===\n\n")

  # Filtere NA Werte
  filtered_data <- enhancedPatients[!is.na(enhancedPatients$ageGroup10), ]

  # Erstelle numerische Variable für Prophylaxemedikation (0 = nein, 1 = ja)
  filtered_data$prophylaxeMed_numeric <- as.numeric(filtered_data$prophylaxeMed)

  # Berechne Anzahl der Prophylaxemedikamente pro Patient
  # (falls diese Variable bereits existiert, sonst verwende prophylaxeMed_numeric)
  if ("medCount" %in% names(filtered_data)) {
    test_variable <- "medCount"
    variable_name <- "Anzahl der Medikamente"
  } else {
    test_variable <- "prophylaxeMed_numeric"
    variable_name <- "Prophylaxemedikation (binär)"
  }

  # Extrahiere die drei Gruppen
  groups <- levels(as.factor(filtered_data$ageGroup10))
  cat("Analysierte Altersgruppen:", paste(groups, collapse = ", "), "\n")
  cat("Analysierte Variable:", variable_name, "\n\n")

  # Erstelle alle möglichen Paarkombinationen
  group_combinations <- combn(groups, 2, simplify = FALSE)

  # Speichere Ergebnisse
  results <- list()
  p_values <- c()

  for (i in seq_along(group_combinations)) {
    group1 <- group_combinations[[i]][1]
    group2 <- group_combinations[[i]][2]

    cat("--- Vergleich:", group1, "vs.", group2, "---\n")

    # Extrahiere Daten für beide Gruppen
    data1 <- filtered_data[filtered_data$ageGroup10 == group1, test_variable]
    data2 <- filtered_data[filtered_data$ageGroup10 == group2, test_variable]

    # Entferne NA Werte
    data1 <- data1[!is.na(data1)]
    data2 <- data2[!is.na(data2)]

    cat("Gruppengröße", group1, ":", length(data1), "\n")
    cat("Gruppengröße", group2, ":", length(data2), "\n")

    # Berechne deskriptive Statistiken
    cat("Median", group1, ":", round(median(data1), 3), "\n")
    cat("Median", group2, ":", round(median(data2), 3), "\n")
    cat("Mittelwert", group1, ":", round(mean(data1), 3), "\n")
    cat("Mittelwert", group2, ":", round(mean(data2), 3), "\n")

    # Führe Mann-Whitney-U-Test durch
    if (length(data1) > 0 && length(data2) > 0) {
      wilcox_test <- wilcox.test(data1, data2, exact = FALSE)

      cat("Mann-Whitney-U-Test Ergebnisse:\n")
      cat("W-Statistik:", wilcox_test$statistic, "\n")
      cat("p-Wert:", round(wilcox_test$p.value, 6), "\n")

      # Effektgröße (r = Z / sqrt(N))
      # Für approximative Berechnung verwenden wir den p-Wert
      n1 <- length(data1)
      n2 <- length(data2)
      total_n <- n1 + n2

      # Approximative Z-Statistik aus p-Wert berechnen
      if (wilcox_test$p.value > 0) {
        z_score <- qnorm(wilcox_test$p.value / 2, lower.tail = FALSE)
        effect_size_r <- z_score / sqrt(total_n)
        cat("Effektgröße (r):", round(effect_size_r, 3), "\n")

        if (effect_size_r < 0.1) {
          cat("Effektgröße: KLEIN\n")
        } else if (effect_size_r < 0.3) {
          cat("Effektgröße: MITTEL\n")
        } else {
          cat("Effektgröße: GROSS\n")
        }
      }

      # Speichere p-Wert für Bonferroni-Korrektur
      p_values <- c(p_values, wilcox_test$p.value)

      # Speichere Ergebnisse
      results[[paste(group1, "vs", group2)]] <- list(
        group1 = group1,
        group2 = group2,
        n1 = n1,
        n2 = n2,
        median1 = median(data1),
        median2 = median(data2),
        mean1 = mean(data1),
        mean2 = mean(data2),
        test_result = wilcox_test,
        effect_size_r = if (exists("effect_size_r")) effect_size_r else NA
      )
    } else {
      cat("WARNUNG: Eine oder beide Gruppen sind leer!\n")
    }

    cat("\n")
  }

  # Bonferroni-Korrektur
  cat("=== BONFERRONI-KORREKTUR ===\n")
  cat("Anzahl Tests:", length(p_values), "\n")
  cat("Korrigiertes Signifikanzniveau:", 0.05 / length(p_values), "\n\n")

  adjusted_p_values <- p.adjust(p_values, method = "bonferroni")

  cat("ZUSAMMENFASSUNG DER ERGEBNISSE:\n")
  cat("Vergleich\t\t\tOriginal p\tKorrigiert p\tSignifikant (α=0.05)\n")
  cat("================================================================================\n")

  for (i in seq_along(group_combinations)) {
    group1 <- group_combinations[[i]][1]
    group2 <- group_combinations[[i]][2]
    comparison_name <- paste(group1, "vs", group2)

    original_p <- round(p_values[i], 6)
    adjusted_p <- round(adjusted_p_values[i], 6)
    significant <- if (adjusted_p_values[i] < 0.05) "JA" else "NEIN"

    cat(sprintf("%-25s\t%.6f\t%.6f\t%s\n", comparison_name, original_p, adjusted_p, significant))
  }

  cat("\n")

  # Interpretation
  significant_comparisons <- sum(adjusted_p_values < 0.05)
  cat("INTERPRETATION:\n")
  cat("Anzahl signifikanter Vergleiche (nach Bonferroni-Korrektur):", significant_comparisons, "von", length(p_values), "\n")

  if (significant_comparisons > 0) {
    cat("Es gibt signifikante Unterschiede zwischen den Altersgruppen bzgl.", variable_name, "\n")
  } else {
    cat("Es gibt keine signifikanten Unterschiede zwischen den Altersgruppen bzgl.", variable_name, "\n")
  }

  return(list(
    results = results,
    p_values = p_values,
    adjusted_p_values = adjusted_p_values,
    variable_tested = variable_name
  ))
}

# Individuelle Funktionen für jede Altersgruppe (Medikamentengruppen)
plotMedicationGroupsAgeGroup1 <- function() {
  plots <- plotMedicationGroupsAgeGroups()
  if (length(plots) > 0) {
    return(plots[[1]])
  } else {
    return(ggplot() +
      labs(title = "Keine Daten verfügbar"))
  }
}

plotMedicationGroupsAgeGroup2 <- function() {
  plots <- plotMedicationGroupsAgeGroups()
  if (length(plots) > 1) {
    return(plots[[2]])
  } else {
    return(ggplot() +
      labs(title = "Keine Daten verfügbar"))
  }
}

plotMedicationGroupsAgeGroup3 <- function() {
  plots <- plotMedicationGroupsAgeGroups()
  if (length(plots) > 2) {
    return(plots[[3]])
  } else {
    return(ggplot() +
      labs(title = "Keine Daten verfügbar"))
  }
}

# Funktion um alle drei Altersgruppen-Plots zu zeigen (Medikamentengruppen)
plotMedicationGroupsAllAgeGroups <- function() {
  plots <- plotMedicationGroupsAgeGroups()

  if (length(plots) == 0) {
    return(ggplot() +
      labs(title = "Keine Daten verfügbar"))
  }

  # Zeige die ersten drei Plots
  plot1 <- if (length(plots) > 0) {
    plots[[1]]
  } else {
    ggplot() +
      labs(title = "Keine Daten")
  }
  plot2 <- if (length(plots) > 1) {
    plots[[2]]
  } else {
    ggplot() +
      labs(title = "Keine Daten")
  }
  plot3 <- if (length(plots) > 2) {
    plots[[3]]
  } else {
    ggplot() +
      labs(title = "Keine Daten")
  }

  return(plot1 + plot2 + plot3 + plot_layout(ncol = 3))
}
# nolint end
