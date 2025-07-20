# nolint start
# Integration der Chi-Quadrat-Test-Ergebnisse in die Medikamentenverteilungsplots
# Diese Datei erweitert die BasicAnalysis.R um statistische Informationen

# Benötigte Libraries
library(ggplot2)
library(patchwork)
library(dplyr)
library(rcompanion)

# Erweiterte Funktionen für Medikamentenanalyse mit statistischen Tests
source("BasicAnalysis.R")

# Funktion um Chi-Quadrat-Tests für Medikamentengruppen durchzuführen
perform_medication_chisq_tests <- function() {
    # Hilfsfunktion für einen einzelnen Test
    chisq_test_medication_stats <- function(patients, test_name = "Test") {
        patients_with_age_group <- patients[!is.na(patients$ageGroup10), ]

        # Get column names that start with "preventive_name_p1_K"
        preventive_cols <- grep("^preventive_name_p1_K", names(patients_with_age_group), value = TRUE)

        if (length(preventive_cols) == 0) {
            warning("Keine preventive_name_p1_K Spalten gefunden")
            return(NULL)
        }

        column <- preventive_cols[1]
        filtered_patients <- subset(patients_with_age_group, patients_with_age_group[, column] != "0")

        if (nrow(filtered_patients) == 0) {
            warning("Keine Patienten mit Medikamenten gefunden")
            return(NULL)
        }

        # Define medication groups (gleich wie in MedicationChisqTest.R)
        med_groups <- list(
            "Betablocker" = c("Propranolol", "Metoprolol", "Bisoprolol"),
            "Flunarizin" = "Flunarizin",
            "Topiramat" = "Topiramat",
            "Amitriptylin" = "Amitriptylin",
            "OnabotulinumtoxinA" = "OnabotulinumtoxinA",
            "CGRP(R)-Ak" = c("Eptinezumab", "Erenumab", "Fremanezumab", "Galcanezumab")
        )

        # Function to map medication to group
        map_med_to_group <- function(medication) {
            for (group_name in names(med_groups)) {
                if (medication %in% med_groups[[group_name]]) {
                    return(group_name)
                }
            }
            return(NA)
        }

        # Map medications to groups
        original_meds <- filtered_patients[[column]]
        med_groups_vector <- sapply(original_meds, map_med_to_group)

        # Filter patients to only include those with medications that map to a group
        filtered_patients <- filtered_patients[!is.na(med_groups_vector), ]
        med_groups_vector <- med_groups_vector[!is.na(med_groups_vector)]

        if (length(med_groups_vector) == 0) {
            warning("Keine Medikamente in definierten Gruppen gefunden")
            return(NULL)
        }

        # Create contingency table
        cont_table <- table(filtered_patients$ageGroup10, med_groups_vector)

        # Calculate tests
        chisq_test <- chisq.test(cont_table)
        expected <- chisq_test$expected
        cohenW_value <- cohenW(cont_table)

        # Determine which test to use
        test_result <- NULL
        p_value <- NULL
        test_type <- ""

        if (min(expected) < 5) {
            fisher_test <- fisher.test(filtered_patients$ageGroup10, med_groups_vector,
                simulate.p.value = TRUE, B = 10000
            )
            test_result <- fisher_test
            p_value <- fisher_test$p.value
            test_type <- "Fisher's Exact Test"
        } else {
            chi_result <- chisq.test(filtered_patients$ageGroup10, med_groups_vector)
            test_result <- chi_result
            p_value <- chi_result$p.value
            test_type <- "Chi-square Test"
        }

        # Effect size interpretation
        effect_interpretation <- case_when(
            cohenW_value < 0.1 ~ "Sehr kleiner Effekt",
            cohenW_value < 0.3 ~ "Kleiner Effekt",
            cohenW_value < 0.5 ~ "Mittlerer Effekt",
            TRUE ~ "Großer Effekt"
        )

        return(list(
            test_name = test_name,
            test_type = test_type,
            p_value = p_value,
            cohens_w = cohenW_value,
            is_significant = p_value < 0.05,
            contingency_table = cont_table,
            n_patients = nrow(filtered_patients),
            effect_size_interpretation = effect_interpretation,
            medication_groups = med_groups
        ))
    }

    # Führe Tests für alle drei Gruppen durch
    results <- list()

    tryCatch(
        {
            results$general <- chisq_test_medication_stats(enhancedPatients, "Allgemein")
            results$female <- chisq_test_medication_stats(
                subset(enhancedPatients, gender == "weiblich"), "Weibliche Patienten"
            )
            results$male <- chisq_test_medication_stats(
                subset(enhancedPatients, gender == "männlich"), "Männliche Patienten"
            )
        },
        error = function(e) {
            warning(paste("Fehler bei der Durchführung der Tests:", e$message))
        }
    )

    return(results)
}

# Funktion um statistische Informationen zu einem Plot hinzuzufügen
add_statistical_annotation <- function(plot, test_result) {
    if (is.null(test_result)) {
        return(plot)
    }

    significance_text <- ifelse(test_result$is_significant,
        paste0("p = ", sprintf("%.4f", test_result$p_value), " *"),
        paste0("p = ", sprintf("%.4f", test_result$p_value), " n.s.")
    )

    stat_text <- paste0(
        test_result$test_type, "\n",
        significance_text, "\n",
        "Cohen's W = ", sprintf("%.3f", test_result$cohens_w), "\n",
        "(", test_result$effect_size_interpretation, ")\n",
        "n = ", test_result$n_patients
    )

    plot +
        annotate("text",
            x = Inf, y = Inf,
            label = stat_text,
            hjust = 1.1, vjust = 1.1,
            size = 3,
            color = ifelse(test_result$is_significant, "darkred", "darkblue"),
            fontface = "bold",
            alpha = 0.8
        )
}

# Erweiterte Plot-Funktionen, die die bestehenden Plots aus BasicAnalysis.R mit Stats erweitern

# Erweiterte allgemeine Medikamentengruppen-Verteilung mit Stats
plotMedicationGroupsGeneral_WithStats <- function(test_results = NULL) {
    if (is.null(test_results)) {
        test_results <- perform_medication_chisq_tests()
    }

    # Verwende die Kontingenztabelle für detaillierte Altersgruppen-Aufschlüsselung
    if (!is.null(test_results$general)) {
        # Erstelle Plot basierend auf der Kontingenztabelle (wie bei geschlechtsspezifischen Plots)
        base_plot <- create_contingency_plot(test_results$general)
        # Ändere den Titel um zu verdeutlichen, dass es die allgemeine Analyse ist
        base_plot <- base_plot +
            labs(title = "Medikamentengruppen: Allgemeine Patienten (nach Altersgruppen)")
        return(base_plot)
    } else {
        # Fallback auf den ursprünglichen Plot falls keine Testergebnisse verfügbar
        base_plot <- plotMedicationGroupsGeneral()
        return(base_plot)
    }
}

# Erweiterte Medikamentengruppen-Verteilung für Wechseljahre mit Stats
plotMedicationGroupsMenopause_WithStats <- function(test_results = NULL) {
    if (is.null(test_results)) {
        test_results <- perform_medication_chisq_tests()
    }

    base_plot <- plotMedicationGroupsMenopause()

    if (!is.null(test_results$female)) {
        return(add_statistical_annotation(base_plot, test_results$female))
    }
    return(base_plot)
}

# Neue Funktion: Vergleich aller drei Gruppen mit statistischen Informationen
plotMedicationGroupsAllComparison_WithStats <- function() {
    test_results <- perform_medication_chisq_tests()

    # Erstelle individuelle Plots mit Stats
    plot_general <- if (!is.null(test_results$general)) {
        plotMedicationGroupsGeneral_WithStats(test_results)
    } else {
        plotMedicationGroupsGeneral()
    }

    plot_female <- if (!is.null(test_results$female)) {
        # Erstelle einen Plot basierend auf der Kontingenztabelle für weibliche Patienten
        create_contingency_plot(test_results$female)
    } else {
        ggplot() +
            labs(title = "Weibliche Patienten - Keine Daten")
    }

    plot_male <- if (!is.null(test_results$male)) {
        # Erstelle einen Plot basierend auf der Kontingenztabelle für männliche Patienten
        create_contingency_plot(test_results$male)
    } else {
        ggplot() +
            labs(title = "Männliche Patienten - Keine Daten")
    }

    # Kombiniere Plots
    combined_plot <- plot_general / plot_female / plot_male +
        plot_layout(ncol = 1) +
        plot_annotation(
            title = "Statistische Analyse: Medikamentengruppen nach Altersgruppen",
            subtitle = "Chi-Quadrat/Fisher's Exact Tests mit Effektgrößen",
            caption = "* = statistisch signifikant (p < 0.05), n.s. = nicht signifikant"
        )

    return(combined_plot)
}

# Hilfsfunktion um Plot aus Kontingenztabelle zu erstellen
create_contingency_plot <- function(test_result) {
    cont_table <- test_result$contingency_table
    plot_data <- as.data.frame(cont_table)
    names(plot_data) <- c("ageGroup", "medication_group", "count")

    base_plot <- ggplot(plot_data, aes(x = medication_group, y = count, fill = ageGroup)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(
            title = paste("Medikamentengruppen:", test_result$test_name),
            x = "Medikamentengruppe",
            y = "Anzahl Patienten",
            fill = "Altersgruppe"
        ) +
        theme_minimal() +
        scale_fill_brewer(type = "qual", palette = "Set3")

    return(add_statistical_annotation(base_plot, test_result))
}

# Funktion für detaillierte Ergebnis-Tabelle
create_detailed_test_summary <- function() {
    test_results <- perform_medication_chisq_tests()

    summary_list <- list()

    for (group_name in names(test_results)) {
        result <- test_results[[group_name]]
        if (!is.null(result)) {
            summary_list[[group_name]] <- data.frame(
                Gruppe = result$test_name,
                Test = result$test_type,
                p_Wert = sprintf("%.4f", result$p_value),
                Signifikanz = ifelse(result$is_significant, "Ja*", "Nein"),
                Cohens_W = sprintf("%.3f", result$cohens_w),
                Effektgroesse = result$effect_size_interpretation,
                n_Patienten = result$n_patients,
                stringsAsFactors = FALSE
            )
        }
    }

    if (length(summary_list) > 0) {
        summary_df <- do.call(rbind, summary_list)
        rownames(summary_df) <- NULL

        cat("\n=== DETAILLIERTE ZUSAMMENFASSUNG DER STATISTISCHEN TESTS ===\n")
        print(summary_df)

        # Zusätzliche Interpretation
        cat("\n=== INTERPRETATION ===\n")
        cat("Signifikante Ergebnisse (p < 0.05):\n")
        significant_results <- summary_df[summary_df$Signifikanz == "Ja*", ]
        if (nrow(significant_results) > 0) {
            for (i in 1:nrow(significant_results)) {
                cat(paste0(
                    "- ", significant_results$Gruppe[i],
                    " (p = ", significant_results$p_Wert[i],
                    ", ", significant_results$Effektgroesse[i], ")\n"
                ))
            }
        } else {
            cat("- Keine statistisch signifikanten Unterschiede gefunden\n")
        }

        return(summary_df)
    } else {
        cat("Keine Testergebnisse verfügbar\n")
        return(NULL)
    }
}

# Funktion für Post-hoc-Analyse bei signifikanten Chi-Quadrat-Tests
perform_posthoc_analysis <- function(test_result) {
    if (is.null(test_result) || !test_result$is_significant) {
        return(NULL)
    }

    cont_table <- test_result$contingency_table

    # Berechne standardisierte Residuen
    chi_test <- chisq.test(cont_table)
    std_residuals <- chi_test$stdres

    # Identifiziere Zellen mit |standardisierte Residuen| > 2 (signifikant)
    significant_cells <- which(abs(std_residuals) > 2, arr.ind = TRUE)

    if (nrow(significant_cells) == 0) {
        return(list(
            has_significant_cells = FALSE,
            message = "Keine einzelnen Zellen zeigen signifikante Abweichungen (|std. Residuen| > 2)"
        ))
    }

    # Erstelle Ergebnistabelle
    results_df <- data.frame(
        Altersgruppe = rownames(cont_table)[significant_cells[, 1]],
        Medikamentengruppe = colnames(cont_table)[significant_cells[, 2]],
        Beobachtet = cont_table[significant_cells],
        Erwartet = round(chi_test$expected[significant_cells], 1),
        Std_Residuum = round(std_residuals[significant_cells], 3),
        Interpretation = ifelse(std_residuals[significant_cells] > 2,
            "Überrepräsentiert", "Unterrepräsentiert"
        ),
        stringsAsFactors = FALSE
    )

    # Sortiere nach absoluten standardisierten Residuen (stärkste Effekte zuerst)
    results_df <- results_df[order(abs(results_df$Std_Residuum), decreasing = TRUE), ]

    return(list(
        has_significant_cells = TRUE,
        significant_combinations = results_df,
        std_residuals_matrix = std_residuals,
        interpretation = generate_posthoc_interpretation(results_df)
    ))
}

# Funktion zur Interpretation der Post-hoc-Ergebnisse
generate_posthoc_interpretation <- function(results_df) {
    interpretations <- character()

    for (i in 1:nrow(results_df)) {
        row <- results_df[i, ]
        direction <- ifelse(row$Std_Residuum > 0, "häufiger", "seltener")
        strength <- case_when(
            abs(row$Std_Residuum) >= 3 ~ "sehr stark",
            abs(row$Std_Residuum) >= 2.5 ~ "stark",
            TRUE ~ "moderat"
        )

        interpretation <- paste0(
            "- ", row$Medikamentengruppe, " wird in der Altersgruppe '",
            row$Altersgruppe, "' ", strength, " ", direction,
            " verwendet als erwartet (Std. Residuum: ", row$Std_Residuum, ")"
        )
        interpretations <- c(interpretations, interpretation)
    }

    return(interpretations)
}

# Erweiterte Funktion für detaillierte Ergebnis-Tabelle mit Post-hoc-Analyse
create_detailed_test_summary_with_posthoc <- function() {
    test_results <- perform_medication_chisq_tests()

    summary_list <- list()
    posthoc_results <- list()

    for (group_name in names(test_results)) {
        result <- test_results[[group_name]]
        if (!is.null(result)) {
            # Basis-Zusammenfassung
            summary_list[[group_name]] <- data.frame(
                Gruppe = result$test_name,
                Test = result$test_type,
                p_Wert = sprintf("%.4f", result$p_value),
                Signifikanz = ifelse(result$is_significant, "Ja*", "Nein"),
                Cohens_W = sprintf("%.3f", result$cohens_w),
                Effektgroesse = result$effect_size_interpretation,
                n_Patienten = result$n_patients,
                stringsAsFactors = FALSE
            )

            # Post-hoc-Analyse
            posthoc_results[[group_name]] <- perform_posthoc_analysis(result)
        }
    }

    if (length(summary_list) > 0) {
        summary_df <- do.call(rbind, summary_list)
        rownames(summary_df) <- NULL

        cat("\n=== DETAILLIERTE ZUSAMMENFASSUNG DER STATISTISCHEN TESTS ===\n")
        print(summary_df)

        # Post-hoc-Ergebnisse anzeigen
        cat("\n=== POST-HOC-ANALYSE: SPEZIFISCHE UNTERSCHIEDE ===\n")

        for (group_name in names(posthoc_results)) {
            posthoc <- posthoc_results[[group_name]]
            if (!is.null(posthoc)) {
                cat(paste0("\n--- ", test_results[[group_name]]$test_name, " ---\n"))

                if (posthoc$has_significant_cells) {
                    cat("Signifikante Abweichungen zwischen Altersgruppen und Medikamentengruppen:\n\n")
                    print(posthoc$significant_combinations)

                    cat("\nInterpretation:\n")
                    for (interpretation in posthoc$interpretation) {
                        cat(interpretation, "\n")
                    }
                } else {
                    cat(posthoc$message, "\n")
                }
            }
        }

        # Allgemeine Interpretation
        cat("\n=== GESAMTINTERPRETATION ===\n")
        cat("Signifikante Haupteffekte (p < 0.05):\n")
        significant_results <- summary_df[summary_df$Signifikanz == "Ja*", ]
        if (nrow(significant_results) > 0) {
            for (i in 1:nrow(significant_results)) {
                cat(paste0(
                    "- ", significant_results$Gruppe[i],
                    " (p = ", significant_results$p_Wert[i],
                    ", ", significant_results$Effektgroesse[i], ")\n"
                ))
            }

            cat("\nHinweis: Die Post-hoc-Analyse oben zeigt, welche spezifischen\n")
            cat("Altersgruppen-Medikamentengruppen-Kombinationen für diese\n")
            cat("signifikanten Haupteffekte verantwortlich sind.\n")
        } else {
            cat("- Keine statistisch signifikanten Unterschiede gefunden\n")
        }

        return(list(
            summary_table = summary_df,
            posthoc_results = posthoc_results
        ))
    } else {
        cat("Keine Testergebnisse verfügbar\n")
        return(NULL)
    }
}

# Funktion für visualisierte Post-hoc-Ergebnisse (Heatmap der standardisierten Residuen)
create_posthoc_heatmap <- function(test_result) {
    if (is.null(test_result) || !test_result$is_significant) {
        return(ggplot() +
            labs(title = paste("Keine signifikanten Ergebnisse für:", test_result$test_name)) +
            theme_void())
    }

    posthoc <- perform_posthoc_analysis(test_result)
    if (is.null(posthoc) || !posthoc$has_significant_cells) {
        return(ggplot() +
            labs(title = paste("Keine signifikanten Zell-Abweichungen für:", test_result$test_name)) +
            theme_void())
    }

    # Konvertiere Matrix zu Data Frame für ggplot
    std_res_df <- as.data.frame(as.table(posthoc$std_residuals_matrix))
    names(std_res_df) <- c("Altersgruppe", "Medikamentengruppe", "Std_Residuum")

    # Erstelle Heatmap
    heatmap_plot <- ggplot(std_res_df, aes(x = Medikamentengruppe, y = Altersgruppe, fill = Std_Residuum)) +
        geom_tile(color = "white", size = 0.5) +
        scale_fill_gradient2(
            low = "blue", mid = "white", high = "red",
            midpoint = 0,
            name = "Std.\nResiduum",
            breaks = c(-3, -2, 0, 2, 3),
            labels = c("-3", "-2", "0", "2", "3")
        ) +
        geom_text(aes(label = round(Std_Residuum, 2)),
            color = ifelse(abs(std_res_df$Std_Residuum) > 1.5, "white", "black"),
            size = 3
        ) +
        labs(
            title = paste("Post-hoc-Analyse:", test_result$test_name),
            subtitle = "Standardisierte Residuen (|Wert| > 2 = signifikant)",
            x = "Medikamentengruppe",
            y = "Altersgruppe"
        ) +
        theme_minimal() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5)
        ) +
        # Markiere signifikante Zellen
        geom_tile(
            data = subset(std_res_df, abs(Std_Residuum) > 2),
            aes(x = Medikamentengruppe, y = Altersgruppe),
            color = "black", size = 2, fill = NA
        )

    return(heatmap_plot)
}

# Erweiterte Hauptfunktion mit Post-hoc-Analyse
run_complete_medication_analysis_with_posthoc <- function() {
    cat("=== VOLLSTÄNDIGE MEDIKAMENTENANALYSE MIT POST-HOC-TESTS ===\n\n")

    # 1. Führe alle Tests durch
    test_results <- perform_medication_chisq_tests()

    # 2. Zeige detaillierte Zusammenfassung mit Post-hoc
    detailed_results <- create_detailed_test_summary_with_posthoc()

    # 3. Erstelle Visualisierungen
    cat("\n=== VISUALISIERUNGEN ===\n")

    # Ursprüngliche Plots
    tryCatch(
        {
            combined_plot <- plotMedicationGroupsAllComparison_WithStats()
            print(combined_plot)
            cat("Standard-Plots erstellt\n")
        },
        error = function(e) {
            cat("Fehler beim Erstellen der Standard-Plots:", e$message, "\n")
        }
    )

    # Post-hoc Heatmaps
    cat("\n=== POST-HOC HEATMAPS ===\n")
    for (group_name in names(test_results)) {
        result <- test_results[[group_name]]
        if (!is.null(result) && result$is_significant) {
            tryCatch(
                {
                    heatmap <- create_posthoc_heatmap(result)
                    print(heatmap)
                    cat(paste("Heatmap erstellt für:", result$test_name, "\n"))
                },
                error = function(e) {
                    cat("Fehler beim Erstellen der Heatmap für", result$test_name, ":", e$message, "\n")
                }
            )
        }
    }

    return(list(
        test_results = test_results,
        detailed_results = detailed_results
    ))
}

# Vereinfachte Funktion für schnelle Post-hoc-Analyse einer spezifischen Gruppe
quick_posthoc_analysis <- function(group = "general") {
    test_results <- perform_medication_chisq_tests()

    if (group %in% names(test_results) && !is.null(test_results[[group]])) {
        result <- test_results[[group]]

        cat(paste("=== POST-HOC-ANALYSE:", result$test_name, "===\n"))
        cat(paste(
            "Haupttest p-Wert:", sprintf("%.4f", result$p_value),
            ifelse(result$is_significant, "(signifikant)", "(nicht signifikant)"), "\n\n"
        ))

        if (result$is_significant) {
            posthoc <- perform_posthoc_analysis(result)
            if (!is.null(posthoc) && posthoc$has_significant_cells) {
                cat("Spezifische signifikante Unterschiede:\n")
                print(posthoc$significant_combinations)

                cat("\nInterpretation:\n")
                for (interpretation in posthoc$interpretation) {
                    cat(interpretation, "\n")
                }

                # Zeige Heatmap
                heatmap <- create_posthoc_heatmap(result)
                print(heatmap)

                return(posthoc)
            } else {
                cat("Obwohl der Haupttest signifikant ist, zeigen keine einzelnen\n")
                cat("Zellen signifikante Abweichungen (|std. Residuen| > 2).\n")
            }
        } else {
            cat("Da der Haupttest nicht signifikant ist, wird keine Post-hoc-Analyse durchgeführt.\n")
        }
    } else {
        cat("Keine Daten für Gruppe:", group, "\n")
        cat("Verfügbare Gruppen:", paste(names(test_results), collapse = ", "), "\n")
    }

    return(NULL)
}

# Beispielaufrufe:
# Grundlegende Analysen (ohne Post-hoc):
# run_complete_medication_analysis()

# NEUE: Erweiterte Analysen mit Post-hoc-Tests:
run_complete_medication_analysis_with_posthoc() # Vollständige Analyse mit Post-hoc
# quick_posthoc_analysis("general")                # Post-hoc für alle Patienten
# quick_posthoc_analysis("female")                 # Post-hoc für weibliche Patienten
# quick_posthoc_analysis("male")                   # Post-hoc für männliche Patienten
# create_detailed_test_summary_with_posthoc()      # Detaillierte Tabelle mit Post-hoc

# Andere verfügbare Funktionen:
# plotMedicationGroupsAllComparison_WithStats()
# run_gender_neutral_analysis()                    # Fokus auf geschlechter-unspezifische Analyse
# quick_general_analysis()                         # Schnelle geschlechter-unspezifische Analyse
# quick_female_analysis()
# quick_male_analysis()
# create_detailed_test_summary()

# nolint end
