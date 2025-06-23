# nolint start
# Quellcode für Datenaufbereitung aus BasicAnalysis.R einlesen
library(rcompanion)
source("BasicAnalysis.R")

chisq_test_medication <- function(patients) {
    patients_with_age_group <- patients[!is.na(patients$ageGroup10), ]

    # Get column names that start with "preventive_name_p1_K"
    preventive_cols <- grep("^preventive_name_p1_K", names(patients_with_age_group), value = TRUE)

    # Select one column (you can modify this to select a specific column or use them in a loop)
    column <- preventive_cols[1]

    filtered_patients <- subset(patients_with_age_group, patients_with_age_group[, column] != "0")

    # Use $ notation or [[]] to extract the vector, not the data frame subset
    menopause <- as.integer(filtered_patients$ageGroup10)

    # Define medication groups
    med_groups <<- list(
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
        return(NA) # Return NA if medication doesn't belong to any group
    }

    # Map medications to groups and filter out medications not in any group
    original_meds <- filtered_patients[[column]]
    med_groups_vector <- sapply(original_meds, map_med_to_group)

    # Filter patients to only include those with medications that map to a group
    filtered_patients <- filtered_patients[!is.na(med_groups_vector), ]
    med_groups_vector <- med_groups_vector[!is.na(med_groups_vector)]

    # Convert medication group strings to factor levels
    meds <- factor(med_groups_vector)
    print("Medication group categories:")
    print(levels(meds))
    print("Count of medications by group:")
    print(table(meds))

    # Create and examine the contingency table with grouped medications
    cont_table <- table(filtered_patients$ageGroup10, med_groups_vector)
    print("Contingency table:")
    print(cont_table)

    # Calculate expected values to check if they are very small
    chisq_test <- chisq.test(cont_table)
    expected <- chisq_test$expected
    print("Expected values:")
    print(expected)

    # Calculate Cohen's W for effect size
    cohenW <- cohenW(cont_table)
    print(paste("Cohen's W:", cohenW))

    #
    alpha <- 0.05
    alpha_adj <- alpha / (nrow(cont_table) * ncol(cont_table))

    qnorm(alpha_adj / 2)

    residuals <- chisq_test$stdres
    print("Standardized residuals:")
    print(residuals)

    # Convert menopause and medication groups to numeric for Wilcoxon test
    # Convert ageGroup10 to numeric
    filtered_patients$menopausePhase <- as.numeric(as.character(filtered_patients$ageGroup10))

    # For medication groups, create a numeric mapping
    med_group_levels <- levels(factor(med_groups_vector))
    med_group_mapping <- setNames(seq_along(med_group_levels), med_group_levels)
    med_groups_numeric <- as.numeric(med_group_mapping[med_groups_vector])

    # Print the mapping information for reference
    print("Medication group numeric mapping:")
    print(med_group_mapping)
    print("Age group values are now numeric")

    # Use Fisher's exact test if there are low expected values
    # Fisher's test is more appropriate for small sample sizes
    if (min(expected) < 5) {
        print("Some expected counts are less than 5, using Fisher's exact test:")
        fisher_test <- fisher.test(filtered_patients$menopause, med_groups_vector,
            simulate.p.value = TRUE, B = 10000
        )
        print(fisher_test)
    } else {
        print("Using Chi-square test:")
        chi_result <- chisq.test(filtered_patients$menopause, med_groups_vector)
        print(chi_result)
    }
}
# General tests for medication groups
# chisq_test_medication(enhancedPatients)

# Test only female patients
# Test only female patients
female_patients <- subset(enhancedPatients, gender == "weiblich")
chisq_test_medication(female_patients)

# male_patients <- subset(enhancedPatients, gender == "männlich")
# chisq_test_medication(male_patients)
# nolint end
