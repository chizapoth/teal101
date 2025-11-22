library(teal)
library(teal.modules.mdr)
library(teal.modules.general)


# ____ DATA ____ -----
data <- teal_data() |>
  within({
    
    # adverse_events ----
    adverse_events <- data.frame(
      adverse_event = sample(
        c("Headache", "Nausea", "Fatigue", "Dizziness", "Rash", "Insomnia", "Diarrhea", "Constipation"),
        200,
        replace = TRUE,
        prob = c(0.2, 0.15, 0.15, 0.12, 0.1, 0.1, 0.1, 0.08)
      ),
      severity = sample(
        c("Mild", "Moderate", "Severe"),
        200,
        replace = TRUE,
        prob = c(0.65, 0.25, 0.1)
      ),
      system_organ_class = c(
        rep("Nervous System", 94),
        rep("Gastrointestinal", 60),
        rep("Skin", 20),
        rep("General", 26)
      )[sample(200)],
      subject_id = sample(paste0("S", 1:50), 200, replace = TRUE),
      treatment = sample(c("Experimental", "Control"), 200, replace = TRUE),
      age_group = sample(c("18-35", "36-55", "56-75", "75+"), 200, replace = TRUE),
      center = sample(c("Site A", "Site B", "Site C", "Site D", "Site E"), 200, replace = TRUE),
      preferred_term = sample(
        c(
          "Headache NOS", "Nausea", "Fatigue", "Dizziness postural",
          "Rash generalized", "Insomnia", "Diarrhea NOS", "Constipation"
        ),
        200,
        replace = TRUE
      )
    )
    attr(adverse_events$adverse_event, "label") <- "Adverse Event Term"
    attr(adverse_events$severity, "label") <- "Severity Grade"
    attr(adverse_events$system_organ_class, "label") <- "System Organ Class"
    attr(adverse_events$subject_id, "label") <- "Subject ID"
    attr(adverse_events$treatment, "label") <- "Treatment Group"
    attr(adverse_events$age_group, "label") <- "Age Group"
    attr(adverse_events$center, "label") <- "Study Center"
    attr(adverse_events$preferred_term, "label") <- "Preferred Term"
    
    
    # longitudinal_data ----
    longitudinal_data <- data.frame(
      subject_id = rep(paste0("S", 1:8), each = 5),
      time_week = rep(c(0, 2, 4, 6, 8), 8),
      measurement = rnorm(40, 20, 4) + rep(c(0, 1, 2, 3, 4), 8),
      treatment = rep(c("Active", "Placebo"), each = 20),
      baseline = rep(rnorm(8, 18, 2), each = 5),
      center = rep(c("Site A", "Site B", "Site A", "Site B"), each = 10)
    )
    attr(longitudinal_data$subject_id, "label") <- "Subject ID"
    attr(longitudinal_data$time_week, "label") <- "Time (weeks)"
    attr(longitudinal_data$measurement, "label") <- "Measurement Value"
    attr(longitudinal_data$treatment, "label") <- "Treatment Group"
    attr(longitudinal_data$baseline, "label") <- "Baseline Value"
    attr(longitudinal_data$center, "label") <- "Study Center"
    
    
    # tumor_response ----
    tumor_response <- data.frame(
      subject_id = rep(paste0("S", 1:12), each = 6),
      time_week = rep(c(0, 2, 4, 8, 12, 16), 12),
      tumor_size = c(
        c(45, 40, 32, 28, 25, 22) + rnorm(6, 0, 2),
        c(52, 48, 38, 30, 25, 20) + rnorm(6, 0, 2),
        c(38, 35, 28, 22, 18, 15) + rnorm(6, 0, 2),
        c(60, 55, 45, 38, 32, 28) + rnorm(6, 0, 2),
        c(42, 41, 43, 40, 42, 44) + rnorm(6, 0, 3),
        c(55, 52, 56, 54, 53, 57) + rnorm(6, 0, 3),
        c(35, 36, 34, 37, 35, 38) + rnorm(6, 0, 3),
        c(48, 50, 47, 49, 48, 51) + rnorm(6, 0, 3),
        c(40, 45, 52, 58, 65, 72) + rnorm(6, 0, 2),
        c(35, 38, 45, 50, 58, 65) + rnorm(6, 0, 2),
        c(50, 55, 62, 68, 75, 82) + rnorm(6, 0, 2),
        c(45, 48, 55, 62, 70, 78) + rnorm(6, 0, 2)
      ),
      treatment = rep(c("Experimental", "Control"), each = 36),
      response_category = rep(c(
        "Partial Response", "Complete Response", "Partial Response", "Partial Response",
        "Stable Disease", "Stable Disease", "Stable Disease", "Stable Disease",
        "Progressive Disease", "Progressive Disease", "Progressive Disease", "Progressive Disease"
      ), each = 6),
      age_group = rep(sample(c("Young", "Middle", "Old"), 12, replace = TRUE), each = 6),
      baseline_size = rep(c(45, 52, 38, 60, 42, 55, 35, 48, 40, 35, 50, 45), each = 6),
      center = rep(sample(c("Site A", "Site B", "Site C"), 12, replace = TRUE), each = 6)
    )
    attr(tumor_response$subject_id, "label") <- "Subject ID"
    attr(tumor_response$time_week, "label") <- "Time (weeks)"
    attr(tumor_response$tumor_size, "label") <- "Tumor Size (mm)"
    attr(tumor_response$treatment, "label") <- "Treatment Group"
    attr(tumor_response$response_category, "label") <- "Best Overall Response"
    attr(tumor_response$age_group, "label") <- "Age Group"
    attr(tumor_response$baseline_size, "label") <- "Baseline Tumor Size (mm)"
    attr(tumor_response$center, "label") <- "Study Center"
    
    
    # biomarker_data ----
    biomarker_data <- data.frame(
      subject_id = rep(paste0("S", 1:15), each = 5),
      time_week = rep(c(0, 4, 8, 12, 16), 15),
      biomarker_level = c(
        c(100, 85, 70, 55, 40) + rnorm(5, 0, 3),
        c(95, 78, 62, 48, 35) + rnorm(5, 0, 3),
        c(110, 92, 75, 58, 42) + rnorm(5, 0, 3),
        c(88, 72, 58, 45, 32) + rnorm(5, 0, 3),
        c(105, 88, 70, 52, 38) + rnorm(5, 0, 3),
        c(90, 88, 92, 85, 90) + rnorm(5, 0, 4),
        c(75, 80, 78, 82, 77) + rnorm(5, 0, 4),
        c(85, 82, 88, 84, 86) + rnorm(5, 0, 4),
        c(92, 95, 90, 94, 88) + rnorm(5, 0, 4),
        c(78, 75, 80, 76, 79) + rnorm(5, 0, 4),
        c(95, 100, 108, 115, 125) + rnorm(5, 0, 3),
        c(88, 92, 98, 105, 115) + rnorm(5, 0, 3),
        c(102, 105, 110, 118, 128) + rnorm(5, 0, 3),
        c(85, 90, 95, 102, 112) + rnorm(5, 0, 3),
        c(98, 102, 108, 115, 125) + rnorm(5, 0, 3)
      ),
      treatment = rep(c("Experimental", "Control"), length.out = 75),
      response_category = rep(c(
        rep("Responder", 5),
        rep("Stable", 5),
        rep("Non-responder", 5)
      ), each = 5),
      age_group = rep(sample(c("Young", "Middle", "Old"), 15, replace = TRUE), each = 5),
      baseline_level = rep(c(100, 95, 110, 88, 105, 90, 75, 85, 92, 78, 95, 88, 102, 85, 98), each = 5),
      center = rep(sample(c("Site A", "Site B", "Site C", "Site D"), 15, replace = TRUE), each = 5),
      dose_group = rep(sample(c("Low", "Medium", "High"), 15, replace = TRUE), each = 5)
    )
    attr(biomarker_data$subject_id, "label") <- "Subject ID"
    attr(biomarker_data$time_week, "label") <- "Time (weeks)"
    attr(biomarker_data$biomarker_level, "label") <- "Biomarker Level (ng/mL)"
    attr(biomarker_data$treatment, "label") <- "Treatment Group"
    attr(biomarker_data$response_category, "label") <- "Response Category"
    attr(biomarker_data$age_group, "label") <- "Age Group"
    attr(biomarker_data$baseline_level, "label") <- "Baseline Biomarker Level"
    attr(biomarker_data$center, "label") <- "Study Center"
    attr(biomarker_data$dose_group, "label") <- "Dose Group"
    
    
    # spider_data ----
    spider_data <- data.frame(
      subject_id = rep(paste0("S", 1:8), each = 6),
      time_week = rep(c(0, 4, 8, 12, 16, 20), 8),
      percent_change = c(
        rnorm(24, -10, 15), # First 4 subjects with some improvement
        rnorm(24, 5, 10) # Last 4 subjects with less improvement
      ),
      response_category = rep(c("Complete Response", "Partial Response", "Stable Disease", "Progressive Disease"), 12),
      treatment = rep(c("Active", "Placebo"), each = 24),
      age_group = rep(c("Young", "Old", "Young", "Old"), 12),
      baseline_tumor_size = rep(rnorm(8, 50, 10), each = 6),
      center = rep(c("Site A", "Site B", "Site C", "Site D"), 12)
    )
    
    attr(spider_data$subject_id, "label") <- "Subject ID"
    attr(spider_data$time_week, "label") <- "Time (weeks)"
    attr(spider_data$percent_change, "label") <- "Percent Change from Baseline"
    attr(spider_data$response_category, "label") <- "Response Category"
    attr(spider_data$treatment, "label") <- "Treatment Group"
    attr(spider_data$age_group, "label") <- "Age Group"
    attr(spider_data$baseline_tumor_size, "label") <- "Baseline Tumor Size (mm)"
    attr(spider_data$center, "label") <- "Study Center"
    
    # subjects _____
    subjects <- data.frame(
      subject_id = paste0("S", 1:8),
      age = sample(25:75, 8),
      sex = sample(c("Male", "Female"), 8, replace = TRUE),
      treatment = rep(c("Active", "Placebo"), each = 4),
      baseline_ecog = sample(0:2, 8, replace = TRUE, prob = c(0.6, 0.3, 0.1)),
      enrollment_date = sample(seq(as.Date("2023-01-01"), as.Date("2023-06-01"), by = "day"), 8),
      center = rep(c("Site A", "Site B", "Site C", "Site D"), 2)
    )
    attr(subjects$subject_id, "label") <- "Subject ID"
    attr(subjects$age, "label") <- "Age (years)"
    attr(subjects$sex, "label") <- "Sex"
    attr(subjects$treatment, "label") <- "Treatment Group"
    attr(subjects$baseline_ecog, "label") <- "ECOG Performance Status"
    attr(subjects$enrollment_date, "label") <- "Enrollment Date"
    attr(subjects$center, "label") <- "Study Center"
    
    
    # swimlane_subjects ----
    swimlane_subjects <- data.frame(
      subject_id = paste0("S", sprintf("%03d", 1:12)),
      age = sample(25:75, 12),
      sex = sample(c("Male", "Female"), 12, replace = TRUE),
      treatment_arm = rep(c("Experimental", "Control"), each = 6),
      baseline_ecog = sample(0:2, 12, replace = TRUE, prob = c(0.6, 0.3, 0.1)),
      enrollment_date = sample(seq(as.Date("2023-01-01"), as.Date("2023-06-01"), by = "day"), 12),
      center = sample(c("Site A", "Site B", "Site C"), 12, replace = TRUE),
      baseline_tumor_size = round(rnorm(12, 50, 15), 1)
    )
    attr(swimlane_subjects$subject_id, "label") <- "Subject ID"
    attr(swimlane_subjects$age, "label") <- "Age (years)"
    attr(swimlane_subjects$sex, "label") <- "Sex"
    attr(swimlane_subjects$treatment_arm, "label") <- "Treatment Arm"
    attr(swimlane_subjects$baseline_ecog, "label") <- "ECOG Performance Status"
    attr(swimlane_subjects$enrollment_date, "label") <- "Enrollment Date"
    attr(swimlane_subjects$center, "label") <- "Study Center"
    attr(swimlane_subjects$baseline_tumor_size, "label") <- "Baseline Tumor Size (mm)"
    
    # swimlane_events ----
    swimlane_events <- data.frame(
      subject_id = c(
        rep("S001", 4), rep("S002", 3), rep("S003", 5), rep("S004", 4),
        rep("S005", 3), rep("S006", 4), rep("S007", 3), rep("S008", 5),
        rep("S009", 4), rep("S010", 3), rep("S011", 4), rep("S012", 3)
      ),
      study_day = c(
        c(1, 28, 56, 84), c(1, 28, 84), c(1, 28, 56, 70, 84), c(1, 28, 56, 84),
        c(1, 28, 84), c(1, 28, 42, 84), c(1, 28, 84), c(1, 28, 42, 56, 70),
        c(1, 28, 56, 84), c(1, 28, 56), c(1, 28, 42, 56), c(1, 28, 84)
      ),
      event_type = c(
        c("Baseline", "Response", "Response", "Complete Response"),
        c("Baseline", "Response", "Stable Disease"),
        c("Baseline", "Response", "Stable Disease", "Progression", "Off Study"),
        c("Baseline", "Response", "Stable Disease", "Response"),
        c("Baseline", "Stable Disease", "Stable Disease"),
        c("Baseline", "Stable Disease", "Progression", "Off Study"),
        c("Baseline", "Stable Disease", "Stable Disease"),
        c("Baseline", "Stable Disease", "Progression", "Progression", "Off Study"),
        c("Baseline", "Stable Disease", "Progression", "Off Study"),
        c("Baseline", "Progression", "Off Study"),
        c("Baseline", "Progression", "Progression", "Off Study"),
        c("Baseline", "Stable Disease", "Stable Disease")
      ),
      response_category = c(
        c("Baseline", "Partial Response", "Partial Response", "Complete Response"),
        c("Baseline", "Partial Response", "Stable Disease"),
        c("Baseline", "Partial Response", "Stable Disease", "Progressive Disease", "End of Study"),
        c("Baseline", "Partial Response", "Stable Disease", "Partial Response"),
        c("Baseline", "Stable Disease", "Stable Disease"),
        c("Baseline", "Stable Disease", "Progressive Disease", "End of Study"),
        c("Baseline", "Stable Disease", "Stable Disease"),
        c("Baseline", "Stable Disease", "Progressive Disease", "Progressive Disease", "End of Study"),
        c("Baseline", "Stable Disease", "Progressive Disease", "End of Study"),
        c("Baseline", "Progressive Disease", "End of Study"),
        c("Baseline", "Progressive Disease", "Progressive Disease", "End of Study"),
        c("Baseline", "Stable Disease", "Stable Disease")
      ),
      assessment_visit = c(
        c("Screening", "Cycle 2", "Cycle 4", "Cycle 6"),
        c("Screening", "Cycle 2", "Cycle 6"),
        c("Screening", "Cycle 2", "Cycle 4", "Unscheduled", "End of Treatment"),
        c("Screening", "Cycle 2", "Cycle 4", "Cycle 6"),
        c("Screening", "Cycle 2", "Cycle 6"),
        c("Screening", "Cycle 2", "Cycle 3", "End of Treatment"),
        c("Screening", "Cycle 2", "Cycle 6"),
        c("Screening", "Cycle 2", "Cycle 3", "Cycle 4", "End of Treatment"),
        c("Screening", "Cycle 2", "Cycle 4", "End of Treatment"),
        c("Screening", "Cycle 2", "Cycle 4"),
        c("Screening", "Cycle 2", "Cycle 3", "End of Treatment"),
        c("Screening", "Cycle 2", "Cycle 6")
      )
    )
    swimlane_events <- merge(swimlane_events, swimlane_subjects[c("subject_id", "treatment_arm")], by = "subject_id")
    attr(swimlane_events$subject_id, "label") <- "Subject ID"
    attr(swimlane_events$study_day, "label") <- "Study Day"
    attr(swimlane_events$event_type, "label") <- "Event Type"
    attr(swimlane_events$response_category, "label") <- "Response Category"
    attr(swimlane_events$assessment_visit, "label") <- "Assessment Visit"
    attr(swimlane_events$treatment_arm, "label") <- "Treatment Arm"
    
    
    # waterfall_subjects -----
    waterfall_subjects <- data.frame(
      subject_id = paste0("PT", sprintf("%03d", 1:15)),
      age = sample(25:75, 15),
      sex = sample(c("Male", "Female"), 15, replace = TRUE),
      treatment_arm = rep(c("Experimental Drug", "Standard of Care", "Combination"), each = 5),
      baseline_ecog = sample(0:2, 15, replace = TRUE, prob = c(0.6, 0.3, 0.1)),
      enrollment_date = sample(seq(as.Date("2023-01-01"), as.Date("2023-06-01"), by = "day"), 15),
      center = sample(c("Site A", "Site B", "Site C", "Site D"), 15, replace = TRUE),
      baseline_tumor_size = round(rnorm(15, 50, 15), 1),
      histology = sample(c("Adenocarcinoma", "Squamous Cell", "Large Cell"), 15, replace = TRUE)
    )
    attr(waterfall_subjects$subject_id, "label") <- "Subject ID"
    attr(waterfall_subjects$age, "label") <- "Age (years)"
    attr(waterfall_subjects$sex, "label") <- "Sex"
    attr(waterfall_subjects$treatment_arm, "label") <- "Treatment Arm"
    attr(waterfall_subjects$baseline_ecog, "label") <- "ECOG Performance Status"
    attr(waterfall_subjects$enrollment_date, "label") <- "Enrollment Date"
    attr(waterfall_subjects$center, "label") <- "Study Center"
    attr(waterfall_subjects$baseline_tumor_size, "label") <- "Baseline Tumor Size (mm)"
    attr(waterfall_subjects$histology, "label") <- "Histology Type"
    
    # waterfall_response ----
    waterfall_response <- data.frame(
      subject_id = paste0("PT", sprintf("%03d", 1:15)),
      percent_change = c(
        -85, -72, -58, -45, -31,
        -42, -28, -15, 8, 25,
        -78, -55, -32, 12, 45
      ),
      best_response = c(
        "Complete Response", "Partial Response", "Partial Response", "Partial Response", "Stable Disease",
        "Partial Response", "Stable Disease", "Stable Disease", "Progressive Disease", "Progressive Disease",
        "Complete Response", "Partial Response", "Stable Disease", "Progressive Disease", "Progressive Disease"
      ),
      response_category = c(
        "CR", "PR", "PR", "PR", "SD",
        "PR", "SD", "SD", "PD", "PD",
        "CR", "PR", "SD", "PD", "PD"
      ),
      assessment_week = c(
        12, 8, 12, 16, 12,
        8, 12, 16, 8, 12,
        16, 12, 12, 8, 16
      ),
      duration_response = c(
        24, 18, 22, 16, NA,
        14, NA, NA, NA, NA,
        28, 20, NA, NA, NA
      )
    )
    waterfall_response <- merge(waterfall_response, waterfall_subjects[c("subject_id", "treatment_arm", "baseline_ecog")], by = "subject_id")
    attr(waterfall_response$subject_id, "label") <- "Subject ID"
    attr(waterfall_response$percent_change, "label") <- "Best % Change from Baseline"
    attr(waterfall_response$best_response, "label") <- "Best Overall Response"
    attr(waterfall_response$response_category, "label") <- "Response Category"
    attr(waterfall_response$assessment_week, "label") <- "Assessment Week"
    attr(waterfall_response$duration_response, "label") <- "Duration of Response (weeks)"
    attr(waterfall_response$treatment_arm, "label") <- "Treatment Arm"
    attr(waterfall_response$baseline_ecog, "label") <- "Baseline ECOG"
    
    
    # patient_profile ----
    patient_profile <- data.frame(
      subject_id = rep("S001", 12),
      event_category = c(
        rep("Medical History", 2),
        rep("Adverse Events", 3),
        rep("Concomitant Meds", 3),
        rep("Study Treatment", 2),
        rep("Lab Tests", 2)
      ),
      event_term = c(
        "Hypertension", "Diabetes",
        "Headache", "Nausea", "Fatigue",
        "Aspirin", "Ibuprofen", "Acetaminophen",
        "Drug A", "Drug B",
        "Chemistry Panel", "Hematology"
      ),
      start_day = c(
        -365, -180,
        15, 22, 30,
        1, 15, 25,
        1, 29,
        7, 35
      ),
      end_day = c(
        NA, NA,
        18, 25, 35,
        28, 40, 50,
        28, 56,
        NA, NA
      ),
      event_status = c(
        "Ongoing", "Ongoing",
        "Resolved", "Resolved", "Ongoing",
        "Completed", "Completed", "Ongoing",
        "Completed", "Completed",
        "Completed", "Completed"
      ),
      severity = c(
        "Moderate", "Severe",
        "Mild", "Moderate", "Mild",
        NA, NA, NA,
        NA, NA,
        NA, NA
      )
    )
    
    # Add variable labels for better display
    attr(patient_profile$subject_id, "label") <- "Subject ID"
    attr(patient_profile$event_category, "label") <- "Event Category"
    attr(patient_profile$event_term, "label") <- "Event Term"
    attr(patient_profile$start_day, "label") <- "Study Day"
    attr(patient_profile$end_day, "label") <- "End Day"
    attr(patient_profile$event_status, "label") <- "Status"
    attr(patient_profile$severity, "label") <- "Severity"
    
    # Order event categories
    patient_profile$event_category <- factor(
      patient_profile$event_category,
      levels = c(
        "Medical History", "Adverse Events", "Concomitant Meds",
        "Study Treatment", "Lab Tests"
      )
    )
    
    # lab data ----
    lab_data <- data.frame(
      lab_test = rep(
        c("ALT", "AST", "Creatinine", "Hemoglobin"),
        each = 100
      ),
      lab_value = c(
        # ALT values by treatment
        rnorm(50, 30, 10), # Control
        rnorm(50, 45, 15), # Experimental
        # AST values
        rnorm(50, 35, 12),
        rnorm(50, 50, 18),
        # Creatinine values
        rnorm(50, 1.0, 0.3),
        rnorm(50, 1.2, 0.4),
        # Hemoglobin values
        rnorm(50, 14, 2),
        rnorm(50, 13.5, 2.5)
      ),
      treatment = rep(
        rep(c("Control", "Experimental"), each = 50),
        4
      ),
      subject_id = rep(paste0("S", 1:100), 4),
      visit = sample(
        c("Baseline", "Week 4", "Week 8", "Week 12"),
        400,
        replace = TRUE
      ),
      age_group = rep(
        sample(c("18-35", "36-55", "56-75"), 100, replace = TRUE),
        4
      ),
      center = rep(
        sample(c("Site A", "Site B", "Site C"), 100, replace = TRUE),
        4
      )
    )
    
    # Add variable labels for better table display
    attr(lab_data$lab_test, "label") <- "Laboratory Test"
    attr(lab_data$lab_value, "label") <- "Lab Value"
    attr(lab_data$treatment, "label") <- "Treatment Group"
    attr(lab_data$subject_id, "label") <- "Subject ID"
    attr(lab_data$visit, "label") <- "Study Visit"
    attr(lab_data$age_group, "label") <- "Age Group"
    attr(lab_data$center, "label") <- "Study Center"
    
    # adsl, adae ----
    adsl <- data.frame(
      USUBJID = paste0("S", 1:10),
      AGE = sample(25:75, 10),
      SEX = sample(c("M", "F"), 10, replace = TRUE),
      ARM = rep(c("Placebo", "Treatment"), each = 5)
    )
    
    # Adverse events
    adae <- data.frame(
      USUBJID = sample(paste0("S", 1:10), 20, replace = TRUE),
      AEDECOD = sample(c("Headache", "Nausea", "Fatigue"), 20, replace = TRUE),
      AESEV = sample(c("MILD", "MODERATE", "SEVERE"), 20, replace = TRUE)
    )
    
    # Add labels
    attr(adsl$USUBJID, "label") <- "Subject ID"
    attr(adsl$AGE, "label") <- "Age (years)"
    attr(adsl$ARM, "label") <- "Treatment Arm"
    attr(adae$AEDECOD, "label") <- "Adverse Event"
    attr(adae$AESEV, "label") <- "Severity"
    
  })

join_keys(data) <- join_keys(
  join_key("subjects", "spider_data", keys = c(subject_id = "subject_id")),
  join_key("swimlane_subjects", "swimlane_events", keys = c(subject_id = "subject_id")),
  join_key("waterfall_subjects", "waterfall_response", keys = c(subject_id = "subject_id"))
)




# ____ APP ____ -----

# table with module information
module_variations <- read.csv("mdr_modules_variation.csv", stringsAsFactors = FALSE)
sample_datasets <- read.csv("sample_datasets.csv", stringsAsFactors = FALSE)


app <- init(
  data = data,
  modules = modules(
    tm_front_page(
      label = "Overview",
      header_text = c(
        "MDR Modules Showcase" = "This application demonstrates all available MDR (Medical Data Review) visualization modules from the teal.modules.mdr package.",
        "Purpose" = "Each module provides interactive visualizations commonly used in clinical trial data analysis and reporting."
      ),
      tables = list(
        "Available Modules and Variations" = module_variations,
        "Sample Datasets" = sample_datasets
      ),
      additional_tags = tagList(
        tags$h4("Key Features"),
        tags$ul(
          tags$li("Interactive plotly-based visualizations with hover tooltips"),
          tags$li("Multiple variations per module (simple, with lines, with tables)"),
          tags$li("Customizable colors, symbols, and reference lines"),
          tags$li("Linked data tables with filtering, sorting, and pagination"),
          tags$li("Support for categorical coloring and grouping variables"),
          tags$li("Flexible data transformations and reference line annotations")
        ),
        tags$br(),
        tags$h4("Navigation"),
        tags$p("Use the sidebar menu to explore each module and its variations. Each module demonstrates different use cases and configuration options for clinical trial data visualization.")
      ),
      footnotes = c(
        "Data" = "All datasets are synthetic and generated for demonstration purposes only.",
        "Package Version" = "teal.modules.mdr - MDR visualization modules for teal applications",
        "Contact" = "For questions or issues, please refer to the package documentation"
      )
    ),
    
    # bargraph ----
    modules(
      label = "tm_mdr_bargraph",
      tm_mdr_bargraph(
        label = "simple",
        plot_dataname = "adverse_events",
        y_var = "system_organ_class",
        color_var = "treatment",
        count_var = "subject_id",
        variation = "simple",
        tooltip_vars = c("system_organ_class", "treatment", "adverse_event"),
        bar_colors = c("Experimental" = "#1f77b4", "Control" = "#ff7f0e")
      ),
      tm_mdr_bargraph(
        label = "bar",
        plot_dataname = "adverse_events",
        y_var = "system_organ_class",
        color_var = "treatment",
        count_var = "subject_id",
        variation = "bar",
        secondary_y_var = "adverse_event",
        tooltip_vars = c("system_organ_class", "adverse_event", "treatment", "severity"),
        bar_colors = c("Experimental" = "#2ca02c", "Control" = "#d62728")
      ),
      tm_mdr_bargraph(
        label = "tables",
        plot_dataname = "adverse_events",
        y_var = "system_organ_class",
        color_var = "treatment",
        count_var = "subject_id",
        variation = "tables",
        tooltip_vars = c("system_organ_class", "treatment", "center"),
        bar_colors = c("Experimental" = "#9467bd", "Control" = "#8c564b"),
        table_datanames = "adverse_events",
        reactable_args = list(
          pagination = TRUE,
          searchable = TRUE,
          sortable = TRUE,
          filterable = TRUE,
          resizable = TRUE,
          defaultPageSize = 15
        )
      ),
      tm_mdr_bargraph(
        label = "bar_table",
        plot_dataname = "adverse_events",
        y_var = "system_organ_class",
        color_var = "severity",
        count_var = "subject_id",
        variation = "bar_table",
        secondary_y_var = "adverse_event",
        tooltip_vars = c("system_organ_class", "adverse_event", "severity", "treatment", "center"),
        bar_colors = c("Mild" = "#90EE90", "Moderate" = "#FFD700", "Severe" = "#FF6347"),
        table_datanames = "adverse_events",
        reactable_args = list(
          pagination = TRUE,
          searchable = TRUE,
          sortable = TRUE,
          filterable = TRUE,
          resizable = TRUE,
          defaultPageSize = 20,
          defaultSorted = list(severity = "desc")
        )
      )
    ),
    
    # lineplot ----
    modules(
      label = "tm_mdr_lineplot",
      tm_mdr_lineplot(
        label = "simple",
        plot_dataname = "longitudinal_data",
        x_var = "time_week",
        y_var = "measurement",
        color_var = "treatment",
        group_var = "subject_id",
        variation = "simple",
        colors = c("Active" = "#1f77b4", "Placebo" = "#ff7f0e"),
        tooltip_vars = c("subject_id", "time_week", "measurement", "treatment", "center", "baseline"),
        transformators = list(),
        reference_lines = list(
          baseline = list(label = "Baseline Mean", line_mode = "dash"),
          measurement = list(label = "Measurement Value", line_mode = "solid")
        )
      ),
      tm_mdr_lineplot(
        label = "tables",
        plot_dataname = "longitudinal_data",
        x_var = "time_week",
        y_var = "measurement",
        color_var = "treatment",
        group_var = "subject_id",
        variation = "tables",
        colors = c("Active" = "#2ca02c", "Placebo" = "#d62728"),
        tooltip_vars = c("subject_id", "time_week", "measurement", "treatment", "center", "baseline"),
        transformators = list(),
        reference_lines = list(
          baseline = list(label = "Baseline Mean", line_mode = "dash"),
          measurement = list(label = "Measurement Value", line_mode = "solid")
        ),
        table_datanames = "longitudinal_data",
        reactable_args = list(
          pagination = TRUE,
          searchable = TRUE,
          sortable = TRUE,
          filterable = TRUE,
          resizable = TRUE,
          defaultPageSize = 15,
          defaultSorted = list(subject_id = "asc", time_week = "asc")
        )
      )
    ),
    
    # scatterplot ----
    modules(
      label = "tm_mdr_scatterplot",
      tm_mdr_scatterplot(
        label = "simple",
        plot_dataname = "tumor_response",
        subject_var = "subject_id",
        x_var = "time_week",
        y_var = "tumor_size",
        color_var = "treatment",
        variation = "simple",
        tooltip_vars = c("subject_id", "time_week", "tumor_size", "treatment", "response_category"),
        point_colors = c("Experimental" = "#2ca02c", "Control" = "#d62728"),
        transformators = list(),
        reference_lines = list(
          baseline_size = list(label = "Baseline Mean", line_mode = "dash")
        )
      ),
      tm_mdr_scatterplot(
        label = "line",
        plot_dataname = "tumor_response",
        subject_var = "subject_id",
        x_var = "time_week",
        y_var = "tumor_size",
        color_var = "treatment",
        variation = "line",
        tooltip_vars = c("subject_id", "time_week", "tumor_size", "treatment", "response_category"),
        point_colors = c("Experimental" = "#1f77b4", "Control" = "#ff7f0e"),
        transformators = list(),
        reference_lines = list(
          baseline_size = list(label = "Baseline Mean", line_mode = "dash")
        )
      ),
      tm_mdr_scatterplot(
        label = "tables",
        plot_dataname = "tumor_response",
        subject_var = "subject_id",
        x_var = "time_week",
        y_var = "tumor_size",
        color_var = "response_category",
        variation = "tables",
        tooltip_vars = c("subject_id", "time_week", "tumor_size", "response_category", "treatment", "age_group", "center"),
        point_colors = c(
          "Complete Response" = "#00FF00",
          "Partial Response" = "#FFFF00",
          "Stable Disease" = "#FFA500",
          "Progressive Disease" = "#FF0000"
        ),
        transformators = list(),
        reference_lines = list(
          baseline_size = list(label = "Baseline Mean", line_mode = "dash"),
          "30" = list(label = "Response Threshold (-30%)", line_mode = "dot")
        ),
        table_datanames = "tumor_response",
        reactable_args = list(
          pagination = TRUE,
          searchable = TRUE,
          sortable = TRUE,
          filterable = TRUE,
          resizable = TRUE,
          defaultPageSize = 15,
          defaultSorted = list(subject_id = "asc", time_week = "asc")
        )
      ),
      tm_mdr_scatterplot(
        label = "line_table",
        plot_dataname = "tumor_response",
        subject_var = "subject_id",
        x_var = "time_week",
        y_var = "tumor_size",
        color_var = "response_category",
        variation = "line_table",
        tooltip_vars = c("subject_id", "time_week", "tumor_size", "response_category", "treatment", "age_group", "center"),
        point_colors = c(
          "Complete Response" = "#00FF00",
          "Partial Response" = "#FFFF00",
          "Stable Disease" = "#FFA500",
          "Progressive Disease" = "#FF0000"
        ),
        transformators = list(),
        reference_lines = list(
          baseline_size = list(label = "Baseline Mean", line_mode = "dash"),
          "30" = list(label = "Response Threshold (-30%)", line_mode = "dot")
        ),
        table_datanames = "tumor_response",
        reactable_args = list(
          pagination = TRUE,
          searchable = TRUE,
          sortable = TRUE,
          filterable = TRUE,
          resizable = TRUE,
          defaultPageSize = 15,
          defaultSorted = list(subject_id = "asc", time_week = "asc")
        )
      )
    ),
    
    # spaghetti plot ----
    modules(
      label = "tm_mdr_spaghetti",
      tm_mdr_spaghetti(
        label = "simple",
        plot_dataname = "biomarker_data",
        group_var = "subject_id",
        x_var = "time_week",
        y_var = "biomarker_level",
        color_var = "treatment",
        variation = "simple",
        tooltip_vars = c("subject_id", "time_week", "biomarker_level", "treatment", "response_category"),
        point_colors = c("Experimental" = "#1f77b4", "Control" = "#ff7f0e"),
        transformators = list(),
        reference_lines = list(
          baseline_level = list(label = "Baseline Mean", line_mode = "dash")
        )
      ),
      tm_mdr_spaghetti(
        label = "line",
        plot_dataname = "biomarker_data",
        group_var = "subject_id",
        x_var = "time_week",
        y_var = "biomarker_level",
        color_var = "treatment",
        variation = "line",
        tooltip_vars = c("subject_id", "time_week", "biomarker_level", "treatment", "response_category"),
        point_colors = c("Experimental" = "#2ca02c", "Control" = "#d62728"),
        transformators = list(),
        reference_lines = list(
          baseline_level = list(label = "Baseline Mean", line_mode = "dash")
        )
      ),
      tm_mdr_spaghetti(
        label = "tables",
        plot_dataname = "biomarker_data",
        group_var = "subject_id",
        x_var = "time_week",
        y_var = "biomarker_level",
        color_var = "response_category",
        variation = "tables",
        tooltip_vars = c("subject_id", "time_week", "biomarker_level", "response_category", "treatment", "age_group", "dose_group", "center"),
        point_colors = c(
          "Responder" = "#00FF00",
          "Stable" = "#FFA500",
          "Non-responder" = "#FF0000"
        ),
        transformators = list(),
        reference_lines = list(
          baseline_level = list(label = "Baseline Mean", line_mode = "dash"),
          "80" = list(label = "Target Level", line_mode = "dot")
        ),
        table_datanames = "biomarker_data",
        reactable_args = list(
          pagination = TRUE,
          searchable = TRUE,
          sortable = TRUE,
          filterable = TRUE,
          resizable = TRUE,
          defaultPageSize = 20
        )
      ),
      tm_mdr_spaghetti(
        label = "line_table",
        plot_dataname = "biomarker_data",
        group_var = "subject_id",
        x_var = "time_week",
        y_var = "biomarker_level",
        color_var = "response_category",
        variation = "line_table",
        tooltip_vars = c("subject_id", "time_week", "biomarker_level", "response_category", "treatment", "age_group", "dose_group", "center"),
        point_colors = c(
          "Responder" = "#00FF00",
          "Stable" = "#FFA500",
          "Non-responder" = "#FF0000"
        ),
        transformators = list(),
        reference_lines = list(
          baseline_level = list(label = "Baseline Mean", line_mode = "dash"),
          "80" = list(label = "Target Level", line_mode = "dot")
        ),
        table_datanames = "biomarker_data",
        reactable_args = list(
          pagination = TRUE,
          searchable = TRUE,
          sortable = TRUE,
          filterable = TRUE,
          resizable = TRUE,
          defaultPageSize = 20
        )
      )
    ),
    
    # spider plot ----
    modules(
      label = "tm_mdr_spiderplot",
      tm_mdr_spiderplot(
        label = "simple",
        plot_dataname = "spider_data",
        time_var = "time_week",
        value_var = "percent_change",
        subject_var = "subject_id",
        color_var = "response_category",
        filter_event_var = "treatment",
        variation = "simple"
      ),
      tm_mdr_spiderplot(
        label = "tables",
        plot_dataname = "spider_data",
        time_var = "time_week",
        value_var = "percent_change",
        subject_var = "subject_id",
        color_var = "response_category",
        filter_event_var = "treatment",
        variation = "tables",
        table_datanames = c("subjects", "spider_data"),
        size_var = "baseline_tumor_size",
        tooltip_vars = c("subject_id", "time_week", "percent_change", "response_category", "treatment", "age_group", "center"),
        point_colors = c(
          "Complete Response" = "#00FF00",
          "Partial Response" = "#FFFF00",
          "Stable Disease" = "#FFA500",
          "Progressive Disease" = "#FF0000"
        ),
        point_symbols = c(
          "Complete Response" = "circle",
          "Partial Response" = "square",
          "Stable Disease" = "triangle-up",
          "Progressive Disease" = "diamond"
        ),
        plot_height = c(700, 400, 1000),
        reactable_args = list(
          pagination = TRUE,
          searchable = TRUE,
          sortable = TRUE,
          filterable = TRUE,
          resizable = TRUE
        )
      )
    ),
    
    # swimlane plot ----
    modules(
      label = "tm_mdr_swimlane",
      tm_mdr_swimlane(
        label = "simple",
        plot_dataname = "swimlane_events",
        time_var = "study_day",
        subject_var = "subject_id",
        color_var = "response_category",
        group_var = "event_type",
        variation = "simple",
        sort_var = "study_day",
        tooltip_vars = c("subject_id", "study_day", "response_category", "event_type", "assessment_visit"),
        point_size = 10,
        point_colors = c(
          "Baseline" = "#808080",
          "Complete Response" = "#00AA00",
          "Partial Response" = "#66BB6A",
          "Stable Disease" = "#FFA726",
          "Progressive Disease" = "#EF5350",
          "End of Study" = "#424242"
        ),
        point_symbols = character(0),
        plot_height = c(700, 400, 1200)
      ),
      tm_mdr_swimlane(
        label = "tables",
        plot_dataname = "swimlane_events",
        time_var = "study_day",
        subject_var = "subject_id",
        color_var = "response_category",
        group_var = "assessment_visit",
        variation = "tables",
        sort_var = "study_day",
        tooltip_vars = c("subject_id", "study_day", "response_category", "assessment_visit", "treatment_arm"),
        point_size = 12,
        point_colors = c(
          "Baseline" = "#808080",
          "Complete Response" = "#00AA00",
          "Partial Response" = "#66BB6A",
          "Stable Disease" = "#FFA726",
          "Progressive Disease" = "#EF5350",
          "End of Study" = "#424242"
        ),
        point_symbols = c(
          "Baseline" = "circle",
          "Complete Response" = "star",
          "Partial Response" = "diamond",
          "Stable Disease" = "square",
          "Progressive Disease" = "triangle-up",
          "End of Study" = "x"
        ),
        plot_height = c(800, 500, 1200),
        table_datanames = c("swimlane_subjects", "swimlane_events"),
        reactable_args = list(
          pagination = TRUE,
          searchable = TRUE,
          sortable = TRUE,
          filterable = TRUE,
          resizable = TRUE,
          defaultPageSize = 15
        )
      )
    ),
    
    # waterfall plot ----
    modules(
      label = "tm_mdr_waterfall",
      tm_mdr_waterfall(
        label = "simple",
        plot_dataname = "waterfall_response",
        subject_var = "subject_id",
        x_var = "subject_id",
        value_var = "percent_change",
        variation = "simple",
        sort_var = NULL,
        color_var = "response_category",
        tooltip_vars = c("subject_id", "percent_change", "best_response", "treatment_arm"),
        bar_colors = c(
          "CR" = "#00AA00",
          "PR" = "#66BB6A",
          "SD" = "#FFA726",
          "PD" = "#EF5350"
        ),
        value_arbitrary_hlines = c(20, -30),
        plot_title = "Best Tumor Response - Simple View",
        plot_height = c(600, 400, 1200)
      ),
      tm_mdr_waterfall(
        label = "tables",
        plot_dataname = "waterfall_response",
        subject_var = "subject_id",
        x_var = "subject_id",
        value_var = "percent_change",
        variation = "tables",
        sort_var = "percent_change",
        color_var = "response_category",
        tooltip_vars = c("subject_id", "percent_change", "best_response", "treatment_arm", "assessment_week", "duration_response"),
        bar_colors = c(
          "CR" = "#00AA00",
          "PR" = "#66BB6A",
          "SD" = "#FFA726",
          "PD" = "#EF5350"
        ),
        value_arbitrary_hlines = c(20, -30),
        plot_title = "Best Tumor Response by Treatment Arm - With Tables",
        plot_height = c(700, 500, 1000),
        table_datanames = c("waterfall_subjects", "waterfall_response"),
        reactable_args = list(
          pagination = TRUE,
          searchable = TRUE,
          sortable = TRUE,
          filterable = TRUE,
          resizable = TRUE,
          defaultPageSize = 10
        )
      )
    ), 
    
    # butterfly plot ----
    modules = modules(
      label = "tm_mdr_butterfly",
      tm_mdr_butterfly(
        label = "simple",
        plot_dataname = "adverse_events",
        y_var = "adverse_event",
        color_var = "severity",
        count_var = "subject_id",
        group_var = "treatment",
        variation = "simple"
      ),
      tm_mdr_butterfly(
          label = "tables",
          plot_dataname = "adverse_events",
          y_var = "adverse_event",
          color_var = "severity",
          count_var = "subject_id",
          group_var = "treatment",
          variation = "tables",
          table_datanames = "adverse_events"
        ),
        tm_mdr_butterfly(
          label = "static_tables",
          plot_dataname = "adverse_events",
          y_var = "adverse_event",
          color_var = "severity",
          count_var = "subject_id",
          group_var = "treatment",
          variation = "static_tables",
          show_percent = TRUE,
          butterfly_colors = c(
            "Mild" = "#90EE90",
            "Moderate" = "#FFD700",
            "Severe" = "#FF6347"
          ),
          table_datanames = "adverse_events",
          reactable_args = list(
            pagination = TRUE,
            searchable = TRUE,
            sortable = TRUE,
            filterable = TRUE,
            resizable = TRUE
          )
        
      )
      
    ), 
    
    # patient_profile ----
    modules = modules(
      label = "tm_mdr_patient_profile",
      tm_mdr_patient_profile(
        label = "simple",
        plot_dataname = "patient_profile",
        y_var = "event_term",
        start_date_var = "start_day",
        end_date_var = "end_day",
        color_var = "event_status",
        group_var = "event_category",
        variation = "simple"
      ), 
      tm_mdr_patient_profile(
        label = "simple - Custom Colors and Symbols",
        plot_dataname = "patient_profile",
        y_var = "event_term",
        start_date_var = "start_day",
        end_date_var = "end_day",
        color_var = "event_status",
        group_var = "event_category",
        variation = "simple",
        profile_colors = c(
          "Ongoing" = "#E41A1C", # Red
          "Resolved" = "#4DAF4A", # Green
          "Completed" = "#377EB8" # Blue
        ),
        point_symbols_start = c(
          "Ongoing" = "star",
          "Resolved" = "square",
          "Completed" = "diamond"
        ),
        point_symbols_end = c(
          "Ongoing" = "circle-open",
          "Resolved" = "square-open",
          "Completed" = "diamond-open"
        ),
        tooltip_vars = c("subject_id", "event_status", "event_term", "start_day", "end_day"),
        line_width = 4,
        reference_lines = list(
          "Screening" = -230,
          "First Administration" = 0
        )
      )#,
      
      # tm_mdr_patient_profile(
      #   label = "static tables",
      #   plot_dataname = "patient_profile",
      #   y_var = "event_term",
      #   start_date_var = "start_day",
      #   end_date_var = "end_day",
      #   color_var = "event_status",
      #   group_var = "event_category",
      #   variation = "static_tables",
      #   tooltip_vars = c("subject_id", "event_status", "event_term", "start_day", "end_day"),
      #   profile_colors = c(
      #     "Ongoing" = "#E41A1C",
      #     "Resolved" = "#4DAF4A",
      #     "Completed" = "#377EB8"
      #   ),
      #   table_datanames = "patient_profile",
      #   reactable_args = list(
      #     pagination = TRUE,
      #     searchable = TRUE,
      #     sortable = TRUE,
      #     filterable = TRUE,
      #     resizable = TRUE
      #   )
      # )
    ), 
    
    # boxplot ----
    modules = modules(
      label = 'tm_mdr_boxplot',
      tm_mdr_boxplot(
        label = "Simple Box Plot",
        plot_dataname = "lab_data",
        y_var = "lab_value",
        x_var = "lab_test",
        color_var = "treatment",
        variation = "simple"
      ), 
      tm_mdr_boxplot(
        label = "Box Plot with Linked Tables",
        plot_dataname = "lab_data",
        y_var = "lab_value",
        x_var = "lab_test",
        color_var = "treatment",
        variation = "tables",
        box_colors = c(
          "Control" = "#1f77b4",
          "Experimental" = "#ff7f0e"
        ),
        tooltip_vars = c("subject_id", "lab_test", "lab_value", "treatment", "visit"),
        table_datanames = "lab_data",
        reactable_args = list(
          pagination = TRUE,
          searchable = TRUE,
          sortable = TRUE,
          filterable = TRUE
        )
      ), 
      
      tm_mdr_boxplot(
        label = "Box Plot with Static Tables",
        plot_dataname = "lab_data",
        y_var = "lab_value",
        x_var = "lab_test",
        color_var = "treatment",
        variation = "static_tables",
        tooltip_vars = c("subject_id", "lab_test", "lab_value", "treatment", "visit", "age_group"),
        table_datanames = "lab_data",
        reactable_args = list(
          pagination = TRUE,
          searchable = TRUE,
          sortable = TRUE,
          filterable = TRUE,
          resizable = TRUE
        )
      )
      
    )#,
    
    # reactables ----# 
    # modules = modules(
    #   label = "tm_t_reactables",
    #   tm_t_reactables(
    #     label = "Interactive Tables"
    #   ), 
    #   
    #   tm_t_reactables(
    #     label = "Advanced Tables",
    #     datanames = c("adsl", "adae"),
    #     colnames = list(
    #       adsl = c("USUBJID", "AGE", "SEX", "ARM"),
    #       adae = c("USUBJID", "AEDECOD", "AESEV")
    #     ),
    #     reactable_args = list(
    #       pagination = TRUE,
    #       searchable = TRUE,
    #       filterable = TRUE,
    #       sortable = TRUE,
    #       defaultPageSize = 10,
    #       highlight = TRUE,
    #       striped = TRUE
    #     )
    #   )
    # )
  )
)


# RUN APP ----

shinyApp(app$ui, app$server)
