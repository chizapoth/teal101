# Prepare example data for patient event profiles
library(teal)

data <- teal_data() |>
  within({
    # Create sample patient event data with multiple event types
    set.seed(123)
    df <- data.frame(
      subject_id = rep("S001", 12),
      event_category = c(
        rep("Medical History", 2),
        rep("Adverse Events", 3),
        rep("Concomitant Meds", 3),
        rep("Study Treatment", 2),
        rep("Lab Tests", 2)
      ),
      event_term = c(
        "Hypertension",
        "Diabetes",
        "Headache",
        "Nausea",
        "Fatigue",
        "Aspirin",
        "Ibuprofen",
        "Acetaminophen",
        "Drug A",
        "Drug B",
        "Chemistry Panel",
        "Hematology"
      ),
      start_day = c(
        -365,
        -180,
        15,
        22,
        30,
        1,
        15,
        25,
        1,
        29,
        7,
        35
      ),
      end_day = c(
        NA,
        NA,
        18,
        25,
        35,
        28,
        40,
        50,
        28,
        56,
        NA,
        NA
      ),
      event_status = c(
        "Ongoing",
        "Ongoing",
        "Resolved",
        "Resolved",
        "Ongoing",
        "Completed",
        "Completed",
        "Ongoing",
        "Completed",
        "Completed",
        "Completed",
        "Completed"
      ),
      severity = c(
        "Moderate",
        "Severe",
        "Mild",
        "Moderate",
        "Mild",
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA
      )
    )

    # Add variable labels for better display
    attr(df$subject_id, "label") <- "Subject ID"
    attr(df$event_category, "label") <- "Event Category"
    attr(df$event_term, "label") <- "Event Term"
    attr(df$start_day, "label") <- "Study Day"
    attr(df$end_day, "label") <- "End Day"
    attr(df$event_status, "label") <- "Status"
    attr(df$severity, "label") <- "Severity"

    # Order event categories
    df$event_category <- factor(
      df$event_category,
      levels = c(
        "Medical History",
        "Adverse Events",
        "Concomitant Meds",
        "Study Treatment",
        "Lab Tests"
      )
    )
  })


# ---------------------------------------------------------------------------
# Example 1: Simple variation - Basic patient event profile
# ---------------------------------------------------------------------------
# Use case: Visualize all event types for a patient on one timeline
# Features: Events grouped by category with visual separation

app <- init(
  data = data,
  modules = modules(
    tm_mdr_patient_profile(
      label = "Patient Event Timeline",
      plot_dataname = "df",
      y_var = "event_term",
      start_date_var = "start_day",
      end_date_var = "end_day",
      color_var = "event_status",
      group_var = "event_category",
      variation = "simple"
    )
  )
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}

# ---------------------------------------------------------------------------
# Example 2: Patient profile with custom colors
# ---------------------------------------------------------------------------
# Use case: Analyze event patterns with status-based colors
# Features: Custom color palette for event status

app <- init(
  data = data,
  modules = modules(
    tm_mdr_patient_profile(
      label = "Patient Events - Custom Colors",
      plot_dataname = "df",
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
      line_width = 4
    )
  )
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}

# ---------------------------------------------------------------------------
# Example 3: Patient profile with data tables
# ---------------------------------------------------------------------------
# Use case: Interactive exploration with linked data tables
# Features: Selection on plot filters data tables

app <- init(
  data = data,
  modules = modules(
    tm_mdr_patient_profile(
      label = "Interactive Patient Event Profile",
      plot_dataname = "df",
      y_var = "event_term",
      start_date_var = "start_day",
      end_date_var = "end_day",
      color_var = "event_status",
      group_var = "event_category",
      variation = "tables",
      table_datanames = "df"
    )
  )
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}
