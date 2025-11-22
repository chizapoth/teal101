# Another example with simpler data ----
data <- teal_data() |>
  within({
    subjects <- data.frame(
      subject_id = paste0("S", sprintf("%03d", 1:12)),
      age = sample(25:75, 12),
      sex = sample(c("Male", "Female"), 12, replace = TRUE),
      treatment_arm = rep(c("Experimental", "Control"), each = 6),
      baseline_ecog = sample(0:2, 12, replace = TRUE, prob = c(0.6, 0.3, 0.1)),
      enrollment_date = sample(
        seq(as.Date("2023-01-01"), as.Date("2023-06-01"), by = "day"),
        12
      ),
      center = sample(c("Site A", "Site B", "Site C"), 12, replace = TRUE),
      baseline_tumor_size = round(rnorm(12, 50, 15), 1)
    )

    # Clinical events swimlane data
    swimlane_events <- data.frame(
      subject_id = c(
        rep("S001", 4),
        rep("S002", 3),
        rep("S003", 5),
        rep("S004", 4),
        rep("S005", 3),
        rep("S006", 4),
        rep("S007", 3),
        rep("S008", 5),
        rep("S009", 4),
        rep("S010", 3),
        rep("S011", 4),
        rep("S012", 3)
      ),
      study_day = c(
        # S001: Experimental arm - good response
        c(1, 28, 56, 84),
        # S002: Experimental arm - moderate response
        c(1, 28, 84),
        # S003: Experimental arm - progression
        c(1, 28, 56, 70, 84),
        # S004: Experimental arm - mixed response
        c(1, 28, 56, 84),
        # S005: Experimental arm - stable
        c(1, 28, 84),
        # S006: Experimental arm - early progression
        c(1, 28, 42, 84),
        # S007: Control arm - limited response
        c(1, 28, 84),
        # S008: Control arm - progression
        c(1, 28, 42, 56, 70),
        # S009: Control arm - stable then progress
        c(1, 28, 56, 84),
        # S010: Control arm - no response
        c(1, 28, 56),
        # S011: Control arm - rapid progression
        c(1, 28, 42, 56),
        # S012: Control arm - minimal benefit
        c(1, 28, 84)
      ),
      event_type = c(
        c("Baseline", "Response", "Response", "Complete Response"), # S001
        c("Baseline", "Response", "Stable Disease"), # S002
        c("Baseline", "Response", "Stable Disease", "Progression", "Off Study"), # S003
        c("Baseline", "Response", "Stable Disease", "Response"), # S004
        c("Baseline", "Stable Disease", "Stable Disease"), # S005
        c("Baseline", "Stable Disease", "Progression", "Off Study"), # S006
        c("Baseline", "Stable Disease", "Stable Disease"), # S007
        c(
          "Baseline",
          "Stable Disease",
          "Progression",
          "Progression",
          "Off Study"
        ), # S008
        c("Baseline", "Stable Disease", "Progression", "Off Study"), # S009
        c("Baseline", "Progression", "Off Study"), # S010
        c("Baseline", "Progression", "Progression", "Off Study"), # S011
        c("Baseline", "Stable Disease", "Stable Disease") # S012
      ),
      response_category = c(
        c(
          "Baseline",
          "Partial Response",
          "Partial Response",
          "Complete Response"
        ), # S001
        c("Baseline", "Partial Response", "Stable Disease"), # S002
        c(
          "Baseline",
          "Partial Response",
          "Stable Disease",
          "Progressive Disease",
          "End of Study"
        ), # S003
        c("Baseline", "Partial Response", "Stable Disease", "Partial Response"), # S004
        c("Baseline", "Stable Disease", "Stable Disease"), # S005
        c("Baseline", "Stable Disease", "Progressive Disease", "End of Study"), # S006
        c("Baseline", "Stable Disease", "Stable Disease"), # S007
        c(
          "Baseline",
          "Stable Disease",
          "Progressive Disease",
          "Progressive Disease",
          "End of Study"
        ), # S008
        c("Baseline", "Stable Disease", "Progressive Disease", "End of Study"), # S009
        c("Baseline", "Progressive Disease", "End of Study"), # S010
        c(
          "Baseline",
          "Progressive Disease",
          "Progressive Disease",
          "End of Study"
        ), # S011
        c("Baseline", "Stable Disease", "Stable Disease") # S012
      ),
      assessment_visit = c(
        c("Screening", "Cycle 2", "Cycle 4", "Cycle 6"), # S001
        c("Screening", "Cycle 2", "Cycle 6"), # S002
        c("Screening", "Cycle 2", "Cycle 4", "Unscheduled", "End of Treatment"), # S003
        c("Screening", "Cycle 2", "Cycle 4", "Cycle 6"), # S004
        c("Screening", "Cycle 2", "Cycle 6"), # S005
        c("Screening", "Cycle 2", "Cycle 3", "End of Treatment"), # S006
        c("Screening", "Cycle 2", "Cycle 6"), # S007
        c("Screening", "Cycle 2", "Cycle 3", "Cycle 4", "End of Treatment"), # S008
        c("Screening", "Cycle 2", "Cycle 4", "End of Treatment"), # S009
        c("Screening", "Cycle 2", "Cycle 4"), # S010
        c("Screening", "Cycle 2", "Cycle 3", "End of Treatment"), # S011
        c("Screening", "Cycle 2", "Cycle 6") # S012
      )
    )

    # Add treatment arm to events data
    swimlane_events <- merge(
      swimlane_events,
      subjects[c("subject_id", "treatment_arm")],
      by = "subject_id"
    )

    # Add labels
    attr(subjects$subject_id, "label") <- "Subject ID"
    attr(subjects$age, "label") <- "Age (years)"
    attr(subjects$sex, "label") <- "Sex"
    attr(subjects$treatment_arm, "label") <- "Treatment Arm"
    attr(subjects$baseline_ecog, "label") <- "ECOG Performance Status"
    attr(subjects$enrollment_date, "label") <- "Enrollment Date"
    attr(subjects$center, "label") <- "Study Center"
    attr(subjects$baseline_tumor_size, "label") <- "Baseline Tumor Size (mm)"

    attr(swimlane_events$subject_id, "label") <- "Subject ID"
    attr(swimlane_events$study_day, "label") <- "Study Day"
    attr(swimlane_events$event_type, "label") <- "Event Type"
    attr(swimlane_events$response_category, "label") <- "Response Category"
    attr(swimlane_events$assessment_visit, "label") <- "Assessment Visit"
    attr(swimlane_events$treatment_arm, "label") <- "Treatment Arm"
  })

join_keys(data) <- join_keys(
  join_key("subjects", "swimlane_events", keys = c(subject_id = "subject_id"))
)


data$swimlane_events
class(data$swimlane_events) # probably just need to convert it into a factor?


app <- init(
  data = data,
  modules = modules(
    tm_mdr_swimlane(
      label = "Simple Swimlane Plot",
      plot_dataname = "swimlane_events",
      time_var = "study_day",
      subject_var = "subject_id",
      color_var = "response_category",
      group_var = "event_type",
      sort_var = "study_day",
      variation = "simple"
    )
  )
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}

# Example 2: Tables variation - Swimlane plot with linked tables
app <- init(
  data = data,
  modules = modules(
    tm_mdr_swimlane(
      label = "Swimlane Plot with Tables",
      plot_dataname = "swimlane_events",
      time_var = "study_day",
      subject_var = "subject_id",
      color_var = "response_category",
      group_var = "assessment_visit",
      sort_var = "study_day",
      variation = "tables",
      table_datanames = c("subjects", "swimlane_events"),
      tooltip_vars = c(
        "subject_id",
        "study_day",
        "event_type",
        "response_category",
        "assessment_visit",
        "treatment_arm"
      ),
      point_size = c(
        "Complete Response" = 15,
        "Partial Response" = 12,
        "Stable Disease" = 10,
        "Progressive Disease" = 8,
        "Baseline" = 6,
        "End of Study" = 6
      ),
      point_colors = c(
        "Complete Response" = "#00FF00", # Green
        "Partial Response" = "#FFFF00", # Yellow
        "Stable Disease" = "#FFA500", # Orange
        "Progressive Disease" = "#FF0000", # Red
        "Baseline" = "#808080", # Gray
        "End of Study" = "#000000" # Black
      ),
      point_symbols = c(
        "Complete Response" = "circle",
        "Partial Response" = "square",
        "Stable Disease" = "triangle-up",
        "Progressive Disease" = "diamond",
        "Baseline" = "cross",
        "End of Study" = "x"
      ),
      legend_order = c(
        "Progressive Disease",
        "Stable Disease",
        "Partial Response",
        "Complete Response",
        "Baseline",
        "End of Study"
      ),
      plot_height = c(800, 500, 1200),
      reactable_args = list(
        pagination = TRUE,
        searchable = TRUE,
        sortable = TRUE,
        filterable = TRUE,
        resizable = TRUE
      )
    )
  )
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}
