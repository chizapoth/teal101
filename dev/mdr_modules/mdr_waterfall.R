data <- teal_data() |>
  within({
    # Subject demographics and baseline characteristics
    subjects <- data.frame(
      subject_id = paste0("PT", sprintf("%03d", 1:15)),
      age = sample(25:75, 15),
      sex = sample(c("Male", "Female"), 15, replace = TRUE),
      treatment_arm = rep(
        c("Experimental Drug", "Standard of Care", "Combination"),
        each = 5
      ),
      baseline_ecog = sample(0:2, 15, replace = TRUE, prob = c(0.6, 0.3, 0.1)),
      enrollment_date = sample(
        seq(as.Date("2023-01-01"), as.Date("2023-06-01"), by = "day"),
        15
      ),
      center = sample(
        c("Site A", "Site B", "Site C", "Site D"),
        15,
        replace = TRUE
      ),
      baseline_tumor_size = round(rnorm(15, 50, 15), 1),
      histology = sample(
        c("Adenocarcinoma", "Squamous Cell", "Large Cell"),
        15,
        replace = TRUE
      )
    )

    # Tumor response waterfall data
    tumor_response <- data.frame(
      subject_id = paste0("PT", sprintf("%03d", 1:15)),
      percent_change = c(
        # Experimental Drug arm - better responses
        -85,
        -72,
        -58,
        -45,
        -31,
        # Standard of Care arm - mixed responses
        -42,
        -28,
        -15,
        8,
        25,
        # Combination arm - variable responses
        -78,
        -55,
        -32,
        12,
        45
      ),
      best_response = c(
        # Experimental Drug responses
        "Complete Response",
        "Partial Response",
        "Partial Response",
        "Partial Response",
        "Stable Disease",
        # Standard of Care responses
        "Partial Response",
        "Stable Disease",
        "Stable Disease",
        "Progressive Disease",
        "Progressive Disease",
        # Combination responses
        "Complete Response",
        "Partial Response",
        "Stable Disease",
        "Progressive Disease",
        "Progressive Disease"
      ),
      response_category = c(
        # Experimental Drug
        "CR",
        "PR",
        "PR",
        "PR",
        "SD",
        # Standard of Care
        "PR",
        "SD",
        "SD",
        "PD",
        "PD",
        # Combination
        "CR",
        "PR",
        "SD",
        "PD",
        "PD"
      ),
      assessment_week = c(
        # Assessment timing varies by response
        12,
        8,
        12,
        16,
        12,
        8,
        12,
        16,
        8,
        12,
        16,
        12,
        12,
        8,
        16
      ),
      duration_response = c(
        # Response duration in weeks (NA for non-responders)
        24,
        18,
        22,
        16,
        NA,
        14,
        NA,
        NA,
        NA,
        NA,
        28,
        20,
        NA,
        NA,
        NA
      )
    )

    # Add treatment arm to response data
    tumor_response <- merge(
      tumor_response,
      subjects[c("subject_id", "treatment_arm", "baseline_ecog")],
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
    attr(subjects$histology, "label") <- "Histology Type"

    attr(tumor_response$subject_id, "label") <- "Subject ID"
    attr(
      tumor_response$percent_change,
      "label"
    ) <- "Best % Change from Baseline"
    attr(tumor_response$best_response, "label") <- "Best Overall Response"
    attr(tumor_response$response_category, "label") <- "Response Category"
    attr(tumor_response$assessment_week, "label") <- "Assessment Week"
    attr(
      tumor_response$duration_response,
      "label"
    ) <- "Duration of Response (weeks)"
    attr(tumor_response$treatment_arm, "label") <- "Treatment Arm"
    attr(tumor_response$baseline_ecog, "label") <- "Baseline ECOG"
  })

join_keys(data) <- join_keys(
  join_key("subjects", "tumor_response", keys = c(subject_id = "subject_id"))
)

# Example 1: Simple variation - Waterfall plot with no tables
app <- init(
  data = data,
  modules = modules(
    tm_mdr_waterfall(
      label = "Simple Waterfall Plot",
      plot_dataname = "tumor_response",
      subject_var = "subject_id",
      x_var = "subject_id",
      value_var = "percent_change",
      color_var = "response_category",
      variation = "simple"
    )
  )
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}

# Example 2: Tables variation - Waterfall plot with linked tables
app <- init(
  data = data,
  modules = modules(
    tm_mdr_waterfall(
      label = "Waterfall Plot with Tables",
      plot_dataname = "tumor_response",
      subject_var = "subject_id",
      x_var = "subject_id",
      value_var = "percent_change",
      variation = "tables",
      sort_var = "percent_change",
      color_var = "response_category",
      tooltip_vars = c(
        "subject_id",
        "percent_change",
        "best_response",
        "treatment_arm",
        "assessment_week",
        "duration_response"
      ),
      bar_colors = c(
        "CR" = "#00AA00", # Dark Green for Complete Response
        "PR" = "#66BB6A", # Light Green for Partial Response
        "SD" = "#FFA726", # Orange for Stable Disease
        "PD" = "#EF5350" # Red for Progressive Disease
      ),
      value_arbitrary_hlines = c(20, -30), # RECIST thresholds
      plot_title = "Best Tumor Response by Treatment Arm",
      plot_height = c(700, 500, 1000),
      table_datanames = c("subjects", "tumor_response"),
      reactable_args = list(
        pagination = TRUE,
        searchable = TRUE,
        sortable = TRUE,
        filterable = TRUE,
        resizable = TRUE,
        defaultPageSize = 10
      )
    )
  )
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}
