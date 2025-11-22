?tm_mdr_bargraph
# Prepare example data for adverse event analysis
library(teal)

data <- teal_data() |>
  within({
    # Adverse event data mimicking ADAE structure
    df <- data.frame(
      adverse_event = sample(
        c(
          "Headache",
          "Nausea",
          "Fatigue",
          "Dizziness",
          "Rash",
          "Insomnia",
          "Diarrhea",
          "Constipation"
        ),
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
        rep("Nervous System", 94), # Headache, Dizziness, Fatigue, Insomnia
        rep("Gastrointestinal", 60), # Nausea, Diarrhea, Constipation
        rep("Skin", 20), # Rash
        rep("General", 26) # Fatigue overlap, others
      )[sample(200)],
      subject_id = sample(paste0("S", 1:50), 200, replace = TRUE),
      treatment = sample(c("Experimental", "Control"), 200, replace = TRUE),
      age_group = sample(
        c("18-35", "36-55", "56-75", "75+"),
        200,
        replace = TRUE
      ),
      center = sample(
        c("Site A", "Site B", "Site C", "Site D", "Site E"),
        200,
        replace = TRUE
      ),
      preferred_term = sample(
        c(
          "Headache NOS",
          "Nausea",
          "Fatigue",
          "Dizziness postural",
          "Rash generalized",
          "Insomnia",
          "Diarrhea NOS",
          "Constipation"
        ),
        200,
        replace = TRUE
      )
    )

    # Add variable labels for better table display
    attr(df$adverse_event, "label") <- "Adverse Event Term"
    attr(df$severity, "label") <- "Severity Grade"
    attr(df$system_organ_class, "label") <- "System Organ Class"
    attr(df$subject_id, "label") <- "Subject ID"
    attr(df$treatment, "label") <- "Treatment Group"
    attr(df$age_group, "label") <- "Age Group"
    attr(df$center, "label") <- "Study Center"
    attr(df$preferred_term, "label") <- "Preferred Term"
  })


data$df |> head()


# ---------------------------------------------------------------------------
# Example 1: Simple variation - Basic bar chart visualization
# ---------------------------------------------------------------------------
# Use case: Quick visual summary of adverse events by system organ class
# Features: Single bar chart with color picker, no interaction

app <- init(
  data = data,
  modules = modules(
    tm_mdr_bargraph(
      label = "Simple Bar Graph",
      plot_dataname = "df",
      y_var = "system_organ_class",
      color_var = "treatment",
      count_var = "subject_id",
      variation = "simple"
    )
  )
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}

# ---------------------------------------------------------------------------
# Example 2: Bar variation - Hierarchical analysis
# ---------------------------------------------------------------------------
# Use case: Analyze adverse events from system organ class down to specific terms
# Features: Two linked bar charts - select SOC to see breakdown by specific AE
# Interaction: Brush (drag) on top chart to filter bottom chart

app <- init(
  data = data,
  modules = modules(
    tm_mdr_bargraph(
      label = "AE Drilldown: SOC to Term",
      plot_dataname = "df",
      y_var = "system_organ_class",
      color_var = "treatment",
      count_var = "subject_id",
      variation = "bar",
      secondary_y_var = "adverse_event",
      tooltip_vars = c(
        "system_organ_class",
        "adverse_event",
        "treatment",
        "center"
      )
    )
  )
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}

# ---------------------------------------------------------------------------
# Example 3: Table variation - Summary with record-level detail
# ---------------------------------------------------------------------------
# Use case: Explore AE summaries and view detailed records for selected categories
# Features: Bar chart with linked data tables
# Interaction: Brush on chart to filter tables to matching records

app <- init(
  data = data,
  modules = modules(
    tm_mdr_bargraph(
      label = "AE Summary with Details",
      plot_dataname = "df",
      y_var = "system_organ_class",
      color_var = "treatment",
      count_var = "subject_id",
      variation = "table",
      table_datanames = "df",
      tooltip_vars = c("system_organ_class", "treatment", "severity"),
      reactable_args = list(
        pagination = TRUE,
        searchable = TRUE,
        defaultPageSize = 10
      )
    )
  )
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}

# ---------------------------------------------------------------------------
# Example 4: Bar_table variation - Complete MDR workflow
# ---------------------------------------------------------------------------
# Use case: Full adverse event analysis workflow for medical data review
# Features: Hierarchical drill-down with linked record-level tables
# Interaction: Select from either chart to filter tables
# This is the most comprehensive variation combining all features

app <- init(
  data = data,
  modules = modules(
    tm_mdr_bargraph(
      label = "Complete AE Analysis",
      plot_dataname = "df",
      y_var = "system_organ_class",
      color_var = "severity",
      count_var = "subject_id",
      variation = "bar_table",
      secondary_y_var = "adverse_event",
      tooltip_vars = c(
        "system_organ_class",
        "adverse_event",
        "severity",
        "treatment",
        "center"
      ),
      bar_colors = c(
        "Mild" = "#90EE90", # Light green
        "Moderate" = "#FFD700", # Gold
        "Severe" = "#FF6347" # Tomato red
      ),
      table_datanames = "df",
      reactable_args = list(
        pagination = TRUE,
        searchable = TRUE,
        sortable = TRUE,
        filterable = TRUE,
        resizable = TRUE,
        defaultPageSize = 15
      )
    )
  )
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}
