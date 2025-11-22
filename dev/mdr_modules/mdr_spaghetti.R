data <- teal_data() |>
  within({
    df <- data.frame(
      subject_id = rep(paste0("S", 1:15), each = 5),
      time_week = rep(c(0, 4, 8, 12, 16), 15),
      biomarker_level = c(
        # Responders (S1-S5): decreasing biomarker levels
        c(100, 85, 70, 55, 40) + rnorm(5, 0, 3),
        c(95, 78, 62, 48, 35) + rnorm(5, 0, 3),
        c(110, 92, 75, 58, 42) + rnorm(5, 0, 3),
        c(88, 72, 58, 45, 32) + rnorm(5, 0, 3),
        c(105, 88, 70, 52, 38) + rnorm(5, 0, 3),
        # Stable/Mixed (S6-S10): variable patterns
        c(90, 88, 92, 85, 90) + rnorm(5, 0, 4),
        c(75, 80, 78, 82, 77) + rnorm(5, 0, 4),
        c(85, 82, 88, 84, 86) + rnorm(5, 0, 4),
        c(92, 95, 90, 94, 88) + rnorm(5, 0, 4),
        c(78, 75, 80, 76, 79) + rnorm(5, 0, 4),
        # Non-responders (S11-S15): increasing or stable high levels
        c(95, 100, 108, 115, 125) + rnorm(5, 0, 3),
        c(88, 92, 98, 105, 115) + rnorm(5, 0, 3),
        c(102, 105, 110, 118, 128) + rnorm(5, 0, 3),
        c(85, 90, 95, 102, 112) + rnorm(5, 0, 3),
        c(98, 102, 108, 115, 125) + rnorm(5, 0, 3)
      ),
      treatment = rep(c("Experimental", "Control"), length.out = 75),
      response_category = rep(
        c(
          rep("Responder", 5), # S1-S5
          rep("Stable", 5), # S6-S10
          rep("Non-responder", 5) # S11-S15
        ),
        each = 5
      ),
      age_group = rep(
        sample(c("Young", "Middle", "Old"), 15, replace = TRUE),
        each = 5
      ),
      baseline_level = rep(
        c(100, 95, 110, 88, 105, 90, 75, 85, 92, 78, 95, 88, 102, 85, 98),
        each = 5
      ),
      center = rep(
        sample(c("Site A", "Site B", "Site C", "Site D"), 15, replace = TRUE),
        each = 5
      ),
      dose_group = rep(
        sample(c("Low", "Medium", "High"), 15, replace = TRUE),
        each = 5
      )
    )

    # Add labels
    attr(df$subject_id, "label") <- "Subject ID"
    attr(df$time_week, "label") <- "Time (weeks)"
    attr(df$biomarker_level, "label") <- "Biomarker Level (ng/mL)"
    attr(df$treatment, "label") <- "Treatment Group"
    attr(df$response_category, "label") <- "Response Category"
    attr(df$age_group, "label") <- "Age Group"
    attr(df$baseline_level, "label") <- "Baseline Biomarker Level"
    attr(df$center, "label") <- "Study Center"
    attr(df$dose_group, "label") <- "Dose Group"
  })

# Example 1: Simple variation - Spaghetti plot only
app <- init(
  data = data,
  modules = modules(
    tm_mdr_spaghetti(
      label = "Simple Spaghetti Plot",
      plot_dataname = "df",
      group_var = "subject_id",
      x_var = "time_week",
      y_var = "biomarker_level",
      color_var = "treatment",
      variation = "simple"
    )
  )
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}

# Example 2: Line variation - Spaghetti plot with linked line plot
app <- init(
  data = data,
  modules = modules(
    tm_mdr_spaghetti(
      label = "Spaghetti + Line Plot",
      plot_dataname = "df",
      group_var = "subject_id",
      x_var = "time_week",
      y_var = "biomarker_level",
      color_var = "treatment",
      variation = "line"
    )
  )
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}

# Example 3: Tables variation - Spaghetti plot with linked tables
app <- init(
  data = data,
  modules = modules(
    tm_mdr_spaghetti(
      label = "Spaghetti Plot with Tables",
      plot_dataname = "df",
      group_var = "subject_id",
      x_var = "time_week",
      y_var = "biomarker_level",
      color_var = "response_category",
      variation = "tables",
      tooltip_vars = c(
        "subject_id",
        "time_week",
        "biomarker_level",
        "response_category",
        "treatment",
        "age_group",
        "dose_group",
        "center"
      ),
      point_colors = c(
        "Responder" = "#00FF00", # Green
        "Stable" = "#FFA500", # Orange
        "Non-responder" = "#FF0000" # Red
      ),
      table_datanames = "df",
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

# Example 4: Line_Table variation - Spaghetti plot with linked line plot and tables
app <- init(
  data = data,
  modules = modules(
    tm_mdr_spaghetti(
      label = "Spaghetti + Line Plot with Tables",
      plot_dataname = "df",
      group_var = "subject_id",
      x_var = "time_week",
      y_var = "biomarker_level",
      color_var = "response_category",
      variation = "line_table",
      tooltip_vars = c(
        "subject_id",
        "time_week",
        "biomarker_level",
        "response_category",
        "treatment",
        "age_group",
        "dose_group",
        "center"
      ),
      point_colors = c(
        "Responder" = "#00FF00", # Green
        "Stable" = "#FFA500", # Orange
        "Non-responder" = "#FF0000" # Red
      ),
      reference_lines = list(
        baseline_level = list(label = "Baseline Mean", line_mode = "dash"),
        "80" = list(label = "Target Level", line_mode = "dot")
      ),
      table_datanames = "df",
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
