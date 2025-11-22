data <- teal_data() |>
  within({
    df <- data.frame(
      subject_id = rep(paste0("S", 1:12), each = 6),
      time_week = rep(c(0, 2, 4, 8, 12, 16), 12),
      tumor_size = c(
        # Responders (subjects 1-4): tumor shrinkage over time
        c(45, 40, 32, 28, 25, 22) + rnorm(6, 0, 2),
        c(52, 48, 38, 30, 25, 20) + rnorm(6, 0, 2),
        c(38, 35, 28, 22, 18, 15) + rnorm(6, 0, 2),
        c(60, 55, 45, 38, 32, 28) + rnorm(6, 0, 2),
        # Stable disease (subjects 5-8): relatively stable
        c(42, 41, 43, 40, 42, 44) + rnorm(6, 0, 3),
        c(55, 52, 56, 54, 53, 57) + rnorm(6, 0, 3),
        c(35, 36, 34, 37, 35, 38) + rnorm(6, 0, 3),
        c(48, 50, 47, 49, 48, 51) + rnorm(6, 0, 3),
        # Progressive disease (subjects 9-12): tumor growth
        c(40, 45, 52, 58, 65, 72) + rnorm(6, 0, 2),
        c(35, 38, 45, 50, 58, 65) + rnorm(6, 0, 2),
        c(50, 55, 62, 68, 75, 82) + rnorm(6, 0, 2),
        c(45, 48, 55, 62, 70, 78) + rnorm(6, 0, 2)
      ),
      treatment = rep(c("Experimental", "Control"), each = 36),
      response_category = rep(
        c(
          "Partial Response",
          "Complete Response",
          "Partial Response",
          "Partial Response", # Responders
          "Stable Disease",
          "Stable Disease",
          "Stable Disease",
          "Stable Disease", # Stable
          "Progressive Disease",
          "Progressive Disease",
          "Progressive Disease",
          "Progressive Disease" # Progressive
        ),
        each = 6
      ),
      age_group = rep(
        sample(c("Young", "Middle", "Old"), 12, replace = TRUE),
        each = 6
      ),
      baseline_size = rep(
        c(45, 52, 38, 60, 42, 55, 35, 48, 40, 35, 50, 45),
        each = 6
      ),
      center = rep(
        sample(c("Site A", "Site B", "Site C"), 12, replace = TRUE),
        each = 6
      )
    )

    # Add labels
    attr(df$subject_id, "label") <- "Subject ID"
    attr(df$time_week, "label") <- "Time (weeks)"
    attr(df$tumor_size, "label") <- "Tumor Size (mm)"
    attr(df$treatment, "label") <- "Treatment Group"
    attr(df$response_category, "label") <- "Best Overall Response"
    attr(df$age_group, "label") <- "Age Group"
    attr(df$baseline_size, "label") <- "Baseline Tumor Size (mm)"
    attr(df$center, "label") <- "Study Center"
  })

# Example 1: Simple variation - Scatter plot only
app <- init(
  data = data,
  modules = modules(
    tm_mdr_scatterplot(
      label = "Simple Scatter Plot",
      plot_dataname = "df",
      subject_var = "subject_id",
      x_var = "time_week",
      y_var = "tumor_size",
      color_var = "treatment",
      variation = "simple"
    )
  )
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}

# Example 2: Line variation - Scatter plot with linked line plot
app <- init(
  data = data,
  modules = modules(
    tm_mdr_scatterplot(
      label = "Scatter + Line Plot",
      plot_dataname = "df",
      subject_var = "subject_id",
      x_var = "time_week",
      y_var = "tumor_size",
      color_var = "treatment",
      variation = "line"
    )
  )
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}

# Example 3: Tables variation - Scatter plot with linked tables
app <- init(
  data = data,
  modules = modules(
    tm_mdr_scatterplot(
      label = "Scatter Plot with Tables",
      plot_dataname = "df",
      subject_var = "subject_id",
      x_var = "time_week",
      y_var = "tumor_size",
      color_var = "response_category",
      variation = "tables",
      tooltip_vars = c(
        "subject_id",
        "time_week",
        "tumor_size",
        "response_category",
        "treatment",
        "age_group",
        "center"
      ),
      point_colors = c(
        "Complete Response" = "#00FF00", # Green
        "Partial Response" = "#FFFF00", # Yellow
        "Stable Disease" = "#FFA500", # Orange
        "Progressive Disease" = "#FF0000" # Red
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

# Example 4: Line_Tables variation - Scatter plot with linked line plot and tables
app <- init(
  data = data,
  modules = modules(
    tm_mdr_scatterplot(
      label = "Scatter + Line Plot with Tables",
      plot_dataname = "df",
      subject_var = "subject_id",
      x_var = "time_week",
      y_var = "tumor_size",
      color_var = "response_category",
      variation = "line_tables",
      tooltip_vars = c(
        "subject_id",
        "time_week",
        "tumor_size",
        "response_category",
        "treatment",
        "age_group",
        "center"
      ),
      point_colors = c(
        "Complete Response" = "#00FF00", # Green
        "Partial Response" = "#FFFF00", # Yellow
        "Stable Disease" = "#FFA500", # Orange
        "Progressive Disease" = "#FF0000" # Red
      ),
      reference_lines = list(
        baseline_size = list(label = "Baseline Mean", line_mode = "dash"),
        "30" = list(label = "Response Threshold (-30%)", line_mode = "dot")
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
