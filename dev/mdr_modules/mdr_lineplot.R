# lineplot
library(teal)
library(teal.modules.mdr)

data <- teal_data() |>
  within({
    df <- data.frame(
      subject_id = rep(paste0("S", 1:8), each = 5),
      time_week = rep(c(0, 2, 4, 6, 8), 8),
      measurement = rnorm(40, 20, 4) + rep(c(0, 1, 2, 3, 4), 8),
      treatment = rep(c("Active", "Placebo"), each = 20),
      baseline = rep(rnorm(8, 18, 2), each = 5),
      center = rep(c("Site A", "Site B", "Site A", "Site B"), each = 10)
    )

    # Add labels
    attr(df$subject_id, "label") <- "Subject ID"
    attr(df$time_week, "label") <- "Time (weeks)"
    attr(df$measurement, "label") <- "Measurement Value"
    attr(df$treatment, "label") <- "Treatment Group"
    attr(df$baseline, "label") <- "Baseline Value"
    attr(df$center, "label") <- "Study Center"
  })

# Example 1: Simple variation - Line plot with color picker, no tables
app <- init(
  data = data,
  modules = modules(
    tm_mdr_lineplot(
      label = "Simple Line Plot",
      plot_dataname = "df",
      x_var = "time_week",
      y_var = "measurement",
      color_var = "treatment",
      group_var = "subject_id",
      variation = "simple"
    )
  )
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}

# Example 2: Tables variation - Line plot with color picker and linked tables
app <- init(
  data = data,
  modules = modules(
    tm_mdr_lineplot(
      label = "Line Plot with Tables",
      plot_dataname = "df",
      x_var = "time_week",
      y_var = "measurement",
      color_var = "treatment",
      group_var = "subject_id",
      variation = "tables",
      colors = c("Active" = "#1f77b4", "Placebo" = "#ff7f0e"),
      tooltip_vars = c(
        "subject_id",
        "time_week",
        "measurement",
        "treatment",
        "center",
        "baseline"
      ),
      reference_lines = list(
        baseline = list(label = "Baseline Mean", line_mode = "dash"),
        measurement = list(label = "Measurement Value", line_mode = "solid")
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
