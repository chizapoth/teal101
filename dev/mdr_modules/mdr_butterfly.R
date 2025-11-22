library(teal)

data <- teal_data() |>
  within({
    set.seed(123)
    df <- data.frame(
      adverse_event = sample(
        c(
          "Headache",
          "Nausea",
          "Fatigue",
          "Dizziness",
          "Rash",
          "Insomnia",
          "Back Pain",
          "Cough",
          "Diarrhea",
          "Fever"
        ),
        250,
        replace = TRUE,
        prob = c(0.18, 0.15, 0.14, 0.12, 0.1, 0.09, 0.08, 0.06, 0.05, 0.03)
      ),
      severity = sample(
        c("Mild", "Moderate", "Severe"),
        250,
        replace = TRUE,
        prob = c(0.6, 0.3, 0.1)
      ),
      subject_id = sample(paste0("S", 1:60), 250, replace = TRUE),
      treatment = sample(
        c("Active", "Placebo", "Active, Placebo"),
        250,
        replace = TRUE,
        prob = c(0.45, 0.45, 0.1)
      ),
      age_group = sample(c("Young", "Middle", "Old"), 250, replace = TRUE),
      center = sample(
        c("Site A", "Site B", "Site C", "Site D"),
        250,
        replace = TRUE
      )
    )

    attr(df$adverse_event, "label") <- "Adverse Event Type"
    attr(df$severity, "label") <- "Severity Grade"
    attr(df$subject_id, "label") <- "Subject ID"
    attr(df$treatment, "label") <- "Treatment Group"
    attr(df$age_group, "label") <- "Age Group"
    attr(df$center, "label") <- "Study Center"
  })

app <- init(
  data = data,
  modules = modules(
    tm_mdr_butterfly(
      label = "Butterfly Plot - Treatment Comparison",
      plot_dataname = "df",
      y_var = "adverse_event",
      color_var = "severity",
      count_var = "subject_id",
      group_var = "treatment",
      variation = "simple"
    )
  )
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}

app <- init(
  data = data,
  modules = modules(
    tm_mdr_butterfly(
      label = "Butterfly Plot - Custom Colors",
      plot_dataname = "df",
      y_var = "adverse_event",
      color_var = "severity",
      count_var = "subject_id",
      group_var = "treatment",
      variation = "simple",
      butterfly_colors = c(
        "Mild" = "#90EE90",
        "Moderate" = "#FFD700",
        "Severe" = "#FF6347"
      )
    )
  )
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}
