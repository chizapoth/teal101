data <- teal_data() |>
  within({
    # Subject demographics and baseline characteristics
    subjects <- data.frame(
      subject_id = paste0("S", sprintf("%03d", 1:10)),
      age = sample(25:75, 10),
      sex = sample(c("Male", "Female"), 10, replace = TRUE),
      treatment = rep(c("Experimental", "Control"), each = 5),
      baseline_ecog = sample(0:2, 10, replace = TRUE, prob = c(0.6, 0.3, 0.1)),
      enrollment_date = sample(
        seq(as.Date("2023-01-01"), as.Date("2023-06-01"), by = "day"),
        10
      ),
      center = sample(c("Site A", "Site B", "Site C"), 10, replace = TRUE)
    )

    # Longitudinal spider plot data (tumor measurements)
    spider_data <- data.frame(
      subject_id = rep(paste0("S", sprintf("%03d", 1:10)), each = 5),
      time_week = rep(c(0, 6, 12, 18, 24), 10),
      percent_change = c(
        # Experimental arm - better responses
        c(0, -15, -25, -35, -45) + rnorm(5, 0, 5), # S001 - good responder
        c(0, -10, -18, -22, -28) + rnorm(5, 0, 5), # S002 - partial responder
        c(0, -5, -8, -12, -15) + rnorm(5, 0, 5), # S003 - minimal response
        c(0, 2, 8, 15, 25) + rnorm(5, 0, 5), # S004 - progression
        c(0, -20, -30, -25, -20) + rnorm(5, 0, 5), # S005 - initial response, then progression
        # Control arm - less favorable
        c(0, 5, 12, 20, 30) + rnorm(5, 0, 5), # S006 - progression
        c(0, -3, -5, -2, 5) + rnorm(5, 0, 5), # S007 - stable
        c(0, 8, 18, 25, 35) + rnorm(5, 0, 5), # S008 - progression
        c(0, -8, -12, -10, -5) + rnorm(5, 0, 5), # S009 - minor response
        c(0, 15, 25, 40, 55) + rnorm(5, 0, 5) # S010 - rapid progression
      ),
      treatment = rep(rep(c("Experimental", "Control"), each = 5), each = 5),
      visit_type = rep(
        c("Baseline", "Week 6", "Week 12", "Week 18", "Week 24"),
        10
      ),
      assessment_status = sample(
        c("Evaluable", "Non-evaluable"),
        50,
        replace = TRUE,
        prob = c(0.9, 0.1)
      ),
      tumor_size_mm = rep(sample(30:80, 10), each = 5) *
        (1 +
          rep(
            c(
              0,
              -15,
              -25,
              -35,
              -45,
              0,
              -10,
              -18,
              -22,
              -28,
              0,
              -5,
              -8,
              -12,
              -15,
              0,
              2,
              8,
              15,
              25,
              0,
              -20,
              -30,
              -25,
              -20,
              0,
              5,
              12,
              20,
              30,
              0,
              -3,
              -5,
              -2,
              5,
              0,
              8,
              18,
              25,
              35,
              0,
              -8,
              -12,
              -10,
              -5,
              0,
              15,
              25,
              40,
              55
            ) +
              rnorm(50, 0, 5),
            each = 1
          ) /
            100)
    )

    attr(subjects$subject_id, "label") <- "Subject ID"
    attr(subjects$age, "label") <- "Age (years)"
    attr(subjects$sex, "label") <- "Sex"
    attr(subjects$treatment, "label") <- "Treatment Group"
    attr(subjects$baseline_ecog, "label") <- "ECOG Performance Status"
    attr(subjects$enrollment_date, "label") <- "Enrollment Date"
    attr(subjects$center, "label") <- "Study Center"

    attr(spider_data$subject_id, "label") <- "Subject ID"
    attr(spider_data$time_week, "label") <- "Time (weeks)"
    attr(spider_data$percent_change, "label") <- "Percent Change from Baseline"
    attr(spider_data$treatment, "label") <- "Treatment Group"
    attr(spider_data$visit_type, "label") <- "Visit Type"
    attr(spider_data$assessment_status, "label") <- "Assessment Status"
    attr(spider_data$tumor_size_mm, "label") <- "Tumor Size (mm)"
  })

join_keys(data) <- join_keys(
  join_key("subjects", "spider_data", keys = c(subject_id = "subject_id"))
)

# Example 1: Simple variation - Spider plot with no tables
app <- init(
  data = data,
  modules = modules(
    tm_mdr_spiderplot(
      label = "Simple Spider Plot",
      plot_dataname = "spider_data",
      time_var = "time_week",
      value_var = "percent_change",
      subject_var = "subject_id",
      color_var = "treatment",
      filter_event_var = "visit_type",
      variation = "simple"
    )
  )
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}

# Example 2: Tables variation - Spider plot with linked tables
app <- init(
  data = data,
  modules = modules(
    tm_mdr_spiderplot(
      label = "Spider Plot with Tables",
      plot_dataname = "spider_data",
      time_var = "time_week",
      value_var = "percent_change",
      subject_var = "subject_id",
      color_var = "treatment",
      filter_event_var = "assessment_status",
      variation = "tables",
      table_datanames = c("subjects", "spider_data"),
      size_var = "tumor_size_mm",
      tooltip_vars = c(
        "subject_id",
        "time_week",
        "percent_change",
        "treatment",
        "visit_type",
        "tumor_size_mm"
      ),
      point_colors = c(
        "Experimental" = "#1f77b4", # Blue
        "Control" = "#ff7f0e" # Orange
      ),
      point_symbols = c(
        "Experimental" = "circle",
        "Control" = "square"
      ),
      plot_height = c(700, 500, 1000),
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
