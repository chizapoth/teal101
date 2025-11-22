# learn the construction of transformator

# Simple Scatterplot Example - Core Functionality Only
# Demonstrates x/y variable selection and group-based coloring

library(teal)
library(teal.modules.general)
# library(teal.modules.clinical)
# Generate simple fake data
set.seed(42)
n <- 150

simple_data <- data.frame(
  id = 1:n,
  measurement_a = rnorm(n, 50, 10),
  measurement_b = rnorm(n, 100, 20),
  measurement_c = rnorm(n, 75, 15),
  treatment_group = sample(
    c("Group_A", "Group_B", "Group_C"),
    n,
    replace = TRUE
  ),
  gender = sample(c("Male", "Female"), n, replace = TRUE)
)

# Convert to factors
simple_data$treatment_group <- as.factor(simple_data$treatment_group)
simple_data$gender <- as.factor(simple_data$gender)

# Create teal_data
data <- teal_data(study_data = simple_data)


# Set join keys
join_keys(data) <- join_keys(
  join_key("study_data", "study_data", "id")
)

# Create minimal scatterplot app
app <- init(
  data = data,
  modules = modules(
    tm_g_scatterplot(
      label = "Simple Scatterplot",

      # X variable selection
      x = data_extract_spec(
        dataname = "study_data",
        select = select_spec(
          label = "X-axis variable:",
          choices = variable_choices(
            data[["study_data"]],
            c("measurement_a", "measurement_b", "measurement_c")
          ),
          selected = "measurement_a",
          multiple = FALSE
        )
      ),

      # Y variable selection
      y = data_extract_spec(
        dataname = "study_data",
        select = select_spec(
          label = "Y-axis variable:",
          choices = variable_choices(
            data[["study_data"]],
            c("measurement_a", "measurement_b", "measurement_c")
          ),
          selected = "measurement_b",
          multiple = FALSE
        )
      ),

      # Color grouping
      color_by = data_extract_spec(
        dataname = "study_data",
        select = select_spec(
          label = "Color by:",
          choices = variable_choices(
            data[["study_data"]],
            c("treatment_group", "gender")
          ),
          selected = "treatment_group",
          multiple = FALSE
        )
      )
    )
  )
)

# Run the app
if (interactive()) {
  shinyApp(app$ui, app$server)
}

cat(
  "Simple scatterplot example ready! Run the script to start the interactive app.\n"
)
