# data
iris

# try to filter based on 2 variables
filter(iris, Sepal.Length < 5.0)
filter(iris, Species == 'setosa')
filter(iris, Sepal.Length < 5.0 & Species == 'setosa')


# set some filters
f1 <- teal_slice(
  dataname = 'IRIS',
  varname = 'Species',
  selected = 'setosa'
)

f1
# ?how do I select non-setosa?

# numeric filter
f2 <- teal_slice(
  dataname = 'IRIS',
  varname = 'Sepal.Length',
  selected = c(-Inf, 5.0) # this includes 5.0
)

# combine
f3 <- teal_slice(
  dataname = 'IRIS',
  id = 'small non setosa',
  expr = "Sepal.Length < 5.0 & Species != 'setosa'",
  title = 'testing'
)

# fixed
# can still remove it, but can add it back with + sign
f4 <- teal_slice(
  dataname = 'IRIS',
  varname = 'Species',
  selected = 'setosa',
  fixed = T
)


# anchored, can not remove it, but can still de-select
f5 <- teal_slice(
  dataname = 'IRIS',
  varname = 'Species',
  selected = 'setosa',
  anchored = T
)


# single module filters ----
# add data
data <- teal_data(IRIS = iris)
# init
app <- init(
  data = data,
  modules = example_module(),
  filter = teal_slices(
    f2,
    f5
  )
)

shinyApp(app$ui, app$server)

# multiple modules -----

app <- init(
  data = teal_data(IRIS = iris),
  modules = modules(
    example_module(label = 'M1'),
    example_module(label = 'M2')
  ),
  filter = teal_slices(
    teal_slice(
      dataname = 'IRIS',
      varname = 'Species',
      selected = 'setosa',
      id = 'f1'
    ),
    teal_slice(
      dataname = 'IRIS',
      varname = 'Sepal.Length',
      selected = c(-Inf, 5.0),
      id = 'f2'
    ),
    teal_slice(
      dataname = 'IRIS',
      varname = 'Sepal.Width',
      selected = c(3, 4),
      id = 'f3'
    ),

    module_specific = T,
    mapping = list(
      'M1' = 'f1',
      'M2' = 'f2',
      global_filters = 'f3'
    )
  )
)


if (interactive()) {
  shinyApp(app$ui, app$server)
}
