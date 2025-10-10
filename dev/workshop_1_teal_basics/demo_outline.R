# DEMO OUTLINE

# first app ----
library(teal)
library(teal.modules.general)
library(teal.modules.clinical)

path <- "data/cdisc"
ADSL <- read.csv(file.path(path, 'adsl.csv'))


data <- teal_data(
  ADSL = ADSL
)

app <- init(
  data = data,
  modules = example_module()
)

shinyApp(app$ui, app$server)


# teal_data (data) ----

data <- teal_data()
data


# add ADSL data
data <- teal_data(
  ADSL = ADSL
)

# extract teal_data content
names(data)
data$ADSL

colnames(data$ADSL)

# add ADAE data

path <- "data/cdisc"
ADAE <- read.csv(file.path(path, 'adae.csv'))

data <- teal_data(
  ADSL = ADSL,
  ADAE = ADAE
)


# filter on each dataset, check join_keys
join_keys(data)


# cdisc_data ----
# use cdisc_data rather than teal_data

data_c <- cdisc_data(
  ADSL = ADSL,
  ADAE = ADAE
)

# compare the data objects
data
data_c


# within use-case 1: put data inside
data <- within(teal_data(), {
  path <- "data/cdisc"
  ADSL <- read.csv(file.path(path, 'adsl.csv'))
})


# alternatively (suggested)
data <- teal_data() |>
  within({
    path <- "data/cdisc"
    ADSL <- read.csv(file.path(path, 'adsl.csv'))
  })


# within use-case 2: processing data (e.g. load data script)

data <- teal_data() |>
  within(code, code = parse(text = readLines("dev/workshop/preprocess.R")))


# join keys ----

# check again keys
join_keys(data)
teal.data::default_cdisc_join_keys
names(default_cdisc_join_keys) |> sort()


default_cdisc_join_keys['ADSL']
default_cdisc_join_keys['ADAE']
default_cdisc_join_keys[c("ADAE", "ADSL")]

join_keys(data) <- default_cdisc_join_keys[c("ADAE", "ADSL")]

# now run the app again

# small example to illustrate the idea
d1 <- data.frame(
  id = c(1, 2, 3),
  letter_upper = c('A', 'B', 'C')
)

d2 <- data.frame(
  id = c(1, 2, 3),
  letter_lower = c('a', 'b', 'c')
)

d <- teal_data(
  D1 = d1,
  D2 = d2
)

join_keys(d)

# manually set key
# set keys
join_keys(d) <- join_keys(
  # primary key
  join_key('D1', keys = 'id'),
  join_key('D2', keys = 'id'),
  # foreign key
  join_key('D1', 'D2', keys = 'id')
)


app <- init(
  data = d,
  modules = modules(
    tm_data_table()
  )
)


shinyApp(app$ui, app$server)

# teal_slices (filter) ----

filter1 <- teal_slice()


# use iris data as an example
iris

# filter by default

# pre-specified filter: single
# try to filter based on 2 variables
filter(iris, Sepal.Length < 5.0)
filter(iris, Species == 'setosa')
filter(iris, Sepal.Length < 5.0 & Species == 'setosa')

# convert these into filter objects (teal_slice)

f1 <- teal_slice(
  dataname = 'IRIS',
  varname = 'Species',
  selected = 'setosa'
)

# numeric filter
f2 <- teal_slice(
  dataname = 'IRIS',
  varname = 'Sepal.Length',
  selected = c(-Inf, 5.0) # this includes 5.0
)

# combine (custom), requires id
f3 <- teal_slice(
  dataname = 'IRIS',
  id = 'small non setosa',
  expr = "Sepal.Length < 5.0 & Species != 'setosa'",
  title = 'testing'
)

# fixed, anchor
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


# module specific ----
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

# modify appearance -----
