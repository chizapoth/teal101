# Show R Code PR example app

# Make sure the required branches are checked-out
# - teal_reporter: 1534-show_r_code@main
# - teal: 1534-show_r_code@main

# load packages
pkg_path <- '/Users/zhanc200/Documents/GitHub/teal_development'

file.path(
  pkg_path,
  c(
    "teal.logger",
    "teal.code",
    "teal.data",
    "teal.widgets",
    "teal.reporter",
    "teal.slice",
    "teal.transform",
    "teal"
  )
) |>
  lapply(pkgload::load_all)

# pkgload::load_all(file.path(pkg_path, "teal.logger"))

# pkgload::load_all("../teal.logger")
# pkgload::load_all("../teal.code")
# pkgload::load_all("../teal.data")
# pkgload::load_all("../teal.widgets")
# pkgload::load_all("../teal.reporter")
# pkgload::load_all("../teal.slice")
# pkgload::load_all("../teal.transform")
# pkgload::load_all("../teal")

library(teal.logger)
library(teal.code)
library(teal.data)
library(teal.reporter)
library(teal.slice)
library(teal.widgets)
library(teal.transform)
library(teal)

do.call(
  options,
  rlang::list2(
    "teal.bs_theme" = bslib::bs_theme(version = "5"),
    teal.log_level = "TRACE",
    teal.show_js_log = TRUE,
    teal.show_r_code = TRUE,
  )
)

example_extended <- function(
  label = "example teal module",
  datanames = "all",
  transformators = list(),
  decorators = list()
) {
  checkmate::assert_string(label)
  checkmate::assert_list(decorators, "teal_transform_module")

  mod <- example_module(label, datanames, transformators, decorators)

  module(
    label,
    server = function(id, data, decorators) {
      moduleServer(id, function(input, output, session) {
        result <- mod$server("example", data, decorators)

        reactive({
          q <- data()
          attr(q@code, "teal.show_src") <- FALSE
          q
        })
      })
    },
    ui = function(id, decorators) mod$ui(shiny::NS(id, "example"), decorators),
    ui_args = mod$ui_args,
    server_args = mod$server_args,
    datanames = mod$datanames,
    transformators = mod$transformators
  )
}

teal::init(
  data = within(teal_data(), {
    iris <- iris
    require(nestcolor)
    CO2 <- CO2
  }),
  modules = modules(
    example_module(label = "Module (full)"),
    example_extended(label = "📓 Module (manual)"),
    example_module(label = "📓 Module (pipe disable show r code)") |>
      disable_src(),
    example_module(label = "🗒️ Module (pipe disable report)") |>
      disable_report(),
    example_module(label = "🗒️📓 Module (pipe disable both)") |>
      disable_src() |>
      disable_report()
  )
) |>
  shiny::runApp()
