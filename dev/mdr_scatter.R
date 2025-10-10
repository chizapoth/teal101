trigger_tooltips_deps <- function() {
  htmltools::htmlDependency(
    name = "teal-modules-mdr-trigger-tooltips",
    # modified to clinical
    version = utils::packageVersion("teal.modules.clinical"),
    package = "teal.modules.clinical",
    src = "triggerTooltips",
    script = "triggerTooltips.js",
    stylesheet = "triggerTooltips.css"
  )
}


tm_mdr_scatterplot <- function(
  label = "Scatter Plot",
  plot_dataname,
  subject_var,
  x_var,
  y_var,
  color_var,
  variation = c("simple", "line", "tables", "line_tables"),
  tooltip_vars = NULL,
  point_colors = character(0),
  transformators = list(),
  reference_lines = NULL,
  table_datanames = character(0),
  reactable_args = list()
) {
  variation <- match.arg(variation)

  if (
    variation %in% c("tables", "line_tables") && length(table_datanames) == 0
  ) {
    stop(
      "'table_datanames' must be specified when variation = '",
      variation,
      "'"
    )
  }

  module_functions <- switch(
    variation,
    simple = list(
      ui = ui_mdr_scatterplot_simple,
      server = srv_mdr_scatterplot_simple
    ),
    line = list(
      ui = ui_mdr_scatterplot_line,
      server = srv_mdr_scatterplot_line
    ),
    tables = list(
      ui = ui_mdr_scatterplot_table,
      server = srv_mdr_scatterplot_table
    ),
    line_tables = list(
      ui = ui_mdr_scatterplot_line_table,
      server = srv_mdr_scatterplot_line_table
    )
  )

  module(
    label = label,
    ui = module_functions$ui,
    server = module_functions$server,
    ui_args = list(),
    server_args = list(
      plot_dataname = plot_dataname,
      subject_var = subject_var,
      x_var = x_var,
      y_var = y_var,
      tooltip_vars = tooltip_vars,
      color_var = color_var,
      point_colors = point_colors,
      reference_lines = reference_lines,
      table_datanames = table_datanames,
      reactable_args = reactable_args
    ),
    transformators = transformators,
    datanames = union(plot_dataname, table_datanames)
  )
}

ui_mdr_scatterplot_simple <- function(id) {
  ns <- NS(id)
  bslib::page_fluid(
    shinyjs::useShinyjs(),
    tags$div(
      trigger_tooltips_deps(),
      tags$div(
        style = "margin-bottom: 15px;",
        tags$strong("Color Settings: "),
        colour_picker_ui(ns("colors"))
      ),
      bslib::card(
        full_screen = TRUE,
        plotly::plotlyOutput(ns("scatter_plot"), height = "100%")
      )
    )
  )
}

srv_mdr_scatterplot_simple <- function(
  id,
  data,
  plot_dataname,
  subject_var,
  x_var,
  y_var,
  tooltip_vars,
  color_var,
  point_colors,
  reference_lines,
  table_datanames,
  reactable_args,
  filter_panel_api
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    color_inputs <- colour_picker_srv(
      "colors",
      x = reactive({
        data()[[plot_dataname]][[color_var]]
      }),
      default_colors = point_colors
    )

    scatter_q <- reactive({
      req(color_inputs())
      data() |>
        within(
          code,
          code = scatterplotly(
            df = plot_dataname,
            x_var = x_var,
            y_var = y_var,
            color_var = color_var,
            id_var = subject_var,
            colors = color_inputs(),
            source = session$ns("scatterplot"),
            tooltip_vars = tooltip_vars
          )
        )
    })

    output$scatter_plot <- plotly::renderPlotly({
      scatter_q()$p %>%
        setup_trigger_tooltips(session$ns("scatter_plot"))
    })
  })
}

ui_mdr_scatterplot_line <- function(id) {
  ns <- NS(id)
  bslib::page_fluid(
    shinyjs::useShinyjs(),
    tags$div(
      trigger_tooltips_deps(),
      tags$div(
        style = "margin-bottom: 15px;",
        tags$strong("Color Settings: "),
        colour_picker_ui(ns("colors"))
      ),
      bslib::card(
        full_screen = TRUE,
        plotly::plotlyOutput(ns("scatter_plot"), height = "100%")
      ),
      tags$br(),
      uiOutput(ns("line_plot_ui"))
    )
  )
}

srv_mdr_scatterplot_line <- function(
  id,
  data,
  plot_dataname,
  subject_var,
  x_var,
  y_var,
  tooltip_vars,
  color_var,
  point_colors,
  reference_lines,
  table_datanames,
  reactable_args,
  filter_panel_api
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    color_inputs <- colour_picker_srv(
      "colors",
      x = reactive({
        data()[[plot_dataname]][[color_var]]
      }),
      default_colors = point_colors
    )

    scatter_q <- reactive({
      req(color_inputs())
      data() |>
        within(
          code,
          code = scatterplotly(
            df = plot_dataname,
            x_var = x_var,
            y_var = y_var,
            color_var = color_var,
            id_var = subject_var,
            colors = color_inputs(),
            source = session$ns("scatterplot"),
            tooltip_vars = tooltip_vars
          )
        )
    })

    output$line_plot_ui <- renderUI({
      selected_data <- plotly::event_data(
        "plotly_selected",
        source = ns("scatterplot")
      )
      req(selected_data, nrow(selected_data) > 0)

      bslib::card(
        full_screen = TRUE,
        plotly::plotlyOutput(ns("line_plot"), height = "100%")
      )
    })

    line_q <- reactive({
      req(color_inputs())

      selected_data <- plotly::event_data(
        "plotly_selected",
        source = ns("scatterplot")
      )
      req(selected_data, nrow(selected_data) > 0, selected_data$customdata)

      filtered_data <- data() |>
        within(
          {
            plot_data <- df |>
              dplyr::mutate(customdata = dplyr::row_number())

            selected_plot_data <- plot_data |>
              dplyr::filter(customdata %in% plotly_selected_customdata)

            df <- df |>
              dplyr::filter(
                subject_var_sym %in% selected_plot_data[[subject_var_str]]
              ) |>
              dplyr::mutate(customdata = dplyr::row_number())
          },
          df = str2lang(plot_dataname),
          subject_var_sym = str2lang(subject_var),
          subject_var_str = subject_var,
          plotly_selected_customdata = selected_data$customdata
        )

      filtered_data |>
        within(
          code,
          code = lineplotly(
            df = plot_dataname,
            x_var = x_var,
            y_var = y_var,
            color_var = color_var,
            group_var = subject_var,
            colors = color_inputs(),
            tooltip_vars = tooltip_vars,
            reference_lines = reference_lines,
            source = session$ns("lineplot")
          )
        )
    })

    output$scatter_plot <- plotly::renderPlotly({
      scatter_q()$p %>%
        setup_trigger_tooltips(session$ns("scatter_plot")) %>%
        plotly::event_register("plotly_selected")
    })

    output$line_plot <- plotly::renderPlotly({
      line_q()$p %>%
        setup_trigger_tooltips(session$ns("line_plot"))
    })
  })
}

ui_mdr_scatterplot_table <- function(id) {
  ns <- NS(id)
  bslib::page_fluid(
    shinyjs::useShinyjs(),
    tags$div(
      trigger_tooltips_deps(),
      tags$div(
        style = "margin-bottom: 15px;",
        tags$strong("Color Settings: "),
        colour_picker_ui(ns("colors"))
      ),
      bslib::card(
        full_screen = TRUE,
        plotly::plotlyOutput(ns("scatter_plot"), height = "100%")
      ),
      tags$br(),
      uiOutput(ns("tables_ui"))
    )
  )
}

srv_mdr_scatterplot_table <- function(
  id,
  data,
  plot_dataname,
  subject_var,
  x_var,
  y_var,
  tooltip_vars,
  color_var,
  point_colors,
  reference_lines,
  table_datanames,
  reactable_args,
  filter_panel_api
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    color_inputs <- colour_picker_srv(
      "colors",
      x = reactive({
        data()[[plot_dataname]][[color_var]]
      }),
      default_colors = point_colors
    )

    scatter_q <- reactive({
      req(color_inputs())
      data() |>
        within(
          code,
          code = scatterplotly(
            df = plot_dataname,
            x_var = x_var,
            y_var = y_var,
            color_var = color_var,
            id_var = subject_var,
            colors = color_inputs(),
            source = session$ns("scatterplot"),
            tooltip_vars = tooltip_vars
          )
        )
    })

    output$scatter_plot <- plotly::renderPlotly({
      scatter_q()$p %>%
        setup_trigger_tooltips(session$ns("scatter_plot")) %>%
        plotly::event_register("plotly_selected")
    })

    plotly_selected_scatter <- reactive({
      plotly::event_data("plotly_selected", source = session$ns("scatterplot"))
    })

    output$tables_ui <- renderUI({
      scatter_selected <- plotly::event_data(
        "plotly_selected",
        source = ns("scatterplot")
      )

      has_selection <- (!is.null(scatter_selected) &&
        nrow(scatter_selected) > 0)

      req(has_selection, length(table_datanames) > 0)

      ui_t_reactables(ns("subtables"))
    })

    filtered_data_q <- reactive({
      scatter_selected <- plotly_selected_scatter()

      if (!is.null(scatter_selected)) {
        selected_values <- scatter_q()$plot_data |>
          dplyr::filter(customdata %in% scatter_selected$customdata)
        data() |>
          within(
            {
              for (table_name in table_datanames) {
                current_table <- get(table_name)
                filtered_table <- current_table |>
                  dplyr::filter(subject_var_sym %in% subject_var_selected)
                assign(table_name, filtered_table)
              }
            },
            table_datanames = table_datanames,
            subject_var_sym = str2lang(subject_var),
            subject_var_selected = selected_values[[subject_var]]
          )
      } else {
        data()
      }
    })

    srv_t_reactables(
      "subtables",
      data = filtered_data_q,
      filter_panel_api = filter_panel_api,
      datanames = table_datanames,
      reactable_args = reactable_args
    )
  })
}

ui_mdr_scatterplot_line_table <- function(id) {
  ns <- NS(id)
  bslib::page_fluid(
    shinyjs::useShinyjs(),
    tags$div(
      trigger_tooltips_deps(),
      tags$div(
        style = "margin-bottom: 15px;",
        tags$strong("Color Settings: "),
        colour_picker_ui(ns("colors"))
      ),
      bslib::card(
        full_screen = TRUE,
        plotly::plotlyOutput(ns("scatter_plot"), height = "100%")
      ),
      tags$br(),
      uiOutput(ns("line_plot_ui")),
      tags$br(),
      uiOutput(ns("tables_ui"))
    )
  )
}

srv_mdr_scatterplot_line_table <- function(
  id,
  data,
  plot_dataname,
  subject_var,
  x_var,
  y_var,
  tooltip_vars,
  color_var,
  point_colors,
  reference_lines,
  table_datanames,
  reactable_args,
  filter_panel_api
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    color_inputs <- colour_picker_srv(
      "colors",
      x = reactive({
        data()[[plot_dataname]][[color_var]]
      }),
      default_colors = point_colors
    )

    scatter_q <- reactive({
      req(color_inputs())
      data() |>
        within(
          code,
          code = scatterplotly(
            df = plot_dataname,
            x_var = x_var,
            y_var = y_var,
            color_var = color_var,
            id_var = subject_var,
            colors = color_inputs(),
            source = session$ns("scatterplot"),
            tooltip_vars = tooltip_vars
          )
        )
    })

    output$line_plot_ui <- renderUI({
      selected_data <- plotly::event_data(
        "plotly_selected",
        source = ns("scatterplot")
      )
      req(selected_data, nrow(selected_data) > 0)

      bslib::card(
        full_screen = TRUE,
        plotly::plotlyOutput(ns("line_plot"), height = "100%")
      )
    })

    line_q <- reactive({
      req(color_inputs())

      selected_data <- plotly::event_data(
        "plotly_selected",
        source = ns("scatterplot")
      )
      req(selected_data, nrow(selected_data) > 0, selected_data$customdata)

      filtered_data <- data() |>
        within(
          {
            plot_data <- df |>
              dplyr::mutate(customdata = dplyr::row_number())

            selected_plot_data <- plot_data |>
              dplyr::filter(customdata %in% plotly_selected_customdata)

            df <- df |>
              dplyr::filter(
                subject_var_sym %in% selected_plot_data[[subject_var_str]]
              ) |>
              dplyr::mutate(customdata = dplyr::row_number())
          },
          df = str2lang(plot_dataname),
          subject_var_sym = str2lang(subject_var),
          subject_var_str = subject_var,
          plotly_selected_customdata = selected_data$customdata
        )

      filtered_data |>
        within(
          code,
          code = lineplotly(
            df = plot_dataname,
            x_var = x_var,
            y_var = y_var,
            color_var = color_var,
            group_var = subject_var,
            colors = color_inputs(),
            tooltip_vars = tooltip_vars,
            reference_lines = reference_lines,
            source = session$ns("lineplot")
          )
        )
    })

    output$scatter_plot <- plotly::renderPlotly({
      scatter_q()$p %>%
        setup_trigger_tooltips(session$ns("scatter_plot")) %>%
        plotly::event_register("plotly_selected")
    })

    output$line_plot <- plotly::renderPlotly({
      line_q()$p %>%
        setup_trigger_tooltips(session$ns("line_plot")) %>%
        plotly::event_register("plotly_selected")
    })

    plotly_selected_scatter <- reactive({
      plotly::event_data("plotly_selected", source = session$ns("scatterplot"))
    })

    plotly_selected_line <- reactive({
      plotly::event_data("plotly_selected", source = session$ns("lineplot"))
    })

    output$tables_ui <- renderUI({
      scatter_selected <- plotly::event_data(
        "plotly_selected",
        source = ns("scatterplot")
      )
      line_selected <- plotly::event_data(
        "plotly_selected",
        source = ns("lineplot")
      )

      has_selection <- (!is.null(scatter_selected) &&
        nrow(scatter_selected) > 0) ||
        (!is.null(line_selected) && nrow(line_selected) > 0)

      req(has_selection, length(table_datanames) > 0)

      ui_t_reactables(ns("subtables"))
    })

    filtered_data_q <- reactive({
      scatter_selected <- plotly_selected_scatter()

      if (!is.null(scatter_selected)) {
        selected_values <- scatter_q()$plot_data |>
          dplyr::filter(customdata %in% scatter_selected$customdata)
        data() |>
          within(
            {
              for (table_name in table_datanames) {
                current_table <- get(table_name)
                filtered_table <- current_table |>
                  dplyr::filter(subject_var_sym %in% subject_var_selected)
                assign(table_name, filtered_table)
              }
            },
            table_datanames = table_datanames,
            subject_var_sym = str2lang(subject_var),
            subject_var_selected = selected_values[[subject_var]]
          )
      } else {
        data()
      }
    })

    line_filtered_data_q <- reactive({
      req(filtered_data_q())
      line_selected <- plotly_selected_line()
      if (!is.null(line_selected)) {
        selected_values <- line_q()[[plot_dataname]] |>
          dplyr::filter(customdata %in% line_selected$customdata)
        filtered_data_q() |>
          within(
            {
              for (table_name in table_datanames) {
                current_table <- get(table_name)
                filtered_table <- current_table |>
                  dplyr::filter(subject_var_sym %in% subject_var_selected)
                assign(table_name, filtered_table)
              }
            },
            table_datanames = table_datanames,
            subject_var_sym = str2lang(subject_var),
            subject_var_selected = selected_values[[subject_var]]
          )
      } else {
        filtered_data_q()
      }
    })

    srv_t_reactables(
      "subtables",
      data = line_filtered_data_q,
      filter_panel_api = filter_panel_api,
      datanames = table_datanames,
      reactable_args = reactable_args
    )
  })
}
