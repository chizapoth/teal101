# MDR swimlane example reproduce
# below I'm using code from this link:
# https://code.roche.com/nest/docs/teal.mdr.handbook/-/blob/main/apps/app-swimlane-01.R?ref_type=heads

library(synthetic.raw)
library(mini.teal.interact) # this is the standlaone function for swimlane


library(dplyr)
# library(arrow)
library(teal)
library(DT)
library(labelled)
library(reactable)
library(teal)
library(teal.modules.general)
# library(teal.modules.mdr)

data <- within(teal_data(), {
  txinf1 <- synthetic.raw::txinf1
  txinf2 <- synthetic.raw::txinf2
  txinj1 <- synthetic.raw::txinj1
  txinj2 <- synthetic.raw::txinj2
  rrsp1 <- synthetic.raw::rrsp1
  disc1 <- synthetic.raw::disc1
  actragm <- synthetic.raw::actragm

  for (dt in list("txinf1", "txinj1", "txinf2", "txinj2", "rrsp1", "disc1")) {
    assign(
      dt,
      get(dt) |>
        dplyr::mutate(
          dplyr::across(
            dplyr::where(is.character),
            \(x) {
              dplyr::if_else(nchar(x) > 20, paste0(substr(x, 1, 20), "..."), x)
            }
          )
        )
    )
  }

  inf1 <- txinf1 |>
    dplyr::mutate(event_result = "Infusion Drug A")

  inj1 <- txinj1 |>
    dplyr::mutate(event_result = "Injection Drug B")

  inf2 <- txinf2 |>
    dplyr::mutate(event_result = "Infusion Drug C")

  inj2 <- txinj2 |>
    dplyr::mutate(event_result = "Injection Drug D")

  tx_all <- dplyr::bind_rows(
    inf1,
    inj1,
    inf2,
    inj2
  )

  names(tx_all) <- tolower(names(tx_all))

  t_all <- tx_all |>
    dplyr::mutate(
      event_type = "Study Drug Administration",
      event_study_day = txd_study_day,
      tx_row_id = dplyr::row_number(),
    ) |>
    dplyr::select(
      subject,
      event_type,
      event_result,
      event_study_day,
      foldername,
      tx_dc,
      txs_tm,
      txe_dc,
      txe_tm,
      txnam,
      txrec,
      tx_row_id
    )

  names(rrsp1) <- tolower(names(rrsp1))

  rrsp1 <- rrsp1 |>
    dplyr::select(
      subject,
      site,
      visit_date,
      rspdn,
      rsp_dc,
      rspd_study_day,
      orsp
    ) |>
    dplyr::mutate(
      event_type = "Response Assessment",
      event_study_day = rspd_study_day,
      event_result = orsp,
    ) |>
    dplyr::select(
      subject,
      event_type,
      event_study_day,
      event_result,
      rsp_dc
    )

  names(disc1) <- tolower(names(disc1))

  disc1 <- disc1 |>
    dplyr::mutate(
      event_type = "Disposition",
      event_study_day = dccd_study_day,
      event_result = dcrs,
    ) |>
    dplyr::select(
      subject,
      event_type,
      event_study_day,
      event_result,
      dcrs
    )

  names(actragm) <- tolower(names(actragm))

  actragm <- actragm |>
    dplyr::select(subject, cohrt)

  interim <- dplyr::bind_rows(
    t_all,
    rrsp1,
    disc1
  ) |>
    dplyr::mutate(
      event_study_day = as.numeric(event_study_day)
    ) |>
    dplyr::group_by(subject) |>
    dplyr::mutate(
      max_study_day = max(event_study_day, na.rm = TRUE),
      event_study_week = floor(as.numeric(event_study_day) / 7)
    ) |>
    dplyr::arrange(subject, event_study_day) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(event_study_day) & !is.na(event_result)) |>
    dplyr::left_join(actragm, by = "subject")

  variable_labels <- c(
    cohrt = "Actual Study Cohort",
    date_administered = "Date Administered",
    dcrs = "Primary Cause of Death",
    event_result = "Event Result",
    event_study_day = "Event Study Day",
    event_study_week = "Event Study Week",
    event_type = "Event Type",
    foldername = "Visit Name",
    frqdv = "Frequency",
    latest_known_day = "Latest Known Day",
    latest_tx_study_day = "Latest TX Study Day",
    max_study_day = "Maximum Study Day",
    orsp = "Response",
    pdl1res = "PDL1 Test Result",
    raise_query = "Raise Query",
    rspd = "Response Date",
    rsp_dc = "Response Date",
    rspd_study_day = "Response Date Study Day",
    rspdn = "Assessment Performed",
    site = "Site",
    stdyp = "Study Phase",
    subject = "Subject",
    tx_dc = "Date Administered",
    txadc = "Total Dose Administered",
    txadu = "Total Dose Administered Unit",
    txd = "Date Administered",
    txdmae = "AE related to Dose Modification",
    txdmod = "Dose Modification",
    txd_study_day = "Date Administered Study Day",
    txed = "End Date Administered",
    txed_study_day = "End Study Day",
    txe_dc = "End Date Administered",
    txe_tm = "End Time Administered",
    txetm = "End Time Administered",
    txetmu = "End Time Administered Unknown",
    txform = "Dose Formulation",
    txnam = "Study Drug Name",
    txpdos = "Planned Dose per Admin",
    txpdosu = "Planned Dose per Admin Unit",
    txrec = "Treatment Received",
    txrmod = "Dose Modification Reason",
    txrte = "Route of Administration",
    tx_row_id = "Treatment Row ID",
    txs_tm = "Start Time Administered",
    txstm = "Start Time Administered",
    txstmu = "Start Time Administered Unknown",
    visit_date = "Visit Date"
  )

  interim <- labelled::set_variable_labels(
    interim,
    .labels = variable_labels[names(interim)]
  )

  t_all <- labelled::set_variable_labels(
    t_all,
    .labels = variable_labels[names(t_all)]
  )

  t_all <- labelled::set_label_attribute(
    t_all,
    "Study Drug Administration data"
  )

  t_all <- t_all |>
    # Reorder columns to have subject, txnam, foldername at the front
    dplyr::select(
      subject,
      txnam,
      foldername,
      dplyr::everything(),
    )

  sm_ds <- labelled::set_label_attribute(
    interim,
    "Swimlane data"
  )
})


# end of data
# inspect inside this data object
names(data)


join_keys(data) <- join_keys(
  join_key("sm_ds", keys = c("subject", "tx_row_id")),
  join_key("t_all", keys = c("subject", "tx_row_id")),
  join_key("t_all", "sm_ds", keys = c("subject", "tx_row_id"), directed = FALSE)
)

# Use consistent colors and symbols for the points in the plot
point_colors <- c(
  `Infusion Drug A` = "goldenrod",
  `Injection Drug B` = "darkorange2",
  `Infusion Drug C` = "deepskyblue3",
  `Injection Drug D` = "dodgerblue",
  `Complete Remission` = "#50af28",
  `Partial Remission` = "#c2e173",
  `Non Complete Response/Non Progressive Disease` = "#d1e000",
  `Stable Disease` = "#f7d533",
  `Progressive Disease` = "#d8181c",
  `Unevaluable` = "#4581b9"
)

# Possible markers https://plotly.com/python/marker-style/ try them out as not
# all are available in R
point_symbols <- c(
  `Infusion Drug A` = "x",
  `Injection Drug B` = "x",
  `Infusion Drug C` = "x",
  `Injection Drug D` = "x",
  `Complete Remission` = "square-open",
  `Partial Remission` = "square-open",
  `Non Complete Response/Non Progressive Disease` = "square-open",
  `Stable Disease` = "square-open",
  `Progressive Disease` = "square-open",
  `Unevaluable` = "square-open"
)

# Reactable Arguments for the listing -----
reactable_args <- list(
  # Need small font size
  style = list(fontFamily = "Work Sans, sans-serif", fontSize = "0.8em"),
  defaultColDef = colDef(
    sortNALast = TRUE,
    # Keep the header text wrapping meanwhile table body text no wrapping
    header = function(value) {
      span(value, style = "white-space: normal;")
    },
    # Necessary to enable hover effect
    html = TRUE,
    # Make the full text available on hover, e.g. if the truncated text is shown
    # as "some text..." and you hover over with a mouse, then the full text
    # will appear. If it is empty or NA, return an empty string to avoid
    # displaying undefined.
    cell = JS(
      "
      function(cellInfo) {
        if (cellInfo.value === null || cellInfo.value === undefined || cellInfo.value === '') {
          return cellInfo.value;
        }

        return '<span title=\"' + cellInfo.value + '\">' + cellInfo.value + '</span>';
      }
    "
    ),
  ),
  colGroup(
    columns = c(
      "subject",
      "txnam",
      "foldername"
    ),
    sticky = "left"
  ),
  striped = TRUE, # striped rows
  filterable = TRUE, # columns filters
  height = 500, # setting height makes it scrollable
  showPageSizeOptions = TRUE, # drop down for page size
  pageSizeOptions = c(10, 50, 100),
  wrap = FALSE, # disable text wrapping
  resizable = TRUE # columns are not re-sizable when their width is fixed.
)

# Variables to show in tooltip. As a reminder, if a particular event type
# does not have that variable, then it shows NA in the tooltip. To avoid that
# one can construct a new variable that combines the information from multiple.
# Please see the data preparation section, `tooltip_var`, above for an example.
tooltip_vars <- c(
  "event_type",
  "txnam",
  "event_result",
  "event_study_day",
  "foldername",
  "txrec",
  "tx_dc",
  "txs_tm",
  "txe_dc",
  "txe_tm",
  "rsp_dc",
  "dcrs",
  "cohrt"
)

# mdr_url <- "https://code.roche.com/nest/docs/teal.mdr.handbook.git"
# raw_url <- "https://code.roche.com/botany/synthetic.raw"

# mini.teal.interact::tm_mdr_swimlane()
?tm_mdr_swimlane


# this function need to be updated

app <- init(
  data = data,
  modules = modules(
    # temporarily using mini.teal.interact pkg (my own copy)
    mini.teal.interact::tm_mdr_swimlane(
      plot_dataname = "sm_ds",
      variation = "tables",
      time_var = choices_selected(
        choices = c(
          # if named vector provided than names are displayed in the input
          `Event Study Day` = "event_study_day",
          `Event Study Week` = "event_study_week"
        ),
        selected = "event_study_day"
      ),
      subject_var = "subject",
      color_var = "event_result",
      group_var = "event_type",
      sort_var = teal.transform::choices_selected(
        choices = c(
          `Maximum Study Day` = "max_study_day"
        ),
        selected = "max_study_day"
      ),
      tooltip_vars = tooltip_vars,
      point_colors = point_colors,
      point_symbols = point_symbols,
      plot_height = c(400, 400, 1200),
      table_datanames = c(`All Treatments` = "t_all"),
      reactable_args = reactable_args
    )
  )
) |>
  teal::modify_title("MDR Swimlane Module Example 01") |>
  teal::modify_header(
    shiny::tagList(
      shiny::tags$h1(
        "Medical Data Review (MDR) Handbook, Swimlane Module Example 01"
      ) #,
      #shiny::tags$a(
      #  mdr_url,
      #  href = mdr_url
      #)
    )
  ) |>
  teal::modify_footer(
    shiny::tagList(
      shiny::tags$p("Data are from synthetic.raw package") #,
      #shiny::tags$a(
      #  mdr_url,
      #  href = mdr_url
      #)
    )
  )


shinyApp(app$ui, app$server)
