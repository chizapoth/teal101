# missing data ------

library(random.cdisc.data)
library(dplyr)
library(tidyr)
library(ggExtra)
library(ggpmisc)
library(ggpp)
library(goftest)
library(gridExtra)
library(htmlwidgets)
library(jsonlite)
library(lattice)
library(MASS)
library(rlang)
library(rtables)
library(nestcolor)
library(broom)
library(colourpicker)
library(sparkline)
ADSL <- radsl(seed = 1)
ADRS <- radrs(ADSL, seed = 1)
ADLB <- radlb(ADSL, seed = 1)
ADLBPCA <- ADLB %>%
  dplyr::select(USUBJID, STUDYID, SEX, ARMCD, AVAL, AVISIT, PARAMCD) %>%
  tidyr::pivot_wider(
    values_from = "AVAL",
    names_from = c("PARAMCD", "AVISIT"),
    names_sep = " - "
  )
stopifnot(rlang::hash(ADSL) == "d6ae57c85cf4dbe4e4a953b09a99dad7") # @linksto ADSL
stopifnot(rlang::hash(ADLB) == "5c4c1afffd6d15d164bdc04a40170ce3") # @linksto ADLB
stopifnot(rlang::hash(ADLBPCA) == "85a017be92be0f52a9c1f21659adc025") # @linksto ADLBPCA
stopifnot(rlang::hash(ADRS) == "8df217ad9f6c8edafc7534f25f57587c") # @linksto ADRS
.raw_data <- list2env(list(
  ADSL = ADSL,
  ADLB = ADLB,
  ADLBPCA = ADLBPCA,
  ADRS = ADRS
))
lockEnvironment(.raw_data) # @linksto .raw_data
ADLB <- dplyr::inner_join(
  x = ADLB,
  y = ADSL[, c("STUDYID", "USUBJID"), drop = FALSE],
  by = c("STUDYID", "USUBJID")
)
ADRS <- dplyr::inner_join(
  x = ADRS,
  y = ADSL[, c("STUDYID", "USUBJID"), drop = FALSE],
  by = c("STUDYID", "USUBJID")
)
library("dplyr")
library("ggplot2")
library("tidyr")
library("gridExtra")
ANL <- ADSL
create_cols_labels <- function(cols, just_label = FALSE) {
  column_labels <- c(
    STUDYID = "Study Identifier",
    USUBJID = "Unique Subject Identifier",
    ADTHAUT = "Autopsy Performed",
    DTHDT = "Date of Death",
    DTHCAUS = "Cause of Death",
    DTHCAT = "Cause of Death Category",
    LDDTHELD = "Elapsed Days from Last Dose to Death",
    LDDTHGR1 = "Last Dose to Death - Days Elapsed Grp 1",
    DTHADY = "Relative Day of Death",
    DCSREAS = "Reason for Discontinuation from Study",
    TRTEDTM = "Datetime of Last Exposure to Treatment",
    TRT01EDTM = "Datetime of Last Exposure in Period 01",
    TRT02SDTM = "Datetime of First Exposure to Treatment in Period 02",
    TRT02EDTM = "Datetime of Last Exposure to Treatment in Period 02",
    AP01EDTM = "Period 01 End Datetime",
    AP02SDTM = "Period 02 Start Datetime",
    AP02EDTM = "Period 02 End Datetime",
    EOSDT = "End of Study Date",
    EOSDY = "End of Study Relative Day",
    LSTALVDT = "Date Last Known Alive",
    SUBJID = "Subject Identifier for the Study",
    SITEID = "Study Site Identifier",
    AGE = "Age",
    AGEU = "Age Units",
    SEX = "Sex",
    RACE = "Race",
    ETHNIC = "Ethnicity",
    COUNTRY = "Country",
    DTHFL = "Subject Death Flag",
    INVID = "Investigator Identifier",
    INVNAM = "Investigator Name",
    ARM = "Description of Planned Arm",
    ARMCD = "Planned Arm Code",
    ACTARM = "Description of Actual Arm",
    ACTARMCD = "Actual Arm Code",
    TRT01P = "Planned Treatment for Period 01",
    TRT01A = "Actual Treatment for Period 01",
    TRT02P = "Planned Treatment for Period 02",
    TRT02A = "Actual Treatment for Period 02",
    REGION1 = "Geographic Region 1",
    STRATA1 = "Stratification Factor 1",
    STRATA2 = "Stratification Factor 2",
    BMRKR1 = "Continuous Level Biomarker 1",
    BMRKR2 = "Categorical Level Biomarker 2",
    ITTFL = "Intent-To-Treat Population Flag",
    SAFFL = "Safety Population Flag",
    BMEASIFL = "Response Evaluable Population Flag",
    BEP01FL = "Biomarker Evaluable Population Flag",
    AEWITHFL = "AE Leading to Drug Withdrawal Flag",
    RANDDT = "Date of Randomization",
    TRTSDTM = "Datetime of First Exposure to Treatment",
    TRT01SDTM = "Datetime of First Exposure to Treatment in Period 01",
    AP01SDTM = "Period 01 Start Datetime",
    EOSSTT = "End of Study Status",
    EOTSTT = "End of Treatment Status",
    new_col_name = "**anyna**"
  )
  column_labels[is.na(column_labels) | length(column_labels) == 0] <- ""
  if (just_label) {
    labels <- column_labels[cols]
  } else {
    labels <- ifelse(
      cols == "**anyna**" | cols == "",
      cols,
      paste0(column_labels[cols], " [", cols, "]")
    )
  }
  labels
}
combination_cutoff <- ANL %>%
  dplyr::mutate_all(is.na) %>%
  dplyr::group_by_all() %>%
  dplyr::tally() %>%
  dplyr::ungroup()
data_combination_plot_cutoff <- combination_cutoff %>%
  dplyr::filter(n >= 59L) %>%
  dplyr::mutate(id = rank(-n, ties.method = "first")) %>%
  tidyr::pivot_longer(-c(n, id), names_to = "key", values_to = "value") %>%
  dplyr::arrange(n)
labels <- data_combination_plot_cutoff %>%
  dplyr::filter(key == key[[1]]) %>%
  getElement(name = 1)
combination_plot_top <- data_combination_plot_cutoff %>%
  dplyr::select(id, n) %>%
  dplyr::distinct() %>%
  ggplot2::ggplot(ggplot2::aes(x = id, y = n)) +
  ggplot2::geom_bar(
    stat = "identity",
    fill = c(getOption("ggplot2.discrete.colour")[2], "#ff2951ff")[1]
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = n),
    position = ggplot2::position_dodge(width = 0.9),
    vjust = -0.25
  ) +
  ggplot2::ylim(c(0, max(data_combination_plot_cutoff$n) * 1.5)) +
  ggplot2::labs(caption = NULL, x = "", y = "") +
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.position = "bottom",
    axis.text.x = ggplot2::element_blank()
  )
graph_number_rows <- length(unique(data_combination_plot_cutoff$id))
graph_number_cols <- nrow(data_combination_plot_cutoff) / graph_number_rows
combination_plot_bottom <- data_combination_plot_cutoff %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = create_cols_labels(key), y = id - 0.5, fill = value) +
  ggplot2::geom_tile(alpha = 0.85, height = 0.95) +
  ggplot2::scale_fill_manual(
    name = "",
    values = c(
      "grey90",
      c(getOption("ggplot2.discrete.colour")[2], "#ff2951ff")[1]
    ),
    labels = c("Present", "Missing")
  ) +
  ggplot2::geom_hline(yintercept = seq_len(1 + graph_number_rows) - 1) +
  ggplot2::geom_vline(
    xintercept = seq_len(1 + graph_number_cols) - 0.5,
    linetype = "dotted"
  ) +
  ggplot2::coord_flip() +
  ggplot2::labs(title = NULL, caption = "NEST PROJECT", x = "", y = "") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "bottom",
    axis.text.x = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank()
  )
g1 <- ggplot2::ggplotGrob(combination_plot_top)
g2 <- ggplot2::ggplotGrob(combination_plot_bottom)
combination_plot <- gridExtra::gtable_rbind(g1, g2, size = "last")
combination_plot$heights[7] <- grid::unit(0.2, "null")
grid::grid.newpage()
grid::grid.draw(combination_plot)


library(random.cdisc.data)
library(dplyr)
library(tidyr)
library(ggExtra)
library(ggpmisc)
library(ggpp)
library(goftest)
library(gridExtra)
library(htmlwidgets)
library(jsonlite)
library(lattice)
library(MASS)
library(rlang)
library(rtables)
library(nestcolor)
library(broom)
library(colourpicker)
library(sparkline)
ADSL <- radsl(seed = 1)
ADRS <- radrs(ADSL, seed = 1)
ADLB <- radlb(ADSL, seed = 1)
ADLBPCA <- ADLB %>%
  dplyr::select(USUBJID, STUDYID, SEX, ARMCD, AVAL, AVISIT, PARAMCD) %>%
  tidyr::pivot_wider(
    values_from = "AVAL",
    names_from = c("PARAMCD", "AVISIT"),
    names_sep = " - "
  )
stopifnot(rlang::hash(ADSL) == "d6ae57c85cf4dbe4e4a953b09a99dad7") # @linksto ADSL
stopifnot(rlang::hash(ADLB) == "5c4c1afffd6d15d164bdc04a40170ce3") # @linksto ADLB
stopifnot(rlang::hash(ADLBPCA) == "85a017be92be0f52a9c1f21659adc025") # @linksto ADLBPCA
stopifnot(rlang::hash(ADRS) == "8df217ad9f6c8edafc7534f25f57587c") # @linksto ADRS
.raw_data <- list2env(list(
  ADSL = ADSL,
  ADLB = ADLB,
  ADLBPCA = ADLBPCA,
  ADRS = ADRS
))
lockEnvironment(.raw_data) # @linksto .raw_data
ADLB <- dplyr::inner_join(
  x = ADLB,
  y = ADSL[, c("STUDYID", "USUBJID"), drop = FALSE],
  by = c("STUDYID", "USUBJID")
)
ADRS <- dplyr::inner_join(
  x = ADRS,
  y = ADSL[, c("STUDYID", "USUBJID"), drop = FALSE],
  by = c("STUDYID", "USUBJID")
)
library("dplyr")
library("ggplot2")
library("tidyr")
library("gridExtra")
ANL <- ADSL
create_cols_labels <- function(cols, just_label = FALSE) {
  column_labels <- c(
    STUDYID = "Study Identifier",
    USUBJID = "Unique Subject Identifier",
    ADTHAUT = "Autopsy Performed",
    DTHDT = "Date of Death",
    DTHCAUS = "Cause of Death",
    DTHCAT = "Cause of Death Category",
    LDDTHELD = "Elapsed Days from Last Dose to Death",
    LDDTHGR1 = "Last Dose to Death - Days Elapsed Grp 1",
    DTHADY = "Relative Day of Death",
    DCSREAS = "Reason for Discontinuation from Study",
    TRTEDTM = "Datetime of Last Exposure to Treatment",
    TRT01EDTM = "Datetime of Last Exposure in Period 01",
    TRT02SDTM = "Datetime of First Exposure to Treatment in Period 02",
    TRT02EDTM = "Datetime of Last Exposure to Treatment in Period 02",
    AP01EDTM = "Period 01 End Datetime",
    AP02SDTM = "Period 02 Start Datetime",
    AP02EDTM = "Period 02 End Datetime",
    EOSDT = "End of Study Date",
    EOSDY = "End of Study Relative Day",
    LSTALVDT = "Date Last Known Alive",
    SUBJID = "Subject Identifier for the Study",
    SITEID = "Study Site Identifier",
    AGE = "Age",
    AGEU = "Age Units",
    SEX = "Sex",
    RACE = "Race",
    ETHNIC = "Ethnicity",
    COUNTRY = "Country",
    DTHFL = "Subject Death Flag",
    INVID = "Investigator Identifier",
    INVNAM = "Investigator Name",
    ARM = "Description of Planned Arm",
    ARMCD = "Planned Arm Code",
    ACTARM = "Description of Actual Arm",
    ACTARMCD = "Actual Arm Code",
    TRT01P = "Planned Treatment for Period 01",
    TRT01A = "Actual Treatment for Period 01",
    TRT02P = "Planned Treatment for Period 02",
    TRT02A = "Actual Treatment for Period 02",
    REGION1 = "Geographic Region 1",
    STRATA1 = "Stratification Factor 1",
    STRATA2 = "Stratification Factor 2",
    BMRKR1 = "Continuous Level Biomarker 1",
    BMRKR2 = "Categorical Level Biomarker 2",
    ITTFL = "Intent-To-Treat Population Flag",
    SAFFL = "Safety Population Flag",
    BMEASIFL = "Response Evaluable Population Flag",
    BEP01FL = "Biomarker Evaluable Population Flag",
    AEWITHFL = "AE Leading to Drug Withdrawal Flag",
    RANDDT = "Date of Randomization",
    TRTSDTM = "Datetime of First Exposure to Treatment",
    TRT01SDTM = "Datetime of First Exposure to Treatment in Period 01",
    AP01SDTM = "Period 01 Start Datetime",
    EOSSTT = "End of Study Status",
    EOTSTT = "End of Treatment Status",
    new_col_name = "**anyna**"
  )
  column_labels[is.na(column_labels) | length(column_labels) == 0] <- ""
  if (just_label) {
    labels <- column_labels[cols]
  } else {
    labels <- ifelse(
      cols == "**anyna**" | cols == "",
      cols,
      paste0(column_labels[cols], " [", cols, "]")
    )
  }
  labels
}
library("forcats")
library("glue")
summary_data <- ANL %>%
  dplyr::mutate(`:=`(
    STUDYID,
    forcats::fct_na_value_to_level(as.factor(STUDYID), "NA")
  )) %>%
  dplyr::group_by_at("STUDYID") %>%
  dplyr::filter(STUDYID %in% "AB12345")
count_data <- dplyr::summarise(summary_data, n = dplyr::n())
summary_data <- dplyr::summarise_all(summary_data, function(x) {
  round(sum(is.na(x)) / length(x), 4)
}) %>%
  dplyr::mutate(`:=`(
    STUDYID,
    paste0("STUDYID", ":", STUDYID, "(N=", count_data$n, ")")
  )) %>%
  tidyr::pivot_longer(
    !dplyr::all_of("STUDYID"),
    names_to = "Variable",
    values_to = "out"
  ) %>%
  tidyr::pivot_wider(names_from = "STUDYID", values_from = "out") %>%
  dplyr::mutate(
    `Variable label` = create_cols_labels(Variable, just_label = TRUE),
    .after = Variable
  )
table <- rtables::df_to_tt(summary_data)
table
table


# distribution -------

library(random.cdisc.data)
library(dplyr)
library(tidyr)
library(ggExtra)
library(ggpmisc)
library(ggpp)
library(goftest)
library(gridExtra)
library(htmlwidgets)
library(jsonlite)
library(lattice)
library(MASS)
library(rlang)
library(rtables)
library(nestcolor)
library(broom)
library(colourpicker)
library(sparkline)
ADSL <- radsl(seed = 1)
stopifnot(rlang::hash(ADSL) == "d6ae57c85cf4dbe4e4a953b09a99dad7") # @linksto ADSL
.raw_data <- list2env(list(ADSL = ADSL))
lockEnvironment(.raw_data) # @linksto .raw_data
library("ggplot2")
library("dplyr")
ANL_1 <- ADSL %>% dplyr::select(STUDYID, USUBJID, BMRKR1)
ANL_2 <- ADSL %>% dplyr::select(STUDYID, USUBJID, SEX)
ANL_3 <- ADSL %>% dplyr::filter(FALSE) %>% dplyr::select(STUDYID, USUBJID)
ANL <- ANL_1
ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID"))
ANL <- dplyr::full_join(ANL, ANL_3, by = c("STUDYID", "USUBJID"))
ANL <- ANL %>%
  teal.data::col_relabel(BMRKR1 = "Continuous Level Biomarker 1", SEX = "Sex")
library("forcats")
ANL[["SEX"]] <- forcats::fct_na_value_to_level(as.factor(ANL[["SEX"]]), "NA")
params <- as.list(c(5.85, 3.54))
names(params) <- c("mean", "sd")
strata_vars <- "SEX"
summary_table_data <- ANL %>%
  dplyr::group_by_at(dplyr::vars(dplyr::any_of(strata_vars))) %>%
  dplyr::summarise(
    min = round(min(BMRKR1, na.rm = TRUE), 2L),
    median = round(stats::median(BMRKR1, na.rm = TRUE), 2L),
    mean = round(mean(BMRKR1, na.rm = TRUE), 2L),
    max = round(max(BMRKR1, na.rm = TRUE), 2L),
    sd = round(stats::sd(BMRKR1, na.rm = TRUE), 2L),
    count = dplyr::n()
  )
histogram_plot <- ggplot2::ggplot(ANL, ggplot2::aes(BMRKR1, col = SEX)) +
  ggplot2::geom_histogram(
    position = "identity",
    ggplot2::aes(y = ggplot2::after_stat(density), fill = SEX),
    bins = 30L,
    alpha = 0.3
  ) +
  ggplot2::stat_density(
    ggplot2::aes(y = ggplot2::after_stat(1 * density)),
    geom = "line",
    position = "identity",
    alpha = 0.5,
    size = 2,
    n = 512
  ) +
  ggplot2::labs(caption = "NEST PROJECT") +
  ggplot2::theme_linedraw()
histogram_plot
summary_table <- rtables::df_to_tt(summary_table_data)
library("tidyr")
test_table_data <- ANL %>%
  dplyr::select("BMRKR1", "SEX", character(0)) %>%
  dplyr::group_by_at(dplyr::vars(dplyr::any_of(character(0)))) %>%
  dplyr::do(
    tests = generics::glance(do.call(
      stats::aov,
      list(stats::formula(BMRKR1 ~ SEX), .)
    ))
  ) %>%
  tidyr::unnest(tests) %>%
  dplyr::mutate_if(is.numeric, round, 3)
test_table <- rtables::df_to_tt(test_table_data)
test_table


# outlier -----

library(random.cdisc.data)
library(dplyr)
library(tidyr)
library(ggExtra)
library(ggpmisc)
library(ggpp)
library(goftest)
library(gridExtra)
library(htmlwidgets)
library(jsonlite)
library(lattice)
library(MASS)
library(rlang)
library(rtables)
library(nestcolor)
library(broom)
library(colourpicker)
library(sparkline)
ADSL <- radsl(seed = 1)
ADLB <- radlb(ADSL, seed = 1)
stopifnot(rlang::hash(ADSL) == "d6ae57c85cf4dbe4e4a953b09a99dad7") # @linksto ADSL
stopifnot(rlang::hash(ADLB) == "5c4c1afffd6d15d164bdc04a40170ce3") # @linksto ADLB
.raw_data <- list2env(list(ADSL = ADSL, ADLB = ADLB))
lockEnvironment(.raw_data) # @linksto .raw_data
ADLB <- dplyr::inner_join(
  x = ADLB,
  y = ADSL[, c("STUDYID", "USUBJID"), drop = FALSE],
  by = c("STUDYID", "USUBJID")
)
library("dplyr")
library("tidyr")
library("tibble")
library("ggplot2")
ANL_1 <- ADLB %>% dplyr::select(STUDYID, USUBJID, PARAMCD, AVISIT, AVAL)
ANL <- ANL_1
ANL <- ANL %>% teal.data::col_relabel(AVAL = "Analysis Value")
ANL_OUTLIER <- ANL %>%
  dplyr::mutate(is_outlier = {
    q1_q3 <- stats::quantile(AVAL, probs = c(0.25, 0.75))
    iqr <- q1_q3[2] - q1_q3[1]
    !(AVAL >= q1_q3[1] - 1.5 * iqr & AVAL <= q1_q3[2] + 1.5 * iqr)
  }) %>%
  dplyr::mutate(is_outlier_selected = {
    q1_q3 <- stats::quantile(AVAL, probs = c(0.25, 0.75))
    iqr <- q1_q3[2] - q1_q3[1]
    !(AVAL >= q1_q3[1] - 3L * iqr & AVAL <= q1_q3[2] + 3L * iqr)
  }) %>%
  dplyr::filter(is_outlier | is_outlier_selected) %>%
  dplyr::select(-is_outlier)
ANL_OUTLIER_EXTENDED <- dplyr::left_join(
  ANL_OUTLIER,
  dplyr::select(
    ADSL,
    dplyr::setdiff(
      names(ADSL),
      dplyr::setdiff(names(ANL_OUTLIER), c("STUDYID", "USUBJID"))
    )
  ),
  by = c("STUDYID", "USUBJID")
)
cat("No categorical variable selected, summary table cannot be created.")
ecdf_df <- ANL %>%
  dplyr::mutate(y = (stats::ecdf(ANL[["AVAL"]]))(ANL[["AVAL"]]))
outlier_points <- dplyr::left_join(
  ecdf_df,
  ANL_OUTLIER,
  by = dplyr::setdiff(names(ecdf_df), "y")
) %>%
  dplyr::filter(!is.na(is_outlier_selected))
cumulative_plot <- ANL %>%
  ggplot2::ggplot(ggplot2::aes(x = AVAL)) +
  ggplot2::stat_ecdf() +
  ggplot2::geom_point(
    data = outlier_points,
    ggplot2::aes(x = AVAL, y = y, color = is_outlier_selected)
  ) +
  ggplot2::scale_color_manual(values = c(`TRUE` = "red", `FALSE` = "black")) +
  ggplot2::labs(caption = "NEST PROJECT", color = "Is outlier?") +
  ggplot2::theme_gray() +
  ggplot2::theme(legend.position = "top")
columns_index <- union(
  setdiff(names(ANL_OUTLIER), c("is_outlier_selected", "order")),
  "INVNAM"
)
ANL_OUTLIER_EXTENDED[ANL_OUTLIER_EXTENDED$is_outlier_selected, columns_index]
print(cumulative_plot)


library(random.cdisc.data)
library(dplyr)
library(tidyr)
library(ggExtra)
library(ggpmisc)
library(ggpp)
library(goftest)
library(gridExtra)
library(htmlwidgets)
library(jsonlite)
library(lattice)
library(MASS)
library(rlang)
library(rtables)
library(nestcolor)
library(broom)
library(colourpicker)
library(sparkline)
ADSL <- radsl(seed = 1)
ADLB <- radlb(ADSL, seed = 1)
stopifnot(rlang::hash(ADSL) == "d6ae57c85cf4dbe4e4a953b09a99dad7") # @linksto ADSL
stopifnot(rlang::hash(ADLB) == "5c4c1afffd6d15d164bdc04a40170ce3") # @linksto ADLB
.raw_data <- list2env(list(ADSL = ADSL, ADLB = ADLB))
lockEnvironment(.raw_data) # @linksto .raw_data

ADLB <- dplyr::inner_join(
  x = ADLB,
  y = ADSL[, c("STUDYID", "USUBJID"), drop = FALSE],
  by = c("STUDYID", "USUBJID")
)
library("dplyr")
library("tidyr")
library("tibble")
library("ggplot2")
ANL_1 <- ADLB %>% dplyr::select(STUDYID, USUBJID, PARAMCD, AVISIT, AVAL)
ANL <- ANL_1
ANL <- ANL %>% teal.data::col_relabel(AVAL = "Analysis Value")
ANL_OUTLIER <- ANL %>%
  dplyr::mutate(is_outlier = {
    q1_q3 <- stats::quantile(AVAL, probs = c(0.25, 0.75))
    iqr <- q1_q3[2] - q1_q3[1]
    !(AVAL >= q1_q3[1] - 1.5 * iqr & AVAL <= q1_q3[2] + 1.5 * iqr)
  }) %>%
  dplyr::mutate(is_outlier_selected = {
    q1_q3 <- stats::quantile(AVAL, probs = c(0.25, 0.75))
    iqr <- q1_q3[2] - q1_q3[1]
    !(AVAL >= q1_q3[1] - 3L * iqr & AVAL <= q1_q3[2] + 3L * iqr)
  }) %>%
  dplyr::filter(is_outlier | is_outlier_selected) %>%
  dplyr::select(-is_outlier)
ANL_OUTLIER_EXTENDED <- dplyr::left_join(
  ANL_OUTLIER,
  dplyr::select(
    ADSL,
    dplyr::setdiff(
      names(ADSL),
      dplyr::setdiff(names(ANL_OUTLIER), c("STUDYID", "USUBJID"))
    )
  ),
  by = c("STUDYID", "USUBJID")
)
cat("No categorical variable selected, summary table cannot be created.")
box_plot <- ANL %>%
  ggplot() +
  ggplot2::geom_boxplot(outlier.shape = NA) +
  ggplot2::aes(x = "Entire dataset", y = AVAL) +
  ggplot2::scale_x_discrete() +
  ggplot2::geom_point(
    data = ANL_OUTLIER,
    ggplot2::aes(x = "Entire dataset", y = AVAL, color = is_outlier_selected)
  ) +
  ggplot2::scale_color_manual(values = c(`TRUE` = "red", `FALSE` = "black")) +
  ggplot2::labs(caption = "NEST PROJECT", color = "Is outlier?") +
  ggplot2::theme_gray() +
  ggplot2::theme(legend.position = "top")
columns_index <- union(
  setdiff(names(ANL_OUTLIER), c("is_outlier_selected", "order")),
  "INVNAM"
)
ANL_OUTLIER_EXTENDED[ANL_OUTLIER_EXTENDED$is_outlier_selected, columns_index]
print(box_plot)


# bivariate plots ----
library(random.cdisc.data)
library(dplyr)
library(tidyr)
library(ggExtra)
library(ggpmisc)
library(ggpp)
library(goftest)
library(gridExtra)
library(htmlwidgets)
library(jsonlite)
library(lattice)
library(MASS)
library(rlang)
library(rtables)
library(nestcolor)
library(broom)
library(colourpicker)
library(sparkline)
ADSL <- radsl(seed = 1)
ADLB <- radlb(ADSL, seed = 1)
stopifnot(rlang::hash(ADSL) == "d6ae57c85cf4dbe4e4a953b09a99dad7") # @linksto ADSL
stopifnot(rlang::hash(ADLB) == "5c4c1afffd6d15d164bdc04a40170ce3") # @linksto ADLB
.raw_data <- list2env(list(ADSL = ADSL, ADLB = ADLB))
lockEnvironment(.raw_data) # @linksto .raw_data
ADLB <- dplyr::inner_join(
  x = ADLB,
  y = ADSL[, c("STUDYID", "USUBJID"), drop = FALSE],
  by = c("STUDYID", "USUBJID")
)
library("ggplot2")
library("dplyr")
ANL_1 <- ADSL %>% dplyr::select(STUDYID, USUBJID, AGE)
ANL_2 <- ADLB %>%
  dplyr::filter(PARAMCD == "ALT" & AVISIT == "SCREENING") %>%
  dplyr::select(STUDYID, USUBJID, AVAL)
ANL <- ANL_1
ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID"))
ANL <- ANL %>% teal.data::col_relabel(AGE = "Age", AVAL = "Analysis Value")
plot <- ggplot2::ggplot(ANL) +
  ggplot2::aes(x = AGE, y = AVAL) +
  ggplot2::geom_point(alpha = 0.5, size = 2L, pch = 21) +
  ggplot2::labs(
    caption = "NEST PROJECT",
    x = "Age [AGE]",
    y = "Analysis Value [AVAL]"
  ) +
  ggplot2::theme_gray()
plot


# regression ----

library(random.cdisc.data)
library(dplyr)
library(tidyr)
library(ggExtra)
library(ggpmisc)
library(ggpp)
library(goftest)
library(gridExtra)
library(htmlwidgets)
library(jsonlite)
library(lattice)
library(MASS)
library(rlang)
library(rtables)
library(nestcolor)
library(broom)
library(colourpicker)
library(sparkline)
ADSL <- radsl(seed = 1)
ADRS <- radrs(ADSL, seed = 1)
stopifnot(rlang::hash(ADSL) == "d6ae57c85cf4dbe4e4a953b09a99dad7") # @linksto ADSL
stopifnot(rlang::hash(ADRS) == "8df217ad9f6c8edafc7534f25f57587c") # @linksto ADRS
.raw_data <- list2env(list(ADSL = ADSL, ADRS = ADRS))
lockEnvironment(.raw_data) # @linksto .raw_data
ADRS <- dplyr::inner_join(
  x = ADRS,
  y = ADSL[, c("STUDYID", "USUBJID"), drop = FALSE],
  by = c("STUDYID", "USUBJID")
)
library("ggplot2")
library("dplyr")
ANL_1 <- ADSL %>% dplyr::select(STUDYID, USUBJID, BMRKR1)
ANL_2 <- ADRS %>%
  dplyr::filter(PARAMCD == "BESRSPI") %>%
  dplyr::select(STUDYID, USUBJID, AVALC)
ANL <- ANL_1
ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID"))
ANL <- ANL %>%
  teal.data::col_relabel(
    BMRKR1 = "Continuous Level Biomarker 1",
    AVALC = "Analysis Value (C)"
  )
fit <- stats::lm(BMRKR1 ~ AVALC, data = ANL)
for (regressor in names(fit$contrasts)) {
  alts <- paste0(levels(ANL[[regressor]]), collapse = "|")
  names(fit$coefficients) <- gsub(
    paste0("^(", regressor, ")(", alts, ")$"),
    paste0("\\1", ": ", "\\2"),
    names(fit$coefficients)
  )
}
fit_summary <- summary(fit)
fit_summary
class(fit$residuals) <- NULL
data <- ggplot2::fortify(fit)
smooth <- function(x, y) {
  as.data.frame(stats::lowess(x, y, f = 2 / 3, iter = 3))
}
smoothy_aes <- ggplot2::aes_string(x = "x", y = "y")
reg_form <- deparse(fit$call[[2]])
smoothy <- smooth(data$.fitted, sqrt(abs(data$.stdresid)))
plot <- ggplot2::ggplot(
  data = data,
  ggplot2::aes(.fitted, sqrt(abs(.stdresid)))
) +
  ggplot2::geom_point(size = 2L, alpha = 1L) +
  ggplot2::geom_line(data = smoothy, mapping = smoothy_aes) +
  ggrepel::geom_text_repel(
    label = dplyr::if_else(
      data$.cooksd > 9L * mean(data$.cooksd, na.rm = TRUE),
      as.character(stats::na.omit(ANL)[["USUBJID"]]),
      ""
    ) %>%
      dplyr::if_else(is.na(.), "cooksd == NaN", .),
    color = "red",
    hjust = 0,
    vjust = 1,
    max.overlaps = Inf,
    min.segment.length = 0.5,
    segment.alpha = 0.5,
    seed = 123
  ) +
  ggplot2::labs(
    caption = "NEST PROJECT",
    x = paste0("Fitted values\nlm(", reg_form, ")"),
    y = expression(sqrt(abs(`Standardized residuals`))),
    title = "Scale-Location"
  ) +
  ggplot2::theme_bw()
plot


# response plot ----

library(random.cdisc.data)
library(dplyr)
library(tidyr)
library(ggExtra)
library(ggpmisc)
library(ggpp)
library(goftest)
library(gridExtra)
library(htmlwidgets)
library(jsonlite)
library(lattice)
library(MASS)
library(rlang)
library(rtables)
library(nestcolor)
library(broom)
library(colourpicker)
library(sparkline)
ADSL <- radsl(seed = 1)
ADRS <- radrs(ADSL, seed = 1)
stopifnot(rlang::hash(ADSL) == "d6ae57c85cf4dbe4e4a953b09a99dad7") # @linksto ADSL
stopifnot(rlang::hash(ADRS) == "8df217ad9f6c8edafc7534f25f57587c") # @linksto ADRS
.raw_data <- list2env(list(ADSL = ADSL, ADRS = ADRS))
lockEnvironment(.raw_data) # @linksto .raw_data
ADRS <- dplyr::inner_join(
  x = ADRS,
  y = ADSL[, c("STUDYID", "USUBJID"), drop = FALSE],
  by = c("STUDYID", "USUBJID")
)
library("ggplot2")
library("dplyr")
ANL_1 <- ADRS %>%
  dplyr::filter(PARAMCD == "BESRSPI") %>%
  dplyr::select(STUDYID, USUBJID, AVALC)
ANL_2 <- ADSL %>% dplyr::select(STUDYID, USUBJID, STRATA2)
ANL <- ANL_1
ANL <- dplyr::full_join(ANL, ANL_2, by = c("STUDYID", "USUBJID"))
ANL <- ANL %>%
  teal.data::col_relabel(
    AVALC = "Analysis Value (C)",
    STRATA2 = "Stratification Factor 2"
  )
ANL[["AVALC"]] <- factor(ANL[["AVALC"]])
ANL2 <- ANL %>%
  dplyr::group_by_at(dplyr::vars(STRATA2, AVALC, NULL, NULL)) %>%
  dplyr::summarise(ns = dplyr::n()) %>%
  dplyr::group_by_at(dplyr::vars(STRATA2, NULL, NULL)) %>%
  dplyr::mutate(sums = sum(ns), percent = round(ns / sums * 100, 1))
ANL3 <- ANL %>%
  dplyr::group_by_at(dplyr::vars(STRATA2, NULL, NULL)) %>%
  dplyr::summarise(ns = dplyr::n())
plot <- ggplot2::ggplot(ANL2, ggplot2::aes(x = STRATA2, y = ns)) +
  ggplot2::geom_bar(
    ggplot2::aes(fill = AVALC),
    stat = "identity",
    position = "fill"
  ) +
  ggplot2::expand_limits(y = c(0, 1.1)) +
  ggplot2::geom_text(
    data = ANL2,
    ggplot2::aes(label = ns, x = STRATA2, y = ns, group = AVALC),
    col = "white",
    vjust = "middle",
    hjust = "middle",
    position = position_fill(0.5)
  ) +
  ggplot2::geom_text(
    data = ANL3,
    ggplot2::aes(label = ns, x = STRATA2, y = 1.1),
    hjust = "middle",
    vjust = -1,
    position = "fill"
  ) +
  ggplot2::labs(
    caption = "NEST PROJECT",
    x = "Stratification Factor 2 [STRATA2]",
    y = "Proportion of Analysis Value (C) [AVALC]",
    fill = "Analysis Value (C) [AVALC]"
  ) +
  ggplot2::theme_linedraw() +
  ggplot2::theme(legend.position = "bottom")
plot


# scatterplot matrix ----
library(random.cdisc.data)
library(dplyr)
library(tidyr)
library(ggExtra)
library(ggpmisc)
library(ggpp)
library(goftest)
library(gridExtra)
library(htmlwidgets)
library(jsonlite)
library(lattice)
library(MASS)
library(rlang)
library(rtables)
library(nestcolor)
library(broom)
library(colourpicker)
library(sparkline)
ADSL <- radsl(seed = 1)
stopifnot(rlang::hash(ADSL) == "d6ae57c85cf4dbe4e4a953b09a99dad7") # @linksto ADSL
.raw_data <- list2env(list(ADSL = ADSL))
lockEnvironment(.raw_data) # @linksto .raw_data
library("dplyr")
library("lattice")
ANL_1 <- ADSL %>% dplyr::select(STUDYID, USUBJID, AGE, BMRKR1)
ANL <- ANL_1
ANL <- ANL %>%
  teal.data::col_relabel(AGE = "Age", BMRKR1 = "Continuous Level Biomarker 1")
ANL <- ANL[, structure(
  c(AGE = "AGE", BMRKR1 = "BMRKR1"),
  dataname = "ADSL",
  always_selected = character(0)
)] %>%
  droplevels()
plot <- lattice::splom(
  ANL,
  varnames = c("Age [AGE]", "Continuous Level\nBiomarker 1 [BMRKR1]"),
  pch = 16,
  alpha = 0.5,
  cex = 2.55
)
plot


# scatterplot -----
library(random.cdisc.data)
library(dplyr)
library(tidyr)
library(ggExtra)
library(ggpmisc)
library(ggpp)
library(goftest)
library(gridExtra)
library(htmlwidgets)
library(jsonlite)
library(lattice)
library(MASS)
library(rlang)
library(rtables)
library(nestcolor)
library(broom)
library(colourpicker)
library(sparkline)
ADSL <- radsl(seed = 1)
stopifnot(rlang::hash(ADSL) == "d6ae57c85cf4dbe4e4a953b09a99dad7") # @linksto ADSL
.raw_data <- list2env(list(ADSL = ADSL))
lockEnvironment(.raw_data) # @linksto .raw_data
library("ggplot2")
library("dplyr")
ANL_1 <- ADSL %>% dplyr::select(STUDYID, USUBJID, AGE, BMRKR1)
ANL <- ANL_1
ANL <- ANL %>%
  teal.data::col_relabel(AGE = "Age", BMRKR1 = "Continuous Level Biomarker 1")
plot <- ANL %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = AGE, y = BMRKR1) +
  ggplot2::geom_point(
    alpha = 1L,
    size = 3L,
    shape = "circle",
    color = "#C42727"
  ) +
  ggplot2::labs(
    caption = "NEST PROJECT",
    y = "Continuous Level Biomarker 1 [BMRKR1]",
    x = "Age [AGE]"
  ) +
  ggplot2::theme_gray() +
  ggplot2::theme(legend.position = "bottom")
plot


# table choices ----
library(random.cdisc.data)
library(dplyr)
library(tidyr)
library(ggExtra)
library(ggpmisc)
library(ggpp)
library(goftest)
library(gridExtra)
library(htmlwidgets)
library(jsonlite)
library(lattice)
library(MASS)
library(rlang)
library(rtables)
library(nestcolor)
library(broom)
library(colourpicker)
library(sparkline)
ADSL <- radsl(seed = 1)
stopifnot(rlang::hash(ADSL) == "d6ae57c85cf4dbe4e4a953b09a99dad7") # @linksto ADSL
.raw_data <- list2env(list(ADSL = ADSL))
lockEnvironment(.raw_data) # @linksto .raw_data
library("rtables")
library("tern")
library("dplyr")
ANL_1 <- ADSL %>% dplyr::select(STUDYID, USUBJID, STRATA2, ARMCD)
ANL <- ANL_1
ANL <- ANL %>%
  teal.data::col_relabel(
    STRATA2 = "Stratification Factor 2",
    ARMCD = "Planned Arm Code"
  )
title <- "Cross-Table of Stratification Factor 2 [STRATA2] (rows) vs. Planned Arm Code [ARMCD] (columns)"
table <- rtables::basic_table() %>%
  rtables::split_cols_by("ARMCD") %>%
  rtables::add_colcounts() %>%
  tern::analyze_vars(
    vars = "STRATA2",
    var_labels = c(STRATA2 = "Stratification Factor 2 [STRATA2]"),
    na.rm = FALSE,
    denom = "N_col",
    .stats = c("mean_sd", "median", "range", "count_fraction")
  )
ANL <- tern::df_explicit_na(ANL)
table <- rtables::build_table(lyt = table, df = ANL[order(ANL[["ARMCD"]]), ])
table


# pca ----
library(random.cdisc.data)
library(dplyr)
library(tidyr)
library(ggExtra)
library(ggpmisc)
library(ggpp)
library(goftest)
library(gridExtra)
library(htmlwidgets)
library(jsonlite)
library(lattice)
library(MASS)
library(rlang)
library(rtables)
library(nestcolor)
library(broom)
library(colourpicker)
library(sparkline)
ADSL <- radsl(seed = 1)
ADLB <- radlb(ADSL, seed = 1)
ADLBPCA <- ADLB %>%
  dplyr::select(USUBJID, STUDYID, SEX, ARMCD, AVAL, AVISIT, PARAMCD) %>%
  tidyr::pivot_wider(
    values_from = "AVAL",
    names_from = c("PARAMCD", "AVISIT"),
    names_sep = " - "
  )
stopifnot(rlang::hash(ADSL) == "d6ae57c85cf4dbe4e4a953b09a99dad7") # @linksto ADSL
stopifnot(rlang::hash(ADLB) == "5c4c1afffd6d15d164bdc04a40170ce3") # @linksto ADLB
stopifnot(rlang::hash(ADLBPCA) == "85a017be92be0f52a9c1f21659adc025") # @linksto ADLBPCA
.raw_data <- list2env(list(ADLBPCA = ADLBPCA))
lockEnvironment(.raw_data) # @linksto .raw_data
library("ggplot2")
library("dplyr")
library("tidyr")
ANL_1 <- ADLBPCA %>%
  dplyr::select(
    `ALT - WEEK 5 DAY 36`,
    `CRP - WEEK 5 DAY 36`,
    `IGA - WEEK 5 DAY 36`
  )
ANL <- ANL_1
ANL <- ANL %>%
  teal.data::col_relabel(
    `ALT - WEEK 5 DAY 36` = "Analysis Value",
    `CRP - WEEK 5 DAY 36` = "Analysis Value",
    `IGA - WEEK 5 DAY 36` = "Analysis Value"
  )
keep_columns <- c(
  "ALT - WEEK 5 DAY 36",
  "CRP - WEEK 5 DAY 36",
  "IGA - WEEK 5 DAY 36"
)
pca <- summary(stats::prcomp(
  ANL[keep_columns],
  center = TRUE,
  scale. = TRUE,
  retx = TRUE
))
tbl_importance <- dplyr::as_tibble(pca$importance, rownames = "Metric")
tbl_importance
tbl_eigenvector <- dplyr::as_tibble(pca$rotation, rownames = "Variable")
tbl_eigenvector
elb_dat <- pca$importance[
  c("Proportion of Variance", "Cumulative Proportion"),
] %>%
  dplyr::as_tibble(rownames = "metric") %>%
  tidyr::gather("component", "value", -metric) %>%
  dplyr::mutate(
    component = factor(
      component,
      levels = unique(stringr::str_sort(component, numeric = TRUE))
    )
  )
cols <- c(
  getOption("ggplot2.discrete.colour"),
  c("lightblue", "darkred", "black")
)[1:3]
elbow_plot <- ggplot2::ggplot(
  mapping = ggplot2::aes_string(x = "component", y = "value")
) +
  ggplot2::geom_bar(
    ggplot2::aes(fill = "Single variance"),
    data = dplyr::filter(elb_dat, metric == "Proportion of Variance"),
    color = "black",
    stat = "identity"
  ) +
  ggplot2::geom_point(
    ggplot2::aes(color = "Cumulative variance"),
    data = dplyr::filter(elb_dat, metric == "Cumulative Proportion")
  ) +
  ggplot2::geom_line(
    ggplot2::aes(group = 1, color = "Cumulative variance"),
    data = dplyr::filter(elb_dat, metric == "Cumulative Proportion")
  ) +
  ggplot2::labs(
    caption = "NEST PROJECT",
    x = "Principal component",
    y = "Proportion of variance explained",
    color = "",
    fill = "Legend"
  ) +
  ggplot2::scale_color_manual(
    values = c(`Cumulative variance` = cols[2], `Single variance` = cols[3])
  ) +
  ggplot2::scale_fill_manual(
    values = c(`Cumulative variance` = cols[2], `Single variance` = cols[1])
  ) +
  ggplot2::theme_gray() +
  ggplot2::theme(
    legend.position = "right",
    legend.spacing.y = grid::unit(-5, "pt"),
    legend.title = ggplot2::element_text(vjust = 25),
    axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
    text = ggplot2::element_text(size = 12L)
  )
elbow_plot
