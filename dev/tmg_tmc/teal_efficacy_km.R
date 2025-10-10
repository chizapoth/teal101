# test the code for making KM plot 
library(dplyr)
library(random.cdisc.data)
library(nestcolor)
library(sparkline)


# generate some data
ADSL <- radsl(seed = 1)
.adsl_labels <- teal.data::col_labels(ADSL, fill = FALSE)
.char_vars_asl <- names(Filter(isTRUE, sapply(ADSL, is.character)))
.adsl_labels <- c(.adsl_labels, AGEGR1 = "Age Group")
ADSL <- ADSL %>% mutate(AGEGR1 = factor(case_when(AGE < 45 ~ "<45", AGE >= 45 ~ ">=45"))) %>% mutate_at(.char_vars_asl, factor)
teal.data::col_labels(ADSL) <- .adsl_labels
ADTTE <- radtte(ADSL, seed = 1)
# stopifnot(rlang::hash(ADSL) == "a38e9170aea81f1199dfe480c202142e") # @linksto ADSL
# stopifnot(rlang::hash(ADTTE) == "72632be95359c50314783fa510aaca94") # @linksto ADTTE
.raw_data <- list2env(list(ADSL = ADSL, ADTTE = ADTTE))
# lockEnvironment(.raw_data) # @linksto .raw_data


ADTTE <- dplyr::inner_join(x = ADTTE, y = ADSL[, c("STUDYID", "USUBJID"), drop = FALSE], by = c("STUDYID", "USUBJID"))
ANL_1 <- ADTTE %>% dplyr::select(STUDYID, USUBJID, PARAMCD, AVAL, CNSR, AVALU)
ANL_2 <- ADSL %>% dplyr::select(STUDYID, USUBJID, ARM, STRATA1, AGEGR1)
ANL_3 <- ADTTE %>% dplyr::filter(PARAMCD == "OS") %>% dplyr::select(STUDYID, USUBJID, PARAMCD)
ANL <- ANL_1
ANL <- dplyr::inner_join(ANL, ANL_2, by = c("STUDYID", "USUBJID"))
ANL <- dplyr::inner_join(ANL, ANL_3, by = c("STUDYID", "USUBJID", "PARAMCD"))
ANL <- ANL %>% teal.data::col_relabel(AVAL = "Analysis Value", CNSR = "Censor", ARM = "Description of Planned Arm", PARAMCD = "Parameter Code", STRATA1 = "Stratification Factor 1", AGEGR1 = "Age Group", AVALU = "Analysis Value Unit")
anl <- ANL %>% dplyr::filter(ARM %in% c("A: Drug X", "B: Placebo", "C: Combination")) %>% dplyr::mutate(ARM = stats::relevel(ARM, ref = "A: Drug X")) %>% dplyr::mutate(ARM = droplevels(ARM)) %>% dplyr::mutate(is_event = CNSR == 0)
variables <- list(tte = "AVAL", is_event = "is_event", arm = "ARM", strata = "STRATA1")

facets <- droplevels(anl$AGEGR1)
anl <- split(anl, f = facets)
g_km_counter_generator <- function() {
    plot_number <- 0L
    function(x) {
        plot_number <<- plot_number + 1L
        g_km(x, variables = variables, control_surv = control_surv_timepoint(conf_level = 0.95), xticks = NULL, xlab = sprintf("%s (%s)", "Time", gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", tolower(x$AVALU[1]), perl = TRUE)), yval = "Survival", ylim = 0:1, title = sprintf("%s%s", sprintf("%s%s", "KM Plot of OS", if (!is.null(facets)) {
            sprintf(", %s = %s", as.character(quote(AGEGR1)), unique(x[[as.character(quote(AGEGR1))]]))
        }
        else {
            ""
        }), if (length("STRATA1") != 0) {
            sprintf("\nStratified by %s", toString("STRATA1"))
        }
        else {
            ""
        }), footnotes = if (TRUE) {
            paste("Ties for Coxph (Hazard Ratio):", "exact", "\n", "p-value Method for Coxph (Hazard Ratio):", "log-rank")
        }, font_size = 11L, ci_ribbon = FALSE, annot_surv_med = TRUE, annot_coxph = TRUE, control_coxph_pw = control_coxph(conf_level = 0.95, pval_method = "log-rank", ties = "exact"), control_annot_surv_med = list(x = 0.8, y = 0.85, w = 0.32, h = 0.16, fill = TRUE), control_annot_coxph = list(x = 0.27, y = 0.35, w = 0.3, h = 0.125, fill = TRUE, ref_lbls = FALSE), legend_pos = c(0.9, 0.5), rel_height_plot = 0.8)
    }
}
g_km_counter <- g_km_counter_generator()

# the data is anl
View(anl)
plot_list <- lapply(anl, g_km_counter)

plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 1)
print(plot)
