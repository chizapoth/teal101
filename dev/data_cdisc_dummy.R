# understand what are typical data inputs for teal app
# install.packages('random.cdisc.data')

# creating random cdisc data
library(random.cdisc.data)

?radsl

# ADSL ----
# radsl: Subject-Level Analysis Dataset (ADSL)
# create 10 patients
# this is the mother file of all

adsl <- radsl(N = 10, study_duration = 2, seed = 1)
adsl

View(adsl)

class(adsl)
attributes(adsl)

attr(adsl, 'label')
head(adsl)

View(adsl)
adsl$STUDYID


# check the colnames
colnames(adsl)

teal.data::rADSL

?random.cdisc.data::radsl

# ADAE ----
# radae: Adverse Event Analysis Dataset (ADAE)
# requires adsl
adae <- radae(adsl, seed = 2)
head(adae)
nrow(adae)
# View(adae)

# ADMH ----
# radmh: Medical History Analysis Dataset (ADMH)
# requires adsl

admh <- radmh(adsl, seed = 2)
admh


# ADLB ----
# radlb: Laboratory Data Analysis Dataset (ADLB)

adlb <- radmh(adsl, seed = 2)
adlb

# ADAETTE ----
# radaette: Time to Adverse Event Analysis Dataset (ADAETTE)
adaette <- radaette(adsl, seed = 2)
adaette


# ADEG ----
# radeg: ECG Analysis Dataset (ADEG)

adeg <- radeg(adsl, seed = 2)
adeg


# ADCM ----
# radcm: Previous and Concomitant Medications Analysis Dataset (ADCM)
adcm <- radcm(adsl, seed = 2)
adcm

# ADDV ----
# raddv: Protocol Deviations Analysis Dataset (ADDV)
addv <- raddv(adsl, seed = 2)
addv


# ADEX ----
# radex: Exposure Analysis Dataset (ADEX)
adex <- radex(adsl, seed = 2)
adex

# ADHY ----
# radhy: Hys Law Analysis Dataset (ADHY)
adhy <- radhy(adsl, seed = 2)
adhy


# ADPC ----
# radpc: Pharmacokinetics Analysis Dataset (ADPC)
adpc <- radpc(adsl, seed = 2)
adpc


# ADAB ----
# radab: Anti-Drug Antibody Analysis Dataset (ADAB)
# requires adsl, adpc

adab <- radab(adsl, adpc, seed = 2)
adab
# ?radab

# ADPP ----
# radpp: Pharmacokinetics Parameters Dataset (ADPP)
adpp <- radpp(adsl, seed = 2)
adpp

# ADQLQC ----#
# radqlqc: EORTC QLQ-C30 V3 Analysis Dataset (ADQLQC)
# THIS IS PROBABLY NOT GOING TO BE NEEDED
# adqlqc <- radqlqc(adsl, seed = 2)
# adqlqc
# ?radqlqc

# ADQS ----
# radqs: Questionnaires Analysis Dataset (ADQS)
adqs <- radqs(adsl, seed = 2)
adqs

# ADRS ----
# radrs: Response Analysis Dataset (ADRS)
adrs <- radrs(adsl, seed = 2)
adrs


# radsaftte ----
# radsaftte: Time to Safety Event Analysis Dataset (ADSAFTTE, wrapper of radaette)
adssaftte <- radsaftte(adsl, seed = 2)
adssaftte

# ADSUB ----
# radsub: Subcategory Analysis Dataset (ADSUB)
adsub <- radsub(adsl, seed = 2)
adsub


# ADTR ----
# radtr: Tumor Response Analysis Dataset (ADTR)
adtr <- radtr(adsl, seed = 2)
adtr

# ADTTE ----
# radtte: Time-to-Event Dataset (ADTTE)
adtte <- radtte(adsl, seed = 2)
adtte

# ADVS ----
# radvs: Vital Signs Analysis Dataset (ADVS)
advs <- radvs(adsl, seed = 2)
advs


# _______-----
# save gsheet ----
# save in gsheet so it's easier to see by human eyes
library(googlesheets4)

ss <- gs4_create("dummy_cdisc", sheets = 'ADSL')
ss
class(ss)

str(ss)
# save sheet id
sheet_id <- as_sheets_id(ss)

# if need to check id
googledrive::as_id(ss)
as_id(ss)


# gs4_find("fluffy-bunny")

sheet_write(adsl, ss, sheet = "ADSL")
sheet_write(adae, ss, sheet = "ADAE")
sheet_write(admh, ss, sheet = "ADMH")
sheet_write(adlb, ss, sheet = "ADLB")
sheet_write(adab, ss, sheet = "ADAB")
sheet_write(adaette, ss, sheet = "ADAETTE")
sheet_write(adeg, ss, sheet = "ADEG")
sheet_write(adcm, ss, sheet = "ADCM")
sheet_write(addv, ss, sheet = "ADDV")
sheet_write(adex, ss, sheet = "ADEX")
sheet_write(adhy, ss, sheet = "ADHY")
sheet_write(adpc, ss, sheet = "ADPC")
sheet_write(adpp, ss, sheet = "ADPP")
sheet_write(adqlqc, ss, sheet = "ADQLQC") #
sheet_write(adqs, ss, sheet = "ADQS")
sheet_write(adrs, ss, sheet = "ADRS")
sheet_write(adssaftte, ss, sheet = "ADSAFTTE")
sheet_write(adsub, ss, sheet = "ADSUB")
sheet_write(adtr, ss, sheet = "ADTR")
sheet_write(adtte, ss, sheet = "ADTTE")
sheet_write(advs, ss, sheet = "ADVS")


# if want to open the sheet to browse
gs4_browse(sheet_id)


# labels for common datasets -----

# only needed for ADSL, ADMH, ADAE, ADTTE, ADLB, ADCM, ADVS, ADRS
# check the label of the dataset
attr(adsl$STUDYID, 'label')

label_adsl <- sapply(adsl, function(x) {
  attr(x, 'label')
})

# create a data frame with labels
clean_label <- function(cdisc_data, dataname) {
  # extract
  codes <- colnames(cdisc_data)
  labels <- sapply(cdisc_data, function(x) {
    attr(x, 'label')
  })
  # put together, attach name
  df <- data.frame(
    code = codes,
    label = labels,
    dataset = dataname
  )

  return(df)
}


# do it for the selected datasets

labels_adsl <- clean_label(cdisc_data = adsl, dataname = "ADSL")
labels_admh <- clean_label(cdisc_data = admh, dataname = "ADMH")
labels_adae <- clean_label(cdisc_data = adae, dataname = "ADAE")
labels_adlb <- clean_label(cdisc_data = adlb, dataname = "ADLB")
labels_adcm <- clean_label(cdisc_data = adcm, dataname = "ADCM")
labels_adrs <- clean_label(cdisc_data = adrs, dataname = "ADRS")
labels_advs <- clean_label(cdisc_data = advs, dataname = "ADVS")
labels_adtte <- clean_label(cdisc_data = adtte, dataname = "ADTTE")

# combine all labels
labels_all <- rbind(
  labels_adsl,
  labels_admh,
  labels_adae,
  labels_adlb,
  labels_adcm,
  labels_adrs,
  labels_advs,
  labels_adtte
)

# save labels to gsheet

sheet_write(labels_all, ss, sheet = "labels")
