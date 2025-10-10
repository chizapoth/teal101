library(teal)
library(teal.modules.clinical)

library(dplyr)


# the datasets are in tmc, need just two: adsl and adae
adsl <- teal.modules.clinical::tmc_ex_adsl
adae <- teal.modules.clinical::tmc_ex_adae


# explore the data a bit
# 200 unique patients
head(adsl)
filter(adsl, COUNTRY == 'ITA')
table(adsl$COUNTRY)
tail(adsl)

head(adae)
View(adsl)
View(adae)
adsl$USUBJID |> unique() |> length()
adae$USUBJID |> unique() |> length()
# ? need to see which is the key that links them, as they are not exactly matching!!

usubjid_adsl <- adsl$USUBJID |> unique()
usubjid_adae <- adae$USUBJID |> unique()
# check set difference
setdiff(usubjid_adsl, usubjid_adae) # in adsl, not adae - 36 missing in adae!
setdiff(usubjid_adae, usubjid_adsl) # in adae, not in adsl

# save the two datasets somewhere so that I can replace the path
write.csv(adsl, file = 'data/cdisc/adsl.csv', row.names = F)
write.csv(adae, file = 'data/cdisc/adae.csv', row.names = F)
