
rm(list = ls())

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("brazil_tworound_functions.R")


#***************************************************************************************************
# READ IN THE DATA EXTRACT ####
#***************************************************************************************************

ddi <- read_ipums_ddi(list.files(pattern = ".xml"))
ipums <- read_ipums_micro(ddi)

rm(ddi)

#***************************************************************************************************
# READ IN THE MUNICIPALITY CODE CROSSWALK ####
#***************************************************************************************************

muni_codes <- fread("muni_codes.csv")

#***************************************************************************************************
# READ IN THE ELECTIONS DATASET ####
#***************************************************************************************************

elections <- readRDS("elections.rds")

#***************************************************************************************************
# CALCULATE MUNICIPALITY STATISTICS ####
#***************************************************************************************************

#ipums <- data.table(zap_labels(zap_label(zap_formats(ipums))))

ipums <- data.table(ipums)

# literacy of all individual over 15 years of age
ipums[, LIT := Recode(LIT, "0=NA")]
ipums[AGE < 15, LIT := NA]

# employment status of all individuals over 16 years of age and in the labor force
ipums[, EMPSTAT := Recode(EMPSTAT, "c(0,3,9)=NA")]
ipums[AGE < 16 & LABFORCE != 2, EMPSTAT := NA]

# low income status of household, defined as whether household income is below 50% of the minimum
# wage in 1980
ipums[, INCTOT := Recode(INCTOT, "c(9999998,9999999)=NA")]
ipums[, INC_0_50 := ((INCTOT / 4149.6) <= 0.5)]

# calculate municipal summary statistics, by IPUMS municipality
ipums <- ipums[, list(
  illit_80 = 100*weighted.mean(LIT == 1, PERWT, na.rm = TRUE),
  unempl_80 = 100*weighted.mean(EMPSTAT == 2, PERWT, na.rm = TRUE),
  inc_0_50_80 = 100*weighted.mean(INC_0_50, PERWT, na.rm = TRUE)
), by = c("GEO2_BR1980")]

# match IPUMS municipality codes to the TSE municipality codes
ipums <- merge(
  ipums, muni_codes[!is.na(ipums_1980), list(tse_code, ipums_1980)], 
  by.x = "GEO2_BR1980", by.y = "ipums_1980"
)
ipums[, GEO2_BR1980 := NULL]

ipums <- ipums[!is.na(tse_code)]

#***************************************************************************************************
# MERGE INTO ELECTIONS DATASET ####
#***************************************************************************************************

# merge in 1980 municipality statistics
elections <- merge(
  elections, ipums,
  by = "tse_code", all.x = TRUE, suffixes = c("", ".ipums")
)

# update variables with those calculated from IPUMS, as these variables are already provided in the
# elections dataset
elections[, illit_80 := illit_80.ipums]
elections[, unempl_80 := unempl_80.ipums]
elections[, inc_0_50_80 := inc_0_50_80.ipums]
elections[, c("illit_80.ipums", "unempl_80.ipums", "inc_0_50_80.ipums") := NULL]

# save the new elections dataset
saveRDS(elections, "elections.rds")
