# Activate packages -------------------------------------------------------
library(dplyr)
library(tidyverse)
library(tigris)
library(sf)
library(units)

hospital_discharge_inpatient_2010_2020 <- read_csv("E:/OneDrive - Cook County Health/DD-21-002_InPat_CCDPH_2010-2020.csv")
saveRDS(hospital_discharge_inpatient_2010_2020,"E:/OneDrive - Cook County Health/hospital_discharge_inpatient_2010_2020.Rds")


gc()
hospital_discharge_outpatient_2010_2020 <- read_csv("C:/Users/christopher.smith/OneDrive - Cook County Health/DD-21-002_OutPat_CCDPH_2010-2020/DD-21-002_OutPat_CCDPH_2010-2020.csv")
