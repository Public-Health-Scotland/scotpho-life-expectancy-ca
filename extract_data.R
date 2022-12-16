###############################################.
## ScotPHO - Life expectancy - Council Area ----
###############################################.

# The data is extracted from the SG opendata platform - statistics.gov
# install.packages("devtools")
# install the opendata scotland r package which communicates with the statistics.gov wesbite api
devtools::install_github("datasciencescotland/opendatascot")

library(opendatascot) # to extract from statistics.gov
library(phsmethods)   # to add location names
library(readr)        # to write csv
library(dplyr)
# datasets <- ods_all_datasets() # to see available datasets on statistic.gov.scot
  
# UPDATE the analyst's folder - where data should be saved for shiny app to run
shiny_folder <- "/PHI_conf/ScotPHO/1.Analysts_space/Catherine/scotpho-life-expectancy-ca/shiny_app/data/"

# PDATE data file location
data_folder <- "/PHI_conf/ScotPHO/Website/Topics/Life expectancy/202212_update/"


# parameters used to filter the opendata
simd <- c("all")
urban_rural <- c("all")
age_select <- "0-years"


###############################################.
# Life expectancy data by CA
###############################################.

ods_structure("Life-Expectancy") # see structure and variables of this dataset

# date range for LE
date_range_le <- c("2001-2003", "2002-2004", "2003-2005", "2004-2006", "2005-2007",
                   "2006-2008", "2007-2009", "2008-2010", "2009-2011", "2010-2012",
                   "2011-2013", "2012-2014", "2013-2015", "2014-2016", "2015-2017", 
                   "2016-2018", "2017-2019", "2018-2020", "2019-2021") # add most recent year

# extract data
le = ods_dataset("Life-Expectancy", refPeriod = date_range_le, geography= "la",
                  urbanRuralClassification = urban_rural,
                  simdQuintiles = simd, measureType = "count") %>%
  setNames(tolower(names(.))) %>%
  rename("council" = refarea, "year" = refperiod) %>% 
  filter(age == age_select) %>% 
  mutate(measure = "Life expectancy") %>% 
  select(c("council", "year", "measure", "value", "sex")) %>% 
  mutate(council = match_area(council)) %>% 
  arrange(year, council, sex) %>% 
  mutate(sex = case_when(sex == "male" ~ "Male",
            sex == "female" ~ "Female"))


###############################################.
# Healthy life expectancy data by CA
###############################################.

ods_structure("healthy-life-expectancy") # see structure and variables of this dataset

# date range for HLE
date_range_hle <- c("2015-2017", "2016-2018", "2017-2019", "2018-2020") # add most recent year

# extract data
hle = ods_dataset("healthy-life-expectancy", refPeriod = date_range_hle, geography = "la",
                  urbanRuralClassification = urban_rural,
                  simdQuintiles = simd, measureType = "count") %>%
  setNames(tolower(names(.))) %>%
  rename("council" = refarea, "year" = refperiod) %>% 
  filter(age == age_select) %>% 
  mutate(measure = "Healthy life expectancy") %>% 
  select(c("council", "year", "measure", "value", "sex")) %>% 
  mutate(council = match_area(council)) %>%
  arrange(year, council, sex) %>% 
  mutate(sex = case_when(sex == "male" ~ "Male",
                         sex == "female" ~ "Female"))



# combine datasets
le_hle <- rbind(le, hle)

# save as csv
write_csv(le_hle, paste0(data_folder, "le_hle_ca.csv"))

# Save data to shiny_app folder
saveRDS(le_hle, file = paste0(shiny_folder,"le_hle_ca.rds"))

# END
