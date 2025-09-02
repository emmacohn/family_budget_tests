library(tidyverse)
library(ipumsr)
library(janitor)
library(labelled)
library(fs)

# Load samples
acs_samps <- ipumsr::get_sample_info('usa')

# Create a vector of sample IDs to load
years <- c('us2000a', 'us2001a', 'us2002a', 'us2003a', 'us2004a', 'us2005a',
           'us2006a', 'us2007a', 'us2008a', 'us2009a', 'us2010a', 'us2011a',
           'us2012a', 'us2013a', 'us2014a', 'us2015a', 'us2016a', 'us2017a',
           'us2018a', 'us2019a', 'us2020a', 'us2021a', 'us2022a', 'us2023a')

acs_extr <- define_extract_micro(
  "usa",
  description = 'ACS extract for age gap statistics',
  samples = years,
  # Select the variables to load
  variables = list('STATEFIP','COUNTYFIP', 'SEX', 'AGE', 'RACE', 'HISPAN', 'RELATE', 
                   'FAMUNIT', 'SPLOC', 'MOMLOC', 'POPLOC', 'YNGCH', 'ELDCH', 'SERIAL',
                   'YEAR', 'DATANUM')) |> 
  submit_extract() |> 
  wait_for_extract()

# Download extract to input folder
dl_extr <- download_extract(extract = acs_extr,
                                      download_dir = 'input/',
                                      overwrite = TRUE)

# NOTE: Your project directory and xml file may look different!
acs_raw <- read_ipums_micro(ddi = 'input/usa_00026.xml')

acs <- acs_raw |> 
  # Use the janitor library to clean up names
  janitor::clean_names() |> 
  filter(year == c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) |> 
  unite(col = "famid", c(serial, famunit), na.rm = TRUE) |>
  mutate(child = case_when(age < 18 ~ 1,
                           TRUE ~ 0))
