library(tidyverse)
library(ipumsr)
library(janitor)
library(labelled)
library(fs)
library(epidatatools)
library(openxlsx2)

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
                   'FAMUNIT', 'SPLOC', 'MOMLOC', 'POPLOC', 'YNGCH', 'ELDCH',
                   'YEAR', 'NCHILD')) |> 
  submit_extract() |> 
  wait_for_extract()

# Download extract to input folder
dl_extr <- download_extract(extract = acs_extr,
                                      download_dir = 'input/',
                                      overwrite = TRUE)

# NOTE: Your project directory and xml file may look different!
acs_raw <- read_ipums_micro(ddi = 'input/usa_00028.xml')

acs <- acs_raw |> 
  # Use the janitor library to clean up names
  janitor::clean_names() #|> 
# serial = unique hh idea, famunit = unique when multiple fams in one hh
#  unite(col = "famid", c(serial, famunit), na.rm = TRUE) |> 
#  mutate(child = case_when(age < 18 ~ 1,
#                           TRUE ~ 0))

#across all families
child_age_gaps <- acs |> 
  filter(nchild > 1) |> 
  group_by(year) |> 
  mutate(gap = (eldch - yngch)/(nchild - 1)) |> 
  ungroup() |> 
  summarise(avg_gap = weighted.mean(gap, perwt, na.rm = TRUE),
            wt_med_gap = weighted.median(gap, perwt, na.rm = TRUE),
            med_gap = median(gap, na.rm = TRUE),
            .by = year)

# 2 kid families
two_child_gap <- acs |> 
  filter(nchild == 2) |> 
  group_by(year) |> 
  mutate(gap = (eldch - yngch)) |> 
  ungroup() |> 
  summarise(two_gap = weighted.mean(gap, perwt, na.rm = TRUE),
            two_med_gap = median(gap, na.rm = TRUE),
            .by = year)

# 3 kid families
three_child_gap <- acs |> 
  filter(nchild == 3) |> 
  group_by(year) |> 
  mutate(gap = (eldch - yngch)/2) |> 
  ungroup() |> 
  summarise(three_gap = weighted.mean(gap, perwt, na.rm = TRUE),
            three_med_gap = median(gap, na.rm = TRUE),
            .by = year)

# 4 kid families
four_child_gap <- acs |> 
  filter(nchild == 4) |> 
  group_by(year) |> 
  mutate(gap = (eldch - yngch)/3) |> 
  ungroup() |> 
  summarise(four_gap = weighted.mean(gap, perwt, na.rm = TRUE),
            four_med_gap = median(gap, na.rm = TRUE),
            .by = year)

age_gaps <-  left_join(child_age_gaps, two_child_gap, by='year') |>
  left_join(three_child_gap) |> 
  left_join(four_child_gap)

wb$add_worksheet(sheet = "Age gaps") $
  add_data(x = age_gaps)

crosstab(acs, nchild, year)

crosstabs <- crosstab(acs, child, year)

wb$add_worksheet(sheet = "Crosstab") $
  add_data(x = crosstabs)

wb_save(wb, "output/age_gaps_FBC.xlsx")

