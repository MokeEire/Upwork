## Author: Mark Barrett - markkbarrett@gmail.com
## Purpose: Convert .dat files to STATA files
## Data: Youth Risk Behavior Survey (YRBS) data from CDC https://www.cdc.gov/healthyyouth/data/yrbs/data.htm
##       Files are available in ASCII format.
##
## Overview:
##    1) Use SAS script to identify column widths in order to read in data
##    2) Use SAS script to identify column names and labels
##    3) Read in data and apply column names and labels to the unlabeled data
##    4) Output files in .dta format

library(tidyverse)
library(haven)
library(labelled)
library(here)


# 1) Use SAS script to identify column widths to read in data -------------


# Extract widths from sas input file
file_widths = read_lines("2017_sadc_sas_input_program.sas", skip = 19, n_max = 314) %>% 
  str_extract(., "[0-9]{1,3}\\-[0-9]{1,3}$") %>% map_dbl(function(x) eval(parse(text = x)) %>% abs() + 1)


# 2) Use SAS script to identify column names and labels -------------------


# Extract column names from sas input file
file_cols = read_lines("2017_sadc_sas_input_program.sas", skip = 19, n_max = 314) %>% 
  str_extract(., "[A-z0-9]+")

# Extract labels from sas input file as list for use with labelled::set_variable_labels()
file_labels_script = read_lines("2017_sadc_sas_input_program.sas", skip = 482, n_max = 314) %>% 
  # capture the variable name and its label by splitting the string on the equals sign
  map(function(x) str_split(string = x, pattern = "=\\\"")) %>% 
  flatten() %>% 
  transpose()

# Set the variable name as the list name for each element
file_labels = set_names(file_labels_script[[2]], file_labels_script[[1]]) %>% 
  map(str_remove, "\\\"")


# 3) Read in data and apply column names and labels to the unlabel --------


## Write function to 
read_yrbs_data = function(file){
  read_fwf(file, 
           col_positions = fwf_widths(file_widths, col_names = file_cols), 
           na = c("NA", ".")) %>% 
    # Mutating logicals to numeric allows for consistency when joining files together (although does make files significantly larger)
    mutate_if(is_logical, list(~as.numeric(.))) %>%
    # Set variable labels
    set_variable_labels(.labels = file_labels)
}

national_data = read_yrbs_data("sadc_2017_national.dat")
state_data_a_m = read_yrbs_data("sadc_2017_state_a_m.dat")
state_data_n_z = read_yrbs_data("sadc_2017_state_n_z.dat")

# 4) Output files in .dta format ------------------------------------------


write_dta(data = national_data, path = here("sadc_2017_national.dta"), version = 13)
write_dta(data = state_data_a_m, path = here("sadc_2017_state_a_m.dta"), version = 13)
write_dta(data = state_data_n_z, path = here("sadc_2017_state_n_z.dta"), version = 13)

