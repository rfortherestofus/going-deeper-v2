# Load Packages -----------------------------------------------------------

library(tidyverse)
library(fs)
library(readxl)
library(janitor)

# Create Directories ------------------------------------------------------

dir_create("data-raw")

# Download Data -----------------------------------------------------------

# https://www.oregon.gov/ode/reports-and-data/students/Pages/Student-Enrollment-Reports.aspx

# download.file("https://www.oregon.gov/ode/reports-and-data/students/Documents/fallmembershipreport_20222023.xlsx",
#               mode = "wb",
#               destfile = "data-raw/fallmembershipreport_20222023.xlsx")
# 
# download.file("https://www.oregon.gov/ode/reports-and-data/students/Documents/fallmembershipreport_20212022.xlsx",
#               mode = "wb",
#               destfile = "data-raw/fallmembershipreport_20212022.xlsx")
# 
# download.file("https://www.oregon.gov/ode/reports-and-data/students/Documents/fallmembershipreport_20202021.xlsx",
#               mode = "wb",
#               destfile = "data-raw/fallmembershipreport_20202021.xlsx")
# 
# download.file("https://www.oregon.gov/ode/reports-and-data/students/Documents/fallmembershipreport_20192020.xlsx",
#               mode = "wb",
#               destfile = "data-raw/fallmembershipreport_20192020.xlsx")
# 
# download.file("https://www.oregon.gov/ode/reports-and-data/students/Documents/fallmembershipreport_20182019.xlsx",
#               mode = "wb",
#               destfile = "data-raw/fallmembershipreport_20182019.xlsx")

# Import Data -------------------------------------------------------------

enrollment_2022_2023 <- read_excel(path = "data-raw/fallmembershipreport_20222023.xlsx",
                                   sheet = "School 2022-23") |> 
  clean_names()

enrollment_2021_2022 <- read_excel(path = "data-raw/fallmembershipreport_20212022.xlsx",
                                   sheet = "School 2021-22") |> 
  clean_names()


clean_enrollment_data <- function(excel_file,
                                  sheet_name) {
  
  read_excel(path = YOURCODEHERE,
             sheet = YOURCODEHERE) |> 
    clean_names() |> 
    
    # I've selected by column position rather than names 
    # because the column names vary in the data between years
    # but they're always in the same positions
    select(1, 7:20) |> 
    select(-contains("percent")) |> 
    
    # Set the names of the columns
    set_names("district_institution_id",
              YOURCODEHERE,
              YOURCODEHERE,
              YOURCODEHERE,
              YOURCODEHERE,
              YOURCODEHERE,
              YOURCODEHERE,
              YOURCODEHERE) |> 
    pivot_longer(cols = -district_institution_id,
                 names_to = "race_ethnicity",
                 values_to = "number_of_students") |> 
    mutate(race_ethnicity = case_when(
      race_ethnicity == "american_indian_alaska_native" ~ "American Indian Alaska Native",
      race_ethnicity == "asian" ~ "Asian",
      race_ethnicity == "black_african_american" ~ "Black/African American",
      race_ethnicity == "hispanic_latino" ~ "Hispanic/Latino",
      race_ethnicity == "multi_racial" ~ "Multiracial",
      race_ethnicity == "native_hawaiian_pacific_islander" ~ "Native Hawaiian Pacific Islander",
      race_ethnicity == "white" ~ "White"
    )) |> 
    mutate(number_of_students = parse_number(number_of_students)) |> 
    group_by(district_institution_id) |> 
    mutate(pct = number_of_students / sum(number_of_students, na.rm = TRUE)) |> 
    ungroup() |> 
    mutate(year = sheet_name)
  
}

enrollment_by_race_ethnicity_2021_2022 <- 
  clean_enrollment_data(excel_file = YOURCODEHERE,
                        sheet_name = YOURCODEHERE) 

enrollment_by_race_ethnicity_2022_2023 <-
  clean_enrollment_data(excel_file = YOURCODEHERE,
                        sheet_name = YOURCODEHERE) 


enrollment_by_race_ethnicity <-
  bind_rows(enrollment_by_race_ethnicity_2021_2022,
            enrollment_by_race_ethnicity_2022_2023) 