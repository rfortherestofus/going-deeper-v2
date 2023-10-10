# Load Packages -----------------------------------------------------------

library(tidyverse)
library(fs)
library(readxl)
library(janitor)

# Create Directories ------------------------------------------------------

dir_create("data-raw")
dir_create("data")

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
#
# download.file("https://github.com/rfortherestofus/going-deeper-v2/raw/main/data-raw/oregon-districts.xlsx",
#               mode = "wb",
#               destfile = "data-raw/oregon-districts.xlsx")

# Import Data -------------------------------------------------------------

enrollment_2022_2023 <- read_excel(path = "data-raw/fallmembershipreport_20222023.xlsx",
                                   sheet = "School 2022-23") |> 
  clean_names()

enrollment_2021_2022 <- read_excel(path = "data-raw/fallmembershipreport_20212022.xlsx",
                                   sheet = "School 2021-22") |> 
  clean_names()


# Function ----------------------------------------------------------------

clean_enrollment_data <- function(excel_file, sheet_name) {
  
  read_excel(path = excel_file,
             sheet = sheet_name) |> 
    clean_names() |> 
    select(1, 7:20) |> 
    select(-contains("percent")) |> 
    set_names("district_institution_id",
              "american_indian_alaska_native",
              "asian",
              "native_hawaiian_pacific_islander",
              "black_african_american",
              "hispanic_latino",
              "white",
              "multi_racial") |> 
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

enrollment_by_race_ethnicity_2022_2023 <-
  clean_enrollment_data(excel_file = "data-raw/fallmembershipreport_20222023.xlsx",
                        sheet_name = "School 2022-23")

enrollment_by_race_ethnicity_2021_2022 <-
  clean_enrollment_data(excel_file = "data-raw/fallmembershipreport_20212022.xlsx",
                        sheet_name = "School 2021-22")

enrollment_by_race_ethnicity <-
  bind_rows(enrollment_by_race_ethnicity_2021_2022,
            enrollment_by_race_ethnicity_2022_2023) 


oregon_districts <-
  read_excel("data-raw/oregon-districts.xlsx") |> 
  clean_names() |> 
  rename(district_institution_id = attending_district_institutional_id)

enrollment_by_race_ethnicity_with_district_names <- 
  left_join(enrollment_by_race_ethnicity,
            oregon_districts,
            join_by(district_institution_id))


# Export ------------------------------------------------------------------

enrollment_by_race_ethnicity_with_district_names |> 
  write_rds("data/enrollment_by_race_ethnicity_with_district_names.rds")