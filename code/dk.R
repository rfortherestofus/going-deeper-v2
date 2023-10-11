library(tidyverse)

# Data from going-deeper-v2-solutions
read_rds("data/third_grade_math_proficiency.rds") |> 
  select(academic_year, school, school_id, district, proficiency_level, number_of_students) |> 
  mutate(is_proficient = case_when(
    proficiency_level >= 3 ~ TRUE,
    .default = FALSE
  )) |> 
  group_by(academic_year, school, district, school_id, is_proficient) |> 
  summarize(number_of_students = sum(number_of_students, na.rm = TRUE)) |> 
  ungroup() |> 
  group_by(academic_year, school, district, school_id) |> 
  mutate(percent_proficient = number_of_students / sum(number_of_students, na.rm = TRUE)) |> 
  ungroup() |> 
  filter(is_proficient == TRUE) |> 
  select(academic_year, school, district, percent_proficient) |> 
  rename(year = academic_year) |> 
  mutate(percent_proficient = case_when(
    is.nan(percent_proficient) ~ NA,
    .default = percent_proficient
  )) |> 
  write_rds("data/third_grade_math_proficiency.rds")

read_rds("data/enrollment_by_race_ethnicity.rds") |> 
  view()
  select(-district_institution_id)  |> 
  select(year, district, everything()) |> 
  mutate(year = case_when(
    year == "School 2021-22" ~ "2021-2022",
    year == "School 2022-23" ~ "2022-2023",
  )) |> 
  write_rds("data/enrollment_by_race_ethnicity.rds")
