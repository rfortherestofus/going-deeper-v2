# Load Packages -----------------------------------------------------------

library(tidyverse)
library(fs)

# Create Directory --------------------------------------------------------

dir_create("data")

# Download Data -----------------------------------------------------------

# download.file("https://github.com/rfortherestofus/going-deeper-v2/raw/main/data/third_grade_math_proficiency.rds",
#               mode = "wb",
#               destfile = "data/third_grade_math_proficiency.rds")

# Import Data -------------------------------------------------------------

third_grade_math_proficiency <- 
  read_rds("data/third_grade_math_proficiency.rds")

# Plot --------------------------------------------------------------------

third_grade_math_proficiency |> 
  filter(year == "2021-2022") |> 
  filter(district == "Portland SD 1J") |> 
  ggplot(aes(x = percent_proficient, 
             y = reorder(school, percent_proficient))) +
  geom_col()

third_grade_math_proficiency |> 
  filter(year == "2021-2022") |> 
  filter(district == "Portland SD 1J") |> 
  mutate(school = fct_reorder(school, percent_proficient)) |>
  ggplot(aes(x = percent_proficient, 
             y = reorder(school, percent_proficient))) +
  geom_col() 
