# Load Packages -----------------------------------------------------------

library(tidyverse)

# Create Directory --------------------------------------------------------

dir_create("data")

# Download Data -----------------------------------------------------------

# download.file("https://rfor.us/enrollment-data",
#               mode = "wb",
#               destfile = "data/enrollment_by_race_ethnicity.rds")

# Import Data -------------------------------------------------------------

enrollment_by_race_ethnicity <- 
  read_rds("data/enrollment_by_race_ethnicity.rds")

# Plot --------------------------------------------------------------------

enrollment_by_race_ethnicity |> 
  filter(year == "2018-2019") |> 
  filter(district == "Portland SD 1J") |> 
  ggplot(aes(x = percent_proficient, 
             y = school)) +
  geom_col()
