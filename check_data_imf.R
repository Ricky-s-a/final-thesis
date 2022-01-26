
# load package ------------------------------------------------------------

library(tidyverse)



# import data -------------------------------------------------------------

df <- read_csv("~/R/Musashi_University/2021_second_semester/final-thesis/Data/WEO_Data.csv")


# tidy data ---------------------------------------------------------------

# column names
columns <- c("country", "subject_descriptor", "units", "scale", "series_notes",
             1980:2026, "estimates_start_after")

df_tidy <- df %>% 
  `names<-`(value = columns) %>% 
  pivot_longer(cols = c(-country, -subject_descriptor, -units, -scale, -series_notes),
               names_to = "year",
               values_to = "value"
               )
  
