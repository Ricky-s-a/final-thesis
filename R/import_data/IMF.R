
# load library -------------------------------------------------------------

library(imfr)
library(tidyverse)



# import data -------------------------------------------------------------

# get the OECD coutnries name
url <- "https://raw.githubusercontent.com/OxfordEconomics/CountryLists/master/countryList-OECD.csv"
oecd <- read_csv(url) %>% 
  `names<-`("country")

iso3c_oecd <- 
  countrycode::countrycode(
    sourcevar = oecd$country, 
    origin = "country.name",
    destination = "iso2c"
  )

# get the database_id, indicator

start_year <- 2000
end_year <- 2020

## list IMF database IDs

database_ids <- imf_ids()

# Retreive the list of codes (codelist) for of an individual IMF database.

database_str <- imf_codelist(database_id = "IFS")

# Retrieve individual database codes

indicators_list <- imf_codes(codelist = code_database[4, 1])

# indicators for "unemployment" and "gdp"

# which indicators should be used?
indicators_filtered <- indicators_list %>% 
  filter(grepl("Unemployment|employment", description)) %>% 
  select(codes)


# import data from IMF though API
df_imf <- 
  imf_data(database_id = 'IFS',
         indicator = indicators_filtered$codes,
         country = iso3c_oecd,
         start = start_year,
         end = end_year,
         freq = "M" # "M" is for monthly data
)

# tidy data ---------------------------------------------------------------


# load data ---------------------------------------------------------------

write_rds(df_imf, file = "~/R/Musashi_University/2021_second_semester/graduation_project/Data/df_imf")

# check to load the downloaded data

read_rds("~/R/Musashi_University/2021_second_semester/graduation_project/Data/df_imf")
