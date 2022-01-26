
# load library -------------------------------------------------------------

library(imfr)
library(tidyverse)



# import data -------------------------------------------------------------

# get the OECD countries name
url <- "https://raw.githubusercontent.com/OxfordEconomics/CountryLists/master/countryList-OECD.csv"

oecd <- read_csv(url) 

oecd <- oecd %>% 
  `names<-`("country") %>% 
  add_row(country = "South Korea") %>% 
  add_row(country = "Slovakia")

iso3c_oecd <- 
  countrycode::countrycode(
    sourcevar = oecd$country, 
    origin = "country.name",
    destination = "iso2c"
  )

# get the database_id, indicator

start_year <- 1950
end_year <- 2021

## list IMF database IDs

database_ids <- imf_ids()

# Retreive the list of codes (codelist) for of an individual IMF database.

database_str <- imf_codelist(database_id = "IFS")

# Retrieve individual database codes

indicators_list <- imf_codes(codelist = database_str[4, 1])

# indicators for "unemployment" and "gdp"

# which indicators should be used?
indicators_unemployment <- 
  indicators_list %>% 
  filter(grepl("Unemployment|employment", description)) %>% 
  select(codes, description)

indicators_gdp <- 
  indicators_list %>% 
  filter(grepl("Gross Domestic Product, Real, Domestic Currency", description))

indicators_filtered <- indicators_list %>% 
  filter(codes %in% c(indicators_unemployment[2, 1], indicators_gdp[1, 1]))

# import data from IMF though API
df_imf <- 
  imf_data(database_id = 'IFS',
         indicator = indicators_filtered$codes,
         country = iso3c_oecd,
         start = start_year,
         end = end_year,
         freq = "A" # "M" is for monthly data
         ) 

# how to get gdp data?

# tidy data ---------------------------------------------------------------

df_imf <- df_imf %>% 
  janitor::clean_names() 

df_imf_tidy <- df_imf %>% 
  mutate(
    country = countrycode::countrycode(
      sourcevar = iso2c, 
      origin = "iso2c",
      destination = "country.name"),
    year = as.numeric(year),
    ) %>% 
  rename(gdp_rate = ngdp_r_xdc,
         uem_rate = lur_pc_cp_a_pt) %>% 
  select(iso2c, country, year, everything()) %>% 
  as_tibble()


# check data --------------------------------------------------------------

skimr::skim(df_imf_tidy)


# load data ---------------------------------------------------------------

path <- "~/R/Musashi_University/2021_second_semester/final-thesis/Data/df_imf"

write_rds(df_imf_tidy, file = path)

# check to load the downloaded data

read_rds(path)
