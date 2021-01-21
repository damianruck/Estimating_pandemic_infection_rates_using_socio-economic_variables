#############
# Load data
#############
### Download and load case data
# download.file(url="https://static.usafacts.org/public/data/covid-19/covid_confirmed_usafacts.csv", destfile="data/covid_confirmed_usafacts.csv", method = "auto", quiet=TRUE)
covidByCounty <- read.csv("data/covid_confirmed_usafacts.csv")

### Download and load death data
# download.file(url="https://static.usafacts.org/public/data/covid-19/covid_deaths_usafacts.csv", destfile="data/covid_deaths_usafacts.csv", method = "auto", quiet=TRUE)
deathsByCounty <- read.csv("data/covid_deaths_usafacts.csv")

### Our world in data daily cases and deaths
dailyCasesDeaths <- read.csv("data/daily-covid-cases-deaths.csv")

### Variable data
variableMatrix1 <- read.csv("data/county_level_df_no_network_v20200909.csv")
variableMatrix2 <- read.csv("data/COVID_fitted_values_v20200909.csv")

### Combine finalMatrix1 and finalMatrix2
finalMatrix <- merge(x = variableMatrix1, y = variableMatrix2, by = "FIPS", all.x = TRUE)
finalMatrix <- finalMatrix %>% 
  rename_at(
    vars(ends_with(".x")),
    ~str_replace(., "\\..$","")
  ) %>% 
  select_at(
    vars(-ends_with(".y"))
  )