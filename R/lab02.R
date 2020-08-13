# Chi Zhang
# 08/12/2020
# COVID-19 Pandemic

library(tidyverse)

homes = read.csv('data/landdata-states.csv')

library(readxl)
pop <- read_excel("data/PopulationEstimates.xls", skip = 2) %>%
  select(pop19 = POP_ESTIMATE_2019, state = State, county = Area_Name) %>%
  group_by(state) %>%
  slice_max(pop19, n = 1)


names(homes)

homes %>%
  filter(State %in% c("CA", "TX")) %>%
  ggplot(aes(x = Year, y = Home.Value)) +
  geom_line(aes(color = State)) +
  labs(title = 'title')

library(tidyverse)

homes = read.csv('data/landdata-states.csv')

pop = readxl:::read_excel("data/populationEstimates.xls", skip = 2) %>%
  select(pop2019 = POP_ESTIMATE_2019, fips = FIPStxt)

covid = read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv')

newData = inner_join(pop, covid, by = "fips") %>%
  group_by(county) %>%
  mutate(newCase = cases - lag(cases))


