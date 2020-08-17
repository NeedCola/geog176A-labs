---
  # Chi Zhang
  # 08/12/2020
  # COVID-19 Pandemic
  ---


library(tidyverse)
library(knitr)
library(readxl)
library(zoo)



covid = read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv')

pop = readxl:::read_excel("data/populationEstimates.xls", skip = 2) %>%
  select(pop2019 = POP_ESTIMATE_2019, State = State, fips = FIPStxt)

pop_ca2019 = pop %>%
  filter(State == "CA") %>%
  slice_max(pop2019, n = 1)

cases = covid %>%
  filter(state %in% c("California")) %>%
  group_by(county) %>%
  mutate(newCases = cases - lag(cases)) %>%
  ungroup() %>%
  filter(date == max(date))


most_cases = cases %>%
  slice_max(cases, n = 5) %>%
  select(county, cases)

knitr::kable(most_cases,
             caption = "Most Cases California Counties",
             col.names = c("County", "Cases"))

most_new_cases = cases %>%
  slice_max(newCases, n = 5) %>%
  select(county, newCases)

knitr::kable(most_new_cases,
             caption = "Most New Cases California Counties",
             col.names = c("County", "New Cases"))

pop_data1 = right_join(pop, cases, by = "fips") %>%
  mutate(cases_percapita = (cases / pop2019) * 100000,
         newCases_percapita = (newCases / pop2019) * 100000)

most_cases_percapita = pop_data1 %>%
  slice_max(cases_percapita, n = 5) %>%
  select(county, cases_percapita)

knitr::kable(most_cases_percapita,
             caption = "Most Cumulative Cases Per Capita",
             col.names = c("County", "Cases"))

most_new_cases_percapita = pop_data1 %>%
  slice_max(newCases_percapita, n = 5) %>%
  select(county, newCases_percapita)

knitr::kable(most_new_cases_percapita,
             caption = "Most New Cases Per Capita",
             col.names = c("County", "New Cases"))



pop_data2 = right_join(pop, covid, by = "fips") %>%
  filter(date >= max(date) - 13, state == "California")

state_daysdata = pop_data2 %>%
  group_by(state, date) %>%
  summarise(cases = sum(cases)) %>%
  mutate(newCases = cases - lag(cases),
         newCases_percapita = (newCases / 39512223) * 100000) %>%
  ungroup()

safe_county = pop_data2 %>%
  group_by(county, pop2019) %>%
  summarize(max_cases = max(cases), min_cases = min(cases)) %>%
  mutate(newCases = max_cases - min_cases,
         newCases_percapita = (newCases / pop2019) * 100000) %>%
  filter(newCases_percapita <= 100) %>%
  ungroup()

Numofcounty = length(safe_county$county)
Numofnewcases = format(max(state_daysdata$cases) - min(state_daysdata$cases))
Numofcases = format(max(state_daysdata$cases))



As the data showing on above, in the last 14 days, the total number of new cases is `r Numofcases`,  the total number of new cases is `r Numofnewcases`, and the total number of safe county is `r Numofcounty`



covid %>%
  filter(state %in% c("New York", "California", "Louisiana", "Florida")) %>%
  group_by(state, date) %>%
  summarise(cases = max(cases))%>%
  mutate(newCases = cases - lag(cases), roll7 = rollmean(newCases, 7, fill = NA, align = "right")) %>%
  ungroup() %>%
  ggplot(aes(x = date)) +
  geom_col(aes(y = newCases), col = NA, fill = "#F5B8B5") +
  geom_line(aes(y = roll7), col = "darkred", size = 1) +
  theme_gray() +
  labs(title = "New Reported Cases by day in 4 States", x = "Date", y = "Newcases") +
  theme(aspect.ratio = .5) +
  facet_grid(~state, scales = "free_y")

COVID1 = covid%>%
  filter(state %in% c("New York", "California", "Louisiana", "Florida")) %>%
  right_join(pop, by = "fips")


state_pop = pop %>%
  filter(State %in% c("NY", "CA","LA", "FL")) %>%
  group_by(State) %>%
  slice_max(pop2019, n = 1) %>%
  right_join(COVID1, by = "State") %>%
  select(pop = pop2019.x, date, state, cases, deaths) %>%
  ungroup() %>%
  filter(state %in% c("New York", "California", "Louisiana", "Florida")) %>%
  group_by(pop, state,date) %>%
  summarise(cases = max(cases)) %>%
  mutate(newCases1 = cases - lag(cases), newCases_Percapita = newCases1 / pop, Roll7 = rollmean(newCases_Percapita, 7, fill = NA, align = "right")) %>%
  ungroup()

ggplot(state_pop, aes(x = date)) +
  geom_col(aes(y = newCases_Percapita), col = "yellow", fill = "#F5B8B5") +
  geom_line(aes(y = Roll7), col = "blue", size = 1) +
  theme_gray() +
  labs(title = "New Reported Cases Per Capita by day in 4 States ", x = "Date", y = "Newcases") +
  theme(aspect.ratio = .5) +
  facet_grid(~state, scales = "free_y")










With the date of population, the graph is more sensitive to the local condition of the pandemic. Because some states' population is small such as Louisiana, the basic infected number is too small to evaluating and we cannot analyze the changes from the cases itself. Cases per capita solves the problem when we try to monitor local pandemic changes. In addition, cases per capita also can help us observe the control of the infection locally. But sometimes it is biased. The total number of the cases matters when we try to define the disease's pandemic condition in specific area. Analysis of both of them is better way to monitor the pandemic.





local = na.omit(readr::read_csv("data/county-centroids.csv"))  %>%
  select(fips, county = name, state = state_name, LON, LAT)

local_data1 = covid %>%
  group_by(date, county, fips) %>%
  #summarize(totalcases = sum(cases)) %>%
  ungroup() %>%
  left_join(local, by = "fips") %>%
  select(date, county = county.x, fips, cases, LAT, LON) %>%
  mutate(m = format(date, "%m")) %>%
  group_by(m) %>%
  summarise(X = sum(LAT * cases, na.rm = TRUE) / sum(cases, na.rm = TRUE),
            Y = sum(LON * cases, na.rm = TRUE) / sum(cases, na.rm = TRUE),
            totalcase = sum(cases, na.rm =TRUE ))




ggplot(local_data1, aes(x = Y, y = X)) +
  borders("state", fill = "gray90", colour = "white") +
  geom_point(aes(color = m, size = totalcase)) +
  labs(title = "CIVID-19 Pandemic Trending ", x = "Longitude", y = "Latitude")










From the data trending, the outbreak started near west, with few cases, but suddenly transferred to the east with numerous cases in the west from March to May, caused by large population flow in big cities such as New York. With the reinforcement of control in the east area, and the ignorance of the disease in the west, the western cases increased and the weighted mean center slowly moved to the west again from June to August. From August, the total cases started to decrease.

