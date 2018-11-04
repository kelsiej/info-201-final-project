le_by_state$State <- str_to_title(le_by_state$State)

le_state <- le_by_state

#filtering the data frame to contain only state, year, male LE, female LE, and the change in LE for
#both sexes between 1987-2009
le_state <- le_state %>% 
  select(State, Year, Male.life.expectancy..years., Female.life.expectancy..years., 
         Male.life.expectancy.change.1987.to.2009..years., Female.life.expectancy.change.1987.to.2009..years.)

#filtering for all state data in year 1987
le_state_1987 <- le_state %>% 
  select(State, Year, Male.life.expectancy..years., Female.life.expectancy..years.) %>% 
  filter(Year == 1987) %>% 
  select(State, Year, Male.life.expectancy..years., Female.life.expectancy..years.)

#adding another column that contains the average LE
le_state_1987 <- mutate(le_state_1987, avg.life.expectancy = (Male.life.expectancy..years. + 
                                                                Female.life.expectancy..years.) / 2)

#map shows LE for each state in year 1987
map_1987 <- hcmap("countries/us/us-all", data = le_state_1987, value = "avg.life.expectancy",
                  joinBy = c("name", "State"), name = "Life Expectancy (in years)",
                  dataLabels = list(enabled = TRUE, format = "{point.name}"),
                  borderColor = "#FAFAFA", borderWidth = 0.1,
                  tooltip = list(valueDecimals = 2, valueSuffix = " years"))

map_1987

#filtering for all state data in year 2009
le_state_2009 <- le_state %>% 
  select(State, Year, Male.life.expectancy..years., Female.life.expectancy..years.) %>% 
  filter(Year == 2009) %>% 
  select(State, Year, Male.life.expectancy..years., Female.life.expectancy..years.)

#adding another column that contains the average LE
le_state_2009 <- mutate(le_state_2009, avg.life.expectancy = (Male.life.expectancy..years. + 
                                                                Female.life.expectancy..years.) / 2)

#map shows LE for each state in year 2009
map_2009 <- hcmap("countries/us/us-all", data = le_state_2009, value = "avg.life.expectancy",
                  joinBy = c("name", "State"), name = "Life Expectancy (in years)",
                  dataLabels = list(enabled = TRUE, format = "{point.name}"),
                  borderColor = "#FAFAFA", borderWidth = 0.1,
                  tooltip = list(valueDecimals = 2, valueSuffix = " years"))

map_2009

#dataframe contains the trend between 1987-2009


trend <- le_state %>% 
  select(State, Male.life.expectancy.change.1987.to.2009..years., Female.life.expectancy.change.1987.to.2009..years.)
trend <- mutate(trend, avg.life.expectancy = (Male.life.expectancy.change.1987.to.2009..years. +
                                                Female.life.expectancy.change.1987.to.2009..years.) / 2)
trend <- trend %>% select(State, avg.life.expectancy)
trend <- unique(trend)

le_range <- range(trend$avg.life.expectancy)


# medians
median_change_in_le <- median(trend$avg.life.expectancy) #3.45

le_for_median_1987 <- le_state %>% 
  filter(Year == "1987")
median_le_male_1987 <- median(le_for_median_1987$Male.life.expectancy..years.) #71.6
median_le_female_1987 <- median(le_for_median_1987$Female.life.expectancy..years.) # 78.5

le_for_median_2009 <- le_state %>% 
  filter(Year == "2009")
median_le_male_2009 <- median(le_for_median_2009$Male.life.expectancy..years.) #76.3
median_le_female_2009 <- median(le_for_median_2009$Female.life.expectancy..years.) # 81.1


