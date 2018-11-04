source("data_wrangling.R")

uninsured_data <- coverage_by_race %>% 
  filter(`Coverage Type` == "Uninsured", `Data Type` == "Percent")

colnames(uninsured_data) <- c("Fips", "Location", "Coverage Type", "Race/Ethnicity", "Year","Data.Type",       
                              "Data", "MOE"  )


multiply_by_100 <- as.numeric(uninsured_data$Data) * 100

uninsured_data$Data <- multiply_by_100

uninsured_data_national <- uninsured_data %>% 
  filter(Location == "United States")


uninsured_data <- uninsured_data %>% 
  filter(Location != "United States")


# input: race; output: rate of uninsured by race for 2016 nationally

get_uninsured_2016 <- function(race) {
  race_to_string <- paste(race)
  
  rate <- uninsured_data_national %>% 
    filter(`Race/Ethnicity` == race_to_string, Year == "2016") %>% 
    select(Data)
  
  rate <- rate[1,1] #5.701
  rate <- paste(rate, "%", sep = "")
  rate
}

his_uninsured <- get_uninsured_2016("Hispanic / Latino") #17.927
other_uninsured <- get_uninsured_2016("Other / Multiple Races") #9.72
afam_uninsured <- get_uninsured_2016("African-American / Black") # 9.555
asian_uninsured <- get_uninsured_2016("Asian") #6.599
white_uninsured <- get_uninsured_2016("White") # 5.701

# MEDIANS

# input: race; output: median uninsured rate for each race using all states and all available years
get_median <- function(race) {
  race_to_string <- paste(race)
  
  median <- uninsured_data %>%  
    filter(`Race/Ethnicity` == race_to_string) %>% 
    select(Data) %>% 
    na.omit()
  
  median <- median(median$Data)
  median
  
}

asian_median <- get_median("Asian") # 11.207
latio_median <- get_median("Hispanic / Latino") # 22.466
white_median <- get_median("White") # 7.917
black_median <- get_median("African-American / Black") # 12.807
other_median <- get_median("Other / Multiple Races") # ll.906

insurance_year_range <- range(uninsured_data_national$Year)