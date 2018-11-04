income_by_race <- read_xlsx("data/income_by_race.xlsx")
le_national <- read_xlsx("data/life_expectancy_death_rates.xlsx")
le_by_state <- read.csv("data/IHME_US_STATE_LIFE_EXPECTANCY_1987_2009.csv", stringsAsFactors = FALSE)
le_at_birth_race <- read.csv("data/le_at_birth_race.csv", stringsAsFactors = FALSE)
gni_le <- suppressWarnings(read.csv("data/wb_gni.csv", stringsAsFactors = FALSE, na.strings = "..", check.names = FALSE))
# Be aware of Mac to Windows conversion. Work was done on a Mac
coverage_by_race <- read.csv("data/Health Insurance Coverage Type by Race.csv", stringsAsFactors = FALSE, check.names = FALSE)

income_by_race <- as.data.frame(income_by_race)
le_national <- as.data.frame(le_national)

################## 
### Question 1 ###
##################

# plot le over time for overview

overview_le <- le_national %>% 
  filter(Sex == "Both Sexes", Race == "All Races") %>% 
  na.omit()

overview_plot <- ggplot(overview_le) +
  geom_point(mapping = aes(x = Year, y = Avg.Life.Expectancy.Years, color = Avg.Life.Expectancy.Years)) +
  scale_color_gradient(low = "orange", high = "blue") +
  ggtitle("Average Life Expectancy Over Time 1900 to 2014") +
  theme(
    plot.title = element_text(size = 18),
    legend.position = "none") +
  labs(x = "Year", y = "Average Life Expectancy")

le_1900 <- overview_le %>% 
  filter(Year == "1900") %>% 
  select(Avg.Life.Expectancy.Years) # 47.3

le_2014 <- overview_le %>% 
  filter(Year == "2014") %>% 
  select(Avg.Life.Expectancy.Years) # 78.9


overview_slope <- lm(overview_le$Year ~ overview_le$Avg.Life.Expectancy.Years)

overview_slope <- coefficients(overview_slope)

overview_slope <- round(overview_slope[2], digits = 2)

# Find correlation between GNI and LE

gni_le <- gni_le %>%
  select(-`Country Name`, -`Country Code`, -`Series Code`)

gni <- gni_le %>% 
  filter(`Series Name` == "Adjusted net national income per capita (constant 2010 US$)")

le <- gni_le %>% 
  filter(`Series Name` == "Life expectancy at birth, total (years)")

gni_long <- gather(gni, "Year", "GNI Per Capita", `1970`:`2016`)

le_long <- gather(le, "Year", "Life Expectancy", `1970`:`2016`)

gni_by_le <- left_join(gni_long, le_long, by = "Year")

correlation_GNI_le <- cor(gni_by_le$`GNI Per Capita`, gni_by_le$`Life Expectancy`)
correlation_GNI_le <- paste(round(correlation_GNI_le*100,digits=2),"%",sep="")



# Wrangle data for Q1

income_black_white <- income_by_race %>%
  filter(Race %in% c("All Races", "White Alone", "Black Alone")) %>%
  select(Year, Race, median)

le_black_white <- le_national %>%
  filter(Sex == "Both Sexes") %>%
  select(-Age.Adusted.Death.Rate)

income_black_white_wide_median <- spread(income_black_white,
                                         key = "Race",
                                         value = "median" )

colnames(income_black_white_wide_median) <- c("Year", "All Races", "Black", "White")

income_black_white <- gather(income_black_white_wide_median,
                             key = "Race",
                             value = "median_income", "All Races", "Black", "White")

income_by_le <- left_join(income_black_white, le_black_white, by = c("Year", "Race"))
income_by_le


years <- unique(income_by_le$Year)


# Get average median and average le

avg_median_income_black <- sum(income_black_white_wide_median$Black) / nrow(income_black_white_wide_median)
avg_median_income_black_string <- paste("$", round(avg_median_income_black, digits = 2), sep = "")

avg_median_income_white <- sum(income_black_white_wide_median$White) / nrow(income_black_white_wide_median)
avg_median_income_white_string <- paste("$", round(avg_median_income_white, digits = 2), sep = "")

avg_median_income_all <- sum(income_black_white_wide_median$`All Races`) / nrow(income_black_white_wide_median)
avg_median_income_all_string <- paste("$", round(avg_median_income_all, digits = 2), sep = "")


le_black <- income_by_le %>% 
  filter(Race == "Black") %>% 
  select(Avg.Life.Expectancy.Years) 

avg_le_black <- sum(le_black, na.rm = TRUE) / (nrow(le_black) - 2)
avg_le_black <- round(avg_le_black, digits = 2)

le_white <- income_by_le %>% 
  filter(Race == "White") %>% 
  select(Avg.Life.Expectancy.Years) 

avg_le_white <- sum(le_white, na.rm = TRUE) / (nrow(le_white) - 2)
avg_le_white <- round(avg_le_white, digits = 2)


le_all <- income_by_le %>% 
  filter(Race == "All Races") %>% 
  select(Avg.Life.Expectancy.Years) 

avg_le_all <- sum(le_all, na.rm = TRUE) / (nrow(le_all) - 2)
avg_le_all <- round(avg_le_all, digits = 2)




average_medians <- c(avg_median_income_black, avg_median_income_white, avg_median_income_all)
average_les <- c(avg_le_black, avg_le_white, avg_le_all)

averages_df <- cbind(average_medians, average_les)
colnames(averages_df) <- c("Average Median Income", "Average Life Expectancy")

correlation_income_le_race <- cor(average_medians, average_les) #0.9985999

################## 
### Question 2 ###
##################
#find life expectancy per state for African Americans

le_at_birth_race <- le_at_birth_race %>%
  mutate(Location = tolower(Location))
le_at_birth_race <- rename(le_at_birth_race, region = Location)

state_data <- map_data('state')
new_data <- left_join(state_data, le_at_birth_race)

new_data$African.American[new_data$African.American %in% "NSD"] <- "0"
new_data[new_data == 0] <- NA
new_data$African.American <- as.numeric(new_data$African.American)

new_data$Asian.American[new_data$Asian.American %in% "NSD"] <- "0"
new_data[new_data == 0] <- NA
new_data$Asian.American <- as.numeric(new_data$Asian.American)

new_data$Latino[new_data$Latino %in% "NSD"] <- "0"
new_data[new_data == 0] <- NA
new_data$Latino <- as.numeric(new_data$Latino)

new_data$Native.American[new_data$Native.American %in% "NSD"] <- "0"
new_data[new_data == 0] <- NA
new_data$Native.American <- as.numeric(new_data$Native.American)

# plot 2b
le_at_birth_race_long <- le_at_birth_race %>% 
  filter(region != "United States") %>% 
  gather(key = "Race",
         value = "Life_Expectancy", "White", "African.American", "Latino", "Asian.American",  
         "Native.American")

locations <- unique(le_at_birth_race_long$region)