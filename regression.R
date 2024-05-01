library(tidyverse)
library(lubridate)
library(car)
library(lmtest)
options(scipen=999)

################################################################
# Manipulating demographic data
data <- read_csv("MiLB_Project_Spreadsheet.xlsx - MiLB_Project_Spreadsheet.csv")

teams_cities <- read_csv("milb_teams_list.csv")

city_pop <- read_csv("cities_population_2022.csv") %>%
  mutate(
    City = str_to_lower(City),
    City = str_replace(City, " city", ""),
    City = str_replace_all(City, " town", "")
  )

city_counties <- read_csv("cities_counties.csv") %>%
  mutate(city_state = str_to_lower(str_c(city_ascii, ", ", state_name))) %>%
  select(city_state, county_name)

county_info <- read_csv("counties_age_gender_population_2022.csv") %>%
  select(
    county = `Geographic Area Name`, 
    population = `Estimate!!Total!!Total population`,
    median_age = `Estimate!!Total!!Total population!!SUMMARY INDICATORS!!Median age (years)`,
    males_per_100_females = `Estimate!!Total!!Total population!!SUMMARY INDICATORS!!Sex ratio (males per 100 females)`
  ) %>%
  mutate(county = str_to_lower(county))

median_incomes <- read_csv("counties_income_2022.csv") %>%
  mutate(county_state = str_to_lower(str_c(county, ", ", State))) %>%
  select(county_state, median_income)

win_percentages <- read_csv("win_loss.csv")

data2 <- data %>%
  mutate(date_team = str_c(Date, Home_Team)) %>%
  mutate(month = month(mdy(Date)), Attendence = as.numeric(Attendence)) %>%
  mutate_at(c("Awareness/Charity", "Appreciation", "Bobblehead/Statue Giveaway", "Shirt Giveaway", "Other Giveaway", "On-Field/Player Experiences", "Concession Deals", "Ticket Deals", "Merchandise Discounts", "Theme Days/Nights", "Fireworks", "Misc."), ~if_else(is.na(.), 0, .)) %>%
  left_join(teams_cities, join_by(Home_Team == Team)) %>%
  mutate(city_state = str_to_lower(str_c(City, ", ", State))) %>%
  left_join(city_pop, join_by(city_state == City)) %>%
  mutate(
    Pop = case_when(
      city_state == "brooklyn, new york" ~ 2590516,
      city_state == "lakewood, new jersey" ~ 139506,
      city_state == "vancouver, british columbia" ~ 2632000,
      city_state == "comstock park, michigan" ~ 10397,
      city_state == "bridgewater, new jersey" ~ 46318,
      city_state == "kodak, tennessee" ~ 10051,
      .default = Pop
    )
  ) %>%
  left_join(city_counties) %>%
  mutate(
    county_name = case_when(
      city_state == "lakewood, new jersey" ~ "ocean",
      city_state == "vancouver, british columbia" ~ "lower mainland",
      city_state == "bridgewater, new jersey" ~ "somerset",
      city_state == "kodak, tennessee" ~ "sevier",
      city_state == "salem, virginia" ~ "roanoke",
      .default = county_name
    ),
    county_state = str_to_lower(str_c(county_name, " County, ", State))
  ) %>%
  left_join(county_info, join_by(county_state == county)) %>%
  rename(city_pop = Pop, county_pop = population) %>% 
  mutate(
    county_pop = case_when(
      county_state == "lenoir county, north carolina" ~ 54633,
      county_state == "fredericksburg county, virginia" ~ 28757,
      county_state == "lower mainland county, british columbia" ~ 2632000,
      .default = county_pop
    ),
    median_age = case_when(
      county_state == "lenoir county, north carolina" ~ 42.9,
      county_state == "fredericksburg county, virginia" ~ 30.7,
      county_state == "lower mainland county, british columbia" ~ 39.9,
      .default = median_age
    ),
    males_per_100_females = case_when(
      county_state == "lenoir county, north carolina" ~ 91.2,
      county_state == "fredericksburg county, virginia" ~ 87.6,
      county_state == "lower mainland county, british columbia" ~ 96.6,
      .default = males_per_100_females
    )
  ) %>%
  left_join(median_incomes) %>%
  mutate(median_income = if_else(
    city_state == "vancouver, british columbia", 
    60775, 
    median_income
  )) %>%
  left_join(win_percentages) %>%
  distinct(date_team, .keep_all = TRUE) %>%
  select(-c(date_team, city_state, county_state, Home_Score, Away_Score)) %>%
  filter(!is.na(Attendence), !is.na(Home_WP), !is.na(Away_WP)) %>%
  rename(attendance = Attendence) %>%
  mutate(county_city_pop_ratio = if_else(county_pop < city_pop, 1, county_pop / city_pop))

write_csv(data2, "data_new.csv")

#########################################################################
# Models and some graphs

final_data <- read_csv("MiLB_Final_Data.csv")
final_data2 <- final_data %>%
  mutate(
    rain = as.numeric(other_weather %in% c(
      "Drizzle",
      "Rain",
      "Snow"
    )),
    wind_speed = as.numeric(str_extract(wind, "\\d+")),
    start_time = hms(start_time),
    time_of_day = as.factor(case_when(
      start_time$hour < 12 ~ "Morning",
      start_time$hour < 17 ~ "Afternoon",
      TRUE ~ "Evening"
    )),
    Day.of.Week.2 = as.factor(if_else(
      Day.of.Week %in% c("Mon", "Tue", "Wed", "Thu"), 
      "Weekday", 
      Day.of.Week
    )),
    num_promos = Awareness.Charity + Appreciation + Bobblehead.Statue.Giveaway + Shirt.Giveaway + Other.Giveaway + On.Field.Player.Experiences + Concession.Deals + Ticket.Deals + Merchandise.Discounts + Theme.Days.Nights + Fireworks + Misc.
  )

final_data2 %>%
  group_by(time_of_day, Level) %>%
  summarise(
    count = n(),
    att = mean(attendance, na.rm = TRUE)
  )

final_data2$Day.of.Week.2 <- relevel(final_data2$Day.of.Week.2, ref = "Weekday")
final_data2$Level <- relevel(as.factor(final_data2$Level), ref = "Single-A")
final_data2$time_of_day <- relevel(final_data2$time_of_day, ref = "Morning")

# Attendance
ggplot(final_data2, aes(x = attendance)) +
  geom_histogram(alpha = 0.5, fill = "black", color = "black", position = "identity", bins = 30) +
  theme_bw() +
  labs(title = "Distribution of Attendance in Minor League Games in 2023", x = "Attendance", y = "Number of Games") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold")
  )

# Attendance by Level
ggplot(final_data2, aes(x = attendance, y = ..density.., fill = Level)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
  geom_density(alpha = 0.5, adjust = 1) +
  theme_bw() +
  labs(title = "Density Plot for Attendance in MiLB Games", x = "Attendance", y = "Density") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold")
  )


######################
# Income

# Analyzing city/class brackets

class_brackets <- final_data2 %>%
  select(Home_Team, Level, 26:31) %>%
  distinct()

summary(class_brackets$median_income)

# Overall Distribution
ggplot(class_brackets, aes(x = median_income)) +
  geom_histogram(alpha = 0.5, fill = "black", color = "black", position = "identity", bins = 30) +
  geom_vline(xintercept = 75000, linetype = "dashed", color = "black") +
  geom_text(aes(x = 75000, y = 16, label = "US Median Household Income"), color = "black", angle = 90, vjust = -0.5, size = 3.5) +
  geom_vline(xintercept = 69825, linetype = "dashed", color = "blue") +
  geom_text(aes(x = 69825, y = 16.5, label = "Sample Median Household Income"), color = "blue", angle = 90, vjust = -0.5, size = 3.5) +
  theme_bw() +
  labs(title = "Distribution Sample Income", x = "Median Household Income", y = "Number of Teams") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold")
  )

# Histogram by Level
ggplot(class_brackets, aes(x = median_income, fill = Level)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
  #geom_density(alpha = 0.5, adjust = 1) +
  theme_bw() +
  labs(title = "Distribution of Income by Level", x = "Median Household Income", y = "Number of Teams") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold")
  )

# Density Plot by level
ggplot(class_brackets) +
  geom_histogram(aes(x = median_income, y = ..density..), fill = "white", color = "black", alpha = 0.5, position = "identity", bins = 30) +
  geom_density(aes(x = median_income, fill = Level), alpha = 0.5) +
  theme_bw() +
  labs(title = "Distribution of Income by Level", x = "Median Household Income", y = "Density") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold")
  )

# Creating Income groups

final_data2 <- final_data2 %>%
  mutate(income_bracket = 
           case_when(median_income <= 60000 ~ "Low Income",
                     median_income <= 70000 & median_income > 60000 ~ "Lower-Middle Income",
                     median_income <= 80000 & median_income > 70000 ~ "Upper-Middle Income",
                     median_income > 80000 ~ "High Income"
           ))
#######################
# Population

summary(class_brackets$county_pop)

ggplot(class_brackets, aes(x = county_pop)) +
  geom_histogram(alpha = 0.5, fill = "black", color = "black", position = "identity", bins = 30) +
  scale_x_continuous(labels = scales::comma) +  # To display labels without scientific notation
  theme_bw() +
  labs(title = "Distribution Sample Population", x = "County Population", y = "Number of Teams") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold")
  )

# Histogram by Level
ggplot(class_brackets, aes(x = county_pop, fill = Level)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 25) +
  #geom_density(alpha = 0.5, adjust = 1) +
  theme_bw() +
  labs(title = "Distribution of Population by Level", x = "County Population", y = "Number of Teams") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold")
  )

# Density Plot by level
ggplot(class_brackets) +
  geom_histogram(aes(x = county_pop, y = ..density..), fill = "white", color = "black", alpha = 0.5, position = "identity", bins = 25) +
  geom_density(aes(x = county_pop, fill = Level), alpha = 0.5) +
  theme_bw() +
  labs(title = "Density Distribution of Population by Level", x = "County Population", y = "Density") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold")
  )

# Creating Population Groups
final_data2 <- final_data2 %>%
  mutate(city_size = 
           case_when(county_pop <= 200000 ~ "Small",
                     county_pop <= 500000 & county_pop > 200000 ~ "Below-Average",
                     county_pop <= 800000 & county_pop > 500000 ~ "Above-Average",
                     county_pop > 800000 ~ "Large"
           ))

# final_data2 <- final_data2 %>%
#   mutate(month = case_when(
#     month == 3 ~ "March",
#     month == 4 ~ "April",
#     month == 5 ~ "May",
#     month == 6 ~ "June",
#     month == 7 ~ "July",
#     month == 8 ~ "August",
#     month == 9 ~ "September",
#     month == 10 ~ "October",
#     TRUE ~ NA_character_
#   ))

# Convert city_size and income_bracket to factors
final_data2$city_size <- factor(final_data2$city_size)
final_data2$income_bracket <- factor(final_data2$income_bracket)

# Relevel the city_size and income_bracket variables
final_data2$city_size <- relevel(final_data2$city_size, ref = "Small")
final_data2$income_bracket <- relevel(final_data2$income_bracket, ref = "Low Income")

##############################
# Model 1 - Base
colnames(final_data2)

mod1 <- lm(attendance ~ 
              as.factor(Day.of.Week.2) + 
              as.factor(Level) + 
              Awareness.Charity + 
              Appreciation + 
              Bobblehead.Statue.Giveaway + 
              Shirt.Giveaway + 
              Other.Giveaway + 
              On.Field.Player.Experiences + 
              Concession.Deals + 
              Ticket.Deals + 
              Merchandise.Discounts + 
              Theme.Days.Nights + 
              Fireworks + 
              Misc. + 
              month + 
              Capacity + 
              county_pop + 
              median_age + 
              males_per_100_females + 
              median_income + 
              Home_WP + 
              Away_WP + 
              temperature + 
              wind_speed + 
              rain + 
              as.factor(time_of_day),
              data = final_data2)
summary(mod1)
vif(mod1)
bptest(mod1) # Heteroskedasticity problem
dwtest(mod1, alternative = "two.sided")

# mod2, take squrt of attendance

mod2 <- lm(sqrt(attendance) ~ 
             as.factor(Day.of.Week.2) + 
             as.factor(Level) + 
             Awareness.Charity + 
             Appreciation + 
             Bobblehead.Statue.Giveaway + 
             Shirt.Giveaway + 
             Other.Giveaway + 
             On.Field.Player.Experiences + 
             Concession.Deals + 
             Ticket.Deals + 
             Merchandise.Discounts + 
             Theme.Days.Nights + 
             Fireworks + 
             Misc. + 
             month +
             Capacity +
             as.factor(city_size) + 
             median_age + 
             males_per_100_females + 
             as.factor(income_bracket) + 
             Home_WP + 
             Away_WP + 
             temperature + 
             wind_speed + 
             rain + 
             as.factor(time_of_day),
           data = final_data2)
summary(mod2)
vif(mod2)
bptest(mod2)
dwtest(mod2, alternative = "two.sided")

#Plot the squared residuals and the fitted values 
Residuals <- mod2$residuals
Fitted <- mod2$fitted.values
table <- data.frame(Fitted, 
                    Residuals)

ggplot(table, 
       aes(x = Fitted, 
           y = Residuals ^ 2)) +
  geom_point(shape = 18, 
             color="darkorange") +
  geom_smooth(method = lm, 
              color="black", 
              se = FALSE) +
  labs(title = "Heteroskedasticity Check for MiLB Attendance Model", 
       x = "Fitted Attendance", 
       y = "Squared Residuals") +
  theme_classic()

# Looks much better, BP still doesnt like it but I think its the best we will get

# mod 3, Danny model w Changes

mod3 <- lm(sqrt(attendance) ~ 
              as.factor(Day.of.Week.2) + 
              as.factor(Level) + 
              Awareness.Charity + 
              Appreciation + 
              Bobblehead.Statue.Giveaway + 
              Shirt.Giveaway + 
              Other.Giveaway + 
              On.Field.Player.Experiences + 
              Concession.Deals + 
              Ticket.Deals + 
              Merchandise.Discounts + 
              Theme.Days.Nights + 
              Fireworks + 
              Misc. + 
              month + 
              I(month^2) + 
              Capacity + 
              as.factor(city_size) + 
              median_age + 
              males_per_100_females + 
              as.factor(income_bracket) + 
              Home_WP + 
              Away_WP + 
              temperature + 
              I(temperature^2) + 
              wind_speed +
              rain + 
              as.factor(time_of_day), 
              data = final_data2)
summary(mod3)


# Interacting Level & Each Group

mod4 <- lm(sqrt(attendance) ~ 
             as.factor(Day.of.Week.2) + 
             as.factor(Level) + 
             Awareness.Charity + 
             Appreciation + 
             Bobblehead.Statue.Giveaway + 
             Shirt.Giveaway + 
             Other.Giveaway + 
             On.Field.Player.Experiences + 
             Concession.Deals + 
             Ticket.Deals + 
             Merchandise.Discounts + 
             Theme.Days.Nights + 
             Fireworks + 
             Misc. + 
             month + 
             I(month^2) + 
             Capacity + 
             as.factor(city_size) + 
             median_age + 
             males_per_100_females + 
             as.factor(income_bracket) + 
             Home_WP + 
             Away_WP + 
             temperature + 
             I(temperature^2) + 
             wind_speed +
             rain + 
             as.factor(time_of_day) +
             as.factor(Level)*Awareness.Charity + 
             as.factor(Level)*Appreciation + 
             as.factor(Level)*Bobblehead.Statue.Giveaway + 
             as.factor(Level)*Shirt.Giveaway + 
             as.factor(Level)*Other.Giveaway + 
             as.factor(Level)*On.Field.Player.Experiences + 
             as.factor(Level)*Concession.Deals + 
             as.factor(Level)*Ticket.Deals + 
             as.factor(Level)*Merchandise.Discounts + 
             as.factor(Level)*Theme.Days.Nights + 
             as.factor(Level)*Fireworks +
             as.factor(Level)*Misc., 
           data = final_data2)
summary(mod4)

# 3 level interactions with day of week, level, & groups
mod5 <- lm(sqrt(attendance) ~ 
             as.factor(Day.of.Week.2) + 
             as.factor(Level) + 
             Awareness.Charity + 
             Appreciation + 
             Bobblehead.Statue.Giveaway + 
             Shirt.Giveaway + 
             Other.Giveaway + 
             On.Field.Player.Experiences + 
             Concession.Deals + 
             Ticket.Deals + 
             Merchandise.Discounts + 
             Theme.Days.Nights + 
             Fireworks + 
             Misc. + 
             month + 
             I(month^2) + 
             Capacity + 
             as.factor(city_size) + 
             median_age + 
             males_per_100_females + 
             as.factor(income_bracket) + 
             Home_WP + 
             Away_WP + 
             temperature + 
             I(temperature^2) + 
             wind_speed +
             rain + 
             as.factor(time_of_day) +
             as.factor(Day.of.Week.2)*as.factor(Level)*Awareness.Charity + 
             as.factor(Day.of.Week.2)*as.factor(Level)*Appreciation + 
             as.factor(Day.of.Week.2)*as.factor(Level)*Bobblehead.Statue.Giveaway + 
             as.factor(Day.of.Week.2)*as.factor(Level)*Shirt.Giveaway + 
             as.factor(Day.of.Week.2)*as.factor(Level)*Other.Giveaway + 
             as.factor(Day.of.Week.2)*as.factor(Level)*On.Field.Player.Experiences + 
             as.factor(Day.of.Week.2)*as.factor(Level)*Concession.Deals + 
             as.factor(Day.of.Week.2)*as.factor(Level)*Ticket.Deals + 
             as.factor(Day.of.Week.2)*as.factor(Level)*Merchandise.Discounts + 
             as.factor(Day.of.Week.2)*as.factor(Level)*Theme.Days.Nights + 
             as.factor(Day.of.Week.2)*as.factor(Level)*Fireworks +
             as.factor(Day.of.Week.2)*as.factor(Level)*Misc., 
           data = final_data2)
summary(mod5)

# 4th level interactions with day of week, level, month, & groups
mod6 <- lm(sqrt(attendance) ~ 
             as.factor(Day.of.Week.2) + 
             as.factor(Level) + 
             Awareness.Charity + 
             Appreciation + 
             Bobblehead.Statue.Giveaway + 
             Shirt.Giveaway + 
             Other.Giveaway + 
             On.Field.Player.Experiences + 
             Concession.Deals + 
             Ticket.Deals + 
             Merchandise.Discounts + 
             Theme.Days.Nights + 
             Fireworks + 
             Misc. + 
             month + 
             I(month^2) + 
             Capacity + 
             as.factor(city_size) + 
             median_age + 
             males_per_100_females + 
             as.factor(income_bracket) + 
             Home_WP + 
             Away_WP + 
             temperature + 
             I(temperature^2) + 
             wind_speed +
             rain + 
             as.factor(time_of_day) +
             month*as.factor(Day.of.Week.2)*as.factor(Level)*Awareness.Charity + 
             month*as.factor(Day.of.Week.2)*as.factor(Level)*Appreciation + 
             month*as.factor(Day.of.Week.2)*as.factor(Level)*Bobblehead.Statue.Giveaway + 
             month*as.factor(Day.of.Week.2)*as.factor(Level)*Shirt.Giveaway + 
             month*as.factor(Day.of.Week.2)*as.factor(Level)*Other.Giveaway + 
             month*as.factor(Day.of.Week.2)*as.factor(Level)*On.Field.Player.Experiences + 
             month*as.factor(Day.of.Week.2)*as.factor(Level)*Concession.Deals + 
             month*as.factor(Day.of.Week.2)*as.factor(Level)*Ticket.Deals + 
             month*as.factor(Day.of.Week.2)*as.factor(Level)*Merchandise.Discounts + 
             month*as.factor(Day.of.Week.2)*as.factor(Level)*Theme.Days.Nights + 
             month*as.factor(Day.of.Week.2)*as.factor(Level)*Fireworks, 
           data = final_data2)
summary(mod6)

# Adding an interaction between income and each group to the triple interaction
mod7 <- lm(sqrt(attendance) ~ 
             as.factor(Day.of.Week.2) + 
             as.factor(Level) + 
             Awareness.Charity + 
             Appreciation + 
             Bobblehead.Statue.Giveaway + 
             Shirt.Giveaway + 
             Other.Giveaway + 
             On.Field.Player.Experiences + 
             Concession.Deals + 
             Ticket.Deals + 
             Merchandise.Discounts + 
             Theme.Days.Nights + 
             Fireworks + 
             Misc. + 
             month + 
             I(month^2) + 
             Capacity + 
             as.factor(city_size) + 
             median_age + 
             males_per_100_females + 
             as.factor(income_bracket) + 
             Home_WP + 
             Away_WP + 
             temperature + 
             I(temperature^2) + 
             wind_speed +
             rain + 
             as.factor(time_of_day) +
             as.factor(income_bracket)*Awareness.Charity + 
             as.factor(income_bracket)*Appreciation + 
             as.factor(income_bracket)*Bobblehead.Statue.Giveaway + 
             as.factor(income_bracket)*Shirt.Giveaway + 
             as.factor(income_bracket)*Other.Giveaway + 
             as.factor(income_bracket)*On.Field.Player.Experiences + 
             as.factor(income_bracket)*Concession.Deals + 
             as.factor(income_bracket)*Ticket.Deals + 
             as.factor(income_bracket)*Merchandise.Discounts + 
             as.factor(income_bracket)*Theme.Days.Nights + 
             as.factor(income_bracket)*Fireworks + 
             as.factor(income_bracket)*Misc. +
             as.factor(Day.of.Week.2)*as.factor(Level)*Awareness.Charity + 
             as.factor(Day.of.Week.2)*as.factor(Level)*Appreciation + 
             as.factor(Day.of.Week.2)*as.factor(Level)*Bobblehead.Statue.Giveaway + 
             as.factor(Day.of.Week.2)*as.factor(Level)*Shirt.Giveaway + 
             as.factor(Day.of.Week.2)*as.factor(Level)*Other.Giveaway + 
             as.factor(Day.of.Week.2)*as.factor(Level)*On.Field.Player.Experiences + 
             as.factor(Day.of.Week.2)*as.factor(Level)*Concession.Deals + 
             as.factor(Day.of.Week.2)*as.factor(Level)*Ticket.Deals + 
             as.factor(Day.of.Week.2)*as.factor(Level)*Merchandise.Discounts + 
             as.factor(Day.of.Week.2)*as.factor(Level)*Theme.Days.Nights + 
             as.factor(Day.of.Week.2)*as.factor(Level)*Fireworks +
             as.factor(Day.of.Week.2)*as.factor(Level)*Misc., 
             data = final_data2)
summary(mod7)

# Doing the same as mod7 with city size
mod8<- lm(sqrt(attendance) ~ 
             as.factor(Day.of.Week.2) + 
             as.factor(Level) + 
             Awareness.Charity + 
             Appreciation + 
             Bobblehead.Statue.Giveaway + 
             Shirt.Giveaway + 
             Other.Giveaway + 
             On.Field.Player.Experiences + 
             Concession.Deals + 
             Ticket.Deals + 
             Merchandise.Discounts + 
             Theme.Days.Nights + 
             Fireworks + 
             Misc. + 
             month + 
             I(month^2) + 
             Capacity + 
             as.factor(city_size) + 
             median_age + 
             males_per_100_females + 
             as.factor(income_bracket) + 
             Home_WP + 
             Away_WP + 
             temperature + 
             I(temperature^2) + 
             wind_speed +
             rain + 
             as.factor(time_of_day) +
             as.factor(income_bracket)*Awareness.Charity + 
             as.factor(income_bracket)*Appreciation + 
             as.factor(income_bracket)*Bobblehead.Statue.Giveaway + 
             as.factor(income_bracket)*Shirt.Giveaway + 
             as.factor(income_bracket)*Other.Giveaway + 
             as.factor(income_bracket)*On.Field.Player.Experiences + 
             as.factor(income_bracket)*Concession.Deals + 
             as.factor(income_bracket)*Ticket.Deals + 
             as.factor(income_bracket)*Merchandise.Discounts + 
             as.factor(income_bracket)*Theme.Days.Nights + 
             as.factor(income_bracket)*Fireworks + 
             as.factor(income_bracket)*Misc. +
             as.factor(city_size)*Awareness.Charity + 
             as.factor(city_size)*Appreciation + 
             as.factor(city_size)*Bobblehead.Statue.Giveaway + 
             as.factor(city_size)*Shirt.Giveaway + 
             as.factor(city_size)*Other.Giveaway + 
             as.factor(city_size)*On.Field.Player.Experiences + 
             as.factor(city_size)*Concession.Deals + 
             as.factor(city_size)*Ticket.Deals + 
             as.factor(city_size)*Merchandise.Discounts + 
             as.factor(city_size)*Theme.Days.Nights + 
             as.factor(city_size)*Fireworks + 
             as.factor(city_size)*Misc. +
             as.factor(Day.of.Week.2)*as.factor(Level)*Awareness.Charity + 
             as.factor(Day.of.Week.2)*as.factor(Level)*Appreciation + 
             as.factor(Day.of.Week.2)*as.factor(Level)*Bobblehead.Statue.Giveaway + 
             as.factor(Day.of.Week.2)*as.factor(Level)*Shirt.Giveaway + 
             as.factor(Day.of.Week.2)*as.factor(Level)*Other.Giveaway + 
             as.factor(Day.of.Week.2)*as.factor(Level)*On.Field.Player.Experiences + 
             as.factor(Day.of.Week.2)*as.factor(Level)*Concession.Deals + 
             as.factor(Day.of.Week.2)*as.factor(Level)*Ticket.Deals + 
             as.factor(Day.of.Week.2)*as.factor(Level)*Merchandise.Discounts + 
             as.factor(Day.of.Week.2)*as.factor(Level)*Theme.Days.Nights + 
             as.factor(Day.of.Week.2)*as.factor(Level)*Fireworks +
             as.factor(Day.of.Week.2)*as.factor(Level)*Misc., 
           data = final_data2)
summary(mod8)

####################
# Model Comparison
model_comparison <- data.frame(
  Model = c("mod1", "mod2", "mod3", "mod4", "mod5", "mod6", "mod7", "mod8"),
  num_coef = numeric(8),
  R_squared = numeric(8),
  Adjusted_R_squared = numeric(8),
  AIC = numeric(8),
  BIC = numeric(8)
)

# Extract model statistics and store them in the data frame
for (i in 1:8) {
  summary_i <- summary(get(paste0("mod", i)))
  model_comparison[i, "num_coef"] <- length(summary_i$coefficients[, "Estimate"])
  model_comparison[i, "R_squared"] <- summary_i$r.squared
  model_comparison[i, "Adjusted_R_squared"] <- summary_i$adj.r.squared
  model_comparison[i, "AIC"] <- AIC(get(paste0("mod", i)))
  model_comparison[i, "BIC"] <- BIC(get(paste0("mod", i)))
}

# Print the model comparison data frame
print(model_comparison)

####################
# Post Modeling Manipulation

# Model Summary Table
summary_mod <- summary(mod8)

coefficients <- coef(summary_mod)
p_values <- coefficients[, "Pr(>|t|)"]
variable_names <- rownames(coefficients)

rounded_coefficients <- round(coefficients[, "Estimate"], 3)
rounded_p_values <- round(p_values, 3)

# Create a data frame to store the coefficients, p-values, and variable names
final_model <- data.frame(
  variable = variable_names,
  coefficient = rounded_coefficients,
  p_value = rounded_p_values,
  stringsAsFactors = FALSE
)

# Reset row names to numeric values
rownames(final_model) <- 1:nrow(final_model)

# Split the interaction column by ":"
interaction_split <- strsplit(final_model$variable, ":")

# Initialize new columns int_1 and int_2
final_model$int_1 <- NA
final_model$int_2 <- NA

# Iterate over each interaction
for (i in seq_along(interaction_split)) {
  # Extract the coefficients before each colon
  coefficients <- unlist(interaction_split[i])
  
  # Assign coefficients to int_1 and int_2 based on the number of variables
  if (length(coefficients) == 3) {
    var1 <- paste0(coefficients[2], ":", coefficients[3])
    var2 <- coefficients[3]
    
    # Find the index where var1 and var2 match final_model$variable
    index_var1 <- which(final_model$variable == var1)
    index_var2 <- which(final_model$variable == var2)
    
    # Assign values to int_1 and int_2
    final_model$int_1[i] <- ifelse(length(index_var2) > 0, final_model$coefficient[index_var2], NA)
    final_model$int_2[i] <- ifelse(length(index_var1) > 0, final_model$coefficient[index_var1], NA)
  } else if (length(coefficients) == 2) {
    # Find the index where var2 matches final_model$variable
    index_var2 <- which(final_model$variable == coefficients[2])
    
    # Assign value to int_1
    final_model$int_1[i] <- ifelse(length(index_var2) > 0, final_model$coefficient[index_var2], NA)
  }
}

final_model$marginal_effects <- final_model$coefficient +
  ifelse(is.na(final_model$int_1), 0, final_model$int_1) +
  ifelse(is.na(final_model$int_2), 0, final_model$int_2)

final_model <- final_model %>%
  mutate(promo = case_when(
    str_detect(variable, "Awareness.Charity") ~ "Awareness/Charity",
    str_detect(variable, "Appreciation") ~ "Appreciation",
    str_detect(variable, "Bobblehead.Statue.Giveaway") ~ "Bobblehead Giveaway",
    str_detect(variable, "Shirt.Giveaway") ~ "Shirt Giveaway",
    str_detect(variable, "Other.Giveaway") ~ "Other Giveaway",
    str_detect(variable, "On.Field.Player.Experiences") ~ "On-Field Experiences",
    str_detect(variable, "Concession.Deals") ~ "Concession Deals",
    str_detect(variable, "Ticket.Deals") ~ "Ticket Deals",
    str_detect(variable, "Merchandise.Discounts") ~ "Merchandise Discounts",
    str_detect(variable, "Theme.Days.Nights") ~ "Theme",
    str_detect(variable, "Fireworks") ~ "Fireworks",
    str_detect(variable, "Misc.") ~ "Misc.",
    TRUE ~ NA_character_
  ),
  level = case_when(
    str_detect(variable, "Triple-A") ~ "Triple-A",
    str_detect(variable, "Double-A") ~ "Double-A",
    str_detect(variable, "High-A") ~ "High-A",
    TRUE ~ NA_character_
  ),
  DOW = case_when(
    str_detect(variable, "Sun") ~ "Sunday",
    str_detect(variable, "Sat") ~ "Saturday",
    str_detect(variable, "Fri") ~ "Friday",
    TRUE ~ NA_character_
  ),
  Income = case_when(
    str_detect(variable, "High Income") ~ "High",
    str_detect(variable, "Upper-Middle Income") ~ "Upper-Middle",
    str_detect(variable, "Lower-Middle Income") ~ "Lower-Middle",
    str_detect(variable, "Low Income") ~ "Low",
    TRUE ~ NA_character_
  ),
  City_Size = case_when(
    str_detect(variable, "Above-Average") ~ "Above-Average",
    str_detect(variable, "Below-Average") ~ "Below-Average",
    str_detect(variable, "Large") ~ "Large",
    TRUE ~ NA_character_
  ))

mod_1D <- final_model %>%
  filter(is.na(promo) & is.na(level) & is.na(DOW) & is.na(Income) & is.na(City_Size))

# Define the order of variables
variable_order <- c("wind_speed", "temperature", "I(temperature^2)", "rain", "month", 
                    "I(month^2)", "males_per_100_females", "as.factor(time_of_day)Evening", 
                    "as.factor(time_of_day)Afternoon", "median_age", "Capacity", "Home_WP", "Away_WP", "(Intercept)")

# Reorder the levels of the variable factor
mod_1D$variable <- factor(mod_1D$variable, levels = variable_order)

ggplot(mod_1D, aes(y = variable, x = marginal_effects, fill = factor(Is_Significant))) +
  geom_col() +
  scale_fill_manual(values = c("1" = "#00bfc4", "0" = "#f8766d")) + 
  theme_bw() +
  labs(title = "Marginal Effects of Non-Interacted Variables",
       fill = "Significance") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold")) +
  scale_y_discrete(labels = c("wind_speed" = "Wind Speed",
                              "temperature" = "Temperature",
                              "I(temperature^2)" = "Quadratic Temperature",
                              "rain" = "Rain",
                              "month" = "Month",
                              "I(month^2)" = "Quadratic Month",
                              "males_per_100_females" = "Female to Male Ratio",
                              "as.factor(time_of_day)Evening" = "Evening Start",
                              "as.factor(time_of_day)Afternoon" = "Afternoon Start",
                              "median_age" = "Median Age"
                              ))

income <- final_model %>%
  filter(!is.na(promo) & !is.na(Income))

ggplot(income, aes(y = marginal_effects, x = Income, fill = factor(Is_Significant))) +
  geom_col() +
  scale_fill_manual(values = c("1" = "#00bfc4", "0" = "#f8766d")) + 
  theme_bw() +
  labs(title = "Marginal Effects of Promotions on City Mean Income Brackets",
       fill = "Significance") +
  facet_grid(cols = vars(promo)) +
  theme(axis.text.x = element_text(size = 14, angle = 90),
        axis.title = element_text(size = 16, face = "bold"),
        plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
        strip.text = element_text(size = 13.75))

City <- final_model %>%
  filter(!is.na(promo) & !is.na(City_Size))

ggplot(City, aes(y = marginal_effects, x = City_Size, fill = factor(Is_Significant))) +
  geom_col() +
  scale_fill_manual(values = c("1" = "#00bfc4", "0" = "#f8766d")) + 
  theme_bw() +
  labs(title = "Marginal Effects of Promotions on City Mean Size Brackets",
       fill = "Significance") +
  facet_grid(cols = vars(promo)) +
  theme(axis.text.x = element_text(size = 14, angle = 90),
        axis.title = element_text(size = 16, face = "bold"),
        plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
        strip.text = element_text(size = 13.75))

mod_3D <- final_model %>%
  filter(!is.na(promo) & !is.na(level) & !is.na(DOW))

# Get unique promo values
unique_promos <- unique(mod_3D$promo)

# Create and save plots for each unique promo
for (promo_value in unique_promos) {
  # Filter data for the current promo value
  promo_data <- subset(mod_3D, promo == promo_value)
  
  # Create plot
  p <- ggplot(promo_data, aes(y = marginal_effects, x = level, fill = factor(Is_Significant))) +
    geom_col() +
    scale_fill_manual(values = c("1" = "#00bfc4", "0" = "#f8766d")) + 
    theme_bw() +
    facet_grid(cols = vars(DOW)) +
    labs(title = paste("Marginal Effects of", promo_value),
         fill = "Significance") +
    theme(axis.text.x = element_text(size = 12, angle = 90),
          axis.title = element_text(size = 14, face = "bold"),
          plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
          strip.text = element_text(size = 14))
  print(p)
  
  # Save plot
  ggsave(paste0("Marginal_Effects_", gsub("/", "_", promo_value), ".png"), plot = p, width = 10, height = 8, units = "in")
}

top_20 <- final_model %>%
  filter(!is.na(promo) & !is.na(level) & !is.na(DOW)) %>%
  select(promo, level, DOW, Is_Significant, marginal_effects) %>%
  filter(Is_Significant == 1) %>%
  arrange(desc(marginal_effects)) %>%
  head(20) %>%
  select(-Is_Significant)

bot_20 <- final_model %>%
  filter(!is.na(promo) & !is.na(level) & !is.na(DOW)) %>%
  select(promo, level, DOW, Is_Significant, marginal_effects) %>%
  filter(Is_Significant == 1) %>%
  arrange(marginal_effects) %>%
  head(20) %>%
  select(-Is_Significant)

#### Dannys Graphs ####

promotion_types_a <- c(
  "Awareness/Charity" = sum(final_data2$Awareness.Charity > 0),
  "Appreciation" = sum(final_data2$Appreciation > 0),
  "Bobblehead/Statue Giveaway" = sum(final_data2$Bobblehead.Statue.Giveaway > 0),
  "Shirt Giveaway" = sum(final_data2$Shirt.Giveaway > 0),
  "Other Giveaway" = sum(final_data2$Other.Giveaway > 0),
  "On-Field/Player Experiences" = sum(final_data2$On.Field.Player.Experiences > 0),
  "Concession Deals" = sum(final_data2$Concession.Deals > 0),
  "Ticket Deals" = sum(final_data2$Ticket.Deals > 0),
  "Merchandise Deals" = sum(final_data2$Merchandise.Discounts > 0),
  "Theme" = sum(final_data2$Theme.Days.Nights > 0),
  "Fireworks" = sum(final_data2$Fireworks > 0),
  "Misc." = sum(final_data2$Misc. > 0),
  "None" = sum(final_data2$num_promos == 0)
) %>%
  enframe(name = "Promotion Type", value = "Number of Games") %>%
  mutate(none = if_else(`Promotion Type` == "None", 1, 0)) 

promotion_types <- promotion_types_a %>%
  arrange(desc(none), `Number of Games`)

promotion_types$`Promotion Type` <- factor(promotion_types$`Promotion Type`, 
                                           levels = unique(promotion_types$`Promotion Type`))

ggplot(promotion_types, aes(y = `Promotion Type`, x = `Number of Games`)) +
  geom_col() +
  theme_bw() +
  labs(title = "Number of Games by Promotion Type") +
  xlim(c(0, 2500)) +  
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold")
  )

attendance_by_month <- final_data2 %>%
  mutate(Month = if_else(month == 3, 4, month)) %>%
  group_by(Month) %>%
  summarize(`Average Attendance` = mean(attendance)) %>%
  ungroup() %>%
  mutate(Month = month(Month, label = TRUE, abbr = FALSE))

ggplot(attendance_by_month, aes(x = Month, y = `Average Attendance`)) +
   geom_col() +
   theme_bw() +
   labs(title = "Average Attendance by Month") +
   theme(axis.text = element_text(size = 12),
         axis.title = element_text(size = 14, face = "bold"),
         plot.title = element_text(size = 16, face = "bold")
   )


level_order <- c("Single-A", "High-A", "Double-A", "Triple-A")
final_data2$Level <- factor(final_data2$Level, levels = level_order)

ggplot(final_data2) +
  geom_boxplot(aes(x = attendance, y = Level)) +
  theme_bw() +
  labs(title = "Distribution of Attendance by Level", x = "Attendance") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold")
  )



final_model %>%
  mutate(Is_Significant = if_else(p_value <= 0.5, 1, 0)) -> final_model

no_interactions %>% final_model[2:38, ]
  

ggplot(coefficients, aes(y = `Promotion Type`, x = `Marginal Effect`)) +
  geom_col() +
  theme_bw() +
  labs(title = "Distribution of Attendance by Level") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold")
  )
