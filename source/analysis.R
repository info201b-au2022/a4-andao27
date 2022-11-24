library(tidyverse)
library(ggplot2)
library(dplyr)
library(maps)
library(mapproj)
# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

incarceration_trends <- read.csv("C:/Users/daole/Documents/info201/data/incarceration_trends.csv")
View(incarceration_trends)

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#
df <- data.frame("year" = incarceration_trends$year,
                 "state" = incarceration_trends$state,
                 "aapi_pop_15to64" = incarceration_trends$aapi_pop_15to64,
                 "white_pop_15to64" = incarceration_trends$white_pop_15to64,
                 "aapi_jail_pop" = incarceration_trends$aapi_jail_pop,
                 "white_jail_pop" = incarceration_trends$white_jail_pop)
df_ratio <- df %>%
  summarise(state, aapi_ratio = aapi_jail_pop/aapi_pop_15to64,
            white_ratio = white_jail_pop/white_pop_15to64)
View(df_ratio)
final_df_ratio = do.call(data.frame, lapply
                         (df_ratio, function(value) replace
                           (value, is.infinite(value), NA))) #replace inf as NA
print(final_df_ratio)
sum_population <-  nrow(incarceration_trends)
print(sum_population)
mean_ratio <- mean(final_df_ratio$aapi_ratio, na.rm = TRUE)
print(mean_ratio)
max_ratio <- max(final_df_ratio$aapi_ratio, na.rm = TRUE)
print(max_ratio)
min_ratio <- min(final_df_ratio$aapi_ratio, na.rm = TRUE)
print(min_ratio)
## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
  df <- data.frame(year = incarceration_trends$year, total_jail_pop = incarceration_trends$total_jail_pop)
  # TODO: Implement this function 
return(df)   
}
#View(get_year_jail_pop())
# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  growth_prison_population <- 
    ggplot(get_year_jail_pop()) +
    geom_col(mapping = aes(x=year, y=total_jail_pop)) +
    ggtitle("Growth of the US Prison Population (1970-2018)") +
    labs(caption = "Fig 3.1: This bar graph shows the trend of US prison population from 1970 to 2018")
  return(growth_prison_population)
  # TODO: Implement this function 
} 
plot(plot_jail_pop_for_us())
## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
get_jail_pop_by_states <- function(states) {
  df <- incarceration_trends %>%
    filter(state %in% states) %>%
    group_by(state, year) %>%
    summarise(total_jail_pop = sum(total_jail_pop, na.rm = TRUE))
  return(df)
}
#View(get_jail_pop_by_states(c("WA", "OR", "CA")))

plot_jail_pop_by_states <- function(states) {
  growth_by_state <-
    ggplot(data = get_jail_pop_by_states(states),
    aes(x = year, y = total_jail_pop, group = state)) +
    geom_line(aes(linetype=state, color=state)) +
    ggtitle("Growth of Prison Population by State (1970-2018)") +
    labs(caption = "Fig 4.1: This line chart shows the growth of prison population by state from 1970 to 2018")
  return(growth_by_state)
    
}
plot(plot_jail_pop_by_states(c("WA", "OR", "CA", "TX")))

#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
male_female <- function() {
  df <- data.frame(female_jail_pop = incarceration_trends$female_jail_pop, male_jail_pop = incarceration_trends$male_jail_pop)
  return(df)
}
#View(male_female())

plot_gender_inequality <- function() {
  gender <- 
    ggplot(male_female(), aes(x = female_jail_pop, y = male_jail_pop)) +
    geom_point() +
    ggtitle("Jail Population by Gender") +
    labs(caption = "Fig 5.1: This scatterplot shows the different patterns in male and female jail populations")
  return(gender)
}
plot(plot_gender_inequality())
## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
df <- data.frame("year" = incarceration_trends$year,
                 "state" = incarceration_trends$state,
                 "aapi_pop_15to64" = incarceration_trends$aapi_pop_15to64,
                 "white_pop_15to64" = incarceration_trends$white_pop_15to64,
                 "aapi_jail_pop" = incarceration_trends$aapi_jail_pop,
                 "white_jail_pop" = incarceration_trends$white_jail_pop)
#View(df)
df_ratio <- df %>%
  summarise(state, aapi_ratio = aapi_jail_pop/aapi_pop_15to64,
            white_ratio = white_jail_pop/white_pop_15to64)
#View(df_ratio)

#Map for aapi jail population
aapi_state_pop <- df_ratio %>%
  group_by(state) %>%
  summarise(aapi_pop_by_state = mean(aapi_ratio, na.rm = TRUE))
aapi_state_pop[is.na(aapi_state_pop)] = 0
aapi_state_pop[sapply(aapi_state_pop, is.infinite)] <- 0

aapi_pop_state <- aapi_state_pop %>%
  mutate(full_name = tolower(state.name[match(aapi_state_pop$state, state.abb)])) %>%
  rename(abbre = state) %>%
  rename(state = full_name)

aapi_state_map <- map_data("state") %>%
  rename(state = region) %>%
  left_join(aapi_pop_state, by = "state")

aapi_plot <- function() {
  ggplot(aapi_state_map) +
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group, fill = aapi_pop_by_state), color = "white", size = .1
    ) +
    coord_map() +
    scale_fill_continuous(low = "#D8D7D7", high = "#F0A8A8", limits = c(0, 0.03)) +
    labs(fill = "ratio",
         caption = "Fig 6.1: This map shows the AAPI jail population by region from 1970 to 2018",
         x = "",
         y = "") +
    theme(plot.caption = element_text(hjust = 0.5)) +
    ggtitle("Ratio of AAPI Jail Population Distributed by Region")
}
aapi_plot()

#Map for white jail population
white_state_pop <- df_ratio %>%
  group_by(state) %>%
  summarise(white_pop_by_state = mean(white_ratio, na.rm = TRUE))
white_state_pop[is.na(white_state_pop)] = 0
white_state_pop[sapply(white_state_pop, is.infinite)] <- 0

white_pop_state <- white_state_pop %>%
  mutate(full_name = tolower(state.name[match(white_state_pop$state, state.abb)])) %>%
  rename(abbre = state) %>%
  rename(state = full_name)

white_state_map <- map_data("state") %>%
  rename(state = region) %>%
  left_join(white_pop_state, by = "state")

white_plot <- function() {
  ggplot(white_state_map) +
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group, fill = white_pop_by_state), color = "white", size = .1
    ) +
    coord_map() +
    scale_fill_continuous(low = "#D8D7D7", high = "#A9D5F9", limits = c(0, 0.02)) +
    labs(fill = "ratio",
         caption = "Fig 6.2: This map shows the White jail population by region from 1970 to 2018",
         x = "",
         y = "") +
    theme(plot.caption = element_text(hjust = 0.5)) +
    ggtitle("Ratio of White Jail Population Distributed by Region")
}
white_plot()
#----------------------------------------------------------------------------#
## Load data frame ---- 
