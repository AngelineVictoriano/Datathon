# importing libraries
library(tidyverse)
library(janitor)
library(kableExtra)
library(readxl)

########################################################
# getting all the data
########################################################

# reading world bank income data
country_class_raw <- read_excel("CLASS.xlsx")

country_class <- country_class_raw %>% 
  rename(country = Economy) %>% 
  select(country, Region, "Income group")


# saving processed data
saveRDS(country_class, here::here("processed_data", "country_class.rds"))

# reading healthcare spending
expenditure_raw <- read_csv(here::here("Healthexpenditure.csv"))

# data wrangling
expenditure_data <- expenditure_raw %>%
  #reformat variable names to be all be in snake_case
  clean_names() %>%
  # the resultant name function is allowed in the syntax. so renaming
  rename(Function = 4) %>%
  pivot_wider(names_from = c(Function, measure_2, provider,
                             financing_scheme),
              values_from = value,
              id_cols = c(year, country))

# saving processed data
saveRDS(expenditure_data, here::here("processed_data", "expenditure_data.rds"))


# loading healthcare utilization data
oecd_data_raw <- read_csv(here::here("OECDUtilisationRaw.csv"))

# data wrangling
oecd_data <- oecd_data_raw %>%
  #reformat variable names to be all be in snake_case
  clean_names() %>%
  pivot_wider(names_from = c(variable, measure),
              values_from = value,
              id_cols = c(year, country))

# saving processed data
saveRDS(oecd_data, here::here("processed_data", "oecd_data.rds"))

# removing raw data to save ram
# rm(country_class_raw, expenditure_raw, oecd_data_raw)

########################################################
# Data wrangling for healthcare expenditure
########################################################

# looking at the table, the recent year with complete data was 2019

health_expenditure_to_GDP <- expenditure_data %>%
  filter(year == 2019) %>%
  # as year 2019 is selected, the year column can be dropped
  select(-year) %>% 
  # retain first column and extract all financing schemes
  select(c(1) | contains("All financing schemes")) %>%
  # similarly, get all expenditure (under name current expenditure)
  select(c(1) | contains("Current expenditure on health (all functions)")) %>%
  # get data from all providers
  select(c(1) | contains("All providers")) %>%
  # get column of GDP %
  select(c(1) | contains("Share of gross domestic product")) %>%
  # rename the column name to expenditure_to_GDP
  rename(expenditure_to_GDP = 2)


###########################################################
# 1. plotting health expenditure and world bank income category 
###############################################################

merged_data <- health_expenditure_to_GDP %>% 
  left_join(country_class, by = "country")

# the resultant tibble has china and Korea misspelled
# renaming the same in health_expenditure_to_GDP
health_expenditure_to_GDP <- health_expenditure_to_GDP %>% 
  mutate(country = replace(country, country == "China (People's Republic of)", "China"))
# changing the simpler Korea name in country tibble

country_class <- country_class %>% 
  mutate(country = replace(country, country == "Korea, Rep.", "Korea"))


# performing the merge again
merged_data <- health_expenditure_to_GDP %>% 
  left_join(country_class, by = "country") %>% 
  # rename the column for better coding practice
  rename(Income_group = "Income group") %>% 
  # make the income group as factor with order
  mutate(Income_group = factor(Income_group, order = TRUE, levels = c("High income", "Upper middle income","Lower middle income")))


# plotting 
merged_data %>%  ggplot(aes(x=reorder(country, expenditure_to_GDP), y = expenditure_to_GDP, fill = Income_group)) +
  geom_bar(stat='identity', width=.4) +
  labs(title = "Barplot showing Healthcare expenditure by each country",
       subtitle = "as percentage of GDP",
       x = "Coutries",
       y = "Healthcare expenditure (% of GDP)",
       fill = "Income level") +
  coord_flip()

###################################################################
# getting share of govt/compulsory input and voluntary/out-of-pocket contributions
# data from 2019 will be used 
###################################################################

share_of_expenditure <- expenditure_data %>%
  filter(year == 2019) %>%
  # as year 2019 is selected, the year column can be dropped
  select(-year) %>% 
  # retain first column (country) and extract govt and individual contributions
  select(c(1) | contains("Government/compulsory schemes") | contains("Voluntary schemes/household out-of-pocket payments")) %>%
  # getting all expenditure (subgroup)
  select(c(1) | contains("Current expenditure on health (all functions)")) %>%
  # get data from all providers
  select(c(1) | contains("All providers")) %>%
  # get "share of current expenditure on health"
  select(c(1) | contains("Share of current expenditure on health")) %>%
  rename(govt_contributions = 2, individual_contributions = 3) %>% 
  # checking integrity of the data. the total contribution should come around 100%
  mutate(total_contributions = govt_contributions + individual_contributions)

# most of the countries show 100% contribution while handful of countries have 1-2% difference This can be shown as "unknown"

share_of_expenditure <- share_of_expenditure %>% 
  mutate(total_contributions = 100 - total_contributions) %>% 
  rename(unknown_contributions = total_contributions)
  

###################################################################
# plotting bar chart
###################################################################







