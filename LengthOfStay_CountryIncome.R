#### This code looks at length of inpatient stay in countries based on their economic status.
#### Prepared by Nada

library(tidyverse)
library(readxl)

utilisation_raw <- read_csv("OECDUtilisationRaw.csv")
country_class <- read_excel("CLASS.xlsx")

# Save columns to keep

income_groups <- country_class %>%
  select("Economy", "Income group", "Region") %>% 
  rename("Country" = "Economy",
         "income_group" = "Income group")

# Join utilisation data to income groups

utilisation_income <- left_join(utilisation_raw, income_groups, by = "Country")

write.csv(utilisation_income, "Utilisation_Income.csv")

# Inpatient average hospital LOS all countries - data available for 40 countries

los_all <- utilisation_income %>% 
  filter(Variable == "Inpatient care average length of stay (all hospitals)") %>% 
  mutate(Country = as.factor(Country),
         income_group = as.factor(income_group)) %>% 
  select(Country, income_group, Region, Year, Value)

# Plot LOS by income classification
los_all %>% ggplot(aes(x = Year, y = Value, group = Country, colour = income_group)) +
  geom_line() +
  labs(title = "Average inpatient length of stay by country income",
       x = "Year", y = "Length of stay (days)",
       colour = "Income classification")
# Lack of data for non-high-income countries

# Plot LOS by country/income classification
los_all %>% ggplot(aes(x = Year, y = Value, colour = Country)) +
  geom_line() +
  facet_wrap(~income_group) +
  labs(title = "Average inpatient length of stay by income classification",
       x = "Year", y = "Length of stay (days)")
# Majority of data from high-income countries
# Possible question: Outlier in Japan (high-income). WHY?

# Focus on high income countries
los_high_income <- los_all %>% 
  filter(income_group == "High income")

# Plot LOS in high income countries by region
los_high_income %>% ggplot(aes(x = Year, y = Value, colour = Country)) +
  geom_line() +
  facet_wrap(~Region)
  labs(title = "Average inpatient length of stay in high-income countries",
       x = "Year", y = "Length of stay (days)")
# East Asia & Pacific Region seem to have the most variation. Zoom in.
# Possible question: Why is there a large variation between Japan & NZ although both are high income countries in East Asia & Pacific?
# Are there other variables that show the same pattern?  