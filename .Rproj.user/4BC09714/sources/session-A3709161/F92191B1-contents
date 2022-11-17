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
  rename(country = Economy, income_group = "Income group") %>% 
  select(country, Region, income_group) %>% 
  mutate(income_group = factor(income_group, levels = c("Low income", "Lower middle income", "Upper middle income", "High income")))
  

# Before saving Renaming Russia and Korea
country_class <- country_class %>% 
  mutate(country = replace(country, country == "Russian Federation", "Russia")) %>% 
  mutate(country = replace(country, country == "Korea, Dem. People's Rep.", "Korea"))
  
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

# Before saving Renaming China and Korea
country_class <- country_class %>% 
  mutate(country = replace(country, country == "China (People's Republic of)", "China"))
  

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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# combing income group and healthcare expenditure
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
income_and_spending <- country_class %>% 
  inner_join(health_expenditure_to_GDP, by = c("country"))

# saving processed data
saveRDS(income_and_spending, here::here("processed_data", "income_and_spending.rds"))


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

# Before saving Renaming China and Korea
share_of_expenditure <- share_of_expenditure %>% 
  mutate(country = replace(country, country == "China (People's Republic of)", "China"))

# most of the countries show 100% contribution while handful of countries have 1-2% difference This can be shown as "unknown"

share_of_expenditure <- share_of_expenditure %>% 
  mutate(total_contributions = 100 - total_contributions) %>% 
  rename(unknown_contributions = total_contributions) %>% 
  # also reorder rows based on contributions
  arrange(desc(govt_contributions))

# saving processed data
saveRDS(share_of_expenditure, here::here("processed_data", "share_of_expenditure.rds"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# combing income group and healthcare expenditure and share of expenditure
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
income_and_spending_with_share <- income_and_spending %>% 
  inner_join(share_of_expenditure, by = c("country"))

# saving processed data
saveRDS(income_and_spending_with_share, here::here("processed_data", "income_and_spending_with_share.rds"))

###################################################################
# plotting bar chart
###################################################################

# making the data frame more longer for better plotting. Using pivot longer function
share_of_expenditure_longer<- share_of_expenditure %>%
  pivot_longer(!country, names_to = "contributions_type", values_to = "contributions")

# Grouped
share_of_expenditure_longer %>% 
ggplot(aes(x=fct_inorder(country), y=contributions, fill=contributions_type)) + 
  geom_bar(position="dodge", stat="identity") +
  coord_flip()

# Grouped but with stacked
share_of_expenditure_longer %>% 
  ggplot(aes(x=fct_inorder(country), y=contributions, fill=contributions_type)) + 
  geom_bar(position="fill", stat="identity") +
  labs(title = "Stacked bar chart showing share of healthcare expenditure  year 2019",
       subtitle = "between governament and individual contributions (voluntary schemes or out-of-pocket)",
       x = "Countries",
       y = "Share of expenditure",
       fill = "Portion of share") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(labels=c('Governament', 'Individual', 'Unknown')) +
  coord_flip() 

###################################################################
# healthcare utilization
###################################################################

# consultations
consultations <- oecd_data %>%
  # selecting first column (country) and columns containing "doctors"
  select(c(1,2) | contains("Doctors")) %>%
  #renaming the column names
  rename(consultations = 3) %>% 
  # getting average across the years
  group_by(country) %>%
  summarise(mean_consultations = mean(consultations, na.rm = TRUE)) %>%
  drop_na(2)

# saving processed data
saveRDS(consultations, here::here("processed_data", "consultations.rds"))

# Before merging. Renaming Russia
country_class <- country_class %>% 
  mutate(country = replace(country, country == "Russian Federation", "Russia"))
  
# merging
merged_data <- country_class %>% 
  inner_join(consultations, by = c("country"))

# merging with health expenditure
merged_data <- merged_data %>% 
  inner_join(health_expenditure_to_GDP, by = c("country"))

# importing additional package
library(ggExtra)
library(hrbrthemes)

# plotting 
merged_data %>%
  rename(income_group = "Income group") %>% 
  ggplot(aes(x=mean_consultations, y = expenditure_to_GDP, shape = income_group, color = income_group)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  labs(title="Correlation of number of consultations with health expenditure",
       subtitle = "is investing more in health reduces number of health consultations?",
       x="Number of consultations per capita", y = "Health expenditure as percentage of GDP") + 
  scale_color_brewer(palette="Dark2") + 
  theme_classic()

########################################
# correlating the same with share of govt or compulsory healthcare funding
#########################################

merged_data <- merged_data %>% 
  inner_join(share_of_expenditure, by = c("country"))

# plotting
merged_data %>%
  rename(income_group = "Income group") %>% 
  ggplot(aes(x=mean_consultations, y = govt_contributions, shape = income_group, color = income_group, size = expenditure_to_GDP)) +
  geom_point(alpha=0.7) +
  labs(title="Correlation of number of consultations with health expenditure",
       subtitle = "is there a relationship with free healthcare and number of consultations?",
       x="Number of consultations per capita", y = "Share of govt or compulsary healthcare financing",
       color = "Income group",
       size="Amount as percentage of GDP",
       shape= "") + 
  scale_color_brewer(palette="Dark2") + 
  theme_classic()

##################################################################################
# getting data for immunization
##################################################################################

mean_immunization <- oecd_data %>%
  # selecting country column and Immunization columns
  select(c(2) | contains("Immunisation")) %>%
  # group the rows by countries and get mean of immunization values
  group_by(country) %>%
  summarise(across(everything(), list(mean), na.rm = TRUE)) %>% 
  # renaming the columns as good variable format
  rename(hepatitis = 2, influenza=3, DPT=4, Measles=5)

# saving processed data
saveRDS(mean_immunization, here::here("processed_data", "mean_immunization.rds"))


# merging with health expenditure
merged_data <- income_and_spending %>% 
  inner_join(mean_immunization, by = c("country"))


# plotting 
merged_data %>%
  ggplot(aes(x=influenza, y = expenditure_to_GDP, shape = income_group, color = income_group)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  labs(title="Correlation of influenza immunisation coverage with health expenditure",
       subtitle = "Is higher spending correlates with higher immunisation cover?",
       x="Percentage of people covered", 
       y = "Health expenditure as percentage of GDP",
       shape = "Income group",
       color = "Income group") + 
  scale_color_brewer(palette="Dark2") + 
  theme_classic()

########################################
# correlating the same with share of govt or compulsory healthcare funding
#########################################

merged_data <- merged_data %>% 
  inner_join(share_of_expenditure, by = c("country"))


# plotting
merged_data %>%
  ggplot(aes(x=influenza, y = govt_contributions, color = income_group, size = expenditure_to_GDP)) +
  geom_point(alpha=0.7) +
  labs(title="Correlation of influensa vaccination cover with health expenditure",
       subtitle = " bimodal distribution of influensa coverate with health expenditure?",
       x="influensa vaccine coverage", y = "Share of govt or compulsary healthcare financing",
       color = "Income group",
       size="Amount as percentage of GDP") + 
  scale_color_brewer(palette="Dark2") + 
  theme_classic()


##########################################################################################################################


# breast screening
mean_screening_rate <- oecd_data %>%
  # selecting country column and cervical screening columns
  select(c(2) | contains("cancer screening, programme data")) %>%
  # selecting cervical, breast colorectal columns. for colorectal cases, selecting "population" only one column
  select(c(1) | contains("Cervical") | contains("Breast") | contains("population")) %>%
  # group the rows by countries and get mean of screening rate
  group_by(country) %>%
  summarise(across(everything(), list(mean), na.rm = TRUE)) %>% 
  # renaming columns
  rename(cervical = 2, breast= 3, colorectal = 4)


#################################
# merging
merged_data <- income_and_spending_with_share %>% 
  inner_join(mean_screening_rate, by = c("country"))

library(ggExtra)

# Scatterplot
ggplot(merged_data, aes(cervical, expenditure_to_GDP, color = income_group)) + 
  geom_count(size = 2) + 
  geom_smooth(method="lm", se=F) +
  labs(title="Correlation of cervical cancer screening with health expenditure",
       subtitle = " Does increased healthcare spending allows more people to get screened?",
       x="Cervical cancer screening rate (programme data)", y = "Healthcare expentidure (% of GDP)",
       color = "Income group") + 
  scale_color_brewer(palette="Dark2") + 
  theme_classic()

# Similar but govt/ compulsory healthcare financing
ggplot(merged_data, aes(cervical, govt_contributions)) + 
  geom_count(size = 2) + 
  geom_smooth(method="lm", se=F) +
  labs(title="Correlation of cervical cancer screening with free healthcare",
       subtitle = "?",
       x="Cervical cancer screening rate (programme data)", y = "Share of free healthcare financing") + 
  scale_color_brewer(palette="Dark2") + 
  theme_classic()


##########################################################################################################################

# length of stay
mean_length_stay <- oecd_data %>%
  # selecting country column and length of stay column
  select(c(2) | contains("Inpatient care average length of stay (all hospitals)")) %>%
  # renaming column
  rename(LOS = 2) %>% 
  # group the rows by countries and get mean of length of stayy
  group_by(country) %>%
  summarise(mean_LOS = mean(LOS, na.rm = TRUE)) %>%
  drop_na(2)


########### merging ########
merged_data <- income_and_spending_with_share %>% 
  inner_join(mean_length_stay, by = c("country"))

# Scatterplot
merged_data %>% 
  # filter(!country %in% c("Japan", "Korea")) %>% 
  ggplot(aes(mean_LOS, expenditure_to_GDP, color = income_group)) + 
  geom_count(size = 2) + 
  labs(title="Correlation of LOS with spending",
       subtitle = "?",
       x="Length of stay", y = "Spending") + 
  scale_color_brewer(palette="Dark2") + 
  theme_classic()


# Scatterplot with Japan removed
merged_data %>% 
  # filter(!country %in% c("Japan", "Korea")) %>% 
  ggplot(aes(mean_LOS, govt_contributions, color = income_group)) + 
  geom_count(size = 2) + 
  labs(title="Correlation of LOS with free healthcare",
       subtitle = "?",
       x="Length of stay", y = "Share of free healthcare financing") + 
  scale_color_brewer(palette="Dark2") + 
  theme_classic()


















