# package for loading world map
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

library(tidyverse)
library(janitor)
library(kableExtra)
library(readxl)
library(hrbrthemes)
library(ggrepel)

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

OECD_countries <- read_csv("https://raw.githubusercontent.com/openclimatedata/countrygroups/main/data/oecd.csv")
OECD_countries_list <- OECD_countries$Name

# package for loading world map
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

# getting map data and removing Antarctica (as it takes space)
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  #removing Antarctica as it takes space
  filter(continent !="Antarctica")

# making simple theme for the map
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

health_expenditure_to_GDP <- health_expenditure_to_GDP %>% 
  rename(name = country)

merged_df <- merge(x=world, y=health_expenditure_to_GDP, by="name", all.y = TRUE) %>%
  filter(name %in% OECD_countries_list)

ggplot() +
  geom_sf(data= world, color = "gray", fill = "gray") +
  geom_sf(data=merged_df, color = "white", aes(fill=expenditure_to_GDP)) +
  labs(title="Healthcare expenditure as percentage of GDP", fill = "% of 
GDP") +
  scale_fill_gradient(low = "green", high = "red") +
  plain
