options(scipen=100,digits=8) 
library(Sleuth3) 
library(tidyverse)
library(mosaic) 
library(gridExtra)
library(gmodels)

df1 <- read_csv("WHR_2015.csv", head(TRUE))
df2 <- read_csv("WHR_2020.csv", head(TRUE))

# Check what countries are common in two datasets 
intersect(df1$country, df2$country)
common_countries <- intersect(df1$country, df2$country)
length(intersect(df1$country, df2$country)) 
# We can see there are 149 common countries. We can use these 149 countries only to prevent bias. 

# Find what countries are in 2015 dataset and not in 2020
anti_join(df1, df2, by="country")
# Find what countries are in 2020 dataset and not in 2015
anti_join(df2, df1, by="country")

# Add year to dataframes
df1 <- add_column(df1, year = 2015, .after = 2)
df2 <- add_column(df2, year = 2020, .after = 2)

#Filtered only common countries.
df2015 <- df1 %>% filter(country %in% common_countries)
df2020 <- df2 %>% filter(country %in% common_countries)

# Check the numbers of observation after filter.
count(df2015)
count(df2020)

# Combine two filtered dataset(2015, 2020)
df_happy <- bind_rows(df2015, df2020)

df_happy
