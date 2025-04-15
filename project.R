options(scipen=100,digits=8) 
library(Sleuth3) 
library(tidyverse)
library(mosaic) 
library(gridExtra)
library(gmodels)

df1 <- read_csv("WHR_2015.csv", head(TRUE))
df2 <- read_csv("WHR_2020.csv", head(TRUE))

anti_join(df1, df2, by="country")
anti_join(df2, df1, by="country")
