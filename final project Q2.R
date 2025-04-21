options(scipen=100,digits=8) 
library(Sleuth3) 
library(tidyverse)
library(mosaic) 
library(gridExtra)
library(gmodels)
library(corrplot)
library(dplyr)

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

# This is our basic dataset
df_happy

# Happiness Summary
df_happy %>%
  group_by(year) %>% 
  summarise(n = n(), Average = mean(happiness_score), Median = median(happiness_score),
            SD = sd(happiness_score),  IQR = IQR(happiness_score),Min = min(happiness_score), 
            Max = max(happiness_score),
            Ratio = max(happiness_score)/min(happiness_score)) %>%
  knitr::kable(digits = 2)

# Boxplot to check normality
ggplot(df_happy, aes(x = factor(year), y = happiness_score, fill = factor(year))) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Happiness Score by Year",
       x = "Year", y = "Happiness Score", fill = "Year")


# Boxplot to check normality
ggplot(df_happy, aes(x = fct_reorder(region, happiness_score), y = happiness_score, fill = factor(year))) +
  geom_boxplot() +
  theme_minimal()+
  labs(title = "Happiness Score by Region (2015 vs 2020)",
       x = "Region", y = "Happiness Score", fill = "Year") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# QQplot to check normality
ggplot(data = df_happy, mapping = aes(sample = happiness_score)) +
  stat_qq(color = "blue") +
  geom_qq_line(color = "red") +
  ggtitle("Q-Q Plot for Happiness Score") +
  xlab("Standard Normal Distribution quantiles") +
  ylab("Happiness Score") +
  theme_bw()

# QQplot to check normality by year
ggplot(data = df_happy, mapping = aes(sample = happiness_score)) +
  stat_qq(color = "blue") +
  geom_qq_line(color = "red") +
  facet_wrap(~ year) +
  ggtitle("Q-Q Plot for Happiness Score") +
  xlab("Standard Normal Distribution quantiles") +
  ylab("Happiness Score") +
  theme_bw()

colnames(df_happy)

# numeric columns
happy_num <- colnames(df_happy)[4:10]
df_happy_num <- df_happy[happy_num]
par(mfrow = c(2, 3))

# Histograms for X
for (col in happy_col) {
  hist(df_happy[[col]],
       main = paste("Histogram of", col),
       xlab = col,
       col = "skyblue",
       border = "white")
}

# X's correlation
happy_x_num <- colnames(df_happy)[5:10]
df_happy_x_num <- df_happy[happy_x_num]
cor_matrix <- cor(df_happy_x_num)
cor_matrix
corrplot(cor_matrix,
         method = "color", 
         type = "upper", 
         tl.col = "black", 
         tl.srt = 45, 
         tl.cex = 0.5,      
         addCoef.col = "black", 
         number.digits = 2)  

# filtering for t-test
df_2015 <- df_happy %>% 
  filter(year == 2015) 

df_2020 <- df_happy %>% 
  filter(year == 2020) 


shapiro.test(df_2015$happiness_score)
shapiro.test(df_2020$happiness_score)

diff_score <- df_2015$happiness_score - df_2020$happiness_score
shapiro.test(diff_score)

# Pair t-test
t.test(df_2015$happiness_score, df_2020$happiness_score, paired = TRUE)

# Pair t-test depending on continent - I did only because curious
df_happy %>%
  group_by(region,year) %>% 
  summarise(n = n(), Average = mean(happiness_score), Median = median(happiness_score),
            SD = sd(happiness_score),  IQR = IQR(happiness_score),Min = min(happiness_score), 
            Max = max(happiness_score),
            Ratio = max(happiness_score)/min(happiness_score)) %>%
  knitr::kable(digits = 2)

unique(df_happy$region)
regions <- unique(df_happy$region)
for (reg in regions) {
  d15 <- df_happy %>% filter(year == 2015, region == reg) %>% arrange(country)
  d20 <- df_happy %>% filter(year == 2020, region == reg) %>% arrange(country)
  
    t_test <- t.test(d15$happiness_score, d20$happiness_score, paired = TRUE)
    
    #print(reg)
    #print(t_test)
    
    cat("\nRegion:", reg, "\n")
    cat("p-value:", round(t_test$p.value, 5), "\n")
    cat("mean difference:", round(t_test$estimate, 5), "\n")
    cat("95% CI:", round(t_test$conf.int[1], 5), "to", round(t_test$conf.int[2], 5), "\n")
}

# Shapiro-test, but it's okay because our sample sizw is enough big.
shapiro.test(df_happy$happiness_score)

shapiro.test(df_happy$gdp_per_capita)
shapiro.test(df_happy$social_support)
shapiro.test(df_happy$healthy_life_expectancy)
shapiro.test(df_happy$freedom_to_make_life_choices)
shapiro.test(df_happy$generosity)


