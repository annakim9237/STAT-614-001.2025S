options(scipen=100,digits=8) 
library(Sleuth3) 
library(tidyverse)
library(mosaic) 
library(gridExtra)
library(gmodels)
library(corrplot)
library(dplyr)
library(DescTools) # this is for after performing ANOVA -one way
library(car) #for Levene test
library(GGally)

df2 <- read_csv("WHR_2020.csv", head(TRUE))
df1 <- read_csv("WHR_2015.csv", head(TRUE))

#Q1 Do the happiness scores of people on different continents in 2020 differ significantly (ANOVA Test
# Happiness Summary descriptive statistics
df2 %>%
  group_by(region) %>% 
  summarise(n = n(), Average = mean(happiness_score), Median = median(happiness_score),
            SD = sd(happiness_score),  IQR = IQR(happiness_score),Min = min(happiness_score), 
            Max = max(happiness_score),
            Ratio = max(happiness_score)/min(happiness_score)) %>%
  knitr::kable(digits = 2)

# Boxplot 
df2 %>%
  mutate(region = fct_reorder(region, happiness_score, .fun = 'median')) %>%
  ggplot(aes(x = reorder(region, happiness_score), y = happiness_score))+
  geom_boxplot(aes(fill = region), show.legend = FALSE) +
  labs (title = "Happiness_score on ten different regions",
        y = " Happiness_score ", x = "Region ") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Histograms 
ggplot(data = df2, mapping = aes(x = happiness_score )) +
  geom_histogram(bins = 20, color = "dark blue", fill = "yellow") +
  ggtitle("Happiness_score on ten different regions") +
  facet_wrap( ~ region)+
  theme_bw()

# qq plots for original data (useful for checking normality for each group)
ggplot(data = df2, mapping = aes(sample = happiness_score )) +
  stat_qq() +
  stat_qq_line() +
  geom_qq( color = "dark red") +
  ggtitle("Happiness_score on ten different regions") +
  ylab("Qualification Score") +
  xlab("N(0,1) quantiles") +
  facet_wrap( ~ region)+
  theme_bw()

#Analyze these data
#I. Fit ANOVA and save output to use it with follow up analysis 
# (like with PostHocTest) and estimated residuals
model.fit <- aov(happiness_score~region,data=df2)  # aov = anova model
anova(model.fit) # anova table provides F-stat and p-value 

#II.Another way to get ANOVA table and S-pooled using data from all groups.
#using linear model lm()
anova(lm(happiness_score~region,data=df2))
summary(lm(happiness_score~region,data=df2)) 

# Easy way to get Least Significant Difference (LSD) CI and tests of hypotheses for planned comparison
#it gives all comparisons of means
PostHocTest(model.fit, method = "lsd")

# residual analysis to check the assumptions
shapiro.test(model.fit$residuals)  

# Levene's test for homogeneity of variance
leveneTest(happiness_score ~ factor(region), data = df2)

#Test of normality (by individual groups)
tapply(df2$happiness_score,df2$region,shapiro.test)


#Q2 Have the happiness scores of people in various countries changed between 2015 and 2020 (before and after the COVID-19) (Pair t-test
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

# -------------------------------------------------------------------------



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

# Histograms for X
hist(df2015$happiness_score,
     main = paste("Histogram of happiness_score(2015)"),
     xlab = "happiness_score",
     ylab = "Average life evaluation score (0–10 scale)",
     col = "skyblue",
     border = "white")

hist(df2020$happiness_score,
     main = paste("Histogram of happiness_score(2020)"),
     xlab = "happiness_score",
     ylab = "Average life evaluation score (0–10 scale)",
     col = "skyblue",
     border = "white")


# seven Histograms, use Previous Plots and Next plots to see all
happy_cols <- names(df_happy_num)
par(mfrow = c(2, 3),    # 2 rows, 3 columns
    mar   = c(4, 4, 2, 1))  # adjust margins if you like
for (col in happy_cols) {
  hist(df_happy_num[[col]],
       main   = paste("Histogram of", col),
       xlab   = col,
       col    = "skyblue",
       border = "white")
}

# Reset to a 1×1 plotting grid
par(mfrow = c(1, 1))

# X's correlation
happy_x_num <- colnames(df_happy)[5:10]
df_happy_x_num <- df_happy[happy_x_num]
cor_matrix <- cor(df_happy_x_num)
cor_matrix
corrplot(cor_matrix,
         method = "color", 
         type = "upper", 
         tl.col = "black", 
         tl.cex = 0.4,      
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
#A paired t-test comparing happiness scores between 2015 and 2020 revealed a statistically significant decrease (t = -2.34, p = 0.0206).
#The average happiness score dropped by 0.11 points

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


#Q3 How do the different explanatory variables influence the happiness score in 2020 (Multiple Regression
df2 <- read_csv("WHR_2020.csv", head(TRUE))

df_multi <- df2

df_multi

df_multi %>%
  summarise(n = n(), Average = mean(happiness_score), Median = median(happiness_score),
            SD = sd(happiness_score),  IQR = IQR(happiness_score),Min = min(happiness_score), 
            Max = max(happiness_score),
            Ratio = max(happiness_score)/min(happiness_score)) %>%
  knitr::kable(digits = 2)

ggpairs(df_multi, columns = c(3:9), upper = NULL)+
  ggtitle("Matrix of Scatterplots for brain weight data")+
  theme_bw()

#SLR  model  of happiness_score on each explanatory variable

model10 <- lm(happiness_score ~ gdp_per_capita, data = df_multi)
summary(model10)
model11 <- lm(happiness_score ~ social_support, data = df_multi)
summary(model11)
model12 <- lm(happiness_score ~ healthy_life_expectancy, data = df_multi)
summary(model12)
model13 <- lm(happiness_score ~ freedom_to_make_life_choices, data = df_multi)
summary(model13)
model14 <- lm(happiness_score ~ generosity, data = df_multi)
summary(model14)
model15 <- lm(happiness_score ~ perceptions_of_corruption, data = df_multi)
summary(model15)

# model 2
model20 <- lm(happiness_score ~ gdp_per_capita + social_support, data = df_multi)
model21 <- lm(happiness_score ~ gdp_per_capita + healthy_life_expectancy, data = df_multi)
model22 <- lm(happiness_score ~ gdp_per_capita + freedom_to_make_life_choices, data = df_multi)
model23 <- lm(happiness_score ~ gdp_per_capita + generosity, data = df_multi)
model24 <- lm(happiness_score ~ gdp_per_capita + perceptions_of_corruption, data = df_multi)
model25 <- lm(happiness_score ~ social_support + healthy_life_expectancy, data = df_multi)
model26 <- lm(happiness_score ~ social_support + freedom_to_make_life_choices, data = df_multi)
model27 <- lm(happiness_score ~ social_support + generosity, data = df_multi)
model28 <- lm(happiness_score ~ social_support + perceptions_of_corruption, data = df_multi)
model29 <- lm(happiness_score ~ healthy_life_expectancy + freedom_to_make_life_choices, data = df_multi)
model210 <- lm(happiness_score ~ healthy_life_expectancy + generosity, data = df_multi)
model211 <- lm(happiness_score ~ healthy_life_expectancy + perceptions_of_corruption, data = df_multi)
model212 <- lm(happiness_score ~ freedom_to_make_life_choices + generosity, data = df_multi)
model213 <- lm(happiness_score ~ freedom_to_make_life_choices + perceptions_of_corruption, data = df_multi)
model214 <- lm(happiness_score ~ generosity + perceptions_of_corruption, data = df_multi)

reg2 <- list(
  model20 = model20,
  model21 = model21,
  model22 = model22,
  model23 = model23,
  model24 = model24,
  model25 = model25,
  model26 = model26,
  model27 = model27,
  model28 = model28,
  model29 = model29,
  model210 = model210,
  model211 = model211,
  model212 = model212,
  model213 = model213,
  model214 = model214
)

for (name in names(reg2)) {
  cat("\n======", name, "======\n")
  print(summary(reg2[[name]]))
}

# Model 3
model30 <- lm(happiness_score ~ gdp_per_capita + social_support + healthy_life_expectancy, data = df_multi)
summary(model30)

model31 <- lm(happiness_score ~ gdp_per_capita + social_support + freedom_to_make_life_choices, data = df_multi)
summary(model31)

model32 <- lm(happiness_score ~ gdp_per_capita + social_support + generosity, data = df_multi)
summary(model32)

model33 <- lm(happiness_score ~ gdp_per_capita + social_support + perceptions_of_corruption, data = df_multi)
summary(model33)

model34 <- lm(happiness_score ~ gdp_per_capita + healthy_life_expectancy + freedom_to_make_life_choices, data = df_multi)
summary(model34)

# model 4
model40 <- lm(happiness_score ~ gdp_per_capita + social_support + healthy_life_expectancy + freedom_to_make_life_choices, data = df_multi)
summary(model40)

model41 <- lm(happiness_score ~ gdp_per_capita + social_support + healthy_life_expectancy + generosity, data = df_multi)
summary(model41)

model42 <- lm(happiness_score ~ gdp_per_capita + social_support + healthy_life_expectancy + perceptions_of_corruption, data = df_multi)
summary(model42)

model43 <- lm(happiness_score ~ gdp_per_capita + social_support + freedom_to_make_life_choices + generosity, data = df_multi)
summary(model43)

model44 <- lm(happiness_score ~ gdp_per_capita + social_support + freedom_to_make_life_choices + perceptions_of_corruption, data = df_multi)
summary(model44)

# model 5
model50 <- lm(happiness_score ~ gdp_per_capita + social_support + healthy_life_expectancy + freedom_to_make_life_choices + generosity, data = df_multi)
summary(model50)

model51 <- lm(happiness_score ~ gdp_per_capita + social_support + healthy_life_expectancy + freedom_to_make_life_choices + perceptions_of_corruption, data = df_multi)
summary(model51)

model52 <- lm(happiness_score ~ gdp_per_capita + social_support + healthy_life_expectancy + generosity + perceptions_of_corruption, data = df_multi)
summary(model52)

model53 <- lm(happiness_score ~ gdp_per_capita + social_support + freedom_to_make_life_choices + generosity + perceptions_of_corruption, data = df_multi)
summary(model53)

model54 <- lm(happiness_score ~ gdp_per_capita + healthy_life_expectancy + freedom_to_make_life_choices + generosity + perceptions_of_corruption, data = df_multi)
summary(model54)

# We can find when we have more than five variables, most of it already have over 0.7 Adjusted R-squred.
# Especially, model 51 has over 0.73 Adjusted R-squared.

# Multicollinearity
vif(model51) 

par(mfrow=c(2,2))
plot(model51)

# Full model 
model60 <- lm(happiness_score ~ gdp_per_capita + social_support + healthy_life_expectancy + freedom_to_make_life_choices + generosity + perceptions_of_corruption, data = df_multi)
summary(model60)
# The full model Adjusted R-squared is 0.738.

# Multicollinearity
vif(model60) 

plot(model60)

# We can see model 51 and model 60 has similar Adjusted R-squared so we want to compare the performance of the models.
summary(model51)
summary(model60)

anova(model60, model51)

df_multi[c(13,16,117), c("country", "happiness_score")]

df_multi_clean <- df_multi[-c(13, 16, 117), ]

model_clean <- lm(happiness_score ~ gdp_per_capita + social_support + healthy_life_expectancy + 
                    freedom_to_make_life_choices + generosity + perceptions_of_corruption, 
                  data = df_multi_clean)
summary(model_clean)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Practice using step function
#Model using Time as quantitative variable

full_model <- lm(happiness_score ~ gdp_per_capita + social_support + healthy_life_expectancy +
                   freedom_to_make_life_choices + generosity + perceptions_of_corruption, 
                 data = df_multi)
summary(full_model)

step_model <- step(full_model, trace = TRUE)
summary(step_model)

