options(scipen=100,digits=8) 
library(Sleuth3) 
library(tidyverse)
library(mosaic) 
library(gridExtra)
library(gmodels)
library(corrplot)
library(dplyr)
library(GGally) 


df2 <- read_csv("WHR_2020.csv", head(TRUE))

df_multi <- df2

df_multi

df_multi %>%
  summarise(n = n(), Average = mean(happiness_score), Median = median(happiness_score),
            SD = sd(happiness_score),  IQR = IQR(happiness_score),Min = min(happiness_score), 
            Max = max(happiness_score),
            Ratio = max(happiness_score)/min(happiness_score)) %>%
  knitr::kable(digits = 2)

ggpairs(df_multi, columns = c(4:10), upper = NULL)+
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

# Full model 
model60 <- lm(happiness_score ~ gdp_per_capita + social_support + healthy_life_expectancy + freedom_to_make_life_choices + generosity + perceptions_of_corruption, data = df_multi)
summary(model60)
# The full model Adjusted R-squared is 0.738.

# We can see model 51 and model 60 has similar Adjusted R-squared so we want to compare the performance of the models.
anova(model60, model51)
anova(model60, model51)
