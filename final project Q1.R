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

df2 <- read_csv("WHR_2020.csv", head(TRUE))
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


  
  