---
title: "TidyTuesday Project - February 28, 2021"
author: "Domenica Vega"
date: "February 28, 2021"
output: 
  html_document:
    keep_md: true 
---



# Loading the necessary packages 

```r
library(tidyverse)
```

# Getting TidyTuesday data from https://github.com/rfordatascience/tidytuesday 

I got the data from https://github.com/rfordatascience/tidytuesday, particularly from https://github.com/rfordatascience/tidytuesday/tree/master/data/2021/2021-02-23
(from the section titled "Get the data here"). This week's data is about Employed Status. 


```r
tuesdata <- tidytuesdayR::tt_load('2021-02-23')
```

```
## 
## 	Downloading file 1 of 2: `earn.csv`
## 	Downloading file 2 of 2: `employed.csv`
```

```r
employed <- tuesdata$employed

earn <- tuesdata$earn
```

# Let's get started! 


```r
industry_data <- employed %>% 
  select(year, industry, industry_total) %>% 
  filter(!is.na(industry_total)) 

industry_data_2 <- industry_data %>% 
  group_by(year, industry) %>%
  distinct() %>%
  arrange(desc(industry_total)) 

set.seed(1234)
chosen_industries <- sample(1:dim(distinct(tibble(industry_data$industry)))[1], 4)

industries <- c(
  distinct(tibble(industry_data$industry))[chosen_industries,1][1,1], distinct(tibble(industry_data$industry))[chosen_industries,1][2,1], distinct(tibble(industry_data$industry))[chosen_industries,1][3,1], 
distinct(tibble(industry_data$industry))[chosen_industries,1][4,1])

industry_data_2 %>%
  filter(industry %in% industries) %>%
  mutate(total_industry = sum(industry_total)) %>%
  ggplot(aes(x = year, y = total_industry)) + 
  geom_point(show.legend = FALSE) + 
  theme_minimal() + 
  facet_wrap(~industry) + 
  labs(x = "Year", 
       y = "Total Industry", 
       title = "Total Industry against Year", 
       caption = "This figure uses TidyTuesday's data from February 23, 2021")
```

![](TidyTuesday-Project-DV---February-28,2021_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


```r
earn %>%  
  select(-c(race, ethnic_origin, sex, n_persons, age)) %>% 
  filter(year == 2020) %>% 
  group_by(quarter) %>% 
  mutate(avg_median_weekly_earn = mean(median_weekly_earn)) %>%
  ggplot(aes(x = quarter, y = avg_median_weekly_earn)) + 
  geom_point() + 
  theme_minimal() +
  labs(x = "Quarter", 
       y = "Average Median Weekly Earning in Current Dollars", 
       title = "Average Median Weekly Earning against Quarter in 2020", 
       caption = "This figure uses TidyTuesday's data from February 23, 2021")
```

![](TidyTuesday-Project-DV---February-28,2021_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


```r
earn %>% 
  select(year, n_persons) %>%
  group_by(year) %>%
  mutate(total_num_persons = sum(n_persons)) %>%
  ggplot(aes(x = year, y = total_num_persons/1000000000)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Year", 
       y = "Total Number of Persons Employed (in billions)", 
       title = "Total Number of Persons Employed against Year", 
       caption = "This figure uses TidyTuesday's data from February 23, 2021") +
  scale_x_discrete(limits= 2010:2020)
```

![](TidyTuesday-Project-DV---February-28,2021_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


```r
earn %>%
  select(year, ethnic_origin, median_weekly_earn) %>%
  group_by(year, ethnic_origin) %>%
  mutate(avg_median_weekly_earn = mean(median_weekly_earn)) %>% 
  ggplot(aes(x = year, y = avg_median_weekly_earn, colour = ethnic_origin)) +
  geom_point() + 
  labs(x = "Year", 
       y = "Average Median Weekly Earning in Current Dollars", 
       title = "Average Median Weekly Earning against Year", 
       colour = "Ethnic Origin", 
       caption = "This figure uses TidyTuesday's data from February 23, 2021") +
  scale_x_discrete(limits = 2010:2020) + 
  theme_minimal() 
```

![](TidyTuesday-Project-DV---February-28,2021_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

