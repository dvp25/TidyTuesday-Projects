---
title: "TidyTuesday Project - February 21, 2021"
author: "Domenica Vega"
date: "February 21, 2021"
output: 
  html_document:
    keep_md: true 
---



# Loading the necessary packages 

```r
library(tidyverse)
```

# Getting TidyTuesday data from https://github.com/rfordatascience/tidytuesday 

I got the data from https://github.com/rfordatascience/tidytuesday, particularly from https://github.com/rfordatascience/tidytuesday/tree/master/data/2021/2021-02-16 (from the section titled "Get the data here"). This week's data is about Dubois Challenge. Its goal is to re-create data visualizations from the 1900 Paris Exposition. These data visualizations won't be re-created in this TidyTuesday Project. 


```r
tuesdata <- tidytuesdayR::tt_load('2021-02-16')
```

```
## 
## 	Downloading file 1 of 8: `freed_slaves.csv`
## 	Downloading file 2 of 8: `census.csv`
## 	Downloading file 3 of 8: `furniture.csv`
## 	Downloading file 4 of 8: `city_rural.csv`
## 	Downloading file 5 of 8: `income.csv`
```

```
## Warning: 1 parsing failure.
## row col  expected    actual         file
##   1  -- 7 columns 6 columns <raw vector>
```

```
## 	Downloading file 6 of 8: `occupation.csv`
## 	Downloading file 7 of 8: `conjugal.csv`
## 	Downloading file 8 of 8: `georgia_pop.csv`
```

```r
georgia_pop <- tuesdata$georgia_pop
census <- tuesdata$census
furniture <- tuesdata$furniture # the variable household value (dollars) represents the furniture value. 
city_rural <- tuesdata$city_rural
income <- tuesdata$income # the variable actual average is the actual average income.
freed_slaves <- tuesdata$freed_slaves # the variables slave & free are expressed using proportions. 
occupation <- tuesdata$occupation
conjugal <- tuesdata$conjugal # the variables single, married, & divorced and widowed are expressed using percentages.
```

Important Note: The individual in charge of providing the data has left "the data as it was found/presented in 1900, and specifically the use of the offensive term "colored" for Black/African-American." (as noted in https://github.com/rfordatascience/tidytuesday/tree/master/data/2021/2021-02-16 under the Data Dictionary section). 

# Let's get started! 


```r
georgia_pop_tidy <- georgia_pop %>% 
  pivot_longer(c(Colored, White), names_to = "Race", values_to = "Population") %>%
  mutate(Race = str_replace(Race, "Colored", "African-American"))

max_population <- max(georgia_pop_tidy$Population)

georgia_pop_tidy %>% 
  ggplot(aes(x = Year, y = Population, fill = Race)) +
  geom_bar(stat = "identity") + 
  theme_minimal() +
  labs(title = "Population in Georgia by race", 
       caption = "This figure uses TidyTuesday's data from February 16, 2021") 
```

![](TidyTuesday-Project-DV---February-21,-2021_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


```r
census_data <- census %>% 
  filter(year %in% c(1860, 1870)) %>% # the chosen year is 1870 since "...the 13th Amendment "officially abolished" slavery in 1865" and the data only includes 1860 and 1870
  mutate(region = str_replace(region, "USA Total", "USA")) %>%
  mutate(region = str_replace(region, "Northeast", "NE")) %>%
  pivot_longer(c(white, black_free, black_slaves), names_to = "race", values_to = "population") %>%
  mutate(race = str_replace(race, "black_free", "african_american_free")) %>%
  mutate(race = str_replace(race, "black_slaves", "african_american_slaves"))
  #group_by(year, region) %>%
  
census_data %>% 
  ggplot(aes(x = region, y = population, fill = race)) +
  geom_bar(stat = "identity") +
  facet_wrap(~year) +
  theme_minimal() + 
  labs(title = "Classification of population in each region", 
       x = "Region", 
       y = "Population", 
       caption = str_c("This figure uses TidyTuesday's data from February 16, 2021\n", "Note: NE stands for Northeast"))
```

![](TidyTuesday-Project-DV---February-21,-2021_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


```r
occupation %>% 
  mutate(Group = str_replace(Group, "Negroes", "African-Americans")) %>%
  ggplot(aes(x = Group, y = Percentage)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~Occupation) + 
  theme_minimal() + 
  labs(title = "Occupations by racial group", 
       x = "Racial Group",
       caption = "This figure uses TidyTuesday's data from February 16, 2021")
```

![](TidyTuesday-Project-DV---February-21,-2021_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


