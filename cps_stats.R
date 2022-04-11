library(tidyverse)
library(epiextractr)
library(epidatatools)
library(haven)

cps <- load_org(2019, emp, dind03, basicwgt, wbho, paidhre, female) %>% 
  filter(emp == 1 & basicwgt > 0) %>% 
  # retail, accom, food services
  mutate(
    my_industry = if_else(dind03 %in% c(22, 45, 46), 1, 0),
    my_industry = labelled(my_industry, c("Retail/Food" = 1, "Other" = 0)),
    black_hispanic = if_else(wbho %in% c(2,3), 1, 0),
    black_hispanic = labelled(black_hispanic, c("Black/Hispanic" = 1, "Other" = 0))
  )

cps %>% 
  crosstab(black_hispanic, my_industry, w = basicwgt, col = TRUE)

cps %>% 
  crosstab(female, my_industry, w = basicwgt, col = TRUE)