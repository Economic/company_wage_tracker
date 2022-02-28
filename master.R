library(tidyverse)
library(haven)
library(scales)
library(jsonlite)

# from shift project
raw_data <- read_dta("shift_wage_data.dta") %>% 
  mutate(across(matches("hardship_lastmonth"), ~ replace_na(.x, 0))) %>% 
  mutate(employer = as.character(as_factor(employer))) %>% 
  mutate(employer = str_remove(employer, "[0-9]+ ")) 

raw_data %>% 
  count(employer) %>% 
  select(shift_employer = employer) %>% 
  write_csv("shift_employer_names.csv")

# misc data units are raw units (not 1000s, etc.)
employer_misc <- read_csv("employer_info_misc.csv") %>% 
  select(
    shift_employer = employer,
    empl_misc = employment_level,
    sales_misc = revenue_level
  )

employer_characteristics <- read_csv("employer_info_compustat.csv") %>%
  # compustat data units are:
  # sales = millions
  # ceo pay = thousands
  # employment = thousands
  transmute(
    shift_employer,
    sales = sales * 10^6,
    ceo_pay = real_dir_comp * 1000,
    empl = empl * 1000
  ) %>% 
  # supplement with misc sources
  full_join(employer_misc, by = "shift_employer") %>% 
  mutate(sales = if_else(is.na(sales), sales_misc, sales)) %>% 
  mutate(empl = if_else(is.na(empl), empl_misc, empl)) %>% 
  mutate(across(sales|empl|ceo_pay, ~ comma(.x, accuracy = 1))) %>% 
  transmute(
    employer = shift_employer,
    revenue = sales,
    employment = empl,
    ceo_pay
  ) %>% 
  arrange(employer)

# company-specific pdf
wages_pdf <- raw_data %>% 
  rename(wage = hourlywage_including_tips) %>% 
  mutate(wage_bin = case_when(
    wage < 10.00 ~ "Under $10",
    wage >= 10.00 & wage < 12.00 ~ "$10 - $12",
    wage >= 12.00 & wage < 14.00 ~ "$12 - $14",
    wage >= 14.00 & wage < 16.00 ~ "$14 - $16",
    wage >= 16.00 & wage < 18.00 ~ "$16 - $18",
    wage >= 18.00 & wage < 20.00 ~ "$18 - $20",
    wage >= 20.00 ~ "At least $20"
  )) %>% 
  count(employer, wage_bin) %>%
  pivot_wider(employer, names_from = wage_bin, values_from = n) %>% 
  mutate(across(-employer, ~ replace_na(.x, 0))) %>% 
  pivot_longer(-employer, names_to = "wage_bin") %>% 
  group_by(employer) %>% 
  mutate(share = value / sum(value)) %>% 
  ungroup() %>% 
  select(employer, wage_bin, share) %>% 
  mutate(share = round(share, 2)) %>% 
  pivot_wider(employer, names_from = wage_bin, values_from = share) %>% 
  select(
    employer,
    "Under $10",
    "$10 - $12",
    "$12 - $14",
    "$14 - $16",
    "$16 - $18",
    "$18 - $20",
    "At least $20"
  ) %>% 
  arrange(employer)

# company-specific cdf
wages_cdf <- raw_data %>% 
  rename(wage = hourlywage_including_tips) %>% 
  mutate(
    wage_bin = as.integer(floor(wage)),
    wage_bin = case_when(
      wage < 10.00 ~ 9L,
      wage > 20.00 ~ 20L,
      TRUE ~ wage_bin
    )
  ) %>% 
  count(employer, wage_bin) %>%
  pivot_wider(employer, names_from = wage_bin, values_from = n) %>% 
  mutate(across(-employer, ~ replace_na(.x, 0))) %>% 
  pivot_longer(-employer, names_to = "wage_bin") %>% 
  mutate(wage_bin = as.integer(wage_bin) + 1) %>% 
  arrange(employer, wage_bin) %>% 
  group_by(employer) %>% 
  mutate(cdf = cumsum(value) / sum(value)) %>% 
  ungroup() %>% 
  filter(wage_bin != 21) %>% 
  mutate(wage_bin = paste0("Under $", wage_bin)) %>% 
  mutate(cdf = round(cdf, 2)) %>% 
  pivot_wider(employer, names_from = wage_bin, values_from = cdf) %>% 
  arrange(employer)

# json output for web app
final_data <- wages_pdf %>% 
  full_join(employer_characteristics, by = "employer") %>% 
  select(-`Under $10`) %>% 
  full_join(wages_cdf, by = "employer")

final_data %>% 
  toJSON() %>% 
  write("data.json")



  