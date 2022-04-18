library(tidyverse)
library(haven)
library(scales)
library(jsonlite)

# requires the survey data from shift project
# the data is not public and not provided in this repo
raw_data <- read_dta("shift_wage_data.dta") %>% 
  # grab and clean employer names from value labels
  mutate(employer = as.character(as_factor(employer))) %>% 
  mutate(employer = str_remove(employer, "[0-9]+ ")) %>% 
  # use hourly wage inclusive of tips
  rename(wage = hourlywage_including_tips) 

# reference USA employer info
employer_info_rusa <- read_csv("employer_info_referenceusa.csv") %>% 
  select(employer, emp_rusa = employees)

# capital iq info
employer_info_ciq <- read_csv("employer_info_revenue_misc.csv") %>% 
  # restrict to year 2019 +
  filter(year >= 2019) %>% 
  filter(source == "Capital IQ") %>% 
  transmute(
    employer, 
    sales_ciq = revenue_millions * 10^6
  ) 

# compustat employer info
employer_info_cstat <- read_csv("employer_info_compustat.csv") %>% 
  transmute(
    employer = shift_employer,
    # compustat data units are:
    # sales = millions
    sales = sales * 10^6,
    # ceo pay = thousands
    ceo_pay = real_dir_comp * 1000,
    # employment = thousands
    emp_cstat = empl * 1000
  )

# combine employer info
employer_info <- employer_info_rusa %>% 
  full_join(employer_info_cstat) %>% 
  full_join(employer_info_ciq) %>% 
  # prefer employment counts : Reference USA > Compustat
  mutate(employment = if_else(!is.na(emp_rusa), emp_rusa, emp_cstat)) %>% 
  # use Capital IQ revenue counts if missing
  mutate(sales = if_else(is.na(sales), sales_ciq, sales)) %>% 
  # use 2021 CEO Amazon value from SEC filing
  # https://d18rn0p25nwr6d.cloudfront.net/CIK-0001018724/4f0d87fc-e047-4001-b977-3b3affd5de04.pdf
  mutate(ceo_pay = if_else(employer == "Amazon", 212701169, ceo_pay)) %>% 
  mutate(across(sales|employment|ceo_pay, ~ comma(.x, accuracy = 1))) %>% 
  select(employer, revenue = sales, employment, ceo_pay) 


# create company-specific pdf, $2 bins
wages_pdf <- raw_data %>% 
  # define bins
  mutate(wage_bin = case_when(
    wage < 10.00 ~ "Under $10",
    wage >= 10.00 & wage < 12.00 ~ "$10 - $12",
    wage >= 12.00 & wage < 14.00 ~ "$12 - $14",
    wage >= 14.00 & wage < 16.00 ~ "$14 - $16",
    wage >= 16.00 & wage < 18.00 ~ "$16 - $18",
    wage >= 18.00 & wage < 20.00 ~ "$18 - $20",
    wage >= 20.00 ~ "At least $20"
  )) %>% 
  # use factors just to convince count() to include zero counts
  mutate(wage_bin = as.factor(wage_bin)) %>% 
  count(employer, wage_bin, .drop = FALSE) %>% 
  # back to character bins
  mutate(wage_bin = as.character(wage_bin)) %>% 
  # calculate pdf
  group_by(employer) %>% 
  mutate(share = n / sum(n)) %>% 
  ungroup() %>% 
  select(employer, wage_bin, share) %>% 
  mutate(share = round(share, 2)) %>% 
  pivot_wider(employer, names_from = wage_bin, values_from = share) 

# create company-specific cdf, $1 increments
wages_cdf <- raw_data %>% 
  # lump wages at bottom and top
  mutate(wage = case_when(wage < 10 ~ 9, wage >= 20 ~ 20, TRUE ~ wage)) %>%
  # define bins
  mutate(wage_bin = as.integer(floor(wage)) + 1) %>% 
  # use factors just to convince count() to include zero counts
  mutate(wage_bin = as.factor(wage_bin)) %>% 
  count(employer, wage_bin, .drop = FALSE) %>%
  # back to numeric bins
  mutate(wage_bin = as.numeric(as.character(wage_bin))) %>% 
  # calculate cdf
  arrange(employer, wage_bin) %>% 
  group_by(employer) %>% 
  mutate(cdf = cumsum(n) / sum(n)) %>% 
  ungroup() %>% 
  filter(wage_bin != 21) %>% 
  mutate(wage_bin = paste0("Under $", wage_bin)) %>% 
  mutate(cdf = round(cdf, 2)) %>% 
  pivot_wider(employer, names_from = wage_bin, values_from = cdf)

# combine wages and other employer characteristics
final_data <- wages_pdf %>% 
  select(-`Under $10`) %>% 
  full_join(wages_cdf, by = "employer") %>% 
  full_join(employer_info, by = "employer") %>% 
  # tidy up employer spelling
  mutate(employer = case_when(
    employer == "Applebees" ~ "Applebee's",
    employer == "Chick-Fil-A" ~ "Chick-fil-A",
    employer == "Dunkin Donuts" ~ "Dunkin' Donuts",
    employer == "Fedex" ~ "FedEx",
    employer == "HEB" ~ "H-E-B",
    employer == "In-N-Out Burgers" ~ "In-N-Out Burger",
    employer == "Kohls" ~ "Kohl's",
    TRUE ~ employer
  )) %>% 
  arrange(employer)

# json output for web app
final_data %>% 
  toJSON() %>% 
  write("data.json")

# csv output for web download
final_data %>% 
  write_csv("company_wage_tracker_data.csv")

  