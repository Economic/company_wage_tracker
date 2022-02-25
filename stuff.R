library(epiextractr)
library(MetricsWeighted)

p <- seq(5, 95, 5)

org_data <- load_org(2021) %>%
  filter(wage > 0) %>% 
  filter(
    (ind17 >= 4690 & ind17 <= 5790) |
    (ind17 >= 8570 & ind17 <= 9090)
  ) %>% 
  mutate(data = "org") %>% 
  select(wage = wage, data, orgwgt)
  

raw_data %>% 
  rename(wage = hourlywage_including_tips) %>% 
  mutate(data = "shift", orgwgt = 1) %>% 
  bind_rows(org_data) %>% 
  group_by(data) %>% 
  summarize(p, value = weighted_quantile(wage, w = orgwgt, probs = p / 100)) %>% 
  #pivot_wider(p, names_from = data) %>% 
  #mutate(value = org / shift - 1) %>% 
  ggplot(aes(x = p, y = value, color = data)) +
  #ggplot(aes(x = p, y = value, color = data)) + 
  geom_line() +
  geom_point()


break

  

bind_rows(raw_data) %>% 
  mutate(if_else(is.na(data, "org", data))) %>% 
  mutate(under15 = wage < 10) %>% 
  group_by(employer) %>% 
  summarize(estimate = list(binom.confint(
    sum(under15), 
    n(), 
    conf.level = 0.95, 
    methods = "ac"))) %>% 
  unnest(estimate)


