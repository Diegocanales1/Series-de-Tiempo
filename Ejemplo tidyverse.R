library(tidyverse)
library(fable)
library(tsibble)

pop <- read_csv("https://raw.githubusercontent.com/daniel-nuno/time_series_s2024/main/Intro%20a%20R/world_bank_pop.csv")
head(pop)

pop_tidy <- pop %>%
  # i)
  pivot_longer(cols = -c(country, indicator),
               names_to = "year", values_to = "value") %>%
  ## ii)
  pivot_wider (names_from = indicator,
               values_from = value) %>%
  # iii)
  select(country,year,contains("TOTL")) %>%
  # iv)
  rename(urban_pop = SP.URB.TOTL, total_pop = SP.POP.TOTL) %>%
  #v )
  mutate(rural_pop_pct = (1- urban_pop / total_pop)*100,
         country = as_factor(country),
         year = as.integer(year)
  ) %>%
  #vi)
  filter(country %in% c("MEX", "BRA", "ARG")) %>%
  #vii)
  as_tsibble(key = country, index = year)

pop_tidy

pop_train <- pop_tidy %>%
  filter(year <= 2009)
pop_query <- pop_tidy %>%
  filter(year > 2009 & year <= 2013)
pop_train_query <- pop_tidy %>%
  filter(year <= 2013)

#Total population plot
pop_train %>%
  autoplot(total_pop) + ggtitle("Total population") + 
  ylab("")


# Rural
pop_train %>%
  autoplot(rural_pop_pct) + ggtitle("Rural population (%)") + 
  ylab("")

pop_fit <- pop_train %>%
  model('RW w/ drift' = RW(rural_pop_pct ~ drift()),
        'TSLM w/ trend' = TSLM(rural_pop_pct ~ trend()),
        ETS = ETS(rural_pop_pct ~ error("A") + trend("A") + season("N"))
        )
tidy(pop_fit)

pop_fcst <- pop_fit %>%
  forecast(h = "4 years")

pop_fcst %>%
  autoplot(pop_train_query) + 
  facet_grid(cols = vars(.model), rows = vars(country), scales = "free_y") + 
  guides(color = FALSE) + 
  ylab("Rural population (%)")

pop_fit2 <- pop_train %>%
  model('RW w/ drift' = RW(rural_pop_pct ~ drift()),
        'TSLM w/ trend' = TSLM(rural_pop_pct ~ trend()),
        ETS = ETS(rural_pop_pct ~ error("A") + trend("Ad") + season("N"))
  )
tidy(pop_fit)

pop_fcst2 <- pop_fit2 %>%
  forecast(h = "4 years")

pop_fcst2 %>%
  autoplot(pop_train_query) + 
  facet_grid(cols = vars(.model), rows = vars(country), scales = "free_y") + 
  guides(color = FALSE) + 
  ylab("Rural population (%)")

accuracy(pop_fcst2, pop_train_query) %>%
  arrange(country, MAPE)

pop_train %>%
  model(ETS = ETS(rural_pop_pct ~ error("A") + trend("Ad") + season("N") )
        ) %>%
  forecast(h = "12 years") %>%
  autoplot(pop_tidy) + 
  geom_vline(xintercept = 2014, linetype = "dashed", color = "red") + 
  ylab("Rural population (%)")
