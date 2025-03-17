install.packages("tidyverse")
library(tidyverse)

# change ddeduc into a factor 
educCancerRates$ddeduc <- as.factor(educCancerRates$ddeduc)
# line graphs over time by cancer type and education
ggplot(data=educCancerRates, mapping=aes(x=ddodyear, y=rate_per_100k, color=ddeduc))+
  geom_line() + facet_wrap(~category)

# overall counts (no rates)
educCancerRatesOverall <- educCancerRates |>
  group_by(category, ddeduc) |>
  summarize(total_deaths = sum(total, na.rm = TRUE)) |>
  ungroup()
ggplot(data=educCancerRatesOverall, mapping=aes(x=ddeduc, y = total_deaths, fill=ddeduc)) +
  geom_col() + facet_wrap(~category)

# try again with rates
educCancerRatesOverall <- educCancerRates |>
  group_by(category, ddeduc) |>
  summarize(total_death_rate = sum(rate_per_100k, na.rm = TRUE)/10) |>
  ungroup()
ggplot(data=educCancerRatesOverall, mapping=aes(x=ddeduc, y = total_death_rate, fill=ddeduc)) +
  geom_col() + facet_wrap(~category)
