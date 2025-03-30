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

# ignoring cancer types
ggplot(data=educCancerRatesOverall, mapping=aes(x=ddeduc, y = (total_death_rate/5), fill=ddeduc)) +
  geom_col() # level 3 has the most death rates

# top cancers per year 
cancerRatesOverall <- educCancerRates |>
  group_by(category, ddodyear) |>
  summarize(total_death_rate = sum(rate_per_100k, na.rm = TRUE)/9) |> 
  arrange(desc(total_death_rate))|>
  ungroup()
ggplot(data=cancerRatesOverall, mapping=aes(x=ddodyear, y = total_death_rate, fill=category)) +
  geom_col(position="dodge")  # maybe try reording bars to make it easier to see

# top cancers per year line chart 
ggplot(data=cancerRatesOverall, mapping=aes(x=ddodyear, y=total_death_rate, color=category))+
  geom_line()

# scatter plot/correlation between education and death rate
educRatesOverall <- educCancerRates |>
  group_by(ddeduc) |>
  summarize(total_death_rate = sum(rate_per_100k, na.rm = TRUE)/50) |> 
  arrange(desc(total_death_rate))|>
  ungroup()
educRatesOverall$ddeduc <- as.numeric(educRatesOverall$ddeduc)
ggplot(data=educRatesOverall, mapping=aes(x=ddeduc, y=total_death_rate))+
  geom_point() + geom_smooth(method="lm")
# correlation separated by cancer types
educCancerRatesOverall$ddeduc <- as.numeric(educCancerRatesOverall$ddeduc)
ggplot(data=educCancerRatesOverall, mapping=aes(x=ddeduc, y=total_death_rate, color=category))+
  geom_point() + geom_line()

# MAPS (may need to re-merge data with tidycensus...)
ggplot() + geom_sf(data= educCancerRates, aes(fill=educCancerRates$rate_per_100k))
