install.packages("tidyverse")
library(tidyverse)
install.packages("sf")
library(sf)
install.packages("viridis")
library(viridis)

common_educ <- st_read("data/common_educ.gpkg")
hs_above_rate <- st_read("data/hs_above_rate.gpkg")

# change ddeduc into a factor 
educCancerRates$ddeduc <- as.factor(educCancerRates$ddeduc)
# remove "unknown" education category (wasn't included in tidycensus)
educCancerRates <- educCancerRates |>
  filter(ddeduc!="9")
# line graphs over time by cancer type and education
ggplot(data=educCancerRates, mapping=aes(x=ddodyear, y=rate_per_100k, color=ddeduc))+
  geom_line() + facet_wrap(~category) + labs(title= "Cancer Rates by Education Over Time", x="Year", y="Death Rate Per 100k People") +
  theme_minimal() + labs(color = "Education Level") + scale_color_manual(values = c(
    "#E69F00",  # orange
    "#56B4E9",  # sky blue
    "#009E73",  # teal
    "#F0E442",  # yellow
    "#0072B2",  # blue
    "#D55E00",  # reddish orange
    "#CC79A7",  # pink
    "#999999"   # gray
  ), labels=c("8th Grade or Less", "HS (no diploma)", "HS Diploma/GED", "Some College",
                      "Associates", "Bachelor's", "Masters", "Doctorate/Professional")) + 
  theme(
    plot.title = element_text(
      hjust = 0.5,     # 0 = left, 0.5 = center, 1 = right
      size = 15,       # Font size
      #face = "bold"    # Optional: makes it bold
    )
  )

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
  geom_col() + facet_wrap(~category) + labs(title= "Cancer Rates by Education", x="Education Level", y="Death Rate Per 100k People") +
  theme_minimal() + labs(color = "Education Level") + scale_fill_manual(values = c(
    "#E69F00",  # orange
    "#56B4E9",  # sky blue
    "#009E73",  # teal
    "#F0E442",  # yellow
    "#0072B2",  # blue
    "#D55E00",  # reddish orange
    "#CC79A7",  # pink
    "#999999"   # gray
  ), labels=c("1: 8th Grade or Less", "2: HS (no diploma)", "3: HS Diploma/GED", "4: Some College",
              "5: Associates", "6: Bachelor's", "7: Masters", "8: Doctorate/Professional")) + 
  theme(
    plot.title = element_text(
      hjust = 0.5,     # 0 = left, 0.5 = center, 1 = right
      size = 15,       # Font size
      #face = "bold"    # Optional: makes it bold
    )
  )

# ignoring cancer types
ggplot(data=educCancerRatesOverall, mapping=aes(x=ddeduc, y = (total_death_rate), fill=ddeduc)) +
  geom_col()+ labs(title= "Total Cancer Rates by Education", x="Education Level", y="Total Death Rate Per 100k People") +
  theme_minimal() + labs(color = "Education Level") + scale_fill_manual(values = c(
    "#E69F00",  # orange
    "#56B4E9",  # sky blue
    "#009E73",  # teal
    "#F0E442",  # yellow
    "#0072B2",  # blue
    "#D55E00",  # reddish orange
    "#CC79A7",  # pink
    "#999999"   # gray
  ), labels=c("1: 8th Grade or Less", "2: HS (no diploma)", "3: HS Diploma/GED", "4: Some College",
              "5: Associates", "6: Bachelor's", "7: Masters", "8: Doctorate/Professional")) + 
  theme(
    plot.title = element_text(
      hjust = 0.5,     # 0 = left, 0.5 = center, 1 = right
      size = 15,       # Font size
      #face = "bold"    # Optional: makes it bold
    )
  ) # level 3 has the most death rates

# top cancers per year 
cancerRatesOverall <- educCancerRates |>
  group_by(category, ddodyear) |>
  summarize(total_death_rate = sum(rate_per_100k, na.rm = TRUE)/9) |> 
  arrange(desc(total_death_rate))|>
  ungroup()

cancerRatesOverall <- cancerRatesOverall |>
  group_by(ddodyear) |>
  mutate(category_ordered = fct_reorder2(category, ddodyear, total_death_rate))

ggplot(data=cancerRatesOverall, mapping=aes(x=ddodyear, y = total_death_rate, fill=category_ordered)) +
  geom_col(position="dodge")+ labs(title= "Top Cancers Over Time", x="Year", y="Total Death Rate Per 100k People") +
  theme_minimal() + labs(fill = "Disease Site")+ scale_fill_manual(values = c(
      "#E69F00",  # orange
      "#56B4E9",  # sky blue
      "#009E73",  # teal green
      "#D55E00",  # reddish orange
      "#CC79A7"   # pink/magenta
    ), labels=c("Lung", "Breast", "Pancreas", "Colon",
              "Lymphati or Blood")) + 
  theme(
    plot.title = element_text(
      hjust = 0.5,     # 0 = left, 0.5 = center, 1 = right
      size = 15,       # Font size
      #face = "bold"    # Optional: makes it bold
    )
  )  # maybe try reording bars to make it easier to see

# top cancers per year line chart 
ggplot(data=cancerRatesOverall, mapping=aes(x=ddodyear, y=total_death_rate, color=category))+
  geom_line()+ labs(title= "Top Cancers Over Time", x="Year", y="Total Death Rate Per 100k People") +
  theme_minimal() + labs(fill = "Disease Site")+ scale_color_manual(values = c(
    "#56B4E9",  
    "#D55E00",  
    "#E69F00",  
    "#CC79A7", 
    "#009E73"   
  ), labels=c("Breast", "Colon", "Lung", "Lymphatic or Blood",
              "Pancreas")) + 
  theme(
    plot.title = element_text(
      hjust = 0.5,     # 0 = left, 0.5 = center, 1 = right
      size = 15,       # Font size
      #face = "bold"    # Optional: makes it bold
    )
  ) 

# scatter plot/correlation between education and death rate
educRatesOverall <- educCancerRates |>
  group_by(ddeduc) |>
  summarize(total_death_rate = sum(rate_per_100k, na.rm = TRUE)/50) |> 
  arrange(desc(total_death_rate))|>
  ungroup()
educRatesOverall$ddeduc <- as.numeric(educRatesOverall$ddeduc)
ggplot(data=educRatesOverall, mapping=aes(x=ddeduc, y=total_death_rate))+
  geom_point() + geom_smooth(method="lm") + labs(title="Education Level vs Cancer Death Rate", x="Education Level", y="Avg Death Rate Per 100k")+
  theme(
    plot.title = element_text(
      hjust = 0.5,     # 0 = left, 0.5 = center, 1 = right
      size = 15,       # Font size
      #face = "bold"    # Optional: makes it bold
    ))
# correlation separated by cancer types
educCancerRatesOverall$ddeduc <- as.numeric(educCancerRatesOverall$ddeduc)
ggplot(data=educCancerRatesOverall, mapping=aes(x=ddeduc, y=total_death_rate, color=category))+
  geom_point() + geom_line()+ labs(title="Education Level vs Death Rate by Disease Site", x="Education Level", y="Avg Death Rate Per 100k")+
  theme(
    plot.title = element_text(
      hjust = 0.5,     # 0 = left, 0.5 = center, 1 = right
      size = 15,       # Font size
      #face = "bold"    # Optional: makes it bold
    ))+ labs(color = "Disease Site")+ scale_color_manual(values = c(
      "#56B4E9",  
      "#D55E00",  
      "#E69F00",  
      "#CC79A7", 
      "#009E73"   
    ), labels=c("Breast", "Colon", "Lung", "Lymphatic or Blood",
                "Pancreas"))

# MAPS (may need to re-merge data with tidycensus...)
#ggplot() + geom_sf(data= educCancerRates, aes(fill=educCancerRates$rate_per_100k))

# EDUCATION MAPS
ggplot(common_educ) + geom_sf(mapping=aes(fill = variable))+ labs(title="Most Common Education Level by Tract")+
  theme(
    plot.title = element_text(
      hjust = 0.5,     # 0 = left, 0.5 = center, 1 = right
      size = 15,       # Font size
      #face = "bold"    # Optional: makes it bold
    ))+ labs(fill = "Most Common Education Level")+ scale_fill_manual(values = c(
      "#56B4E9",  
      "#CC79A7", 
      "#009E73"   
    ), labels=c("Bachelor's", "HS Diploma", "Some College"))

ggplot(hs_above_rate) + geom_sf(mapping=aes(fill = rate))+ scale_fill_viridis(option = "plasma") +
  theme_minimal()+ labs(title="Percentage of Population with HS Diploma")+
  theme(
    plot.title = element_text(
      hjust = 0.5,     # 0 = left, 0.5 = center, 1 = right
      size = 15,       # Font size
      #face = "bold"    # Optional: makes it bold
    ))
