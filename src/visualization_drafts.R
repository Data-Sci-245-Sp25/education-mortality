install.packages("tidyverse")
library(tidyverse)
install.packages("sf")
library(sf)
install.packages("viridis")
library(viridis)
install.packages("gganimate")
library(gganimate)
install.packages("gifski")
library(gifski)

common_educ <- st_read("data/common_educ.gpkg")
hs_above_rate <- st_read("data/hs_above_rate.gpkg")

# change ddeduc into a factor 
educCancerRates$ddeduc <- as.factor(educCancerRates$ddeduc)
# remove "unknown" education category (wasn't included in tidycensus)
educCancerRates <- educCancerRates |>
  filter(ddeduc!="9")
# line graphs over time by cancer type and education
ggplot(data=educCancerRates, mapping=aes(x=ddodyear, y=rate_per_100k, color=ddeduc))+
  geom_line() + facet_wrap(~category, nrow=1) + labs(title= "Cancer Rates by Education Over Time", x="Year", y="Death Rate Per 100k People") +
  theme_minimal() + labs(color = "Education Level") + scale_color_manual(values = c(
    "#D35400",  # Burnt orange
    "#C77938",  # Copper
    "#B4985C",  # Khaki brown
    "#9FA58D",  # Desaturated olive
    "#7E94A9",  # Dusty blue
    "#5D7EB2",  # Blue-gray
    "#3F5FA3",  # Rich blue
    "#2C3E50"   # Navy   
  ), labels=c("8th Grade or Less", "Some HS (No Degree)", "HS Diploma/GED", "Some College (No Degree)",
                      "Associates", "Bachelor's", "Master's", "Doctorate/Professional")) + 
  theme(
    plot.title = element_text(
      hjust = 0.5,     # 0 = left, 0.5 = center, 1 = right
      size = 15,       # Font size
      #face = "bold"    # Optional: makes it bold
      family = "Lato"
    )
  )




# colors to use for disease sites: 
"#4e5d6c"
"#5cb85c"
"#5bc0de"
"#d9534f"
"#ffc107"

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

educCancerRatesOverall$ddeduc <- as.factor(educCancerRatesOverall$dd)
ggplot(data=educCancerRatesOverall, mapping=aes(x=ddeduc, y = total_death_rate, fill=ddeduc)) +
  geom_col() + facet_wrap(~category) + labs(title= "Cancer Rates by Education", x="Education Level", y="Death Rate Per 100k People") +
  theme_minimal() + labs(color = "Education Level") + scale_fill_manual(values = c(
    "#D35400",  # Burnt orange
    "#C77938",  # Copper
    "#B4985C",  # Khaki brown
    "#9FA58D",  # Desaturated olive
    "#7E94A9",  # Dusty blue
    "#5D7EB2",  # Blue-gray
    "#3F5FA3",  # Rich blue
    "#2C3E50"   # Navy  
  ), labels=c("1: 8th Grade or Less", "2: Some HS (No Degree)", "3: HS Diploma/GED", "4: Some College (No Degree)",
              "5: Associates", "6: Bachelor's", "7: Master's", "8: Doctorate/Professional")) + 
  theme(
    plot.title = element_text(
      hjust = 0.5,     # 0 = left, 0.5 = center, 1 = right
      size = 15,       # Font size
      face = "bold",    # Optional: makes it bold
      family = "Lato"
    )
  )

# ignoring cancer types
ggplot(data=educCancerRatesOverall, mapping=aes(x=ddeduc, y = (total_death_rate), fill=ddeduc)) +
  geom_col()+ labs(title= "Total Cancer Death Rates by Education", x="Education Level", y="Total Death Rate Per 100k People") +
  theme_minimal() + labs(color = "Education Level") + scale_fill_manual(values = c(
    "#D35400",  # Burnt orange
    "#C77938",  # Copper
    "#B4985C",  # Khaki brown
    "#9FA58D",  # Desaturated olive
    "#7E94A9",  # Dusty blue
    "#5D7EB2",  # Blue-gray
    "#3F5FA3",  # Rich blue
    "#2C3E50"   # Navy
  ), labels=c("1: 8th Grade or Less", "2: Some HS (No Degree)", "3: HS Diploma/GED", "4: Some College (No Degree)",
              "5: Associates", "6: Bachelor's", "7: Master's", "8: Doctorate/Professional")) + 
  theme(
    plot.title = element_text(
      hjust = 0.5,     # 0 = left, 0.5 = center, 1 = right
      size = 15,       # Font size
      face = "bold",    # Optional: makes it bold
      family = "Lato"
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

# don't really need this bc we have line chart:
ggplot(data=cancerRatesOverall, mapping=aes(x=ddodyear, y = total_death_rate, fill=category_ordered)) +
  geom_col(position="dodge")+ labs(title= "Top Cancers Over Time", x="Year", y="Total Death Rate Per 100k People") +
  theme_minimal() + labs(fill = "Disease Site")+ scale_fill_manual(values = c(
    "#4e5d6c",
    "#5cb85c",
    "#5bc0de",
    "#d9534f",
    "#ffc107"
    ), labels=c("Lung", "Breast", "Pancreas", "Colon",
              "Lymphati or Blood")) + 
  theme(
    plot.title = element_text(
      hjust = 0.5,     # 0 = left, 0.5 = center, 1 = right
      size = 15,       # Font size
      face = "bold",    # Optional: makes it bold
      family = "Lato"
    )
  )  

# top cancers per year line chart 
ggplot(data=cancerRatesOverall, mapping=aes(x=ddodyear, y=total_death_rate, color=category))+
  geom_point() +geom_line()+ labs(title= "Top Cancers Over Time", x="Year", y="Total Death Rate Per 100k People") +
  theme_minimal() + labs(fill = "Disease Site")+ scale_color_manual(values = c(
    "#4e5d6c",
    "#5cb85c",
    "#5bc0de",
    "#d9534f",
    "#ffc107" 
  ), labels=c("Breast", "Colon", "Lung", "Lymphatic or Blood",
              "Pancreas")) + 
  theme(
    plot.title = element_text(
      hjust = 0.5,     # 0 = left, 0.5 = center, 1 = right
      size = 15,       # Font size
      face = "bold",    # Optional: makes it bold
      family = "Lato"
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
      face = "bold",    # Optional: makes it bold
      family = "Lato"
    ))

# Fit the model
model <- lm(total_death_rate ~ ddeduc, data = educRatesOverall)
# View the summary
summary(model) # p-value is .03624: at alpha=.05 this is significant
confint(model)


# to do: correlation separated by cancer types
educCancerRatesOverall$ddeduc <- as.numeric(educCancerRatesOverall$ddeduc)

r2_labels <- educCancerRatesOverall |>
  group_by(category) |>
  do({
    model <- lm(total_death_rate ~ ddeduc, data = .)
    r2 <- summary(model)$r.squared
    tibble(
      category = unique(.$category),
      label = paste0("R² = ", round(r2, 2)),
      x = max(.$ddeduc),      # position for label on x-axis
      y = predict(model, newdata = data.frame(ddeduc = max(.$ddeduc)))  # y value from model
    )
  })

ggplot(data=educCancerRatesOverall, mapping=aes(x=ddeduc, y=total_death_rate, color=category))+
  geom_point() + geom_smooth(method="lm", se=FALSE) + labs(title="Education Level vs Death Rate by Disease Site", x="Education Level", y="Avg Death Rate Per 100k")+
  theme(
    plot.title = element_text(
      hjust = 0.5,     # 0 = left, 0.5 = center, 1 = right
      size = 15,       # Font size
      face = "bold",    # Optional: makes it bold
      family = "Lato"
    ))+ labs(color = "Disease Site")+ scale_color_manual(values = c(
      "#4e5d6c",
      "#5cb85c",
      "#5bc0de",
      "#d9534f",
      "#ffc107"  
    ), labels=c("Breast", "Colon", "Lung", "Lymphatic or Blood",
                "Pancreas"))+
  geom_text(data = r2_labels, aes(x = x, y = y, label = label, color = category),
            hjust = 0.15, vjust = -0.5, size = 3.5, show.legend = FALSE)

# Fit the model
model <- lm(total_death_rate ~ ddeduc, data = educCancerRatesOverall)
# View the summary
summary(model)

# LUNG: R^2=.601, p-value=.02381
model <- educCancerRatesOverall |>
  subset(category == "Lung") |>
  lm(total_death_rate ~ ddeduc, data = _)
summary(model)
# BREAST: R^2=.1195, p-value=.4017
model <- educCancerRatesOverall |>
  subset(category == "Breast") |>
  lm(total_death_rate ~ ddeduc, data = _)
summary(model)
# COLON: R^2=.5399, p-value=.03786
model <- educCancerRatesOverall |>
  subset(category == "Colon") |>
  lm(total_death_rate ~ ddeduc, data = _)
summary(model)
# LYMPHATIC OR BLOOD: R^2=.6498, p-value=.01568
model <- educCancerRatesOverall |>
  subset(category == "Lymphatic or Blood") |>
  lm(total_death_rate ~ ddeduc, data = _)
summary(model)
# PANCREAS: R^2=.5734, p-value=.02958
model <- educCancerRatesOverall |>
  subset(category == "Pancreas") |>
  lm(total_death_rate ~ ddeduc, data = _)
summary(model)

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
