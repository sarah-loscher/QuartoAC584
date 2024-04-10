# install.packages("tidyverse")
# 

install.packages("plotly")

library(tidyverse)
library(plotly)

#data
unicef_indicator_1 <- read_csv("unicef_indicator_1.csv")
unicef_metadata <- read_csv("unicef_metadata.csv")

# manual_join <- full_join(unicef_metadata, unicef_indicator_1, by = c("country" = "country", "year" = "time_period"))

options(scipen = 999)

# install.packages("maps")

# data_join <- unicef_metadata %>%
#   full_join(unicef_indicator_1, by = c("country" = "country", "year" = "time_period"))


#MAP 1 of population
# unicef_indicator_1 <- read_csv("unicef_indicator_1.csv")
# unicef_metadata <- read_csv("unicef_metadata.csv")
metadata <- unicef_metadata %>%
  rename('population' = 'Population, total', 'LifeExp'= "Life expectancy at birth, total (years)", 'gdppercap' = "GDP per capita (constant 2015 US$)" )

full_join <- full_join(unicef_indicator_1, metadata, by = c("time_period" = "year", "country" = "country" ))
full_join_2021 <- filter(full_join, time_period == 2021)
full_join_2021_countries <- filter(full_join, 
                                   !(country %in% c("India", "China", "Egypt", "Kenya", "Mexico", "United States")))
full_join_countries <- filter(full_join, !(country %in% c("India", "China", "Egypt", "Kenya", "Mexico", "United States")))

map_world <- map_data("world")
map_join_2021 <- full_join(full_join_2021, map_world, by = c("country" = "region"))
  
  ggplot(data = map_join_2021) +
    aes(x = long, y = lat, group = group, fill = population) +
    geom_polygon() +
    scale_fill_gradient(low = "pink", high = "red", na.value = "grey") +
    labs(
      title = "Countries and their population in 2021",
      subtitle = "Countries in grey have no data, red means a higher population", 
      x = "Longitude",
      y = "Latitude",
      fill = "Country Population"
    ) +
    theme_bw()

#Map 2 obs value
map_world <- map_data("world")
  # full_join <- full_join(unicef_indicator_1, metadata, by = c("time_period" = "year", "country" = "country" ))

map_join_xobv <- full_join(unicef_indicator_1, map_world, by = c("country" = "region"))

ggplot(data = map_join_xobv) +
    aes(x = long, y = lat, group = group, fill = obs_value) +
    geom_polygon() +
    scale_fill_gradient(low = "yellow", high = "red", na.value = "grey") +
  labs(
    title = "Countries and their percentage of children with diarrhoea who received ORT over the past 20 years",
    subtitle = "Countries in grey have no data, red means a higher population", 
    x = "Longitude",
    y = "Latitude",
    fill = "Indicator %"
  ) +
  theme_bw()

  #Time Series of population
timeseries_plot_1 <- metadata %>%
    ggplot() +
    aes(year, population, group = country, colour = country) +
    geom_line() +
  labs(
    title = "Time series showing the population over the past 20 years",
    subtitle = "Different countries shown in different colours", 
    x = "Population",
    y = "Year",
    fill = "Indicator %"
  ) +
  theme_bw() +
  guides(colour = "none") +
  text = element_text(family = 'Arial')

ggplotly(timeseries_plot_1)


timeseries_plot_1 <- metadata %>%
  ggplot() +
  aes(year, population, group = country, colour = country) +
  geom_line() +
  labs(
    title = "Time series showing the population over the past 20 years",
    subtitle = "Different countries shown in different colours", 
    x = "Year",
    y = "Population"
  ) +
  theme_bw() +
  theme(legend.position = "right",
        text = element_text(family = 'Arial')) +
  scale_color_manual(name = "Country", 
                     values = rainbow(length(unique(metadata$country))),
                     breaks = unique(metadata$country))

ggplotly(timeseries_plot_1)
  

  #Scatterplot of obv value and Lifeexp

scatterplot_1 <- ggplot(full_join_2021_countries) +
  aes(LifeExp,obs_value,color = country, size = population) +
  geom_point(alpha = 0.5) +
  labs(
    title = "Scatterplot of life expectancy against the percentage of children who received ORT in 2021",
    x = "Life expectancy at birth",
    y = "Indicator %"
  ) +
  theme_bw() +
guides(colour = "none") +
text = element_text(family = 'Arial')
  guides(colour = "none") 
  
ggplotly(scatterplot_1)


scatterplot_1 <- ggplot(full_join_2021_countries) +
  aes(LifeExp, obs_value, color = country, size = population) +
  geom_point(alpha = 0.5) +
  labs(
    title = "Scatterplot of life expectancy against the percentage of children who received ORT in 2021",
    x = "Life expectancy at birth",
    y = "Indicator %"
  ) +
  theme_bw() +
  facet_wrap(~ time_period) +
  guides(colour = "none") +  
  theme(text = element_text(family = 'Arial'))

ggplotly(scatterplot_1)

  #Barchart of life expt
newfilter_2021_countries <- filter(full_join, 
                                   time_period == 2021, 
                                   country %in% c("India", "China", "Egypt", "Kenya", "Mexico", "United States"))

barchart <- newfilter_2021_countries %>%
  group_by(country) %>%
  summarise(mean_lifeexp = mean(LifeExp, na.rm = TRUE)) %>%
  ggplot() +
  aes(reorder(country, mean_lifeexp), mean_lifeexp, fill = country) +
  geom_col() +
  labs(x = "Country", 
       y = "Average Life Expectancy", 
       title = "Barchart of average life expectancy") +
  theme_bw() +
  theme(text = element_text(family = 'Arial'))

barchart_plotly <- ggplotly(barchart)

barchart <- newfilter_2021_countries %>%
  group_by(country) %>%
  summarise(mean_lifeexp = mean(LifeExp, na.rm = TRUE)) %>%
  ggplot() +
  aes(reorder(country, mean_lifeexp), mean_lifeexp, fill = country) +
  geom_col() +
  labs(x = "Country", 
       y = "Average Life Expectancy", 
       title = "Barchart of average life expectancy") +
  theme_bw() +
  scale_fill_manual(values = c( "purple","lightblue", "lightgreen", "lightpink", "orange", "red"))
theme(text = element_text(family = 'Arial'))
