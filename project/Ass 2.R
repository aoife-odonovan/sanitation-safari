install.packages("tidyverse")
install.packages("plotly")
library(tidyverse)
library(plotly)

data_right_3_ <- read_csv("data_right_3 .csv")
unicef_indicator_1 <- read_csv("unicef_indicator_1.csv")
unicef_metadata <- read_csv("unicef_metadata.csv")

data_join <- full_join(unicef_metadata, data_right_3_)
data_join <- full_join(unicef_metadata, data_right_3_, by = join_by(country))

data_join_2 <- full_join(unicef_metadata, unicef_indicator_1)
data_join_2 <- full_join(unicef_metadata, unicef_indicator_1, by = join_by(country, year == time_period))

# final data object
data_join <- unicef_metadata %>%
  full_join(data_right_3_, by = join_by(country)) %>%
  full_join(unicef_indicator_1, by = join_by(country, year == time_period))

install.packages("maps")
map_world <- map_data("world")

# map 2021
data_join_2021 <- data_join %>%
  filter(year == 2021)

map_data_join_2021 <- full_join(data_join_2021, map_world, by = join_by(country == region))

ggplot(map_data_join_2021) +
  aes(x = long, y = lat, group = group, fill = lifeExp) +
  geom_polygon() +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Life Expectancy in 2021",
    fill = "Age"
  ) +
  theme_bw() +
  theme(text = element_text(family = "serif"))


# map 2000
data_join_2000 <- data_join %>%
  filter(year == 2000)

map_data_join_2000 <- full_join(data_join_2000, map_world, by = join_by(country == region))

ggplot(map_data_join_2000) +
  aes(x = long, y = lat, group = group, fill = lifeExp) +
  geom_polygon() +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Life Expectancy in 2000",
    fill = "Age"
  ) +
  theme_bw() +
  theme(text = element_text(family = "serif"))

# time series 1
timeseries_plot_1 <- data_join %>%
  ggplot() +
  aes(year, lifeExp, group = country, color = continent) +
  geom_line() +
  labs(
    x = "Year",
    y = "Life Expectancy (years)",
    title = "Evolution of Life Expectancy"
  ) +
  theme_bw() +
  theme(text = element_text(family = "serif"))

ggplotly(timeseries_plot_1)

# scatter plot 1
scatter_plot_1 <- data_join %>%
  filter(year == 2021)
  
ggplot(scatter_plot_1) +
  aes(gdp, obs_value, color = continent, size = pop) +
  geom_point(alpha = 0.75) +
  scale_x_continuous(limits = c(0,100000)) +
  labs(
    x = "GDP per capita in USD",
    y = "Schools with basic sanitation services (%)",
    title = "Relationship between GDP per capita and Proportion of Schools with Basic Sanitation Services - 2021"
  ) +
  guides(size = "none") +
  theme_bw() +
  theme(text = element_text(family = "serif"))
 
# scatter plot 2
scatter_plot_2 <- data_join

years_to_show <- c(2005, 2010, 2015, 2020, 2021)

filtered_data <- scatter_plot_2[scatter_plot_2$year %in% years_to_show, ]

ggplot(filtered_data) +
  aes(lifeExp, obs_value, color = continent, size = pop) +
  geom_point(alpha = 0.75) +
  facet_wrap(~ year, nrow = 1) +
  labs(
    x = "Life Expectancy (years)",
    y = "Schools with basic sanitation services (%)",
    title = "Relationship between Life Expectancy and Proportion of Schools with Basic Sanitation Services"
  ) +
  guides(size = "none") +
  theme_bw() +
  theme(text = element_text(family = "serif"))

# bar chart 1
bar_chart_1 <- data_join

years_to_show <- c(2000, 2021)

filtered_data <- bar_chart_1[bar_chart_1$year %in% years_to_show, ]

filtered_data %>%
  group_by(continent, year) %>%
  summarise(m_obs_value = mean(obs_value, na.rm = TRUE)) %>%
  ggplot() +
  aes(reorder(continent, m_obs_value), m_obs_value, fill = continent) +
  geom_col() +
  facet_wrap(~ year) +
  labs(
    x = "",
    y = "Schools with basic sanitation services (%)",
    title = "Proportion of Schools with Basic Sanitation Services"
  ) +
  theme_bw() +
  theme(text = element_text(family = "serif"),
  axis.text.x = element_blank()      
  )








