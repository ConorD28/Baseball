#12/1 Class - Animated Plots
install.packages("gganimate")
install.packages("png")
install.packages("gifski")
library(ggplot2)
library(dplyr)
library(png)
library(gifski)
library(gganimate)

life=read.csv("https://foxweb.marist.edu/users/duy.nguyen2/LifeExp.csv",header=TRUE)
head(life)
tail(life)
graph1 <- life %>%
  ggplot() + 
  geom_point(aes(x = gdpPercap, y = lifeExp,
                 col = continent, size = pop), alpha = 0.8) + 
  theme_minimal() + 
  theme(legend.position = "bottom") + 
  guides(size = "none") + 
  labs(x = "gdpPercap" ,y = "lifeExp",  col = "") 
graph1

graph1 +
  transition_time(year)

graph1 +
  transition_time(year) +
  labs(title = "Year: {frame_time}")

#plot pop vs year, size = gdp:
graph2 <- life %>% ggplot() + geom_point(aes(pop, year, col = continent, size = gdpPercap),
                                         alpha = .8) + theme_minimal() + theme(legend.position = "bottom") + guides(size = "none") + labs(x = "gdpPercap" ,y = "lifeExp",  col = "") 
graph2

graph2 + transition_time(year)

graph2 + transition_time(year) + labs(title = "Year {frame_time}")

graph1 +
  geom_text(aes(x = min(gdpPercap), y = min(lifeExp),
                label = as.factor(year)) ,
            hjust=-2, vjust = -0.2, alpha = 0.2,  
            col = "gray", size = 20) +
  transition_states(as.factor(year), state_length = 0)

#select one country and plot its gdp
life %>%
  filter(country == "United States") %>%
  ggplot(aes(year, pop)) + geom_point() + geom_line() +
  theme_minimal() +
  transition_reveal(year)

life %>%
  filter(country == "Canada") %>%
  ggplot(aes(year, pop)) + geom_point() + geom_line() +
  theme_minimal() +
  transition_reveal(year)

life %>%
  filter(country %in% c("United States", "Canada")) %>%
  ggplot(aes(year, gdpPercap, color = country)) + geom_line() +
  theme_minimal() +
  transition_reveal(year)

#view the y-x axes
life %>%
  filter(country == "United States") %>%
  ggplot(aes(year, pop)) + 
  geom_point() +
  geom_line() + 
  geom_text(aes(x = min(year), 
                y = min(pop), 
                label = as.factor(year)) ,
            hjust=-2, vjust = -0.2,
            alpha = 0.5,  col = "gray", 
            size = 20) +
  theme_minimal() +
  transition_reveal(year) + 
  view_follow()

life %>% filter(country %in% c("United States", "Canada")) %>% ggplot(aes(year, pop, color = country)) + 
  geom_point() +
  geom_line() + 
  geom_text(aes(x = min(year), 
                y = min(pop), 
                label = as.factor(year)) ,
            hjust=-2, vjust = -0.2,
            alpha = 0.5,  col = "gray", 
            size = 20) +
  theme_minimal() +
  transition_reveal(year) + 
  view_follow()

data2 <- life %>%
  group_by(year) %>%
  arrange(year, desc(gdpPercap)) %>%
  mutate(ranking = row_number()) %>%
  filter(ranking <=15)

animation <- data2 %>%
  ggplot() +
  geom_col(aes(ranking, gdpPercap, fill = country)) +
  geom_text(aes(ranking, gdpPercap, label = gdpPercap), hjust=-0.1) +
  geom_text(aes(ranking, y=0 , label = country), hjust=1.1) + 
  geom_text(aes(x=15, y=max(gdpPercap) , label = as.factor(year)), 
            vjust = 0.2, alpha = 0.5,  col = "gray", size = 20) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_x_reverse() +
  theme_minimal() + theme(
    panel.grid = element_blank(), 
    legend.position = "none",
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = margin(1, 4, 1, 3, "cm")
  ) +
  transition_states(year, state_length = 0,
                    transition_length = 2) +
  enter_fade() +
  exit_fade() + 
  ease_aes('quadratic-in-out') 

animate(animation, width = 700, height = 432,
        fps = 12, duration = 15, rewind = FALSE)

#Create similar graph for population of 5 countries:
data3 <- life %>%
  group_by(year) %>%
  arrange(year, desc(pop)) %>%
  mutate(ranking = row_number()) %>%
  filter(ranking <=5)
  
animation <- data3 %>%
  ggplot() +
  geom_col(aes(ranking, pop, fill = country)) +
  geom_text(aes(ranking, pop, label = pop), hjust=-0.1) +
  geom_text(aes(ranking, y=0 , label = country), hjust=1.1) + 
  geom_text(aes(x=15, y=max(pop) , label = as.factor(year)), 
            vjust = 0.2, alpha = 0.5,  col = "gray", size = 20) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_x_reverse() +
  theme_minimal() + theme(
    panel.grid = element_blank(), 
    legend.position = "none",
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = margin(1, 4, 1, 3, "cm")
  ) +
  transition_states(year, state_length = 0,
                    transition_length = 2) +
  enter_fade() +
  exit_fade() + 
  ease_aes('quadratic-in-out') 

animate(animation, width = 700, height = 432,
        fps = 3, duration = 20, rewind = FALSE)
