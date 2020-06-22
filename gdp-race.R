# load the data
load(url("https://github.com/bariscr/data/raw/master/gdp_per_capita_world.Rdata"))
################################################################################
# transform data - make the NA's 0
library(tidyverse)
gdp_data <- gdp_per_capita_world %>% 
  mutate(gdp_pc = 
           case_when(is.na(gdp_pc) == TRUE ~ 0,
                     TRUE ~ gdp_pc)) %>% 
  filter(country_name != "North America") %>%
  mutate(country_name = case_when(country_name == "Bahamas, The" ~ "Bahamas",
                                TRUE ~ country_name))
# give the ranks
gdp_data2 <- gdp_data %>%
  group_by(year) %>%
  mutate(rank = min_rank(-gdp_pc)) %>%
  ungroup()
# select top countries
top_countries <- gdp_data2 %>% 
  filter(rank <= 10) %>%
  select(country_name, year, rank) %>% distinct()
# check country names
top_countries %>% 
  select(country_name) %>% 
  unique() 
 
# plot
library(gganimate)
library(ggdark)
p <- ggplot(top_countries, aes(rank, group = country_name, 
                               fill = as.factor(country_name))) +
  geom_text(aes(y = 0, label = paste0(rank, ". ", country_name, " ")),
            hjust = "center", fontface = "bold", size = 8) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  dark_theme_minimal() +
  labs(title='{closest_state}', caption="Data: World Bank", x = "", y = "") +
  theme(plot.title = element_text(hjust = 0, size = 40),
        axis.ticks.y = element_blank(),  
        axis.text.y  = element_blank(),  
        plot.margin = margin(1,1,1,4, "cm")) +
  transition_states(year, transition_length = 4, state_length = 1) +
  ease_aes('cubic-in-out')
anim <- animate(p, fps = 25, duration = 30, width = 800, height = 600)

anim_save("output.gif", anim)
