#Deploy packages
library(cfbfastR)
library(dplyr)
library(ggplot2)
library(ggimage)

#Scrape data
df <- cfbd_stats_season_team(year = 2022, conference = "SEC")

#Scrape team info (images)
team_info = cfbfastR::cfbd_team_info(year = 2022, only_fbs = TRUE)

#Condense team info, select relevant columns
team_colors_logos = team_info %>% 
  select(school, abbreviation, color, logo, alt_color) %>%
  group_by(school) %>%
  slice(1) %>% 
  ungroup()

#Join logos with data
df2 = df %>%
  left_join(team_colors_logos, by = c("team" = "school"))
  
#Create viz
df2 %>% 
  ggplot(aes(x = abbreviation, y = turnovers)) +
  geom_bar(aes(fill = color, color = alt_color), stat = "identity", alpha = .9) +
  geom_image(aes(image = logo, y = 5), asp = 16/9, size = 0.05) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  geom_text(aes(label = turnovers), nudge_y = + .5) +
  theme_fivethirtyeight() +
  labs(x = "Team",
       y = "Turnovers",
       title = "Total count of turnovers for each SEC team",
       subtitle = "Chart by Corey Caudill (@CoreyCaudBBN)",
  ) +
  scale_y_continuous(breaks = c(0,5,10,15,20,25)) +
  theme(plot.title = element_text(size = 18, face = "bold"), 
        strip.text.x = element_text(size = 11),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 11)) 

#Save viz
ggsave("C://Users//corey//OneDrive//Pictures//Turnovers_Chart.png", 
       dpi = 300, bg = 'white', type = "cairo", width = 10, height = 7, units = "in")

