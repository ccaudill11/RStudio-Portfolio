#Deploy Packages
library(cfbfastR)
library(dplyr)
library(ggplot2)
library(ggimage)

#Scrape play-by-play data
df <- cfbd_stats_season_advanced(year = 2022)

#Scrape team info (for logos)
team_info = cfbfastR::cfbd_team_info(year = 2022, only_fbs = TRUE)

#Select Logos and colors
team_colors_logos = team_info %>% 
  select(school, abbreviation, color, logo, alt_color) %>%
  group_by(school) %>%
  slice(1) %>% 
  ungroup()

#Join tables
df2 = df %>%
  left_join(team_colors_logos, by = c("team" = "school"))

#Filter out G5 schools
p5 = df2 %>%
  filter(conference == "SEC" | conference == "Pac-12" | conference == "ACC" | conference == "Big 12" | conference == "Big Ten")

#Create Viz
ggplot() + 
  geom_jitter(data = p5, aes(x = def_havoc_db, y = def_havoc_front_seven)) +
  geom_image(data = p5, aes(x = def_havoc_db, y = def_havoc_front_seven, image = logo), asp = 16/9, size = .03) +
  geom_hline(yintercept = median(p5$def_havoc_front_seven), linetype = 2, color = "gray20", alpha = .6) +
  geom_vline(xintercept = median(p5$def_havoc_db), linetype = 2, color = "gray20", alpha = .6) +
  theme_minimal() +
  labs(x = 'Def Havoc Rate - Secondary',
       y = "Def Havoc Rate - Front 7",
       title = "Most Disruptive Defenses in CFB - 2022 Season",
       subtitle = "Chart by Corey Caudill (@CoreyCaudBBN)",) +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

#Save Viz
  ggsave("C://Users//corey//OneDrive//Pictures//Def_Havoc.png", 
         dpi = 300, bg = 'white', type = "cairo", width = 10, height = 7, units = "in")
  