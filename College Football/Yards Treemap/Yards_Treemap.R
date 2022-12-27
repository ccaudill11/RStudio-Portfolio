#Deploy Packages
library(cfbfastR)
library(dplyr)
library(ggplot2)
library(treemapify)
library(ggthemes)

#Choose Parameters
team = "Kentucky"
year = 2022
category = "receiving"

#scrape player stats based on variables
df <- cfbd_stats_season_player(
  year = year,
  season_type = "regular",
  team = team,
  category = category
)
#scrape roster based on variables
rost <- cfbd_team_roster(year = year, team = team)

#Select relevant information
rost = rost %>%
  select(athlete_id, position)

#Join tables
df = df %>% 
  group_by(player) %>%
  left_join(rost, by = c("athlete_id" = "athlete_id")) %>%
  arrange(desc(receiving_yds))

#Create Viz
ggplot(df, aes(area = receiving_yds, fill = position, label = paste(player, receiving_yds,sep="\n"), subgroup = position)) +
  geom_treemap() +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(place = "centre", grow = F, alpha = 0.3, colour = "black",
                             fontface = "italic", min.size = 0) +
  geom_treemap_text(colour = "white", place = "topleft", reflow = F) +
  theme_minimal() +
  labs(
    title = paste(team, "Receiving Yards by Player and Position"),
    caption = "Chart by Corey Caudill (@CoreyCaudBBN)") +
  theme(plot.title = element_text(size = 16), legend.position = 0)

#Save Viz
ggsave(paste0("C://Users//corey//OneDrive//Pictures//", category,"_Yds_Treemap.png"), 
       dpi = 300, bg = 'white', type = "cairo", width = 10, height = 7, units = "in")