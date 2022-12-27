#Deploy Packages
library(hoopR)
library(dbplyr)
library(ggplot2)
library(ggimage)
library(ggthemes)
library(readxl)

#Import Excel file and select relevant columns
TeamInfo <- read_excel("TeamInfo.xlsx")
TeamInfo = TeamInfo |>
  select(team, conf, team_id, abbreviation, color, logo, alternate_color)

#Scrape efficiency ratings
pomeroy_ratings <- kp_pomeroy_ratings(min_year = 2023, max_year = 2023)

#Select relevant columns
pomeroy_ratings = pomeroy_ratings |>
  select(team, adj_o, adj_d)

#Join ratings and team info
pomeroy_ratings = pomeroy_ratings %>%
  left_join(TeamInfo, by = c("team" = "team"))

#Select P6 conferences
pomeroy_ratings = pomeroy_ratings %>%
  filter(conf == "SEC" | conf == "P12" | conf == "ACC" | conf == "B12" | conf == "B10" | conf =="BE")

#Create viz
pomeroy_ratings %>% 
  ggplot(aes(x = conf, y = adj_o)) +
  geom_image(aes(x = conf, y = adj_o, image = logo), asp = 16/9, size = .03) +
  theme_minimal() +
  labs(x = 'Conferences',
       y = "Offensive Efficiency Rtg",
       title = "Offensive Efficiency Ratings for P6 Teams",
       subtitle = 'Using Kenpom Adjusted Offensive Efficiency Ratings',
       caption = "Chart by Corey Caudill (@CoreyCaudBBN)") +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12)) 


#Save Viz
ggsave("C://Users//corey//OneDrive//Pictures//Off_Eff_Rtg_Chart.png", 
       dpi = 300, bg = 'white', type = "cairo", width = 10, height = 7, units = "in") 
  
#Create Viz2
pomeroy_ratings %>% 
  ggplot(aes(x = conf, y = adj_d)) +
  geom_image(aes(x = conf, y = adj_d, image = logo), asp = 16/9, size = .03) +
  theme_minimal() +
  labs(x = 'Conferences',
       y = "Defensive Efficiency Rtg",
       title = "Defensive Efficiency Ratings for P6 Teams (The Lower the Better)",
       subtitle = 'Using Kenpom Defensive Efficiency Ratings',
       caption = "Chart by Corey Caudill (@CoreyCaudBBN)") +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12)) 

ggsave("C://Users//corey//OneDrive//Pictures//Def_Eff_Rtg.png", 
       dpi = 300, bg = 'white', type = "cairo", width = 10, height = 7, units = "in")
