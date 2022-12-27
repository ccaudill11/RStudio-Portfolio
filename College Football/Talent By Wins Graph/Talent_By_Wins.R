#Deploy Packages
library(cfbfastR)
library(dplyr)
library(ggplot2)
library(ggimage)

#Creating Data sets using the cfbd_team_talent function for years 2018-2021

t18 <- cfbd_team_talent(year = 2018)
t19 <- cfbd_team_talent(year = 2019)
t20 <- cfbd_team_talent(year = 2020)
t21 <- cfbd_team_talent(year = 2021)

#Combining said data sets into one table
talentdf <- rbind(t18, t19, t20, t21)

#getting info for all cfb teams, assigning it to a table called team_info
team_info <- cfbd_team_info(year = 2022)

#The following 5 lines take the table and alter it
team_info = team_info %>% # Make sure you have DPLYR package active
  select(school, abbreviation, color, logo, alt_color, conference) %>% #Cutting down the table to these columns
  group_by(school) %>% # These 3 lines is a safeguard against duplicates for each school, dont really worry about it top much
  slice(1) %>% 
  ungroup()

#These 3 lines add the columns from team_info table to the Talentdf table
talentdf = talentdf %>%
  left_join(team_info, by = c("school" = "school")) %>% # join the two tables based on the school columns for each table
  filter(conference == "SEC" | conference == "Pac-12" | conference == "ACC" | conference == "Big 12" | conference == "Big Ten") #include only p5 conferences

#Making datasets with the cfbd_game_records function for years 18-21
rec18 <- cfbd_game_records(year = 2018)
rec19 <- cfbd_game_records(year = 2019)
rec20 <- cfbd_game_records(year = 2020)
rec21 <- cfbd_game_records(year = 2021)

#Binding said datasets
records <- rbind(rec18, rec19, rec20, rec21)

#inlcuding only p5 teams
records = records %>%
  filter(conference == "SEC" | conference == "Pac-12" | conference == "ACC" | conference == "Big 12" | conference == "Big Ten")

#Select relevant columns
records = records %>%
  select(team, year, total_wins)

#Joining the tables based on school/team and year columns
talentdf = talentdf %>%
  left_join(records, by = c("school" = "team", "year" = "year"))

#Create viz
ggplot(data = talentdf, aes(x = total_wins, y = talent), size = 0) +
  geom_point() +
  geom_image(data = talentdf, aes(x = total_wins, y = talent, image = logo), asp = 16/9, size = .04) +
  facet_wrap(~year) +
  geom_smooth(method = lm, se = FALSE, color = "black", size = .75) +
  theme_fivethirtyeight() +
  labs(x = 'Wins',
       y = "Composite Team Talent Rating",
       title = "Which P5 teams have performed to their talent level?",
       subtitle = "Comparing Composite Team Talent Rankings from 24/7 Sports to a team's number of wins",
       caption = "Chart by @CoreyCaudBBN") +
  theme(plot.title = element_text(size = 18, face = "bold"), 
        strip.text.x = element_text(size = 11),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 11)) +
  scale_x_continuous(limits = c(0, 15),
                     breaks = c(0, 3, 6, 9, 12,15)) +
  scale_y_continuous(limits = c(500, 1005),
                     breaks = c(500, 750, 1000)) +
  geom_hline(yintercept = 500) +
  geom_vline(xintercept = 0)

#Save viz
ggsave("C://Users//corey//OneDrive//Pictures//Talent_by_Wins.png", 
       dpi = 300, bg = 'white', type = "cairo", width = 10, height = 7, units = "in")



