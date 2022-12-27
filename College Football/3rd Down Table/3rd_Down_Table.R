#Deploy Packages
library(cfbfastR)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(gt)
library(gtExtras)

#scrape play-by-play data
pbp <- load_cfb_pbp(seasons = 2022)

#Filter third downs where the ball was snapped, create distance categories and assign play types and conversion status
pbp1 = pbp %>% 
  filter(pos_team == "Kentucky", down == 3, pass + rush == 1) %>%
  select(distance, yards_gained, pass, rush, play_type) %>%
  mutate(dist_cats = ifelse(distance < 3, "3rd & Short (1-3 yds)", ifelse(distance < 8, "3rd & Medium (4-7 yds)", "3rd & Long (+8 yds)")),
         conversion = ifelse(yards_gained>distance, "convert", "fail"),
         play_type = ifelse(pass == 1 | play_type == "Sack", "Pass", "Run"),
         conv = ifelse(conversion == "convert", 1, 0))
  
#Group by the distance categories, create viz
pbp2 = pbp1 %>%
  group_by(dist_cats, play_type) %>%
  summarize(instances = n(),
            conversions = sum(conv),
            conv_rate = (conversions/instances)) |>
  gt() |>
  gt_theme_538() |>
  cols_align((align = "left")) |>
  cols_label(play_type = "Play Type",
             instances = "Instaces",
             conversions = "Covnversions",
             conv_rate = "Conv. %") |>
  tab_header(title = md("**Kentucky Football 3rd Down Metrics**")) |>
  tab_source_note(source_note = md("Data: CFBFastR <br>TABLE: @CoreyCaudBBN")) |>
  fmt_percent(conv_rate, decimals = 0) |>
  data_color(
    columns = c(conv_rate),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::RdYlGn"
      ) %>%
        as.character(),
      domain = NULL
    )
  )
  
pbp2

#Save viz
gtsave(pbp2, "C://Users//corey//OneDrive//Pictures//3rd_down_Table.png", expand = 50)  
