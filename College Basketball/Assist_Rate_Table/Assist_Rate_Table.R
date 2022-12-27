#Activate Packages
library(ncaahoopR)
library(dplyr)
library(gt)
library(gtExtras)

#Get player images
df <- get_roster("Kentucky", season = '2022-23')
df = df |>
  select(name, player_image)

#Get PBP
pbp <- get_pbp("Kentucky", "2022-23")

#Group by shooter, mutate columns
df2 = pbp |>
  filter(shot_team == "Kentucky") |>
  group_by(shooter) |>
  summarise(
    FGs = sum(free_throw == "FALSE" & shot_outcome == "made" & !is.na(assist)) / sum(free_throw == "FALSE" & shot_outcome == "made"),
    threes = sum(three_pt == "TRUE" & shot_outcome == "made" & !is.na(assist)) / sum(three_pt == "TRUE" & shot_outcome == "made"))


#Remove walk-ons
df = df[df$name != "Brennan Canada",]
df = df[df$name != "Walker Horn",]
df = df[df$name != "Grant Darbyshire",]
df = df[df$name != "Adou Thiero",]
df = df[df$name != "Kareem Watkins",]

#Join tables with data and player images
df3 = df |>
  left_join(df2, by = c("name" = "shooter"))

#create viz
df4 = df3 |>
  arrange(FGs) |>
  gt() |>
  cols_align((align = "left")) |>
  gt_img_rows(player_image, height = 40) |>
  cols_label(name = "Player",
             player_image = "",
             FGs = "Field Goals",
             threes = "Threes") |> 
  fmt_percent(FGs, decimals = 1) |>
  fmt_percent(threes, decimals = 1) |>
  tab_header(
    title = md("**UK Basketball: % of players' FGs that come off assists**")) |>
    #subtitle = md("Looking at the percentage of players' shots come unassisted")) |>
  tab_spanner(label = md("**% of shots that are assisted**"),
              columns = c(FGs, threes)) |>
  tab_source_note(source_note = md("**Table By: Corey Caudill (@CoreyCaudBBN)**")) |>
  
  gtExtras::gt_theme_538() |>
  data_color(
    columns = c(FGs, threes),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::RdYlGn"
      ) %>%
        as.character(),
      domain = NULL,
      reverse = TRUE
    )
  )
df4

#Save png
gtsave(df4, "C://Users//corey//OneDrive//Pictures//Assist_Rates.PNG", expand = 50)

