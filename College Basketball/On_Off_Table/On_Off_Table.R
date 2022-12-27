#Deploy packages
library(bigballR)
library(gt)
library(dplyr)
library(gtExtras)

#Vector of GameIDs
vec1 = c(5355739, 5358242, 5371917, 5363961, 5366525, 5370976, 5368319, 5379260, 5380678, 5376454, 5374088)

#Scrape play-by-play data
pbp <- get_play_by_play(vec1)

#Get lineup data
lineupsT <- get_lineups(pbp)

#Only include Kentuckys lineups
lineupsT = lineupsT |> 
  filter(Team == "Kentucky")

#Assign Variables for player and opponent
playerFN <- "LANCE.WARE"
player <- "Lance Ware"
opponent <- "Florida A&M"

#Calculate on/off chart
onOff <- on_off_generator(playerFN, lineupsT)

#Select Columns
onOff = onOff |>
  select(Status, NETRTG, ORTG, FG., DRTG, deFG., ORB., DRB.)

#Change first column
onOff[onOff == paste0(playerFN," Off")] <- paste0("While NOT in the game")
onOff[onOff == paste0(playerFN," On")] <- paste0("While in the game")

#Create Viz
onOff1 = onOff |>
  gt() %>%
  cols_align((align = "left")) |>
  cols_label(Status = "",
             NETRTG = "NetRTG",
             ORTG = "ORTG",
             FG. = "FG%",
             DRTG = "DRTG",
             deFG. = ("Opp FG%"),
             ORB. = "OReb%",
             DRB. = "Dreb%"
             ) |>
  fmt_percent(columns = c(FG., deFG., ORB., DRB.),
              decimals = 1,
              scale_values = TRUE) |>
  fmt_number(columns = c(NETRTG, ORTG, DRTG),
             decimals = 1) |>
  tab_header(
    title = paste0(player, " vs ", opponent)) |>
  tab_spanner(label = md("**Offense**"),
    columns = c(ORTG, FG.)) |>
  tab_spanner(label = md("**Defense**"),
                  columns = c(DRTG, deFG.)) |>
  tab_spanner(label = md("**Rebounding**"),
              columns = c(ORB., DRB.)) |>
  tab_source_note(source_note = md("**Data: BigBallR <br>TABLE: @CoreyCaudBBN**")) |>
  gt_theme_538() |>
  data_color(apply_to = "fill",
             columns = c(NETRTG, ORTG, FG., ORB., DRB.),
             colors = scales::col_numeric(
               palette = paletteer::paletteer_d(
                 palette = "RColorBrewer::RdYlBu"
               ) %>%
               as.character(),
               domain = NULL)) |>
  data_color(apply_to = "fill",
             columns = c(DRTG, deFG.),
             colors = scales::col_numeric(
               palette = paletteer::paletteer_d(
                 palette = "RColorBrewer::RdYlBu"
               ) %>%
                 as.character(),
               domain = NULL,
               reverse = TRUE))
  

onOff1

#Save viz
gtsave(onOff1, "C://Users//corey//OneDrive//Pictures//LanceWareonoff.PNG")