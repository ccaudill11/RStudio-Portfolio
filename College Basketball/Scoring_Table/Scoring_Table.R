#Deploy Packages
library(ncaahoopR)
library(dplyr)
library(gt)
library(gtExtras)

#Get table with player images
df <- get_roster("Kentucky", season = '2022-23')
df = df |>
  select(name, player_image)

#Get table with box scores (averages and totals)
box <- season_boxscore(team= "Kentucky", season = "2022-23", aggregate = "average")
boxTOT <- season_boxscore(team= "Kentucky", season = "2022-23", aggregate = "total")

#Aggregate field goal percentages, select player, pts, and percentages
boxTOT = boxTOT |>
  ungroup() |>
  mutate(FG = (boxTOT$FGM/boxTOT$FGA)) |> 
  mutate(FG3 = (boxTOT$`3PTM`/boxTOT$`3PTA`)) |>
  select(player, FG, FG3)

#Join with field goal %'s
box = box |> 
  left_join(boxTOT, by = "player") |>
  select(player, PTS, FG, FG3)
  
#Change name formatting
box[box == 'A. Reeves'] <- 'Antonio Reeves'
box[box == 'A. Thiero'] <- 'Adou Thiero'
box[box == 'B. Canada'] <- 'Brennan Canada'
box[box == 'C. Livingston'] <- 'Chris Livingston'
box[box == 'C. Wallace'] <- 'Cason Wallace'
box[box == 'D. Collins'] <- 'Daimion Collins'
box[box == 'J. Toppin'] <- 'Jacob Toppin'
box[box == 'K. Watkins'] <- 'Kareem Watkins'
box[box == 'L. Ware'] <- 'Lance Ware'
box[box == 'O. Tshiebwe'] <- 'Oscar Tshiebwe'
box[box == 'S. Wheeler'] <- 'Sahvir Wheeler'
box[box == 'W. Horn'] <- 'Walker Horn'
box[box == 'U. Onyenso'] <- 'Ugonna Onyenso'

#remove walkons
df = df[df$name != "Brennan Canada",]
df = df[df$name != "Walker Horn",]
df = df[df$name != "Kareem Watkins",]
df = df[df$name != "Grant Darbyshire",]

#Join with player images
df1 = df |>
  left_join(box, by = c("name" = "player"))




#Create Viz
df2 = df1 |>
  arrange(-PTS) |>
  gt(rowname_col = "Player") |> 
  cols_align((align = "left")) |>
  gt_img_rows(player_image, height = 40) |>
  cols_label(name = "Player",
             player_image = "",
             PTS = "PPG",
             FG = "FG%",
             FG3 = "3PT%") |> 
  fmt_percent(FG, decimals = 1) |>
  fmt_percent(FG3, decimals = 1) |>
  fmt_number(PTS, decimals = 1) |>
  tab_header(
    title = md("**UK Basketball Shooting Metrics**")) |>
  tab_source_note(source_note = md("**Table By: Corey Caudill (@CoreyCaudBBN)**")) |>
  
  gtExtras::gt_theme_538() |>
  data_color(
    columns = c(FG,PTS, FG3),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::RdYlGn"
      ) %>%
        as.character(),
      domain = NULL
    )
  )

#Show Viz
df2

#Save viz
gtsave(df2, "C://Users//corey//OneDrive//Pictures//ScoringTable.PNG", expand = 50)
