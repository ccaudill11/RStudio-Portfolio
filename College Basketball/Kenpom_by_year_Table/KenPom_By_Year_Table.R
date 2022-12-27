#Deploy Packages
library(hoopR)
library(dbplyr)
library(ggplot2)
library(gt)
library(gtExtras)

#Scrape efficiency ratings for the past 13 years
eff <- kp_efficiency(min_year = 2010, max_year = 2023)

#Filter out all teams except for Kentucky, select relevant columns
eff1 = eff |>
  filter(team == "Kentucky") |>
  arrange(-year) |>
  select(year, adj_o, adj_o_rk, adj_d, adj_d_rk)

#Create Viz
GTeff = eff1 |>
  gt() |> 
  cols_align(align = "left") |>
  cols_label(year = "Year",
             adj_o = "Offensive Eff.",
             adj_o_rk = "",
             adj_d = "Defensive Eff.",
             adj_d_rk = "RANK") |>
  tab_header(
    title = md("**UK Basketball KenPom Ratings**"),
    subtitle = md("**Offensive and Defensive Efficiency Ratings by Year**")) |>
  tab_source_note(source_note = md("**Table By: Corey Caudill (@CoreyCaudBBN)**")) |>
  gtExtras::gt_theme_538() |>
  gt_merge_stack(col1 = adj_o, col2 = adj_o_rk) %>%
  gt_merge_stack(col1 = adj_d, col2 = adj_d_rk) %>%
  data_color(apply_to = "fill",
    columns = adj_o,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::RdYlGn"
      ) %>%
        as.character(),
      domain = NULL)) |>
  data_color(apply_to = "fill",
             columns = adj_d,
             colors = scales::col_numeric(
               palette = paletteer::paletteer_d(
                 palette = "RColorBrewer::RdYlGn") %>%
                 as.character(),
               domain = NULL,
               reverse = TRUE))

GTeff

#Save Viz
gtsave(GTeff, "C://Users//corey//OneDrive//Pictures//Kenpom_By_Year.PNG", expand = 50)

  
