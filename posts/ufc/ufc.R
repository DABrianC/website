#devtools::install_github("mtoto/ufc.stats")

library(ufc.stats)
library(tidyverse)
library(ggiraph)
library(glue)
library(htmltools)

#UFC colors
ufc_colors <- c("#AF0E14",
                "#D91A2A",
                "#F2F2F2",
                "#A6A6A6",
                "#262626")

data("ufc_stats")

glimpse(ufc_stats)


ufc_stats$fighter <- gsub("'", "&#39", ufc_stats$fighter)

ufc_stats <- ufc_stats |> 
  mutate(win = case_when(winner == "W" ~ 1, 
                         TRUE ~ 0),
         loss = case_when(winner == "L" ~ 1,
                          TRUE ~ 0),
         no_contest = case_when(winner == "NC" ~ 1,
                        TRUE ~ 0),
         draw = case_when(winner == "D" ~ 1,
                          TRUE ~0),
         no_decision = case_when(winner == "" ~ 1,
                                 TRUE ~ 0)
    )

df <- ufc_stats |>
  group_by(id, fighter, win, loss, no_contest, draw, no_decision) |> 
  summarize(strikes = mean(significant_strikes_landed)
            , takedowns = mean(takedown_successful)) |> 
  mutate(tooltip_html = glue(
    "<b>Fighter:</b> {fighter}<br>",
    "<b>Significant Strikes Landed:</b> {strikes}<br>",
    "<b>Successful Takedowns:</b> {takedowns}") |>  
      htmltools::HTML()) |> 
  ungroup() 

df1 <- df |> 
  group_by(fighter) |> 
  summarize(wins = sum(win),
            losses = sum(loss),
            no_contests = sum(no_contest),
            draws = sum(draw),
            no_decisions = sum(no_decision)) |> 
  mutate(tot_fights = wins + losses + no_contests + draws + no_decisions,
         win_perc = wins/tot_fights)

ggplot(df1) +
  geom_histogram(aes(tot_fights)
                 , binwidth = 1
                 , alpha = .5
                 , fill = ufc_colors[4]) +
  geom_density(aes(x = tot_fights, y = after_stat(density) * nrow(df1)), color = ufc_colors[2]) +
  geom_vline(xintercept = mean(df1$tot_fights), color = ufc_colors[1]) +
  annotate("text",label=paste0("Avg. number \nof fights\nper fighter"), 
           x = 15, 
           y = 250,
           color = ufc_colors[1]) +
  geom_curve(aes(x = 12, xend = 8,
                 y = 220, yend = 200,
                 color = "curve"), arrow = grid::arrow(), color = ufc_colors[4]) +
  labs(x = "Total Fights",
       y = "Number of Fighters",
       title = "How many times does a typical UFC fighter fight?") +
  theme_minimal()



plot <- ggplot(df) +
  geom_point_interactive(aes(x = takedowns, y = strikes
                             , tooltip = tooltip_html, color = strikes
                             , data_id = fighter))

girafe(ggobj = plot)  


library(ggplot2)
library(ggiraph)
data <- mtcars
data$carname <- row.names(data)

gg_point = ggplot(data = data) +
  geom_point_interactive(aes(x = wt, y = qsec, color = disp,
                             tooltip = c(carname, strikes), data_id = carname)) + 
  theme_minimal()

girafe(ggobj = gg_point)
             