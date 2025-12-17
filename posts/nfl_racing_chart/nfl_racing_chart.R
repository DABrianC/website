# NFL Cumulative Point Differential Racing Bar Chart
# Load required packages
library(nflfastR)
library(tidyverse)
library(gganimate)

# Load package
library(showtext)


# Check if running in CI environment
is_ci <- Sys.getenv("CI") != ""

# Configure fonts based on environment
if (require("showtext", quietly = TRUE)) {
  library(showtext)
  
  if (is_ci) {
    # In CI: use system fonts, don't try to load custom fonts
    showtext_auto()
    message("Running in CI - using system fonts")
  } else {
    # Local: load your custom fonts as usual
    # Example:
    font_add("Times", regular = "times.ttf")
    showtext_auto()
  }
}
# This automatically loads sysfonts as well
#font_add("Times", "times.ttf")        # Add system fonts

# Enable showtext for rendering
#showtext_auto()

source(here::here("posts/nfl_racing_chart/logos.R"))

# Step 1: Load and filter for 2025 season
nfl_data <- nflreadr::load_schedules(seasons = 2025) |> 
  mutate(home_score = replace_na(home_score, 0),
         away_score = replace_na(away_score, 0))  
  
# Step 2: Calculate point differential for each team and week
# We need to create rows for both home and away teams
home_games <- nfl_data %>%
  filter(!is.na(result)) %>%  # Only completed games
  select(week, home_team, home_score, away_score) %>%
  mutate(
    team = home_team,
    point_diff = home_score - away_score
  ) %>%
  select(week, team, point_diff)

away_games <- nfl_data %>%
  filter(!is.na(result)) %>%  # Only completed games
  select(week, away_team, home_score, away_score) %>%
  mutate(
    team = away_team,
    point_diff = away_score - home_score
  ) %>%
  select(week, team, point_diff)

# Combine home and away games
team_point_diff <- bind_rows(home_games, away_games) %>%
  arrange(team, week)

# Step 2.5 include by weeks with 0 points

all_teams <- unique(team_point_diff$team)
all_weeks <- 1:max(team_point_diff$week) 

complete_weeks <- expand.grid(team = all_teams,
                              week = all_weeks)


# merge the data with left_join

merge_team_point_diff <- left_join(complete_weeks,
                                   team_point_diff, by = c("team", "week"))

#replace NAs with 0s
merge_team_point_diff <- merge_team_point_diff |> 
  mutate(point_diff = replace_na(point_diff,0))

# Step 3: Calculate cumulative point differential
team_cumulative <- merge_team_point_diff %>%
  group_by(team) %>%
  arrange(week) %>%
  mutate(cum_pt_diff = cumsum(point_diff)) %>%
  ungroup() |> 
  #rank the cumulative point differences for each week
  group_by(week) |> 
  mutate(rank = min_rank(-cum_pt_diff)*1) |> 
  ungroup() |> 
  #sort point diff each week by rank and break ties
  group_by(week) |> 
  arrange(rank) |> 
  mutate(tie_break = seq(1,n())) |> 
  ungroup()

team_cumulative <- team_cumulative %>%
  left_join(teams_data %>% select(team_abbr, team_logo_espn), 
            by = c("team" = "team_abbr"))

#vector with teams' colors
team_colors <- setNames(teams_data$team_color, teams_data$team_abbr)

# Step 4 & 5: Create animated racing bar chart
anim <- team_cumulative  |> 
  ggplot(aes(x = cum_pt_diff, y = -tie_break,
             fill = team, group = team)) +
  geom_col(show.legend = FALSE,
           orientation = "y",
           width = .1) +
  geom_point(aes(x = cum_pt_diff,
                 y = -tie_break,
                 group = team,
                 size = ifelse(team=="IND"| team=="NYJ",
                               15, 8)),
             color = "#F0EAD6")+
  scale_size_identity()+
  geom_image(aes(image = team_logo_espn, x = cum_pt_diff),
             hjust = .9,
             size = 0.05) +
  geom_text(aes(label = team, x = min(cum_pt_diff)-15),# x = -200,
            hjust = 1.1,
            size =6,
            color = "black",
            family = "Times")+
  #geom_text(aes(label = sprintf("%+d", cum_pt_diff)), 
     #       hjust = ifelse(team_cumulative$cum_pt_diff >= 0, -.8, 1.5),
      #      size = 8,
      #      color = "black",
      #      family = "Times") +
  
  geom_text(aes(label = sprintf("%+d", cum_pt_diff),
                hjust = ifelse(cum_pt_diff >= -20 & cum_pt_diff <0, 2,
                               ifelse(cum_pt_diff > 0 & cum_pt_diff <= 20, -1,
                                      ifelse(cum_pt_diff > 20, -0.7, #2,
                                             ifelse(cum_pt_diff > 140, .7, 1.7))))),
                                                    #ifelse(cum_pt_diff < -129, -.7, 1.7))))),
                #color = ifelse(cum_pt_diff > 140, "white",
                #               ifelse(cum_pt_diff < -129, "white", "black"))), 
    size = 8,
    family = "Times")+
            #    hjust = ifelse(cum_pt_diff > 150, 1.1,
             #                  ifelse(cum_pt_diff < -128, -0.8,
            #                          ifelse(cum_pt_diff >= 0, -0.8, 1.5)))),
            #size = 8,
            #color = "black",
            #family = "Times")+
  geom_text(x = -100, y = -1,
            family = "Times",
            aes(label = sprintf("Week %d", week)),
            size = 18,
            color = "darkgrey") +
  scale_x_continuous(limits = c(min(team_cumulative$cum_pt_diff)-20, max(team_cumulative$cum_pt_diff)+20),
                     breaks = c(-150, -100, -50, 0,
                                50, 100, 150)) +
  scale_fill_manual(values = team_colors)+
  scale_color_identity()+
  labs(
    title = "NFL Cumulative Point Differential",
    subtitle = "2025 Season",
    x = "Cumulative Point Differential",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 32,
                              face = "bold",
                              color = "black",
                              family = "Times"),
    plot.subtitle = element_text(size = 26,
                                 color = "black",
                                 family = "Times"),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "black",
                               family = "Times",
                               size = 20),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#F0EAD6"),
    legend.position = "none"
  ) +
  # Render and save the animation
gganimate::transition_states(week,
                             transition_length = 20,
                             state_length = 40
                             )


animate(anim, 
        nframes = 300, 
        fps = 10, 
        width = 1000, 
        height = 1100, 
        renderer = gifski_renderer(here::here("posts/nfl_racing_chart/nfl_point_diff_race.gif")))

# Optional: Save static plot for current week
latest_week <- max(team_cumulative$week)
current_standings <- team_cumulative %>%
  filter(week == latest_week)


p <- ggplot(current_standings, aes(x = cum_pt_diff,
                                   y = reorder(team, cum_pt_diff),
                                   fill = team)) +
  geom_col(show.legend = FALSE,
           width = .1) +
  geom_point(aes(x = cum_pt_diff,
                 y = reorder(team, cum_pt_diff)),
             color = "#F0EAD6",
             size = 5.3)+
  geom_image(aes(image = team_logo_espn, x = cum_pt_diff),
             hjust = .9,
             size = 0.05) +
  geom_text(aes(label = sprintf("%+d", cum_pt_diff),
                hjust = ifelse(cum_pt_diff >= -20 & cum_pt_diff <0, 3.5,
                               ifelse(cum_pt_diff > 0 & cum_pt_diff <= 20, -2,
                                      ifelse(cum_pt_diff > 20, -1,2.2))), 
            ),
            size = 10)+
  scale_x_continuous(expand = expansion(mult = c(0.1, 0.15)),
                     limits = c(min(current_standings$cum_pt_diff) - 10, max(current_standings$cum_pt_diff)+ 10),
                     breaks = c( -150, -100, -50, 0,
                                50, 100, 150)) +
  scale_fill_manual(values = team_colors) +
  labs(
    title = "NFL Cumulative Point Differential",
    subtitle = paste0("2025-26 Season: Week ", current_standings$week),
    x = "Point Differential",
    y = NULL
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(size = 48, face = "bold", family = "Times"),
    plot.subtitle = element_text(size = 40, family = "Times"),
    axis.text.y = element_text(size = 30, family = "Times", face = "bold"),
    axis.text.x = element_text(size = 30, family = "Times", face = "bold"),
    axis.title.x = element_text(size = 36, family = "Times", face = "bold"),
    panel.grid.major.y = element_blank(),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#F0EAD6")
  )

ggsave(here::here("posts/nfl_racing_chart/nfl_point_diff_current.png"), 
       plot = p, 
       width = 6, 
       height = 8,
       units = "in",
       dpi = 300)


