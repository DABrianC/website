library(nflreadr)
library(ggimage)

teams_data <- nflreadr::load_teams()

library(magick)

for(i in 1:nrow(teams_data)) {
  team <- teams_data$team_abbr[i]
  logo_url <- teams_data$team_logo_espn[i]
  
  img <- image_read(logo_url)
  image_write(img, path = here::here("logos/", paste0(team, ".png")))
  }

library(purrr)
library(magick)

teams_data <- nflreadr::load_teams()

walk2(
  teams_data$team_abbr,
  teams_data$team_logo_espn,
  ~ image_write(
    image_read(.y),
    path = here::here("posts/nfl_racing_chart/logos/", paste0(.x, ".png"))
  )
)

