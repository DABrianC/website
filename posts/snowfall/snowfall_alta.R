# Load required libraries
#Install if needed: install.packages(c("readxl", "brms", "tidyverse", "bayesplot"))
library(readxl)
library(brms)
library(tidyverse)
library(bayesplot)
library(plotly)

# Set options
options(mc.cores = parallel::detectCores())

# Download and read the data
data_url <- "https://utahavalanchecenter.org/sites/default/files/attached_files/2025.05.01%20Alta%20Guard%20Snow%3AWater.xlsx"

# Download the file
download.file(data_url, destfile = "alta_data.xlsx", mode = "wb")
df <- read_excel("alta_data.xlsx")

cat("Data dimensions:", nrow(df),
    "rows,", ncol(df), "columns\n")
names(df)

head(df)

#This is messy data
#Delete the first seven rows

df1 <- df[7:nrow(df),] 

names(df1) <- c("season",
                "nino",
                "oct_snow",
                "oct_rain",
                "nov_snow",
                "nov_rain",
                "dec_snow",
                "dec_rain",
                "jan_snow",
                "jan_rain",
                "feb_snow",
                "feb_rain",
                "march_snow",
                "march_rain",
                "april_snow",
                "april_rain",
                "may_snow",
                "may_rain",
                "blank2",
                "total_snow",
                "total_rain")

#remove last two columns that have some
# undefined numbers
#also remove the total snow and rain columns
df2 <- df1 |> 
  select(-20:-23, -blank2)

#Make it a longer dataset with a column for 
# month and a column for type of h20
# and a column for the amount of h20

df3 <- df2 |> 
  pivot_longer(cols = 3:18,
               names_to = c("month", "type"),
               names_sep = "_",
               values_to = "amount")

df3$nino <- df3$nino |> 
  recode("N" = "Neutral")

df3$month <- df3$month |> 
  recode("oct" = "Oct",
         "nov" = "Nov",
         "dec" = "Dec",
         "jan" = "Jan",
         "feb" = "Feb",
         "march" = "March",
         "april" = "April",
         "may" = "May")
#now we can look at just the snowfall
df3_snow <- df3 |> 
  filter(type == "snow") |> 
  mutate(amount = as.numeric(amount)) |> 
  filter(!is.na(amount)) |> 
  mutate(month = factor(month, 
                        levels = c("Oct", "Nov", "Dec", "Jan", "Feb", 
                                   "March", "April", "May")),
         month_num = as.numeric(month),
         nino = factor(nino,
                       levels = c("La Nina Strong",
                                  "La Nina Mod",
                                  "La Nina Weak",
                                  "Neutral",
                                  "El Nino Weak",
                                  "El Nino Mod",
                                  "El Nino Strong",
                                  "El Nino Very Strong"))) |> 
  mutate(nino_numeric = case_when(
    nino == "La Nina Strong" ~ -3,
    nino == "La Nina Mod" ~ -2,
    nino == "La Nina Weak" ~ -1,
    nino == "Neutral" ~ 0,
    nino == "El Nino Weak" ~ 1,
    nino == "El Nino Mod" ~ 2,
    nino == "El Nino Strong" ~ 3,
    nino == "El Nino Very Strong" ~ 4,
    TRUE ~ NA_real_
  ))


#look at the data over time

ggplot(df3_snow) +
  geom_jitter(aes(x = month_num,
                 y = amount),
              fill = "blue",
              shape = 21,
              color = "white",
              alpha = .8,
              width = .25,
              size = 4) +
  geom_smooth(aes(x = month_num, y = amount)) +
  scale_x_continuous(breaks = 1:8, 
                     labels = levels(df3_snow$month)) +
  labs(x = "Months",
       y = "Amount of \nSnowfall (in.)",
       title = "Monthly Snowfall at Alta",
       subtitle = "Alta gets a lot of snow, particularly from December to January") +
  theme_minimal()

#summary statistics
stats_tbl <- df3_snow %>%
  group_by(month) %>%
  summarise(
    n = n(),
    mean = mean(amount, na.rm = TRUE),
    median = median(amount, na.rm = TRUE),
    sd = sd(amount, na.rm = TRUE),
    min = min(amount, na.rm = TRUE),
    max = max(amount, na.rm = TRUE)
  )

knitr::kable(stats_tbl)

#color by nino category
plot <- ggplot(df3_snow) +
  geom_jitter(aes(x = month_num,
                  y = amount,
                  fill = nino_numeric,
                  text = paste0("Season: ", season,
                                "\nMonth: ", month,
                                "\nSnowfall: ", round(amount, 1), " in.",
                                "\nENSO: ", nino)),
              shape = 21,
              color = "white",
              alpha = .8,
              width = .25,
              size = 4)  +
  geom_smooth(aes(x = month_num, y = amount)) +
  scale_color_viridis_c(option = "plasma",
                        na.value = "grey50") +
  scale_fill_gradient2(low = "#2166ac",    # La Niña blue
                       mid = "#F5F5F5",       # Neutral
                       high = "#b2182b",    # El Niño red
                       midpoint = 0,
                       na.value = "grey50",
                       breaks = c(-3, 0, 4),
                       labels = c("La Niña\nStrong", "Neutral", "\nEl Niño\nVery Strong"),
                       guide = guide_colorbar(ticks = FALSE, 
                                              frame.colour = "white",
                                              barwidth = 1,
                                              barheight = 5))+ 
  scale_x_continuous(breaks = 1:8, 
                     labels = levels(df3_snow$month)) + 
  labs(x = "Months",
       y = "Amount of \nSnowfall (in.)",
       title = "Monthly Snowfall at Alta",
       subtitle = "Alta gets a lot of snow, particularly from December to January",
       fill = "La Niña ← → El Niño",
       caption = "Source: Utah Avalanche Center, https://utahavalanchecenter.org/alta-monthly-snowfall") +
  theme_minimal()

plot
plotly::ggplotly(plot, tooltip = "text")

ggplot(df3_snow) +
  geom_histogram(aes(amount),
                 binwidth = 5)


plot <- ggplot(df3_snow) +
  geom_jitter(aes(x = month_num,
                  y = amount,
                  fill = nino_numeric),
              shape = 21,
              color = "white",
              alpha = .2,
              width = .25,
              size = 4)  +
  geom_smooth(aes(x = month_num,
                  y = amount,
                  group = nino_numeric,
                  color = nino_numeric),
              se = FALSE,
              show.legend = FALSE) +
  #scale_color_viridis_c(option = "plasma",
   #                     na.value = "grey50") +
  scale_color_gradient2(low = "#2166ac",    # La Niña blue
                        mid = "#F5F5F5",       # Neutral
                        high = "#b2182b",    # El Niño red
                        midpoint = 0,
                        na.value = "grey50",
                        breaks = c(-3, 0, 4),
                        labels = c("La Niña\nStrong", "Neutral", "\nEl Niño\nVery Strong"))+
  scale_fill_gradient2(low = "#2166ac",    # La Niña blue
                       mid = "#F5F5F5",       # Neutral
                       high = "#b2182b",    # El Niño red
                       midpoint = 0,
                       na.value = "grey50",
                       breaks = c(-3, 0, 4),
                       labels = c("La Niña\nStrong", "Neutral", "\nEl Niño\nVery Strong"),
                       guide = guide_colorbar(ticks = FALSE, 
                                              frame.colour = "white",
                                              barwidth = 1,
                                              barheight = 5))+ 
  scale_x_continuous(breaks = 1:8, 
                     labels = levels(df3_snow$month)) + 
  labs(x = "Months",
       y = "Amount of \nSnowfall (in.)",
       title = "Monthly Snowfall at Alta",
       subtitle = "Alta gets a lot of snow, particularly from December to January",
       fill = "La Niña ← → El Niño",
       caption = "Source: Utah Avalanche Center, https://utahavalanchecenter.org/alta-monthly-snowfall") +
  theme_minimal()

plot
