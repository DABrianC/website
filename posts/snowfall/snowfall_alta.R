# Load required libraries
#Install if needed: install.packages(c("readxl", "brms", "tidyverse", "bayesplot"))
library(readxl)
library(brms)
library(tidyverse)
library(bayesplot)

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

#now we can look at just the snowfall
df3_snow <- df3 |> 
  filter(type == "snow") |> 
  mutate(amount = as.numeric(amount)) |> 
  filter(!is.na(amount)) |> 
  mutate(month = factor(month, 
                        levels = c("oct", "nov", "dec", "jan", "feb", 
                                   "march", "april", "may")),
         month_num = as.numeric(month)) |> 
  mutate(nino_numeric = case_when(
    nino == "La Nina Strong" ~ -3,
    nino == "La Nina Mod" ~ -2,
    nino == "La Nina Weak" ~ -1,
    nino == "N" ~ 0,
    nino == "El Nino Weak" ~ 1,
    nino == "El Nino Mod" ~ 2,
    nino == "El Nino Strong" ~ 3,
    nino == "El Nino Very Strong" ~ 4,
    TRUE ~ NA_real_
  ))
                        

#look at the data over time

ggplot(df3_snow) +
  geom_point(aes(x = month_num,
                 y = amount), alpha = .4) +
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
ggplot(df3_snow) +
  geom_point(aes(x = month_num,
                 y = amount,
                 color = nino_numeric), alpha = .4) +
  geom_smooth(aes(x = month_num, y = amount)) +
  scale_x_continuous(breaks = 1:8, 
                     labels = levels(df3_snow$month)) + 
  labs(x = "Months",
       y = "Amount of \nSnowfall (in.)",
       title = "Monthly Snowfall at Alta",
       subtitle = "Alta gets a lot of snow, particularly from December to January",
       color = "La Niña ← → El Niño") +
  theme_minimal()

ggplot(df3_snow) +
  geom_histogram(aes(amount),
                 binwidth = 5)
