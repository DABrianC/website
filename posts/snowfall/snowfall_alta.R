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
#Delete the first five columns

df1 <- df[6:nrow(df),] 
