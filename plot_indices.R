# Matthew Botteon, GEOG573 Data Analysis Project

library(ggplot2)
library(trend)
library(Kendall)
library(tidyr)
library(dplyr)

# SET THIS DIRECTORY TO YOUR OWN COPY OF THE REPOSITORY
setwd("C:/Users/Matthew/Downloads/GHCND")

# Function to plot index results by year
plot_csv <- function(file) {
  df <- read.csv(file)
  yvar <- names(df)[2]
  
  ggplot(df, aes(x = year, y = .data[[yvar]])) +
    geom_line() +
    labs(title = paste(strsplit(basename(file), "_")[[1]][1], yvar),
         x = "Year",
         y = yvar)
}

# Function to convert monthly tables into a one sequence
convert_monthly <- function(file, output_name) {
  df <- read.csv(file)
  
  # Pivot into long format
  df_long <- df %>%
    pivot_longer(
      cols = 2:13,
      names_to = "month",
      values_to = "value"
    )
  
  # Convert lowercase month abbreviations into month numbers
  df_long$month_num <- match(tolower(df_long$month), tolower(month.abb))
  
  # Create a proper sequential date variable
  df_long$date <- as.Date(
    paste(df_long$year, df_long$month_num, "01", sep = "-")
  )
  
  # Order by date
  df_long <- df_long %>% arrange(date)
  
  # JJA subset: June(6), July(7), August(8)
  jja <- df_long %>%
    filter(month_num %in% c(6, 7, 8)) %>%
    mutate(season = "JJA",
           season_year = "year") %>%
    arrange(date)
  
  df_long$annual <- NULL
  df_long$month <- NULL
  df_long$month_num <- NULL
  df_long$date <- NULL
  
  jja$annual <- NULL
  jja$month <- NULL
  jja$month_num <- NULL
  jja$date <- NULL
  
  write.csv(df_long, paste0(output_name, "_All.csv"), row.names = FALSE)
  write.csv(jja, paste0(output_name, "_JJA.csv"), row.names = FALSE)
}

test <- convert_monthly("indices/Orangeburg_Cleaned_TXX.csv", "Orangeburg_Months")
plot_csv("Orangeburg_Months_All.csv")
plot_csv("Orangeburg_Months_JJA.csv")


# Create list with beginning of station file names
stations <- c("Blackville", "Glennville", "Orangeburg", "Yemassee")

# SU- Summer Days; Tmax > 25 °C
# Create lists for Mann Kendall results
mk_su_tau <- c()
mk_su_p   <- c()

for (s in stations) {
  # Create plots and save them as .jpg images
  p <- plot_csv(paste0("indices/", s, "_Cleaned_SU.csv"))
  jpeg(paste0("SU_", s, ".jpg"), width = 800, height = 600, res = 150)
  print(p)
  dev.off()
  # Perform the Mann Kendall test and extract the tau and p-values
  x <- read.csv(paste0("indices/", s, "_Cleaned_SU.csv"))[[2]]
  res <- MannKendall(x)
  mk_su_tau <- c(mk_su_tau, res$tau)
  mk_su_p   <- c(mk_su_p,   res$sl)
}
# View the Mann Kendall results in alphabetical order of the station name
print(mk_su_tau)
print(mk_su_p)



# FD- Frost Days; Tmin < 0 °C
# Create lists for Mann Kendall results
mk_fd_tau <- c()
mk_fd_p   <- c()

for (s in stations) {
  # Create plots and save them as .jpg images
  p <- plot_csv(paste0("indices/", s, "_Cleaned_FD.csv"))
  jpeg(paste0("FD_", s, ".jpg"), width = 800, height = 600, res = 150)
  print(p)
  dev.off()
  # Perform the Mann Kendall test and extract the tau and p-values
  x <- read.csv(paste0("indices/", s, "_Cleaned_FD.csv"))[[2]]
  res <- MannKendall(x)
  mk_fd_tau <- c(mk_fd_tau, res$tau)
  mk_fd_p   <- c(mk_fd_p,   res$sl)
}
# View the Mann Kendall results in alphabetical order of the station name
print(mk_fd_tau)
print(mk_fd_p)



# WSDI - annual count of days with at least 6 consecutive days when Tmax > 90th percentile temp
# Create lists for Mann Kendall results
mk_wsdi_tau <- c()
mk_wsdi_p   <- c()

for (s in stations) {
  # Create plots and save them as .jpg images
  p <- plot_csv(paste0("indices/", s, "_Cleaned_WSDI.csv"))
  jpeg(paste0("WSDI_", s, ".jpg"), width = 800, height = 600, res = 150)
  print(p)
  dev.off()
  # Perform the Mann Kendall test and extract the tau and p-values
  x <- read.csv(paste0("indices/", s, "_Cleaned_WSDI.csv"))[[2]]
  res <- MannKendall(x)
  mk_wsdi_tau <- c(mk_wsdi_tau, res$tau)
  mk_wsdi_p   <- c(mk_wsdi_p,   res$sl)
}
# View the Mann Kendall results in alphabetical order of the station name
print(mk_wsdi_tau)
print(mk_wsdi_p)



# TXX - maxiumum temp in a given month
# Create lists for Mann Kendall results
mk_all_tau <- c()
mk_all_p   <- c()
mk_jja_tau <- c()
mk_jja_p   <- c()

for (s in stations) {
  # Convert monthly files to one sequence
  convert_monthly(paste0("indices/", s, "_Cleaned_TXX.csv"), s)
  # Create plots and save them as .jpg images
  # All months
  p <- plot_csv(paste0(s, "_All.csv"))
  jpeg(paste0("TXX_All_Months", s, ".jpg"), width = 800, height = 600, res = 150)
  print(p)
  dev.off()
  # JJA
  p <- plot_csv(paste0(s, "_JJA.csv"))
  jpeg(paste0("TXX_JJA", s, ".jpg"), width = 800, height = 600, res = 150)
  print(p)
  dev.off()
  # Perform the Mann Kendall test and extract the tau and p-values
  # All
  x <- read.csv(paste0(s, "_All.csv"))[[2]]
  res <- MannKendall(x)
  mk_all_tau <- c(mk_all_tau, res$tau)
  mk_all_p   <- c(mk_all_p,   res$sl)
  # JJA
  x <- read.csv(paste0(s, "_JJA.csv"))[[2]]
  res <- MannKendall(x)
  mk_jja_tau <- c(mk_jja_tau, res$tau)
  mk_jja_p   <- c(mk_jja_p,   res$sl)
}
# View the Mann Kendall results in alphabetical order of the station name
print(mk_all_tau)
print(mk_all_p)
print(mk_jja_tau)
print(mk_jja_p)

# Isolate August for the Blackville station
x <- read.csv("report/monthly_maximum_files/Blackville_aug_TXX.csv")[[2]]
res <- MannKendall(x)
print(res)
