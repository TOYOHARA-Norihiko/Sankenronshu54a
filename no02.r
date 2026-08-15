library(dplyr)
library(tidyr)

data_file_rds <- "X:/data/SOURCE.rds"
needs <- readRDS(data_file_rds)

d_filtered <- needs %>%
  filter(freq == "M", date >= as.Date("2000-01-01"))

valid_series <- d_filtered %>%
  group_by(series) %>%
  summarise(
    start_date = min(date, na.rm = TRUE),
    end_date   = max(date, na.rm = TRUE),
    n_obs      = n(),
    na_count   = sum(is.na(value)),
    .groups    = "drop"
  ) %>%
  filter(start_date <= as.Date("2000-01-01") & na_count == 0)
