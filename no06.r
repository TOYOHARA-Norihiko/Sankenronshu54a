library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(WaveletComp)

needs <- readRDS("X:/data/SOURCE3.rds")

target_start <- as.Date("2010-01-01")
target_end   <- as.Date("2025-12-01")

target_months <- seq.Date(
  target_start,
  target_end,
  by = "month"
)

n_target_months <- length(target_months)

series_data <- needs %>%
  filter(
    freq == "M",
    date >= target_start,
    date <= target_end
  ) %>%
  mutate(
    type = case_when(
      str_detect(series, "^INV[0-9]+$") ~ "inventory",
      str_detect(series, "^X[0-9]+$")   ~ "production",
      str_detect(series, "^S[0-9]+$")   ~ "shipment",
      TRUE ~ NA_character_
    ),
    code = case_when(
      type == "inventory"  ~ str_remove(series, "^INV"),
      type == "production" ~ str_remove(series, "^X"),
      type == "shipment"   ~ str_remove(series, "^S"),
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(type),
    !is.na(code)
  )

complete_series <- series_data %>%
  group_by(
    code,
    type
  ) %>%
  summarise(
    n_month = n_distinct(date[!is.na(value)]),
    .groups = "drop"
  ) %>%
  filter(
    n_month == n_target_months
  )

target_codes <- complete_series %>%
  count(
    code,
    name = "n_type"
  ) %>%
  filter(
    n_type == 3
  ) %>%
  pull(code)

production_data <- needs %>%
  filter(
    freq == "M",
    date >= target_start,
    date <= target_end,
    series %in% paste0("X", target_codes)
  ) %>%
  transmute(
    date,
    code = str_remove(series, "^X"),
    production = value,
    industry = name_jp
  )

shipment_data <- needs %>%
  filter(
    freq == "M",
    date >= target_start,
    date <= target_end,
    series %in% paste0("S", target_codes)
  ) %>%
  transmute(
    date,
    code = str_remove(series, "^S"),
    shipment = value
  )

inventory_data <- needs %>%
  filter(
    freq == "M",
    date >= target_start,
    date <= target_end,
    series %in% paste0("INV", target_codes)
  ) %>%
  transmute(
    date,
    code = str_remove(series, "^INV"),
    inventory = value
  )

analysis_data <- production_data %>%
  inner_join(
    shipment_data,
    by = c("date", "code")
  ) %>%
  inner_join(
    inventory_data,
    by = c("date", "code")
  ) %>%
  arrange(
    code,
    date
  ) %>%
  mutate(
    inventory_ratio = inventory / shipment
  )

representative_codes <- c(
  "2480",
  "1690",
  "2380"
)

wavelet_data <- analysis_data %>%
  filter(
    code %in% representative_codes
  ) %>%
  select(
    date,
    code,
    industry,
    production,
    inventory_ratio
  ) %>%
  arrange(
    code,
    date
  )

wavelet_results <- vector(
  "list",
  length(representative_codes)
)

names(wavelet_results) <- representative_codes

for (cd in representative_codes) {

  d <- wavelet_data %>%
    filter(
      code == cd
    ) %>%
    select(
      date,
      production,
      inventory_ratio
    )

  sink("nul")

  wavelet_results[[cd]] <- analyze.coherency(
    d,
    my.pair = c(
      "production",
      "inventory_ratio"
    ),
    loess.span = 0,
    dt = 1,
    dj = 1/12,
    lowerPeriod = 8,
    upperPeriod = 64,
    make.pval = TRUE,
    n.sim = 1000
  )

  sink()
}

for (cd in representative_codes) {

  d <- wavelet_data %>%
    filter(
      code == cd
    )

  industry_name <- unique(d$industry)[1] %>%
    str_remove("^生産動態\\s+生産\\s+")

  industry_name <- industry_name %>%
    str_remove_all("\\s+")

  wc.image(
    wavelet_results[[cd]],
    which.image = "wc",
    main = paste(
      cd,
      " ",
      industry_name,
      "：生産と在庫率のcoherency"
    ),
    legend.params = list(
      lab = "Wavelet Coherence"
    ),
    show.date = TRUE,
    date.format = "%Y-%m",
    timelab = "Time",
    periodlab = "Period",
    plot.coi = TRUE,
    plot.contour = TRUE,
    siglvl.contour = 0.05
  )

  wc.phasediff.image(
    wavelet_results[[cd]],
    main = paste0(
      cd,
      " ",
      industry_name,
      "：生産と在庫率の位相差"
    ),
    show.date = TRUE,
    date.format = "%Y-%m",
    timelab = "Time",
    periodlab = "Period",
    plot.coi = TRUE
  )
}
