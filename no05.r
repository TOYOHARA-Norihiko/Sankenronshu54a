library(dplyr)
library(tidyr)
library(stringr)
library(tibble)

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

lead_lags <- -6:6

calculate_lag_correlation <- function(d) {

  bind_rows(
    lapply(
      lead_lags,
      function(k) {

        shifted_inventory_ratio <- if (k >= 0) {
          dplyr::lead(
            d$inventory_ratio,
            k
          )
        } else {
          dplyr::lag(
            d$inventory_ratio,
            -k
          )
        }

        tibble(
          lag = k,
          correlation = cor(
            d$production,
            shifted_inventory_ratio,
            use = "complete.obs"
          )
        )
      }
    )
  ) %>%
    filter(
      is.finite(correlation)
    )
}

lag_corr_all <- analysis_data %>%
  group_split(code) %>%
  lapply(
    function(d) {

      calculate_lag_correlation(d) %>%
        mutate(
          code = unique(d$code),
          industry = unique(d$industry)[1],
          period = "All"
        )
    }
  ) %>%
  bind_rows()

result_all <- lag_corr_all %>%
  group_by(
    code,
    industry,
    period
  ) %>%
  slice_max(
    order_by = abs(correlation),
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  mutate(
    abs_correlation = abs(correlation),
    lead_direction = case_when(
      lag < 0 ~ "在庫率先行",
      lag == 0 ~ "同時",
      lag > 0 ~ "生産先行"
    )
  ) %>%
  rename(
    lead_months = lag
  )

analysis_pre <- analysis_data %>%
  filter(
    date <= as.Date("2019-12-01")
  )

analysis_post <- analysis_data %>%
  filter(
    date >= as.Date("2022-01-01")
  )

lag_corr_pre <- analysis_pre %>%
  group_split(code) %>%
  lapply(
    function(d) {

      calculate_lag_correlation(d) %>%
        mutate(
          code = unique(d$code),
          industry = unique(d$industry)[1],
          period = "Pre-COVID-19"
        )
    }
  ) %>%
  bind_rows()

result_pre <- lag_corr_pre %>%
  group_by(
    code,
    industry,
    period
  ) %>%
  slice_max(
    order_by = abs(correlation),
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  mutate(
    abs_correlation = abs(correlation),
    lead_direction = case_when(
      lag < 0 ~ "在庫率先行",
      lag == 0 ~ "同時",
      lag > 0 ~ "生産先行"
    )
  ) %>%
  rename(
    lead_months = lag
  )

lag_corr_post <- analysis_post %>%
  group_split(code) %>%
  lapply(
    function(d) {

      calculate_lag_correlation(d) %>%
        mutate(
          code = unique(d$code),
          industry = unique(d$industry)[1],
          period = "Post-COVID-19"
        )
    }
  ) %>%
  bind_rows()

result_post <- lag_corr_post %>%
  group_by(
    code,
    industry,
    period
  ) %>%
  slice_max(
    order_by = abs(correlation),
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  mutate(
    abs_correlation = abs(correlation),
    lead_direction = case_when(
      lag < 0 ~ "在庫率先行",
      lag == 0 ~ "同時",
      lag > 0 ~ "生産先行"
    )
  ) %>%
  rename(
    lead_months = lag
  )

stable_lead <- result_all %>%
  select(
    code,
    industry,
    lead_all = lead_months,
    cor_all = correlation,
    abs_cor_all = abs_correlation
  ) %>%
  inner_join(
    result_pre %>%
      select(
        code,
        lead_pre = lead_months,
        cor_pre = correlation
      ),
    by = "code"
  ) %>%
  inner_join(
    result_post %>%
      select(
        code,
        lead_post = lead_months,
        cor_post = correlation
      ),
    by = "code"
  ) %>%
  filter(
    lead_all < 0,
    lead_pre < 0,
    lead_post < 0,
    cor_all < 0,
    cor_pre < 0,
    cor_post < 0,
    abs_cor_all >= 0.5
  ) %>%
  arrange(
    desc(abs_cor_all)
  )

stable_lead %>%
  select(
    code,
    industry,
    lead_all,
    lead_pre,
    lead_post,
    cor_all,
    cor_pre,
    cor_post,
    abs_cor_all
  )
