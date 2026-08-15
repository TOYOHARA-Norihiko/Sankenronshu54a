library(dplyr)
library(tibble)
library(stringr)
library(tidyr)

# パスとファイルの設定
data_dir <- "X:/data"
data_file <- file.path(data_dir, "SOURCE1")
doc_file  <- file.path(data_dir, "SOURCE2")

# --- データ本体 (M0, N0) の読み込み ---
lines <- readLines(data_file, encoding = "CP932")
record_id <- substr(lines, 1, 2)
N0_lines <- lines[record_id == "N0"]
M0_lines <- lines[record_id == "M0"]

N0 <- tibble(line = N0_lines) %>%
  transmute(
    record_id  = str_sub(line, 1, 2),
    flag       = str_sub(line, 3, 3),
    series     = str_trim(str_sub(line, 4, 27)),
    freq       = str_trim(str_sub(line, 28, 29)),
    start_code = str_trim(str_sub(line, 30, 31)),
    stop_flag  = as.integer(str_sub(line, 32, 32)),
    agg_code   = as.integer(str_sub(line, 33, 33)),
    decimal    = as.integer(str_sub(line, 34, 34)),
    from       = as.Date(str_sub(line, 35, 42), "%Y%m%d"),
    to         = as.Date(str_sub(line, 43, 50), "%Y%m%d")
  )

M0 <- tibble(line = M0_lines) %>%
  transmute(
    record_id = str_sub(line, 1, 2),
    flag      = str_sub(line, 3, 3),
    series    = str_trim(str_sub(line, 4, 27)),
    freq      = str_trim(str_sub(line, 28, 29)),
    date      = as.Date(str_sub(line, 30, 37), "%Y%m%d"),
    value     = str_trim(str_sub(line, 38, 57)),
    flash     = as.integer(str_sub(line, 58, 58))
  ) %>%
  mutate(
    value = na_if(value, ""),
    value = as.numeric(value)
  )

data_all <- M0 %>%
  left_join(N0, by = c("series", "freq"))

# --- ドキュメントファイル (N1, N2) の読み込み ---
con <- file(doc_file, open = "rb")
raw_data <- readBin(con, what = "raw", n = file.info(doc_file)$size)
close(con)

split_crlf <- function(x) {
  crlf <- which(x[-length(x)] == as.raw(0x0D) & x[-1] == as.raw(0x0A))
  start <- c(1, crlf + 2)
  end   <- c(crlf - 1, length(x))
  lapply(seq_along(start), function(i) x[start[i]:end[i]])
}

records <- split_crlf(raw_data)
records <- records[lengths(records) == 500]

get_field <- function(x, start, length, encoding = "CP932") {
  end <- start + length - 1
  value <- rawToChar(x[start:end])
  value <- iconv(value, from = encoding, to = "UTF-8")
  trimws(value)
}

doc_data <- do.call(
  rbind,
  lapply(records, function(x) {
    data.frame(
      record_id = get_field(x, 1, 2),
      flag      = get_field(x, 3, 1),
      series    = get_field(x, 4, 24),
      freq      = get_field(x, 28, 2),
      name      = get_field(x, 30, 254),
      unit      = get_field(x, 284, 80),
      source    = get_field(x, 364, 80),
      reserve   = get_field(x, 444, 57),
      stringsAsFactors = FALSE
    )
  })
) %>% as_tibble()

doc_en <- doc_data %>% filter(record_id == "N1")
doc_jp <- doc_data %>% filter(record_id == "N2")

doc_all <- doc_en %>%
  select(series, freq, name_en = name, unit_en = unit, source_en = source) %>%
  full_join(
    doc_jp %>% select(series, freq, name_jp = name, unit_jp = unit, source_jp = source),
    by = c("series", "freq")
  )

# --- 数値データとドキュメントの結合・RDS出力 ---
needs <- data_all %>%
  left_join(doc_all, by = c("series", "freq"))

saveRDS(needs, file.path(data_dir, "como_needs.rds"))
