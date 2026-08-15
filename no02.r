library(dplyr)
library(tidyr)

# RDSファイルから読み込み
data_file_rds <- "C:/data/SOURCE.rds"
needs <- readRDS(data_file_rds)

# 1. 月次データかつ2000年1月以降の期間に絞り込み
# （※ データ内に「生産」「出荷」「在庫」を特定するコード体系や分類が存在する場合、
#      品目ごとに3系列が紐付いている必要があります）
d_filtered <- needs %>%
  filter(freq == "M", date >= as.Date("2000-01-01"))

# 2. ワイド形式に変換して、各品目・各時点における「生産」「出荷」「在庫」を揃える
# ※注: 系列コードの命名規則に合わせて列名や識別方法を適宜調整してください
# ここでは例としてロング形式のデータを横展開し、条件判定を行います
# （実際のデータ構造に合わせて系列のグルーピング条件を変更してください）

# サンプルとしての抽出ロジック（各系列がそろっているかの判定）
# 2000年1月以降の期間で、指定した系列群に欠損がない品目を特定する例：
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

# 3. 在庫率（＝在庫／出荷）の計算対象となるデータの構築
# （必要に応じて生産・出荷・在庫のペアを結合して比率を計算します）
# 例：系列名や属性から出荷と在庫をピボット等で対応させる処理


先ほどお伝えしたRスクリプトの続きとして、ロング形式のマスターデータから「生産」「出荷」「在庫」の3系列を品目単位で正しく紐付け、2000年1月以降のデータが完全にそろっているものを抽出し、在庫率（在庫／出荷）を計算して出力するための後半のスクリプトを整理しました。

日経NEEDSデータの品目コードの構造（例えば、同じ品目に対して生産・出荷・在庫それぞれに異なるプレフィックスやコードが割り当てられている場合など）に合わせて、以下のロジックで結合・抽出を行います。

---

### 3. RDS形式から条件に合う系列を抽出し、在庫率を計算・出力するスクリプト（後半）

```R
library(dplyr)
library(tidyr)
library(stringr)

# RDSファイルから読み込み
data_dir <- "C:/data/2026_needs_macro"
needs <- readRDS(file.path(data_dir, "como_needs.rds"))

# ---------------------------------------------------------------------
# A. データの整形と分類（生産・出荷・在庫の紐付け）
# ---------------------------------------------------------------------
# ※注: NEEDSデータの仕様上、品目ごとのコード体系や名称から
#       「生産」「出荷」「在庫」を識別・対応させる必要があります。
#       ここでは、名称や系列ID規則に基づき分類・ピボットする例を示します。

# 1. 2000年1月以降の月次データに絞り込み
d_monthly <- needs %>%
  filter(freq == "M", date >= as.Date("2000-01-01"))

# 2. 品目ごとに「生産」「出荷」「在庫」のデータがそろっているかを判定・ワイド化する処理
# （※実際のデータフレームの列名・構造に応じてキー項目を調整してください）
# 例として、同一時点・同一品目コード等で値が横に並ぶように変形します：
# 以下のロジックは一例です。お手元のデータ項目の持ち方（name_jpやseriesの命名規則など）に合わせます。

# 品目ごとのデータ網羅性（2000年1月以降、欠損なくデータが存在するか）をチェックする関数的処理
# 生産(Production)、出荷(Shipment)、在庫(Inventory)の各変数を抽出して結合するステップ：

# [参考：データ構造に応じたワイド化のイメージ]
# d_wide <- d_monthly %>%
#   select(date, series, name_jp, value) %>%
#   # 品目コードや分類ごとに 生産・出荷・在庫 の系列を特定してピボットする
#   pivot_wider(
#     names_from = type_of_data, # 例: production, shipment, inventory の区別列
#     values_from = value
#   )

# ---------------------------------------------------------------------
# B. 本文の条件（2000年1月以降の連続性 ＆ 在庫率計算可能）の適用
# ---------------------------------------------------------------------

# 各品目（例: 品目IDまたは共通コード単位）でグループ化し、
# 1. 2000年1月〜最新月まで欠損（NA）がないこと
# 2. 分母となる出荷（shipment）が0でないこと（ゼロ割防止）
# を満たす系列を抽出します。

# valid_items <- d_wide %>%
#   group_by(item_code) %>%
#   summarise(
#     start_date = min(date),
#     end_date   = max(date),
#     na_count   = sum(is.na(production) | is.na(shipment) | is.na(inventory)),
#     min_shipment = min(shipment, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   filter(
#     start_date <= as.Date("2000-01-01"),
#     na_count == 0,
#     min_shipment > 0
#   )

# ---------------------------------------------------------------------
# C. 在庫率（＝在庫／出荷）の計算と最終データセットの保存
# ---------------------------------------------------------------------
# final_analysis_data <- d_wide %>%
#   semi_join(valid_items, by = "item_code") %>%
#   mutate(
X#     inventory_ratio = inventory / shipment
#   )

# 最終データの確認
# head(final_analysis_data)

# 分析用データとして保存
# saveRDS(final_analysis_data, file.path(data_dir, "analysis_target_series.rds"))

```
