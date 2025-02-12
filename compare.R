###############################################
# compare_yoy_wide.R
#
# 1) 最新年度にある CSV (itemname) を基準に過去年度分を読み込み
# 2) men/women を gender 列にまとめてロング形式化
# 3) 年度ごとに横並び (ワイド形式) に pivot_wider
# 4) CSV 出力
###############################################

# 0. 初期化 ----
rm(list = ls())
library(tidyverse)
library(readr)

# 1. フォルダ・年度の設定 ----
base_dir    <- "data"                          # CSVが格納されたベースフォルダ
years       <- c("2022", "2023", "2024", "2025") # 取り扱う年度
latest_year <- "2025"                          # 最新年度

# 2. 最新年度の CSV 一覧を取得 ----
latest_path       <- file.path(base_dir, latest_year)
csv_files_latest  <- list.files(
    path       = latest_path,
    pattern    = "\\.csv$",
    full.names = TRUE,
    recursive  = TRUE
)

# 拡張子を除いたファイル名を「項目名(itemname)」とする
latest_files_df <- tibble(
    fullpath_latest = csv_files_latest,
    filename        = basename(csv_files_latest),                           # 例: "市区町村長の男女比.csv"
    itemname        = tools::file_path_sans_ext(basename(csv_files_latest)),# 例: "市区町村長の男女比"
    folder_latest   = basename(dirname(csv_files_latest))                   # 例: "政治", "行政" など
)

# 3. 最新年度 CSV を読み込み ----
df_all_years <- list()
df_latest_list <- lapply(seq_len(nrow(latest_files_df)), function(i) {
    f <- latest_files_df$fullpath_latest[i]
    read_csv(f, locale = locale(encoding = "UTF-8")) %>%
        mutate(
            year     = latest_year,
            folder   = latest_files_df$folder_latest[i],
            filename = latest_files_df$filename[i],
            itemname = latest_files_df$itemname[i]
        )
})
df_all_years[[latest_year]] <- bind_rows(df_latest_list)

# 最新年度にある「項目名」一覧
items_latest <- unique(latest_files_df$itemname)

# 4. 過去年度の CSV を読み込み (最新年度にある項目名のみ) ----
past_years <- setdiff(years, latest_year)

for (y in past_years) {
    year_path <- file.path(base_dir, y)
    csv_files <- list.files(
        path       = year_path,
        pattern    = "\\.csv$",
        full.names = TRUE,
        recursive  = TRUE
    )
    
    csv_df <- tibble(
        fullpath = csv_files,
        filename = basename(csv_files),
        folder   = basename(dirname(csv_files)),
        itemname = tools::file_path_sans_ext(basename(csv_files))
    )
    
    # 最新年度にある項目名のみ絞り込み
    csv_df_target <- csv_df %>% 
        filter(itemname %in% items_latest)
    
    if (nrow(csv_df_target) > 0) {
        df_list <- lapply(seq_len(nrow(csv_df_target)), function(i) {
            f <- csv_df_target$fullpath[i]
            read_csv(f, locale = locale(encoding = "UTF-8")) %>%
                mutate(
                    year     = y,
                    folder   = csv_df_target$folder[i],
                    filename = csv_df_target$filename[i],
                    itemname = csv_df_target$itemname[i]
                )
        })
        df_all_years[[y]] <- bind_rows(df_list)
    }
}

# 5. 全年度分のデータを結合 ----
df_all <- bind_rows(df_all_years)

# 6. men / women を gender 列としてまとめる (ロング化) ----
#    -> "gender" = "men"/"women", "count" = 数値
df_long <- df_all %>%
    # men, women 列がある前提
    pivot_longer(
        cols = c("men", "women"),
        names_to = "gender",
        values_to = "count"
    )

# 7. 年度を列見出しにしてワイド化 ----
#    -> 縦が (itemname, folder, code, prefecture, gender)
#       横が (2022, 2023, 2024, 2025, ...)
df_wide <- df_long %>%
    select(itemname, folder, code, prefecture, gender, year, count) %>%
    distinct() %>%  # 重複排除(必要に応じて)
    pivot_wider(
        names_from = "year",
        values_from = "count"
    ) %>%
    arrange(itemname, code, gender)

# 8. CSV 出力 ----
#    必要に応じて出力パスやファイル名を変更してください
output_file <- "yearly_comparison_wide.csv"
write_csv(df_wide, output_file)

cat("経年比較用のワイド形式CSVを出力しました:", output_file, "\n")