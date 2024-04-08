#
# gender-gap-index.R
#
# (C)2022-2024 Kyodo News

#
# 設定
#

# カテゴリ
CATEGORIES <- c("経済", "政治", "教育", "行政")
# 計算に用いる元データが保存されているディレクトリ
DATA_DIR <- "./data/2024_0404/"
# 都道府県コードのCSVファイル
PREFCODE_CSV <- "./data/JIS_X_0401.csv"
# 出力先ディレクトリ
RESULT_DIR <- "./result/2024_0404/"

#
# BOM付きUTF-8でCSVファイルを出力する関数(MS-Excelでの文字化け対策)
#
write.csv.utf8bom <- function(df, filename) {
  con <- file(filename, "w")
  tryCatch({
    for (i in 1:ncol(df))
      df[, i] = iconv(df[, i], to = "UTF-8")
    writeChar(iconv("\ufeff", to = "UTF-8"), con, eos = NULL)
    write.csv(df,
              file = con,
              row.names = FALSE,
              quote = FALSE)
  }, finally = {
    close(con)
  })
}

#
# メイン処理
#

# Rのバージョンをチェック
if (as.numeric(R.version$major) * 10 + as.numeric(R.version$minor) < 41) {
  stop("このスクリプトを実行するにはR4.1.0以上が必要です！")
}

# 都道府県コードを読み込む
prefecture <- read.csv(PREFCODE_CSV)
# 指数出力用データフレーム
index <- NULL

# 各カテゴリごとに計算を行う
for (category in CATEGORIES) {
  # カテゴリごとの男女比データフレーム
  category_ratio <- NULL
  # ディレクトリ内の各ファイルを読み込む
  for (filename in paste0(DATA_DIR, category) |> list.files(pattern = "csv", full.names = TRUE)) {
    # CSVの読み込み
    csv <- read.csv(filename)
    # CSVを都道府県コードでソートし、47都道府県が揃っているか確認する
    csv <- csv[order(csv$code),]
    if (sum(csv$code != prefecture$code,
            csv$prefecture != prefecture$prefecture) > 0) {
      stop("入力データが不正です")
    }
    # 通常は(女性/男性)の比を計算するが、_inverseがついているファイルはその逆数
    if ("_inverse" |> grep(filename) |> length() == 0) {
      ratio <- csv$women / csv$men
    } else {
      ratio <- csv$men / csv$women
    }
    # 比の上限は1に設定する
    ratio[ratio > 1] <- 1
    # 男女比データフレームに連結し、列名を設定する
    category_ratio <- category_ratio |> cbind(ratio)
    colnames(category_ratio)[ncol(category_ratio)] <-
      paste0(DATA_DIR, category, "/") |> gsub("", filename)
    colnames(category_ratio)[ncol(category_ratio)] <-
      gsub(".csv", "", colnames(category_ratio)[ncol(category_ratio)])
  }
  
  # サブディレクトリを読み込む
  for (subdir in paste0(DATA_DIR, category) |> list.dirs(full.names = TRUE)) {
    if (subdir == paste0(DATA_DIR, category)) {
      next
    }
    subcategory <- subdir |> strsplit("/")
    subcategory <- subcategory[[1]][length(subcategory[[1]])]
    subcategory_ratio <- NULL
    # 各ファイルについての処理
    for (filename in paste0(subdir) |> list.files(pattern = "csv", full.names = TRUE)) {
      # CSVの読み込み
      csv <- read.csv(filename)
      # CSVを都道府県コードでソートし、47都道府県が揃っているか確認する
      csv <- csv[order(csv$code),]
      if (sum(csv$code != prefecture$code,
              csv$prefecture != prefecture$prefecture) > 0) {
        stop("入力データが不正です")
      }
      # 通常は(女性/男性)の比を計算するが、_inverseがついているファイルはその逆数
      if ("_inverse" |> grep(filename) |> length() == 0) {
        ratio <- csv$women / csv$men
      } else {
        ratio <- csv$men / csv$women
      }
      # 比の上限は1に設定する
      ratio[ratio > 1] <- 1
      # 男女比データフレームに連結し、列名を設定する
      subcategory_ratio <- subcategory_ratio |> cbind(ratio)
      colnames(subcategory_ratio)[ncol(subcategory_ratio)] <-
        paste0(DATA_DIR, category, "/") |> gsub("", filename)
      colnames(subcategory_ratio)[ncol(subcategory_ratio)] <-
        gsub(".csv", "", colnames(subcategory_ratio)[ncol(subcategory_ratio)])
      colnames(subcategory_ratio)[ncol(subcategory_ratio)] <-
        paste0(subcategory, "/") |> gsub("", colnames(subcategory_ratio)[ncol(subcategory_ratio)])
    }
    # データフレーム各列ごとの標準偏差ベクトルを求める
    col_sd <- subcategory_ratio |> apply(2, sd)
    # 各列のウェイトを計算する(各列の標準偏差の逆数 / 標準偏差の逆数の総和)
    col_weight <- (1 / col_sd) / sum(1 / col_sd)
    # 各列にウェイトを掛け、各行(都道府県)ごとに足し合わせて指数データフレームに出力する
    category_ratio <-
      subcategory_ratio %*% col_weight |> apply(1, sum) |> cbind(category_ratio)
    colnames(category_ratio)[1] <- subcategory
    # ランキング(higher is better)の生成
    rankings <-
      -subcategory_ratio |> round(3) |> apply(2, rank, ties.method = "min")
    
    # 都道府県データフレームに男女比データフレームを連結し、CSV形式で保存する
    prefecture |> cbind(subcategory_ratio |> round(3)) |> write.csv.utf8bom(paste0(RESULT_DIR, "csv/ratio_", subcategory, ".csv"))
    prefecture |> cbind(rankings) |> write.csv.utf8bom(paste0(RESULT_DIR, "csv/rankings_", subcategory, ".csv"))
  }
  
  # データフレーム各列ごとの標準偏差ベクトルを求める
  col_sd <- category_ratio |> apply(2, sd)
  # 各列のウェイトを計算する(各列の標準偏差の逆数 / 標準偏差の逆数の総和)
  col_weight <- (1 / col_sd) / sum(1 / col_sd)
  # 各列にウェイトを掛け、各行(都道府県)ごとに足し合わせて指数データフレームに出力する
  index <-
    category_ratio %*% col_weight |> apply(1, sum) |> cbind(index)
  colnames(index)[1] <- category
  # ランキング(higher is better)の生成
  rankings <-
    -category_ratio |> round(3) |> apply(2, rank, ties.method = "min")
  
  # 都道府県データフレームに男女比データフレームを連結し、CSV形式で保存する
  prefecture |> cbind(category_ratio |> round(3)) |> write.csv.utf8bom(paste0(RESULT_DIR, "csv/ratio_", category, ".csv"))
  prefecture |> cbind(rankings) |> write.csv.utf8bom(paste0(RESULT_DIR, "csv/rankings_", category, ".csv"))
}

# ランキング(higher is better)の生成
rankings <- -index |> round(3) |> apply(2, rank, ties.method = "min")

# 都道府県データフレームにデータフレームを連結しCSVで保存する
prefecture |> cbind(index |> round(3)) |> write.csv.utf8bom(paste0(RESULT_DIR, "csv/index.csv"))
prefecture |> cbind(rankings) |> write.csv.utf8bom(paste0(RESULT_DIR, "csv/rankings.csv"))