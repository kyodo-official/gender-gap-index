# gender-gap-index.R
#
# (C)2022-2025 Kyodo News

# --- 設定 ------------------------------------------------------

# カテゴリの定義
CATEGORIES <- c("経済", "政治", "教育", "行政")

# データが保存されているディレクトリ
DATA_DIR <- "./data/2024/"

# 都道府県コードのCSVファイル
PREFCODE_CSV <- "./data/JIS_X_0401.csv"

# 出力先ディレクトリ
RESULT_DIR <- "./result/2024/"

# ウェイトの出力先ディレクトリ
WEIGHTS_DIR <- "./weights/"

# --- CSV出力をUTF-8 BOM付きで行う関数 --------------------------

write.csv.utf8bom <- function(df, filename) {
    con <- file(filename, "w")
    tryCatch({
        # データフレームの各列をUTF-8にエンコード
        df[] <- lapply(df, function(x) iconv(as.character(x), to = "UTF-8"))
        # BOMを書き込む
        writeChar("\ufeff", con, eos = NULL)
        # CSVを書き込む
        write.csv(df, file = con, row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")
    }, finally = {
        close(con)
    })
}

# --- ファイルからデータを読み込み、比率を計算する関数 -----------

compute_ratio <- function(filename, prefecture) {
    csv <- read.csv(filename)
    
    # 順序をprefecture$codeに合わせる
    csv <- csv[match(prefecture$code, csv$code), ]
    if (!all(csv$code == prefecture$code & csv$prefecture == prefecture$prefecture)) {
        stop("入力データが不正です")
    }
    
    # ファイル名に "_inverse" が含まれている場合は逆比率 (men / women)
    is_inverse <- grepl("_inverse", filename)
    
    # 男性・女性が 0 かどうかを判定
    zero_men <- (csv$men == 0)
    zero_women <- (csv$women == 0)
    
    # ratio を行数分だけ 0 で初期化
    ratio <- numeric(nrow(csv))
    
    if (!is_inverse) {
        # 通常計算: women / men
        # men=0 & women>0 の場合は無限大扱い → 1 にクリップ
        ratio[zero_men & (csv$women > 0)] <- 1
        
        # men≠0 の行は通常の比率計算
        valid_idx <- !zero_men
        ratio[valid_idx] <- csv$women[valid_idx] / csv$men[valid_idx]
        
    } else {
        # 逆比率計算: men / women
        # women=0 & men>0 の場合は無限大扱い → 1 にクリップ
        ratio[zero_women & (csv$men > 0)] <- 1
        
        # women≠0 の行は通常の比率計算
        valid_idx <- !zero_women
        ratio[valid_idx] <- csv$men[valid_idx] / csv$women[valid_idx]
    }
    
    # men=0 & women=0 の行は特例で 1 (→ 意味づけ次第で NA にする場合も)
    both_zero <- zero_men & zero_women
    ratio[both_zero] <- 1
    
    # 最終的に最大値を1にクリップ
    ratio <- pmin(ratio, 1)
    
    # 行列に変換して列名を設定
    ratio <- as.matrix(ratio)
    colnames(ratio) <- generate_column_name(filename)
    
    return(ratio)
}

# --- 列名を生成する関数 -----------------------------------------

generate_column_name <- function(filename) {
    name <- basename(filename)
    name <- sub("\\.csv$", "", name)
    return(name)
}

# --- 標準偏差に基づいてウェイトを計算する関数 -------------------

compute_weights <- function(ratios_df) {
    col_sd <- apply(ratios_df, 2, sd, na.rm = TRUE)
    if (any(col_sd == 0)) {
        stop("標準偏差がゼロの列があります。")
    }
    inv_sd <- 1 / col_sd
    col_weight <- inv_sd / sum(inv_sd)
    return(col_weight)
}

# --- ディレクトリ（カテゴリまたはサブカテゴリ）を処理する関数 --

process_directory <- function(dir_path, prefix, parent_ratio_df) {
    ratio_df <- data.frame(matrix(nrow = nrow(prefecture), ncol = 0))
    
    # ディレクトリ内のCSVファイルを処理
    filenames <- list.files(dir_path, pattern = "\\.csv$", full.names = TRUE)
    for (filename in filenames) {
        ratio <- compute_ratio(filename, prefecture)
        ratio_df <- cbind(ratio_df, ratio)
    }
    
    # サブディレクトリを処理
    subdirs <- list.dirs(dir_path, recursive = FALSE, full.names = TRUE)
    for (subdir in subdirs) {
        subcategory <- basename(subdir)
        ratio_df <- process_directory(subdir, subcategory, ratio_df)
    }
    
    if (ncol(ratio_df) > 0) {
        # ウェイトの計算と加重平均
        col_weight <- compute_weights(ratio_df)
        weighted_sum <- as.matrix(ratio_df) %*% col_weight
        parent_ratio_df <- cbind(weighted_sum, parent_ratio_df)
        colnames(parent_ratio_df)[1] <- prefix
        
        # ランキングを生成 (比率をマイナスして rank() で昇順→降順化)
        rankings <- apply(-round(ratio_df, 3), 2, rank, ties.method = "min")
        
        # 比率とランキングをCSVファイルに出力
        ratio_output <- cbind(prefecture, round(ratio_df, 3))
        write.csv.utf8bom(ratio_output, paste0(RESULT_DIR, "csv/ratio_", prefix, ".csv"))
        
        ranking_output <- cbind(prefecture, rankings)
        write.csv.utf8bom(ranking_output, paste0(RESULT_DIR, "csv/rankings_", prefix, ".csv"))
        
        # ウェイトをCSVファイルに出力
        weight_output <- data.frame(name = colnames(ratio_df), weight = col_weight)
        write.csv.utf8bom(weight_output, paste0(WEIGHTS_DIR, prefix, ".csv"))
    }
    
    return(parent_ratio_df)
}

# --- メイン処理 -------------------------------------------------

# 都道府県コードと名称を読み込む
prefecture <- read.csv(PREFCODE_CSV)

# 結果を格納するデータフレームを初期化
index <- data.frame(matrix(nrow = nrow(prefecture), ncol = 0))

# 各カテゴリごとの計算
for (category in CATEGORIES) {
    category_dir <- file.path(DATA_DIR, category)
    index <- process_directory(category_dir, category, index)
}

# 分野別ランキングを生成
if (ncol(index) > 0) {
    rankings <- apply(-round(index, 3), 2, rank, ties.method = "min")
    
    # 分野別指数とランキングをCSVファイルに出力
    index_output <- cbind(prefecture, round(index, 3))
    write.csv.utf8bom(index_output, paste0(RESULT_DIR, "csv/index.csv"))
    
    ranking_output <- cbind(prefecture, rankings)
    write.csv.utf8bom(ranking_output, paste0(RESULT_DIR, "csv/rankings.csv"))
}