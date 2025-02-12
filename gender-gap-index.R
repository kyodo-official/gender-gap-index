# gender-gap-index.R
#
# (C)2022-2025 Kyodo News

# --- 設定 ------------------------------------------------------

# カテゴリの定義
CATEGORIES <- c("経済", "政治", "教育", "行政")

# データが保存されているディレクトリ
DATA_DIR <- "./data/2025/"

# 都道府県コードのCSVファイル
PREFCODE_CSV <- "./data/JIS_X_0401.csv"

# 出力先ディレクトリ
RESULT_DIR <- "./result/2025/"

# ウェイトの出力先ディレクトリ
WEIGHTS_DIR <- "./weights/"

# --- CSVをUTF-8 BOM付きで出力する関数 ---------------------------

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
#     ★ここのロジックを修正し、(men=0,women>0) または (women=0,men>0)
#       の場合に比率を1にクリップする処理を追加します。

compute_ratio <- function(filename, prefecture) {
    csv <- read.csv(filename)
    csv <- csv[match(prefecture$code, csv$code), ]
    if (!all(csv$code == prefecture$code & csv$prefecture == prefecture$prefecture)) {
        stop("入力データが不正です")
    }
    # "_inverse" を含むファイル名なら men/women, そうでなければ women/men
    is_inverse <- grepl("_inverse", filename)
    
    zero_men <- (csv$men == 0)
    zero_women <- (csv$women == 0)
    
    # 結果を行数分初期化 (デフォルトは0)
    ratio <- numeric(nrow(csv))
    
    if (!is_inverse) {
        # 通常計算: women / men
        # men=0 & women>0 => 無限大扱い => 1 にクリップ
        ratio[zero_men & (csv$women > 0)] <- 1
        
        # men≠0 の行は通常計算
        valid_idx <- !zero_men
        ratio[valid_idx] <- csv$women[valid_idx] / csv$men[valid_idx]
        
    } else {
        # 逆比率計算: men / women
        # women=0 & men>0 => 無限大扱い => 1 にクリップ
        ratio[zero_women & (csv$men > 0)] <- 1
        
        # women≠0 の行は通常計算
        valid_idx <- !zero_women
        ratio[valid_idx] <- csv$men[valid_idx] / csv$women[valid_idx]
    }
    
    # men=0 & women=0 の場合は特例で 1 (ポリシー次第で NA 扱いも可)
    both_zero <- zero_men & zero_women
    ratio[both_zero] <- 1
    
    # 最後に 1 を上限にクリップ
    ratio <- pmin(ratio, 1)
    
    # データフレーム／行列として返す
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

# --- ウェイトを読み込む関数 --------------------------------------

read_weights <- function(prefix, ratio_df) {
    weight_filename <- paste0(WEIGHTS_DIR, prefix, ".csv")
    if (!file.exists(weight_filename)) {
        stop("ウェイトファイルが見つかりません: ", weight_filename)
    }
    weight_df <- read.csv(weight_filename)
    if (!all(c('name', 'weight') %in% colnames(weight_df))) {
        stop("ウェイトファイルのフォーマットが正しくありません: ", weight_filename)
    }
    # ratio_dfの列名とウェイトの名前を照合
    idx <- match(colnames(ratio_df), weight_df$name)
    if (any(is.na(idx))) {
        stop("ウェイトファイルの名前がデータの列名と一致しません: ", weight_filename)
    }
    col_weight <- weight_df$weight[idx]
    # ウェイトを正規化
    col_weight <- col_weight / sum(col_weight)
    return(col_weight)
}

# --- ディレクトリ（カテゴリまたはサブカテゴリ）を処理する関数 --

process_directory <- function(dir_path, prefix, parent_ratio_df) {
    # このカテゴリ/サブカテゴリで集計された比率のデータフレーム
    ratio_df <- data.frame(matrix(nrow = nrow(prefecture), ncol = 0))
    
    # ディレクトリ内のCSVファイルを処理
    filenames <- list.files(dir_path, pattern = "\\.csv$", full.names = TRUE)
    for (filename in filenames) {
        ratio <- compute_ratio(filename, prefecture)
        ratio_df <- cbind(ratio_df, ratio)
    }
    
    # サブディレクトリを処理 (再帰呼び出し)
    subdirs <- list.dirs(dir_path, recursive = FALSE, full.names = TRUE)
    for (subdir in subdirs) {
        subcategory <- basename(subdir)
        ratio_df <- process_directory(subdir, subcategory, ratio_df)
    }
    
    if (ncol(ratio_df) > 0) {
        # ウェイトを読み込んで加重平均を計算
        col_weight <- read_weights(prefix, ratio_df)
        weighted_sum <- as.matrix(ratio_df) %*% col_weight
        
        # parent_ratio_dfの先頭列として追加
        parent_ratio_df <- cbind(weighted_sum, parent_ratio_df)
        colnames(parent_ratio_df)[1] <- prefix
        
        # ランキングを計算 (比率をマイナスして rank() することで高いほど1位に)
        rankings <- apply(-round(ratio_df, 3), 2, rank, ties.method = "min")
        
        # 比率とランキングをCSVファイルに出力
        ratio_output <- cbind(prefecture, round(ratio_df, 3))
        write.csv.utf8bom(ratio_output, paste0(RESULT_DIR, "csv/ratio_", prefix, ".csv"))
        
        ranking_output <- cbind(prefecture, rankings)
        write.csv.utf8bom(ranking_output, paste0(RESULT_DIR, "csv/rankings_", prefix, ".csv"))
    }
    
    return(parent_ratio_df)
}

# --- メイン処理 -------------------------------------------------

# 都道府県コードと名称を読み込む
prefecture <- read.csv(PREFCODE_CSV)

# 結果を格納するデータフレームを初期化
index <- data.frame(matrix(nrow = nrow(prefecture), ncol = 0))

# 各カテゴリごとに処理を実行
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
