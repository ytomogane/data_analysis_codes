###データ例の作成#####
n <- 30
TestData1 <- data.frame(row.names = sample(paste0("Group", 1:n), n, replace = FALSE),
                        Data1 = rnorm(n) + rnorm(n) + rnorm(n),
                        Data2 = rnorm(n) + rnorm(n) + rnorm(n),
                        Data3 = sample(0:1, n, replace = TRUE))
TestData2 <- data.frame(Data1 = sample(c(LETTERS[15:24], NA), n, replace = TRUE),
                        Data2 = sample(LETTERS[15:24], n, replace = TRUE),
                        Data3 = sample(c(LETTERS[15:24], NA), n, replace = TRUE))
#######

###ヒートマップの基本的な描写:Heatmapコマンド#####
Heatmap(TestData1)
########

###セルの色を指定:colオプション#####
#scalesパッケージで色を設定
library("scales")
x <- seq(0, 1, length = 10)
ColPal <- seq_gradient_pal(c("#e1e6ea", "#505457", "#4b61ba", "#a87963",
                             "#d9bb9c", "#756c6d"))(x)
#例えば,連続変数のヒートマップの場合
Heatmap(TestData1, col = ColPal)

#例えば,カテゴリ変数のヒートマップの場合
#欠損値の色を指定:na_colオプション
#structureコマンドでラベル付きの文字配列を作成
CateColPal <- structure(ColPal, names = LETTERS[15:24])
Heatmap(TestData2, col = CateColPal, na_col = "yellow")
########

###列および行タイトルの設定#####
#列側:column_titleオプション
Heatmap(TestData1, col = ColPal, column_title = "Column TITLE")
#行側:row_titleオプション
Heatmap(TestData1, col = ColPal, row_title = "Row TITLE")
########
列および行タイトルの位置設定#####
#列側:column_title_sideオプション;"top","bottom"の設定が可能
Heatmap(TestData1, col = ColPal, column_title = "Column TITLE",
        column_title_side = "bottom")
#行側:row_title_sideオプション;"left","right"の設定が可能
Heatmap(TestData1, col = ColPal, row_title = "Column TITLE",
        row_title_side = "right")
########
##列および行タイトルの回転#####
#列側:column_title_rot
Heatmap(TestData1, col = ColPal, column_title = "Column TITLE",
        column_title_side = "bottom", column_title_rot = 90)
#行側:row_title_rot
Heatmap(TestData1, col = ColPal, row_title = "Column TITLE",
        row_title_side = "right", row_title_rot = 0)
########

###凡例タイトルの設定:nameオプション#####
Heatmap(TestData1, col = ColPal, name = "Legend TITLE")
#####
###クラスタリング距離計算と結合方法の指定#####
###距離計算:clustering_distance_rows,clustering_distance_columnsオプション#####
#euclidean","maximum","manhattan","canberra","binary","minkowski",
#"pearson","spearman","kendall"の指定が可能
###結合方法:clustering_method_rows,clustering_method_columnオプション#####
#"ward.D","ward.D2","single","complete","average"(= UPGMA),"mcquitty"(= WPGMA),
#"median"(= WPGMC),"centroid"(= UPGMC)の指定が可能
Heatmap(TestData1, clustering_distance_rows = "pearson",
        clustering_method_rows = "single")
########
###クラスタリングの表示有無#####
#列側:cluster_columnsオプション
#行側:cluster_rowsオプション
Heatmap(TestData1, cluster_rows = TRUE, cluster_columns = FALSE)
########
###クラスタリングの表示位置#####
#列側:row_dend_sideオプション:"left","right"の指定が可能
#行側:column_dend_sideオプション:"top","bottom"の指定が可能
Heatmap(TestData1, row_dend_side = "right", column_dend_side = "bottom")
########
###行側クラスタリングの分割と間隔調整#####
#分割:kmオプション
#間隔調整:gapオプション;単位はunitで指定
#分割後のラベル色設定:row_names_gpオプション
#書式はgparオプションで指定;色:col,ラベルサイズ:fontsize;初期値14
#連続変数の場合
Heatmap(TestData1, km = 2, gap = unit(10, "mm"),
        row_names_gp = gpar(col = c("green", "orange"), fontsize = c(10, 14)))
#カテゴリ変数の場合
#分割:splitオプション;内容を文字列で指定
#総数30のデータを各10で3分割
Heatmap(TestData2, split = rep(LETTERS[1:3], each = 10), gap = unit(5, "mm"))
########

###列,行の並び替え########
#列側:column_orderオプション
#行側:row_orderオプション
Heatmap(TestData1, cluster_rows = FALSE, cluster_columns = FALSE,
        column_order = c("Data3", "Data2", "Data1"), row_order = paste0("Group", 30:1))
########
###列,行ラベルの設定#####
#列側:column_names_gpオプション
#行側:row_names_gpオプション
#書式はgparオプションで指定;色:col,ラベルサイズ:fontsize;初期値14
Heatmap(TestData1,
        column_names_gp = gpar(col = c(rep("red", 1), rep("blue", 2)), fontsize = 20),
        row_names_gp = gpar(col = c(rep("red", 20), rep("blue", 10)), fontsize = 10))
########
