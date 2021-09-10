# 区分分類.R




# メモ
# 按分処理　中区分90をどちらの大区分に割り当てるか、割合を算定

graphics.off() # ウィンドウの消去
rm(list = ls()) # 変数の消去
cat("\014") # コンソールを消去


# ==================================================

# open_csv_name <- "~/Dropbox/研究IR/区分分類/kibanS.csv"
# open_csv_name <- "~/Dropbox/研究IR/区分分類/kibanS_2018-2020.csv"
open_xlsx_name <- "~/Dropbox/研究IR/区分分類/KibanS_2018-2020.xlsx"

# Daikubun <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K") # 大区分名
# selected_category <- c("A", "C", "G") # 課題1
selected_category <- c("C", "E", "G", "I") # 課題2

k_folds <- 8 # 層化k-分割交差検証（stratified k-fold cross validation）


# ==================================================
# ポスター作成用のラベル
label_kmeans_task1 <- c(
  "A1","A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12","A13","A14","A15","A16","A17","a18",
  "C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13","C14","C15","c16","C17","C18","C19","C20","C21","C22","C23","C24","C25",
  "G1","G2","G3","G4","G5","G6","G7","G8","G9","G10","G11","G12","G13","g14","G15","G16","G17","G18","G19","G20","G21")

color_svm_task1 <- c(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,1,1,1,1,1,1,1,1,1,3,3,3,3,3,1,3,3,3,3,3,3,3,1,3,3,3,3,3,3,3)
color_svm_task2 <- c(2,2,2,3,2,2,2,2,2,2,2,2,2,2,3,1,2,2,3,2,2,2,2,2,2,3,3,4,3,2,3,3,2,3,2,2,3,3,2,2,2,3,3,3,1,1,4,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,4,4,4,4,4,4,4,1,4,4,4,4,4,4,4,4,4,4,4,4,4,4)


# ==================================================

# ライブラリの読み込み
library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(RMeCab) # 言語処理
library(e1071) # SVM
library(ggplot2)
library(tidyr)
library(scales)
library(rsample)
library(proxy) # cos類似度


# ==================================================
# 変数を準備
num_category <- length(selected_category)


# ==================================================
#

# 科研費データをオープン
# Kaken <- read_csv(open_csv_name, locale = locale(encoding = "cp932")) # for Win
# Kaken <- read_csv(open_csv_name) # for Mac
Kaken <- read_excel(open_xlsx_name)
num_data <- nrow(Kaken) # 件数をカウント
Kaken <- Kaken[order(Kaken$審査区分),] # 大区分で並び替え

# 必要なデータの選択
d1 <- Kaken %>%
  select(Titles = 研究課題名, Abstracts = 研究開始時の研究の概要, Sections = 審査区分) %>%
  mutate(Sections = str_remove(Sections, "大区分")) %>%
  filter(Sections %in% selected_category)

# 各申請に番号をふる
num_each_section <- table(d1$Sections)
sectionID <- integer()
for (i in 1:length(selected_category)){
  sectionID = c(sectionID, 1:num_each_section[i])
}
sectionID <- str_c(d1$Sections, as.character(sectionID))
d1 <- d1 %>% mutate(Sections_ID = sectionID)
# rownames(d1) <- sectionID

# 大区分をfactorに
d1$Sections <- factor(d1$Sections, levels = selected_category) # 選択したカテゴリだけにした
# d1$Sections <- factor(d1$Sections, levels = Daikubun)


# ==================================================
# 言語処理（ターム文書行列に変換）

# TF・IDF法（Term Frequency / Inverse Document Frequency）
# ある単語Aが、ある文書αの文書内にて出現頻度が高く、文書群全体では出現頻度が低い場合に値が高くなる
# https://youtu.be/nsEbfO3U2pY

# norm
# 各文書ベクトルの大きさを１に規格化

# minFreq = 2
# ２つ未満の文書にしか登場しない単語はカウントしない

r_mat <- d1$Abstracts %>%
  # docMatrixDF(pos = c("名詞"), minFreq = 2) %>%
  # docMatrixDF(pos = c("名詞"), weight = "tf*idf", minFreq = 2) %>%
  docMatrixDF(pos = c("名詞"), weight = "tf*idf*norm", minFreq = 2) %>% # best result
  as.data.frame()
colnames(r_mat) <- sectionID

# 括弧・数字・丸数字を除去（分類に不要な情報なので）
r_mat <- filter(r_mat,
                str_detect(row.names(r_mat), "[:punct:]") == F,
                str_detect(row.names(r_mat), "\\d") == F,
                str_detect(row.names(r_mat), "[①-⑨]") == F)

# 重み付け順に並び替え（計算には必要ない）
r_mat <- r_mat[order(rowSums(r_mat), decreasing = T),]


# ==================================================
#  MDS

# アブストの類似度を距離に換算し、距離の関係をなるべく保ちつつ2次元平面にプロット（MDS）

c1 <- t(r_mat) # 転置
c2 <- dist(c1) # 距離に変換(デフォルトのユークリッド距離を使う)
# c2 <- dist(c1, method = "cosine") # 距離に変換(proxyパッケージのcos類似度)
c3 <- cmdscale(c2, eig = T) # MDS
c4 <- c3$points # 2次元の座標だけを取り出す


# ==================================================
#  k-means

c5 <- kmeans(c1, num_category, iter.max = 100, nstart = 100) # k-meansクラスタリング
# c5 <- kmeans(c1, num_category, iter.max = 100, nstart = 100) # k-meansクラスタリング
c6 <- c5$cluster # クラスター分けの結果を取り出す

# アルファベットは各申請書の大区分を表す。
# 色はk-meansでの分類

# ggplot(as.data.frame(c4))+
#   geom_text(aes(x = V1, y = V2, label = d1$Sections_ID, colour = as.factor(c6)))+
#   labs(color = "k-means", x = "D1", y = "D2",
#        title = "Categorization of Kakenhi proposals based on the text of abstracts",
#        caption = "Each letter represents true categories. \nEach color represents the result of k-means clustering.")+
#   theme_gray(base_family = "HiraKakuProN-W3")


# for Poster
ggplot(as.data.frame(c4))+
  geom_text(aes(x = V1, y = V2, label = d1$Sections_ID, colour = as.factor(c6)))+
  # geom_text(aes(x = V1, y = V2, label = d1$Sections_ID, colour = as.factor(color_svm_task1)))+
  # geom_text(aes(x = V1, y = V2, label = d1$Sections_ID, colour = as.factor(color_svm_task2)))+
  guides(colour=FALSE)+
  # labs(color = "k-means", x = "", y = "") #,
  #      title = "Categorization of Kakenhi proposals based on the text of abstracts",
  #      caption = "Each letter represents true categories. \nEach color represents the result of k-means clustering.")+
  theme_gray(base_family = "HiraKakuProN-W3")+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

  
  
  
  

# ==================================================================================
# 教師あり学習であるSVMを用いた申請書の分類
# 『Rによるサポートベクターマシン」で検索
# https://data-science.gr.jp/implementation/iml_r_svm.html
# ==================================================================================

# データの調整
k1 <- r_mat %>%
  t() %>% # 転置
  as.data.frame() %>% # データフレームに変換
  bind_cols("Sections" = d1$Sections, .) # 正解の大区分を付加

# 層化K-分割交差検証（stratified k-fold cross validation）
set.seed(13) # rランダム化のシードを固定
folds <- vfold_cv(k1, strata = "Sections", v = k_folds)



# 変数の用意
dulation <- rep(as.numeric(NA), k_folds)
predict_results <- vector("list", length = k_folds)
Accuracy <- rep(as.numeric(NA), k_folds)
Precision <- matrix(data = NA, nrow = k_folds, ncol = num_category)
colnames(Precision) <- paste("p", selected_category, sep = "")
rownames(Precision) <- folds$id
TPR <- matrix(data = NA, nrow = k_folds, ncol = num_category)
colnames(TPR) <- paste("t", selected_category, sep = "")
rownames(TPR) <- folds$id
F1 <- rep(as.numeric(NA), k_folds)
confusion_matrices <- vector("list", length = k_folds)
tune_list <- vector("list", length = k_folds)


# 交差検証のループ
# for (k in 1:1){ # ループの回数を制限
for (k in 1:k_folds){

  train <-   analysis(folds$splits[[k]])
  test  <- assessment(folds$splits[[k]])


# =================================================
# ハイパーパラメータの最適化

# コストパラメータCは、誤分類をどの程度許容するかを決めるパラメータ
# Cが小さいほど誤分類を許容するように、大きいほど誤分類を許容しないように超平面を決定する。
# ソフトマージンにおいて、ξ（クサイ）の総和はなるべく小さくしたいが、そのξの総和にかかる係数がC。

# ガンマはRBFカーネルの式の中で現れる。
# ガンマが小さいほど単純な決定境界となり、大きいほど複雑な決定境界となる。


# 後の分類を行う際のパラメータ（gammaとcost）をグリッドサーチ（これが時間がかかる）
pt1 <- proc.time() # スタートの時刻を記録
tune <- tune.svm(Sections ~ ., # Sections行をラベルとする
                 data = train, # パラメータ最適化に用いるデータ
                 gamma = 10^(seq(-3, 0, 0.1)), # 振るgammaの値
                 cost = 10^(seq(0, 2, 0.1)), # 振るcostの値
                 tunecontrol = tune.control(sampling = "cross", cross = 5)) # 5-foldクロスバリデーションで探索
# 18分くらいかかる
# tune <- tune.svm(Sections ~ ., data = train,
#                  gamma = 10^(seq(-5, 5, 0.1)), cost = 10^(seq(-2, 2, 0.1)),
#                  tunecontrol = tune.control(sampling = "cross", cross = 5)) # 4時間くらいかかる
pt2 <- proc.time() # 終了の時刻を記録
dulation[k] <- ((pt2 - pt1) / 60)[3]  # 要した時間を分で表す

# 計算で求めた最適化パラメータを格納
tune_list[[k]] <- tune

# 最適化パラメータをわかりやすくプロット
ggplot(tune$performances, aes(x = gamma, y = cost))+
  geom_tile(aes(fill = error), colour = "white")+
  scale_x_log10()+
  scale_y_log10()+
  scale_fill_gradient(low = "#f6adad", high = "#6f64a4",
                      limits = c(tune$best.performance, max(tune$performances$error)/2), 
                      oob = squish)+
  geom_point(data = tune$best.parameters, aes(x = gamma, y = cost))


# =================================================
# 分類器の作成

classifier <- svm(Sections ~ ., # Section行をラベルとする
                  data = train, # 分類器の作成に用いるデータ
                  method = "C-classification",
                  kernel = "radial", # カーネルはラジアルベーシスファンクション
                  gamma = tune$best.parameters$gamma, # 上記最適化で求めたgamma
                  cost = tune$best.parameters$cost) # 上記最適化で求めたcost


# =================================================
# 作成された分類器を用いて、テストデータを分類

# テストデータが分類器によってどこに分類されたのか
predict_results[[k]] <- predict(classifier, test)

# 混同行列　分類結果と真の分類とを比べる
conf_mat <- table(pred = predict(classifier, test), true = test$Sections)
colnames(conf_mat) <- paste("t", colnames(conf_mat), sep = "")
rownames(conf_mat) <- paste("p", rownames(conf_mat), sep = "")

# 正確度（Accuracy）を以下の計算式で算出
Accuracy[k] <- sum(diag(conf_mat)) / sum(conf_mat)
# sprintf("Accuracy: %.1f%%    (%d/%d)", Accuracy * 100, sum(diag(conf_mat)), sum(conf_mat))

# 適合率（Precision）　精度、陽性的中率（PPV）
# 真陽性／（真陽性＋偽陽性）
# 検査で陽性になった人の中で、実際にその病気に罹患している人の割合
# ある分野Aに分類されたアブストの中で、実際にその分野の研究であるアブストの割合
Precision[k,] <- diag(conf_mat) / rowSums(conf_mat)

# 再現率（recall）　感度、真陽性率（true positive rate）
# 真陽性／（真陽性＋偽陰性）
# 実際にその病気に罹患している人の中で、検査で陽性になった人の割合
# ある分野Aのアブストの中で、分類によって正解の分野Aに分類されたアブストの割合
TPR[k,] <- diag(conf_mat) / colSums(conf_mat)

# F1スコア、F値
# 適合率と再現性の調和平均（逆数の算術平均の逆数：割合の平均で使われる）
# カテゴリー毎にF1スコアが計算される。
# 最終的に算出された複数のF1スコアを平均して１つの値にする。
F1[k] <- mean(2 / (1/Precision[k,] + 1/TPR[k,]))

confusion_matrices[[k]] <- conf_mat

# 交差検証のループ終了
}


# =================================================
# 分類結果、分類精度の表示

# ハイパーパラメータ（cost, gamma）決定のためのグリッドサーチに要した時間
sprintf("%.0f minutes", dulation)

# 混同行列
confusion_matrices

# 正確度（Accuracy）
mean(Accuracy, na.rm = T)

# 適合率（Precision）
colMeans(Precision, na.rm = T)

# 再現率（recall）
colMeans(TPR, na.rm = T)

# F1スコア
mean(F1, na.rm = T)



