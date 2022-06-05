install.packages("MASS")
install.packages("tidyverse")
install.packages("ggfortify")
install.packages("GGally")
## パッケージの読み込み (lda/qda)
require(MASS)
require(tidyverse)
require(ggfortify)
require(GGally)
setwd("/Users/gackt/Library/Mobile Documents/com~apple~CloudDocs/Documents/大学３秋/多変量解析/期末レポ")
##データの読み込み
data <- read.csv("train_data.csv",header=T,row.names=2)
##データの内容を確認
help(data)
str(data)
head(data)
##欠損値削除
data=na.omit(data)
## 解析に使う変数だけ取り出しデータを整理しておく
idx <- which(sapply(data,is.numeric)) # 量的変数の index を取得
idx <- idx[names(idx)!="rank"] # "rank"を取り除く
## データの散布図
ggscatmat(data, columns=idx, color="Position", alpha=.8)
## 主成分分析 (データの正規化 (scale.=TRUE) による違いを見る)
model1 <- prcomp(~ ., data=dplyr::select(data,idx), scale.=FALSE)
model2 <- prcomp(~ ., data=dplyr::select(data,idx), scale.=TRUE)
## 寄与率の違いを表示
summary(model1)
summary(model2)
## 寄与率 (正規化なし):
plot(model1, col="lightblue", main="PCA without scaling")
## 寄与率 (正規化あり):
plot(model2, col="lightblue", main="PCA with scaling")
##主成分得点 (正規化なし):
autoplot(model1, data=data,colour="Position",
         label=TRUE,
         label.size=2,
         loadings=TRUE,
         loadings.colour="blue",
         loadings.label=TRUE,
         loadings.label.size=4,
         loadings.label.colour="blue",
         main="PCA without scaling")
## 主成分得点 (正規化あり)
autoplot(model2, data=data,colour="Position",
         label=TRUE,
         label.size=2,
         loadings=TRUE,
         loadings.colour="blue",
         loadings.label=TRUE,
         loadings.label.size=4,
         loadings.label.colour="blue",
         main="PCA with scaling")
## 主成分得点 (中心部拡大)
autoplot(model2, data=data, colour="Position",
         shape=FALSE,
         label=TRUE,
         label.size=4,
         xlim=c(-.15,.15),ylim=c(-.15,.15),
         main="PCA(zoomed)")



##特徴量とカテゴリによる線形判別関数の構成
model <- lda(Position ~ ., data=data)
print(model) # 結果を表示
## 判別得点の散布図
mydata <- data.frame(data,lda=predict(model, newdata=data)$x)
prop <- model$svd^2/sum(model$svd^2) # 判別の寄与率の計算
ggolot(mydata)








