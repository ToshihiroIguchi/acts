#ライブラリ読み込み
library(nonlinearTseries)
library(tseriesChaos)
library(scatterplot3d)
library(ggplot2)

#ベクトル形式に変換
as.vec <- function(x){
  return(x %>% as.matrix() %>% as.vector())
}

#数値が格納されている部分の名前
worth.names <- function(df){
  is.num.names <- df %>% dplyr::summarise_all(funs(is.numeric))
  ret <- colnames(is.num.names)[as.vec(is.num.names)]
  return(ret)
}

#ある文字の場合はNULLを返す
is.chr.null <- function(x, chr = "NA"){
  if(is.null(x)){return(NULL)}
  if(x == chr){return(NULL)}else{return(TRUE)}
}

#時系列をプロット
plot.trend <- function(vec){
  df <- data.frame(x = c(1 : length(vec)), y = vec)
  ggplot(data = df, aes(x = x, y = y)) + 
    geom_line() + 
    labs(x = "Time")+
    theme(axis.text = element_text(size = 20)) #フォントの大きさを変えるのを変数に入れたい。
  
  
}

#サロゲートテスト
surrogate.test <- function(time.series, significance = 0.05, K = 1){
  result <- surrogateTest(time.series = time.series,
                          verbose = FALSE,
                          significance = significance, 
                          K = K,
                          one.sided = FALSE,
                          FUN = timeAsymmetry)
  
}

#サロゲートテストの結果を表示
summary.surrogateTest <- function(result){
  #計算に使用するパラメータ
  min.ss <- min(result$surrogates.statistics)
  max.ss <- max(result$surrogates.statistics)
  ds <- result$data.statistic
  
  #帰無仮説表示
  cat("Null Hypothesis: Data comes from a linear stochastic process\n\n")
  
  #仮説検定の結果を表示
  if(min.ss < ds && max.ss > ds){
    cat("Accept Null hypothesis")
  }else{
    cat("Reject Null hypothesis: Original data's stat is significant larger than surrogates' stats")
  }
}

#埋め込みの表示
embedd.plot <- function(vec, m, d){
  em <- embedd(vec, m = m, d = d)
  scatterplot3d(em, type = "l", color = "red")
}




