#ライブラリ読み込み
library(nonlinearTseries)
library(tseriesChaos)
library(scatterplot3d)
library(ggplot2)
library(grid)
library(grDevices)
library(tseries)


#ベクトル形式に変換
as.vec <- function(x){
  return(x %>% as.matrix() %>% as.vector())
}

#TRUE以外はNULLをかえす
is.true.null <- function(x){
  if(isTRUE(x)){return(TRUE)}else{return(NULL)}
}

#数値が格納されている部分の名前
worth.names <- function(df){
  #is.num.names <- df %>% dplyr::summarise_all(funs(is.numeric))
  is.num.names <- df %>% dplyr::summarise_all(list(~is.numeric(.)))
  ret <- colnames(is.num.names)[as.vec(is.num.names)]
  return(ret)
}

#ある文字の場合はNULLを返す
is.chr.null <- function(x, chr = "NA"){
  if(is.null(x)){return(NULL)}
  if(x == chr){return(NULL)}else{return(TRUE)}
}

#差分を取る
diff.vec <- function(vec, differences = "0"){
  #diffrencesはcharacterで入力
  #shinyで選択させるため
  
  #数値に変換
  diff.num <- as.numeric(differences)
  
  #差分を取る場合と取らない場合で分ける
  if(diff.num == 0){
    #差分をとらない場合はそのまま返す
    ret <- vec
  }else{
    #diffで差分を取る
    ret <- diff(vec, differences = diff.num)
  }
  
  #戻り値
  return(ret)

}

#時系列をプロット
plot.trend <- function(vec, text.size = 12, range = NULL, differences = "0"){
  
  df <- data.frame(x = c(1 : length(vec)), y = vec)
  
  #範囲を指定
  #絶対にもっとスマートな書き方がある
  df.range <- df[c(range[1] : range[2]), ]
  df.range.y <- df.range$y %>% diff.vec(differences = differences)
  df.range <- df.range[c(1:length(df.range.y)), ]
  df.range$y <- df.range.y
  

  gg1 <- ggplot(data = df, aes(x = x, y = y)) + 
    geom_line() + 
    ggtitle("Original Data") +
    xlab(NULL) +
    ylab(NULL) + 
    theme(axis.text = element_text(size = text.size)) +#フォントの大きさを変えるのを変数に入れたい。
    annotate("rect",xmin = range[1],xmax = range[2], ymin = min(vec), ymax = max(vec),
               alpha = 0.1, fill = "green")
  
  gg2 <- ggplot(data = df.range, aes(x = x, y = y)) + 
    geom_line() + 
    ggtitle("Converted Data") +
    xlab(NULL) +
    ylab(NULL) + 
    theme(axis.text = element_text(size = text.size)) #フォントの大きさを変えるのを変数に入れたい。
  
  #グリッド分割
  #https://winlabo.com/post-812
  grid.newpage()#空の画面を作る
  pushViewport(viewport(layout = grid.layout(2, 1)))#画面を区切る
  
  print(gg1, vp = viewport(layout.pos.row=1, layout.pos.col=1))
  print(gg2, vp = viewport(layout.pos.row=2, layout.pos.col=1))
  

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
  cat("Null Hypothesis: Data comes from a linear stochastic process\n")
  
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

#サロゲートテストの結果をggplot2で表示
plot.surrogateTest <- function(result, text.size = 12){
  #データをデータフレームに格納
  df <- data.frame(values = result$`surrogates.statistics`)
  
  #ヒストグラムのバンド幅。scottの方法
  #https://blog.atusy.net/2018/11/09/binwdith-for-geom-histogram/#autobw-by-function
  #https://ja.wikipedia.org/wiki/%E3%83%92%E3%82%B9%E3%83%88%E3%82%B0%E3%83%A9%E3%83%A0#%E3%83%93%E3%83%B3%E3%81%AE%E5%80%8B%E6%95%B0%E3%81%A8%E5%B9%85
  bw.scott <- 3.5*sd(df$values)/((length(df$values))^(1/3))
  
  #表示
  #http://mukkujohn.hatenablog.com/entry/2016/09/28/223957
  #https://stats.biopapyrus.jp/r/ggplot/geom_histogram.html
  #https://blog.atusy.net/2018/11/09/binwdith-for-geom-histogram/#autobw-by-function
  #凡例をつけるなら、surrogate data, original data　かな
  ggplot(df, aes(x = values, y = ..density.., fill = values)) +
    geom_histogram(binwidth = bw.scott, alpha = 0.3, fill = "blue") + 
    geom_vline(xintercept = result$data.statistic, color = "red", size = 1) +
    geom_density(color = "green", fill = "green", alpha = 0.2) +
    ggtitle("Surrogate data testing") +
    xlab("Values of the statistic") +
    theme(axis.text = element_text(size = text.size))
  
  

}








