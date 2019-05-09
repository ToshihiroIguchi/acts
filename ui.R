library(shiny)

#読み込むファイルのタイプ
file.accept <- c(
  #"text/csv", "text/comma-separated-values,text/plain", 
                 ".csv", ".xls", ".xlsx")


shinyUI(fluidPage(

  # Application title
  titlePanel("Analysis of chaotic time series"),

  sidebarLayout(
    sidebarPanel(
      
      #ファイル読み込み
      fileInput("file", "Data file (.csv, .xls, xlsx)", accept = file.accept),
      
      #シート選択
      htmlOutput("sheet"),
      
      #調査対象選択
      htmlOutput("ydata"),
      
      #解析範囲
      htmlOutput("range"),
      
      #差分
      htmlOutput("diff.times"),
      
      
      #閾値
      htmlOutput("significance"),
      
      #サロゲートテストの結果
      verbatimTextOutput("sum")
      
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  
                  #ファイルの内容をそのまま表示
                  tabPanel("Data",
                           #https://code.i-harness.com/ja/q/227d360
                           tags$head(tags$link(
                             rel = "stylesheet", type = "text/css", href = "my.css")),
                           
                           DT::dataTableOutput("data")
                           ),
                  
                  #時系列の内容を表示
                  tabPanel("Trend",
                           plotOutput("trend.plot"),
                           verbatimTextOutput("sum.adf")
                           ),
                  
                  #サロゲートテストの結果表示
                  tabPanel("Surrogate",
                           plotOutput("surrogate.plot"),
                           verbatimTextOutput("su.res")),
                  
                  #埋め込みとリカレンスプロット
                  tabPanel("Plot",
                           
                           #パラメータ設定
                           fluidRow(column(6, numericInput("m", label = "Dimension", 
                                                           value = 3, min = 1, step = 1)),
                                    column(6, numericInput("d", label = "Time delay", 
                                                           value = 1, min = 1, step = 1))),
                           
                           #画像表示
                           fluidRow(column(6, plotOutput("embedd")),
                                    column(6, plotOutput("recurrence")))
                         
                  )
      
      
    )
  )
)
))