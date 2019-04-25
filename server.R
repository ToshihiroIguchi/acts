#ライブラリ読み込み
library(shiny)
library(readxl)
library(dplyr)
library(DT)

#ファイルを読み込む関数
source("read.data.R")
source("ezchaos.R")

#本体
shinyServer(function(input, output, session) {

  #ファイルを選択した場合
  observeEvent(input$file, {
    
    print(input$file$name)
    
    
    #ファイルがxlsかxlsxの場合
    if(is.excel(input$file$name)){
      #xlsかxlsxの場合、シートを選択する
      output$sheet <- renderUI({
        selectInput("sheet", "Excel Sheet",
                    choices = excel.sheet(file = input$file$name))
      })
    }
    
    #ファイル読み込み
    raw.data <- reactive({read.data(input$file$datapath, input$sheet)})
    
    #データテーブルを表示
    #https://github.com/rstudio/shinydashboard/issues/22
    output$data = DT::renderDataTable(raw.data(), options = list(autoWidth = TRUE))
    
    #調査対象の項目名を選択
    output$ydata <- renderUI({
      selectInput("ydata", "Survey target",
                  choices = c("NA", worth.names(raw.data())))
    })
    
    #調査範囲を選択
    output$range <- renderUI({
      sliderInput("range", "Range", min = 1, max = nrow(raw.data()), value = c(1, nrow(raw.data())))
      
    })
    
    
    
    #閾値を選択
    output$significance <- renderUI({
      selectInput("significance", "Significance", 
                  choices = c("0.1", "0.05", "0.01", "0.005", "0.001"),
                  selected = "0.05")
      })
    
    
    #項目名が選択されたら解析
    observeEvent(is.chr.null(input$ydata, chr = "NA"), {
      
      #解析対象のベクトル形式のデータ
      data <- reactive({raw.data() %>% dplyr::select(input$ydata) %>% as.vec()})
      
      #時系列形式のグラフ
      output$trend.plot <- renderPlot({plot.trend(data(), range = input$range)})

      #サロゲートテスト
      result <- reactive({surrogate.test(time.series = data()[c(input$range[1] : input$range[2])], 
                                         significance = as.numeric(input$significance), 
                                         K = 1)})
      
      #結果表示
      output$sum <- renderPrint({summary(result())})
      
      #サロゲートテストの結果プロット
      output$surrogate.plot <- renderPlot({plot(result())})
      
      #サロゲートテストの結果表示
      output$su.res <- renderPrint({print(result())})
      
      #埋め込み
      output$embedd <- renderPlot({embedd.plot(data(), m = input$m, d = input$d)})
      
      #リカレンスプロット
      output$recurrence <- renderPlot({recurr(data(), m = input$m, d = input$d)})

      
    })
    
    
  })



})







