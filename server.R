#ライブラリ読み込み
library(shiny)
library(readxl)
library(dplyr)
library(DT)

#ファイルを読み込む関数
source("read.data.R")
source("ezchaos.R")


#Maximum upload size exceededを回避
#100MB設定
#https://github.com/rstudio/shiny-examples/blob/master/066-upload-file/server.R
#https://stackoverflow.com/questions/18037737/how-to-change-maximum-upload-size-exceeded-restriction-in-shiny-and-save-user
options(shiny.maxRequestSize = 100*1024^2)

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
    
    #差分の回数
    output$diff.times <- renderUI({
      selectInput("diff.times", "Order of the difference", 
                  choices = c("0", "1", "2", "3"),
                  selected = "0")
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
      data <- reactive({raw.data() %>% dplyr::select(input$ydata) %>% 
          as.vec()})
      
      #解析範囲のデータ
      data.range <- reactive({data()[c(input$range[1] : input$range[2])] %>% 
          diff.vec(differences = input$diff.times)})
      
      #時系列形式のグラフ
      output$trend.plot <- renderPlot({plot.trend(data(), range = input$range,
                                                  differences = input$diff.times)})
      
      #単位根検定
      result.adf <- reactive({
        adf.test(data.range())
      })
      
      print(result.adf())
      
      #単位根検定の結果表示
      output$sum.adf <- renderPrint({
        summary(result.adf(), significance = as.numeric(input$significance))
        })
   
      #サロゲートテスト
      result <- reactive({
        surrogate.test(time.series = data.range(), 
                       significance = as.numeric(input$significance), 
                       K = 3)})
      
      #結果表示
      output$sum <- renderPrint({
        summary(result())
        })
      
      #サロゲートテストの結果プロット
      output$surrogate.plot <- renderPlot({plot(result())})
      
      #サロゲートテストの結果表示
      output$su.res <- renderPrint({print(result())})
      
      #埋め込み
      output$embedd <- renderPlot({embedd.plot(data.range(), m = input$m, d = input$d)})
      
      #リカレンスプロット
      output$recurrence <- renderPlot({recurr(data.range(), m = input$m, d = input$d)})

      
    })
    
    
  })



})







