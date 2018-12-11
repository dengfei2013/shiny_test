library(shiny)
library(shinydashboard)
library(data.table)

ui <- dashboardPage(
  dashboardHeader(title = "康普森农业分析平台"),

  dashboardSidebar(
    sidebarMenu(
      a(img(src="kps.logo.png",height=96,width=210),href="http://www.kangpusen.com/AboutUs/"),
      br(),
      br(),
      menuItem("ped文件转换ID",tabName = "a", icon = icon("caret-square-right")),
      menuItem("计算近交系数和亲缘关系系数",tabName = "b", icon = icon("caret-square-right")),
      menuItem("表型数据汇总统计",tabName = "c", icon = icon("caret-square-right"))
      
    )),
  dashboardBody(
    tabItems(
    tabItem(tabName = "a",
            fileInput("dat1_1","上传ped文件",accept = ".ped"),                         
            tableOutput('head1_1'),
            fileInput("dat1_2","上传id文件",accept = ".csv"),
            tableOutput("head1_2"),
            h5("结果文件\n"),
            downloadButton("down1", "下载处理后的ped文件")),
    
    tabItem(tabName = "b",
            fileInput("dat2","上传三列系谱数据",accept = ".csv"),                         
            tableOutput('dat2'),
            h5("结果文件\n"),
            downloadButton("down2", "下载近交系数及亲缘关系系数")
            ),
    
    tabItem(tabName = "c",
            fileInput("dat3","上次表型数据",accept = ".csv"),                         
            tableOutput('head3'),
            h5("结果文件\n"),
            downloadButton("down3", "下载数据汇总报表html")
            )
  )
))

options(shiny.maxRequestSize=3000*1024^2)
server <- function(input, output) {
  
  # data = mtcars
  d1_1 <- reactive({
    inFile1 <- input$dat1_1
    if (is.null(inFile1)) return(NULL)
    fread(inFile1$datapath)
  })
  
  d1_2 <- reactive({
    inFile2 <- input$dat1_2
    if (is.null(inFile2)) return(NULL)
    fread(inFile2$datapath)
  })
  
  d2 <- reactive({
    inFile2 <- input$dat2
    if (is.null(inFile2)) return(NULL)
    fread(inFile2$datapath)
  })
  
  d3 <- reactive({
    inFile3 <- input$dat3
    if (is.null(inFile3)) return(NULL)
    fread(inFile3$datapath)
  })
  
  output$head1_1 <- renderTable({
    dat1 <- d1_1()
    dat1[1:5,1:5]
  })
  
  output$head2 <- renderTable({
    dat2 <- d1_2()
    head(dat2)
  })
  
  output$head2 <- renderTable({
    dat1 <- d2()
    dat1[1:5,1:5]
  })
  
  output$head3 <- renderTable({
    dat2 <- d3()
    head(dat2)
  })
  

  output$down1 <- downloadHandler(
    filename = function() {
      paste("data-", Sys.time(), ".ped", sep=" ")
    },
    content = function(file) {
      dat1 <- d1()
      dat2 = d2()
      x = match(dat1$V2,dat2$sample)
      work = sum(is.na(x))
      dat1$V2 = dat2$pigid[x]
      re = dat1
      fwrite(re, file,sep = " ")
    }
  )
  
  output$down3 <- downloadHandler(
    filename = function() {
      paste('Data-summary', Sys.time(), sep = '.', 'html')
    },
    content = function(file) {
      dat = d3()
      src <- normalizePath('report1.Rmd')
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report1.Rmd', overwrite = TRUE)
      library(rmarkdown)
      out <- render('report1.Rmd', html_document())
      file.rename(out, file)
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
