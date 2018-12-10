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
      menuItem("ped文件ID转换",tabName = "a", icon = icon("caret-square-right")),
      menuItem("计算近交系数和亲缘关系系数",tabName = "b", icon = icon("caret-square-right"))
      
    )),
  dashboardBody(
    tabItems(
    tabItem(tabName = "a",
            fileInput("ped_file","上传ped文件",accept = ".ped"),                         
            tableOutput('head1'),
            fileInput("id_file","上传id文件",accept = ".csv"),
            tableOutput("head2"),
            h5("结果文件\n"),
            downloadButton("downloadData", "下载处理后的ped文件")),
    tabItem(tabName = "b")
  )
))

options(shiny.maxRequestSize=3000*1024^2)
server <- function(input, output) {
  
  # data = mtcars
  d1 <- reactive({
    inFile1 <- input$ped_file
    if (is.null(inFile1)) return(NULL)
    fread(inFile1$datapath)
  })
  
  d2 <- reactive({
    inFile2 <- input$id_file
    if (is.null(inFile2)) return(NULL)
    fread(inFile2$datapath)
  })
  
  output$head1 <- renderTable({
    dat1 <- d1()
    dat1[1:5,1:5]
  })
  
  output$head2 <- renderTable({
    dat2 <- d2()
    head(dat2)
  })
  
  output$re <- renderTable({

  })
  output$downloadData <- downloadHandler(
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
  
}

# Run the application 
shinyApp(ui = ui, server = server)
