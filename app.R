library(shiny)
library(shinydashboard)
library(data.table)
library(learnasreml)
library(nadiv)

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
            h4('使用指南'),
            br('1, ped文件第一列编号,第二列是需要替换的ID,第三列以后为SNP数据;'),
            br('2, ID文件第一列为ped文件中需要替换的ID, 第二列为正确的ID;\n'),
            br('3, 上传完数据, 自动分析, 下载数据结果即可.'),
            br(),
            fileInput("dat1_1","上传ped文件",accept = ".ped"),                         
            tableOutput('head1_1'),
            fileInput("dat1_2","上传id文件",accept = ".csv"),
            tableOutput("head1_2"),
            h5("结果文件\n"),
            downloadButton("down1", "下载处理后的ped文件")),
    
    tabItem(tabName = "b",
            h4('系谱近交系数使用指南'),
            br('1, 上传系谱文件, 系谱文件包括三列:ID, Sire, Dam'),
            br('2, 上传完数据, 自动分析, 下载数据结果即可.'),
            br(),
            fileInput("dat2","上传三列系谱数据",accept = ".csv"),                         
            tableOutput('head2'),
            h5("近交系数结果文件\n"),
            downloadButton("down2_1", "下载近交系数csv"),
            br(),
            br(),
            h5("亲缘关系系数结果文件\n"),
            downloadButton("down2_2", "下载亲缘关系系数csv")
            ),
    
    tabItem(tabName = "c",
            h4('汇总数据使用指南'),
            br('1, 表型数据, 第一列为ID, 第二列以后为观测值, 不限制性状数'),
            br('2, 上传完数据, 自动分析, 下载数据结果即可.结果文件可以下载html和word版'),
            br(),
            fileInput("dat3","上次表型数据",accept = ".csv"),                         
            tableOutput('head3'),
            h5("结果文件\n"),
            downloadButton("down3", "下载数据汇总报表html"),
            br(),
            br(),
            downloadButton("downa", "汇总统计Excel"),
            br(),
            downloadButton("down4", "下载数据汇总报表word")
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
  
  output$head1_2 <- renderTable({
    dat2 <- d1_2()
    head(dat2)
  })
  
  output$head2 <- renderTable({
    dat1 <- d2()
    head(dat1)
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
      dat1 = d1_1()
      dat2 = d1_2()
      x = match(dat1$V2,dat2$sample)
      work = sum(is.na(x))
      dat1$V2 = dat2$pigid[x]
      re = dat1
      fwrite(re, file,sep = " ")
    }
  )
  
  output$down2_1 <- downloadHandler(
    filename = function() {
      paste("data-", Sys.time(), "inbreeding.csv", sep=" ")
    },
    content = function(file) {
      dat = d2()
      options(warn=-1)
      pped = prepPed(dat)
      id = as.data.frame(pped)[,1]
      options(warn=0)
      A = makeA(pped)
      A1 = as.matrix(A)
      re = data.frame(ID = id,inbreeding = round(c(diag(A1) -1),5))
      fwrite(re, file)
    }
  )
  
  output$down2_2 <- downloadHandler(
    filename = function() {
      paste("data-", Sys.time(), "cofficient.csv", sep=" ")
    },
    content = function(file) {
      ped = d2()
      options(warn=-1)
      pped = prepPed(ped)
      options(warn=0)
      A = makeA(pped)
      A1 = as.matrix(A)
      id = as.data.frame(pped)[,1]
      dimnames(A1) = list(c(id),c(id))
      re = mat_2_coefficient(A1)
      fwrite(re, file)
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
  
  output$downa <- downloadHandler(
    filename = function() {
      paste("data-", Sys.time(), ".csv", sep="")
    },
    content = function(file) {
      dat = d3()
      dat = dat
      dd = dat[,2:dim(dat)[2]]
      func <- function(x){c("Total_number" = length(x),"Missing_number" = length(x[is.na(x)]),"Mean"=mean(x,na.rm = T),"Variance"=var(x,na.rm = T),"SD"=sd(x,na.rm = T),"CV"=sd(x,na.rm = T)/mean(x,na.rm = T)*100)}
      
      sm <- as.data.frame(t(apply(dd,2,func)))
      sm = data.frame(ID = row.names(sm),sm)
      fwrite(sm, file,sep = " ")
    }
  )
  
  output$down4 <- downloadHandler(
    filename = function() {
      paste('Data-summary', Sys.time(), sep = '.', 'doc')
    },
    content = function(file) {
      dat = d3()
      src <- normalizePath('report2.Rmd')
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report2.Rmd', overwrite = TRUE)
      library(rmarkdown)
      out <- render('report2.Rmd', word_document())
      file.rename(out, file)
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
