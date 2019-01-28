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
      menuItem("小公主(工具)",tabName = "a", icon = icon("caret-square-right")),
      menuItem("遗传分析",tabName = "b", icon = icon("caret-square-right")),
      menuItem("GWAS分析",tabName = "c", icon = icon("caret-square-right")),
      menuItem("GS分析",tabName = "d", icon = icon("caret-square-right"))
    )),
  dashboardBody(
    tabItems(
    tabItem(tabName = "a",
            tabBox(width = 12,
                   tabPanel("ped文件转换ID",
                            h4('1, ped ID替换使用指南'),
                            br('1, ped文件第一列编号,第二列是需要替换的ID,第三列以后为SNP数据;'),
                            br('2, ID文件第一列为ped文件中需要替换的ID, 第二列为正确的ID;\n'),
                            br('3, 上传完数据, 自动分析, 下载数据结果即可.'),
                            br(),
                            fileInput("dat1_1","上传ped文件",accept = ".ped"),                         
                            tableOutput('head1_1'),
                            fileInput("dat1_2","上传id文件",accept = ".csv"),
                            tableOutput("head1_2"),
                            h5("结果文件\n"),
                            downloadButton("down1", "下载处理后的ped文件")
                            ),
                   tabPanel("计算近交系数和亲缘关系系数",
                            h4('2, 系谱近交系数使用指南'),
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
                   tabPanel("表型数据汇总统计",
                            h4('3, 汇总数据使用指南'),
                            br('1, 表型数据, 第一列为ID, 第二列以后为观测值, 不限制性状数'),
                            br('2, 上传完数据, 自动分析, 下载数据结果即可.结果文件可以下载html和word版'),
                            br(),
                            fileInput("dat3","上传表型数据",accept = ".csv"),                         
                            tableOutput('head3'),
                            h5("结果文件\n"),
                            downloadButton("down3", "下载数据汇总报表html"),
                            br(),
                            br(),
                            downloadButton("downa", "汇总统计Excel"),
                            br(),
                            downloadButton("down4", "下载数据汇总报表word")
                            ),
                   tabPanel("检查系谱是否有问题",
                            h4('4, 检查系谱程序'),
                            br('1, 上传系谱文件, 系谱文件包括三列:ID, Sire, Dam'),
                            br('2, 上传完数据, 自动分析'),
                            br(),
                            fileInput("dat4","上传表型数据",accept = ".csv"),    
                            
                            h4("系谱是否错误, 可以检查的类型包括以下"),
                            br(),
                            br('1, 检查系谱是否存在重复个体编号；如果存在，删除重复个体，并给出提示'),
                            br('2, 检查是否存在同时做父本和母本的个体；如果存在，给出提示'),
                            br('3, 检查是否存在系谱循环Pedigree loop，即个体互为祖先和后裔；如果存在，给出提示并停止运行程序；'),
                            br('4, 将未包括在个体列的奠基者个体加入到系谱中，并将其双亲设置为丢失值NA；'),
                            br(),
                            downloadButton("down5", "下载数据汇总报表html")
                            ),       
                   tabPanel("加拿大100Kg日龄矫正和背膘厚矫正",
                            br(),
                            br(),
                            fileInput("dat5","上传表型数据",accept = ".csv"),   
                            br(),
                            downloadButton("down5_a", "下载转化后的数据csv"),
                            br(),
                            br(),
                            h4('加拿大100Kg日龄矫正和背膘厚矫正公式'),
                            br('1, 行头名是: animalID	breed	birthdate	sex	testdate	weight	beakfat'),
                            br('2, breed为:YY, LL, DD, HP'),
                            br('3, sex为:F或者M'),
                            br('4, birthdate和testdate为日期格式, 可以缺失'),
                            br('5, weight为体重,beakfat为背膘,可以缺失'),
                            br(),
                            br(),
                            h4('矫正日龄计算公式'),
                            img(src="ad01.png",height=200,width=510),
                            br(),
                            br(),
                            h4('矫正背膘厚计算公式'),
                            img(src="ad02.png"),
                            br(),
                            br(),
                            h4('数据格式'),
                            img(src="ad03.png")
                            )
                   )),
    tabItem(tabName = "b",
            tabBox(width = 12,
            tabPanel("保种分析",
                     h4('1, 保种分析'),
                     br('第一步: 上传数据'),
                     br('第二部: 下载结果'),
                     br(),
                     br(),
                     fileInput("b_dat4x_1","上传表型数据",accept = ".csv"), 
                     br(),
                     br(),
                     downloadButton("b_down5x_1", "下载数据汇总报表html"),
                     br()
                     ),
            tabPanel("血缘分析",
                     h4('2, 血缘分析'),
                     br('第一步: 上传数据'),
                     br('第二部: 下载结果'),
                     br(),
                     br(),
                     fileInput("b_dat4x_2","上传表型数据",accept = ".csv"), 
                     br(),
                     br(),
                     downloadButton("b_down5x_2", "下载数据汇总报表html"),
                     br()
            ),
            tabPanel("亲子鉴定",
                     h4('3, 亲子鉴定'),
                     br('第一步: 上传数据'),
                     br('第二部: 下载结果'),
                     br(),
                     br(),
                     fileInput("b_dat4x_3","上传表型数据",accept = ".csv"), 
                     br(),
                     br(),
                     downloadButton("b_down5x_3", "下载数据汇总报表html"),
                     br()
            ),
            tabPanel("群体分析",
                     h4('4, 群体分析'),
                     br('第一步: 上传数据'),
                     br('第二部: 下载结果'),
                     br(),
                     br(),
                     fileInput("b_dat4x_4","上传表型数据",accept = ".csv"), 
                     br(),
                     br(),
                     downloadButton("b_down5x_4", "下载数据汇总报表html"),
                     br()
            ),
            tabPanel("其它分析",
                     h4('5, 其它分析'),
                     br('第一步: 上传数据'),
                     br('第二部: 下载结果'),
                     br(),
                     br(),
                     fileInput("b_dat4x_5","上传表型数据",accept = ".csv"), 
                     br(),
                     br(),
                     downloadButton("b_down5x_5", "下载数据汇总报表html"),
                     br()
            )
            )),
    tabItem(tabName = "c",
            tabBox(width = 12,
            tabPanel("数据清洗",
                     h4('1, 数据清洗'),
                     br('第一步: 上传数据'),
                     br('第二部: 下载结果'),
                     br(),
                     br(),
                     fileInput("c_dat4x_1","上传表型数据",accept = ".csv"), 
                     br(),
                     br(),
                     downloadButton("c_down5x_1", "下载数据汇总报表html"),
                     br()
                     ),
            tabPanel("GWAS分析",
                     h4('2, GWAS分析'),
                     br('第一步: 上传数据'),
                     br('第二部: 下载结果'),
                     br(),
                     br(),
                     fileInput("c_dat4x_2","上传表型数据",accept = ".csv"), 
                     br(),
                     br(),
                     downloadButton("c_down5x_2", "下载数据汇总报表html"),
                     br()
            ),
            tabPanel("结果筛选",
                     h4('3, 结果筛选'),
                     br('第一步: 上传数据'),
                     br('第二部: 下载结果'),
                     br(),
                     br(),
                     fileInput("c_dat4x_3","上传表型数据",accept = ".csv"), 
                     br(),
                     br(),
                     downloadButton("c_down5x_3", "下载数据汇总报表html"),
                     br()
            ),
            tabPanel("其它分析1",
                     h4('4, 其它分析1'),
                     br('第一步: 上传数据'),
                     br('第二部: 下载结果'),
                     br(),
                     br(),
                     fileInput("c_dat4x_4","上传表型数据",accept = ".csv"), 
                     br(),
                     br(),
                     downloadButton("c_down5x_4", "下载数据汇总报表html"),
                     br()
            ),
            tabPanel("其它分析2",
                     h4('5, 其它分析2'),
                     br('第一步: 上传数据'),
                     br('第二部: 下载结果'),
                     br(),
                     br(),
                     fileInput("c_dat4x_5","上传表型数据",accept = ".csv"), 
                     br(),
                     br(),
                     downloadButton("c_down5x_5", "下载数据汇总报表html"),
                     br()
            )
            )),
    tabItem(tabName = "d",
            tabBox(width = 12,
            tabPanel("数据清洗",
                     h4('1, 数据清洗'),
                     br('第一步: 上传数据'),
                     br('第二部: 下载结果'),
                     br(),
                     br(),
                     fileInput("d_dat4x_1","上传表型数据",accept = ".csv"), 
                     br(),
                     br(),
                     downloadButton("d_down5x_1", "下载数据汇总报表html"),
                     br()
                     ),
            tabPanel("设定模型",
                     h4('2, 设定模型'),
                     br('第一步: 上传数据'),
                     br('第二部: 下载结果'),
                     br(),
                     br(),
                     fileInput("d_dat4x_2","上传表型数据",accept = ".csv"), 
                     br(),
                     br(),
                     downloadButton("d_down5x_2", "下载数据汇总报表html"),
                     br()
            ),
            tabPanel("GS分析",
                     h4('3, GS分析'),
                     br('第一步: 上传数据'),
                     br('第二部: 下载结果'),
                     br(),
                     br(),
                     fileInput("d_dat4x_3","上传表型数据",accept = ".csv"), 
                     br(),
                     br(),
                     downloadButton("d_down5x_3", "下载数据汇总报表html"),
                     br()
            ),
            tabPanel("结果筛选",
                     h4('4, 结果筛选'),
                     br('第一步: 上传数据'),
                     br('第二部: 下载结果'),
                     br(),
                     br(),
                     fileInput("d_dat4x_4","上传表型数据",accept = ".csv"), 
                     br(),
                     br(),
                     downloadButton("d_down5x_4", "下载数据汇总报表html"),
                     br()
            ),
            tabPanel("其它分析",
                     h4('5, 其它分析'),
                     br('第一步: 上传数据'),
                     br('第二部: 下载结果'),
                     br(),
                     br(),
                     fileInput("d_dat4x_5","上传表型数据",accept = ".csv"), 
                     br(),
                     br(),
                     downloadButton("d_down5x_5", "下载转化后的数据csv"),
                     br()
            )
            ))
    )
    )
  )


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
  
  d4 <- reactive({
    inFile3 <- input$dat4
    if (is.null(inFile3)) return(NULL)
    fread(inFile3$datapath)
  })
  
  d5 <- reactive({
    inFile3 <- input$dat5
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
      names(dat2) = c("sample","pigid")
      x = match(dat1$V2,dat2$sample)
      work = sum(is.na(x))
      dat1$V2 = dat2$pigid[x]
      re = dat1
      fwrite(re, file,sep = " ",col.names = FALSE)
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
  
  output$down5 <- downloadHandler(
    filename = function() {
      paste('Data-summary', Sys.time(), sep = '.', 'html')
    },
    content = function(file) {
      dat = d4()
      src <- normalizePath('report3.Rmd')
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report3.Rmd', overwrite = TRUE)
      library(rmarkdown)
      out <- render('report3.Rmd', html_document())
      file.rename(out, file)
    })
  
  output$down5_a <- downloadHandler(
    filename = function() {
      paste("data-", Sys.time(), "Adjust.csv", sep=" ")
    },
    content = function(file) {
      dat = d5()
      options(warn=-1)
      # source("./adjust.R",local = TRUE)
      dat$d100_2_days = NA
      head(dat)
      dat$animalID=as.factor(dat$animalID)
      dat$breed = as.factor(dat$breed)
      dat$sex = as.factor(dat$sex)
      dat$birthdate = as.Date(dat$birthdate)
      dat$testdate = as.Date(dat$testdate)
      
      
      if(!is.null(as.numeric(dat$testdate - dat$birthdate))){
        if(dat$sex == "F"){
          CF = (dat$weight/as.numeric(dat$testdate - dat$birthdate))*1.82604
          dat$d100_2_days = as.numeric(dat$testdate - dat$birthdate) - (dat$weight-100)/CF
        }else if(dat$sex == "M"){
          CF = (dat$weight/as.numeric(dat$testdate - dat$birthdate))*1.714615
          dat$d100_2_days = as.numeric(dat$testdate - dat$birthdate) - (dat$weight-100)/CF
        }else{
          dat$d100_2_days=NA
        }
      }else{
        dat$d100_2_days=NA
      }
      
      BF = data.frame(Cul=rep(c("YY","LL","HP","DD"),each=2),
                      Sex = rep(c("M","F"),4),
                      A = c(12.402,13.706,12.826,13.983,13.113,14.288,13.468,15.654),
                      B = c(0.10653,0.119624,0.114379,0.126014,0.11762,0.124425,0.111528,0.156646))
      dat$d100_2_BF = NA
      if(dat$breed == "YY"){
        if(dat$sex == "M"){
          Av = BF[BF$Cul=="YY"&BF$Sex == "M",]$A
          Bv = BF[BF$Cul=="YY"&BF$Sex == "M",4]
          CF = Av/(Av+(Bv*(dat$weight-100)))
          dat$d100_2_BF = dat$beakfat*CF
        }else if(dat$sex == "F"){
          Av = BF[BF$Cul=="YY"&BF$Sex == "F",3]
          Bv = BF[BF$Cul=="YY"&BF$Sex == "F",4]
          CF = Av/(Av+(Bv*(dat$weight-100)))
          dat$d100_2_BF = dat$beakfat*CF
        }else{
          dat$d100_2_BF = NA
        }
      }else if(dat$breed == "LL"){
        if(dat$sex == "M"){
          Av = BF[BF$Cul=="LL"&BF$Sex == "M",]$A
          Bv = BF[BF$Cul=="LL"&BF$Sex == "M",4]
          CF = Av/(Av+(Bv*(dat$weight-100)))
          dat$d100_2_BF = dat$beakfat*CF
        }else if(dat$sex == "F"){
          Av = BF[BF$Cul=="LL"&BF$Sex == "F",3]
          Bv = BF[BF$Cul=="LL"&BF$Sex == "F",4]
          CF = Av/(Av+(Bv*(dat$weight-100)))
          dat$d100_2_BF = dat$beakfat*CF
        }else{
          dat$d100_2_BF = NA
        }
      }else if(dat$breed == "HP"){
        if(dat$sex == "M"){
          Av = BF[BF$Cul=="HP"&BF$Sex == "M",]$A
          Bv = BF[BF$Cul=="HP"&BF$Sex == "M",4]
          CF = Av/(Av+(Bv*(dat$weight-100)))
          dat$d100_2_BF = dat$beakfat*CF
        }else if(dat$sex == "F"){
          Av = BF[BF$Cul=="HP"&BF$Sex == "F",3]
          Bv = BF[BF$Cul=="HP"&BF$Sex == "F",4]
          CF = Av/(Av+(Bv*(dat$weight-100)))
          dat$d100_2_BF = dat$beakfat*CF
        }else{
          dat$d100_2_BF = NA
        }
      }else if(dat$breed == "DD"){
        if(dat$sex == "M"){
          Av = BF[BF$Cul=="DD"&BF$Sex == "M",]$A
          Bv = BF[BF$Cul=="DD"&BF$Sex == "M",4]
          CF = Av/(Av+(Bv*(dat$weight-100)))
          dat$d100_2_BF = dat$beakfat*CF
        }else if(dat$sex == "F"){
          Av = BF[BF$Cul=="DD"&BF$Sex == "F",3]
          Bv = BF[BF$Cul=="DD"&BF$Sex == "F",4]
          CF = Av/(Av+(Bv*(dat$weight-100)))
          dat$d100_2_BF = dat$beakfat*CF
        }else{
          dat$d100_2_BF = NA
        }
      }
      
      
      fwrite(dat, file)
      # options(warn=1)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
