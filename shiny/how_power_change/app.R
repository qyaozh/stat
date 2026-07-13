library(shiny)
library(bslib)
library(ggplot2)

# Define UI -------------------------------------------------------------------
ui <- page_sidebar(
  # 修改标题 TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT####
  title = "统计检验力如何变化",
  # 修改结束 LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL####
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    bg = "#EEEEEE",
    fillable = TRUE,
  
  # 修改输入TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT####  
    helpText("假定H0与H1总体的标准差相等，使用单样本z检验比较H0与H1均值的差异。"),
    numericInput(
      inputId = "muH0",
      label = "H0总体均值",
      step = 0.01,
      value = 0
    ),
    numericInput(
      inputId = "muH1",
      label = "H1总体均值",
      step = 0.01,
      value = 2
    ),
    numericInput(
      inputId = "sigma",
      label = "总体标准差",
      step = 0.01,
      value = 10
    ),
    numericInput(
      inputId = "n",
      label = "样本量",
      min = 1,
      step = 1,
      value = 100
    ),
    # 修改结束 LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL####
    tags$div(
      class = "mt-auto",
      card(
        fill = FALSE,
        class = "w-100",
        card_header("引用"),
        card_body(
          tags$p(
    # 修改citation TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT####
            "张庆垚. (2026).",
            tags$i("统计检验力如何变化"),
            ". 单车欲问边. https://stat.psych.pub/how_power_change/"
    # 修改结束 LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL####
          )
        )
      ),
    tags$a(
        class = "btn btn-sm btn-outline-secondary w-100 text-center",
        href = "https://stat.psych.pub",
        icon("home"),
        "返回主页"
      )
    )
  ),
  
  # 修改Output TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT####
    card(card_header("比较H0与H1抽样分布"),
         plotOutput("H0H1plot"))
  # 修改结束 LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL####
) # ui结束

# Define server----------------------------------------------------------------
server <- function(input, output) {
  
  # 修改计算 TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT####
  
  output$H0H1plot <- renderPlot({

    # 标准误
    SE <- input$sigma/sqrt(input$n)
    
    # z的临界值，H1切割出来的尾巴的临界值
    LL_z <- input$muH0 - abs(input$muH1 - input$muH0)
    UL_z <- input$muH0 + abs(input$muH0 - input$muH1)
    
    # 视野下限与上限
    LS_coord <- range(sapply(c(0.0005, 0.9995), function(x) qnorm(x, c(input$muH0, input$muH1), SE)))
    
    # x轴下限与上限
    LL_x <- min(LL_z, LS_coord)
    UL_x <- max(UL_z, LS_coord)
    
    # H0临界值
    # H0 95%CI下限
    LLCI_H0 <- qnorm(0.025, input$muH0, SE)
    # H0 95%CI上限
    ULCI_H0 <- qnorm(0.975, input$muH0, SE)
    
    # 单样本z检验
    z <- (input$muH1 - input$muH0)/SE
    pz <- (1 - pnorm(abs(z))) * 2
    
    # H0抽样分布的作图数据 -----------------------------------------------
    # 横坐标为均值
    M <- seq(LL_x, UL_x, 0.001)
    # 纵坐标为相应的IQ均值的概率密度
    Density <- dnorm(M, input$muH0, SE)
    datH0 <- data.frame(M, Density)
    
    # H0尾部作图数据
    datH0LeftTail <- data.frame(M = seq(LL_x, LLCI_H0, 0.001), 
                                Density = dnorm(seq(LL_x, LLCI_H0, 0.001), input$muH0, SE))
    datH0LeftTail$Area <- "H0左尾2.5%"
    # H0尾部作图数据
    datH0RightTail <- data.frame(M = seq(ULCI_H0, UL_x, 0.001), 
                                 Density = dnorm(seq(ULCI_H0, UL_x, 0.001), input$muH0, SE))
    datH0RightTail$Area <- "H0右尾2.5%"
    # 合并数据
    datH0p <- rbind(datH0LeftTail, datH0RightTail)
    
    # H0分布中z的精确p值两侧尾部作图数据 -----------------------------------------------

    # z左侧尾部作图数据
    datH1LeftTail <- data.frame(M = seq(LL_x, LL_z, 0.001), 
                                Density = dnorm(seq(LL_x, LL_z, 0.001), input$muH0, SE))
    datH1LeftTail$Area <- "z的p值左尾"
    # z右侧尾部作图数据
    datH1RightTail <- data.frame(M = seq(UL_z, UL_x, 0.001), 
                                 Density = dnorm(seq(UL_z, UL_x, 0.001), input$muH0, SE))
    datH1RightTail$Area <- "z的p值右尾"
    # 合并数据
    datH1p <- rbind(datH1LeftTail, datH1RightTail)
    
    # H1概率密度作图数据 -------------------------------------------------------
    datH1 <- data.frame(M, Density = dnorm(M, mean = input$muH1, sd = SE))
    
    # H1 分布中 beta 数据 ----------------------------------------------------------------
    datBeta <- data.frame(M = seq(LLCI_H0, ULCI_H0, 0.001), 
                          Density = dnorm(seq(LLCI_H0, ULCI_H0, 0.001), mean = input$muH1, sd = SE))
    datBeta$Area <- "二类错误"
    
    # 合并H0H1概率密度曲线数据 -----------------------------------------------------
    datH0H1 <- rbind(datH0, datH1)
    datH0H1$Distribution <- c(rep("H0", nrow(datH0)), rep("H1", nrow(datH0)))
    
    # 合并H0拒绝域与H1beta域
    datH0pBeta <- rbind(datH0p, datBeta)
    datH0pBeta$Area <- factor(datH0pBeta$Area, levels = c("H0左尾2.5%", "H0右尾2.5%", "二类错误"))
    
    # 参考线数据 ---------------------------------------------------------------
    dat_refence <- data.frame(
      xintercept = c(input$muH0, input$muH1), 
      mu = c("H0", "H1"))
    
    # annotation label---------------------------------------------------------------
    caption <- paste0(
      "SE = ", round(SE, 4), ", z = ", round(z, 4), ", p = ", round(pz, 4), 
      ", 二类错误 = ", round(pnorm(ULCI_H0, input$muH1, SE) - pnorm(LLCI_H0, input$muH1, SE), 4),
      ", 统计检验力 = ", round(1 - pnorm(ULCI_H0, input$muH1, SE) + pnorm(LLCI_H0, input$muH1, SE), 4))
    
    # 作图 ---------------------------------------------------
    H0H1plot <- ggplot(datH0H1, aes(M, Density)) +
      geom_path(aes(color = Distribution)) +
      geom_vline(aes(xintercept = xintercept, color = mu), data = dat_refence, alpha = 0.8, linetype = 3) +
      geom_area(aes(fill = Area), data = datH1p, alpha = 0.5) +
      geom_area(aes(M, Density, fill = Area), data = datH0pBeta, alpha = 0.5) +
      annotate("text", 
               x = c(LLCI_H0, ULCI_H0, LL_z, UL_z, input$muH0, input$muH1), 
               y = 0, angle = 90, size = 6, hjust = 0,
               label = round(c(LLCI_H0, ULCI_H0, LL_z, UL_z, input$muH0, input$muH1), 2)) +
      labs(caption = caption) +
      theme_classic(base_size = 20) +
      coord_cartesian(xlim = LS_coord)

    H0H1plot
  })
  # 修改结束 LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL####
}
shinyApp(ui = ui, server = server)