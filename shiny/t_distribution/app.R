# shiny与bslib必须加载
library(shiny)
library(bslib)

# 修改package ##################################################################
library(ggplot2)
# 修改结束 #####################################################################

# Define UI --------------------------------------------------------------------
ui <- page_sidebar(
  
# 修改标题 #####################################################################
  title = "t分布",
# 修改结束 #####################################################################
  
  # Sidebar panel for inputs ---------------------------------------------------
  sidebar = sidebar(
    bg = "#EEEEEE",
    fillable = TRUE,
    
# 修改Input ####################################################################
    numericInput(
      inputId = "df",
      label = "df",
      min = 2,
      value = 2,
      step = 1
    ),
    helpText("H0与H1的自由度相同。"),
    numericInput(
      inputId = "t",
      label = "实际t值(H1)",
      value = 2,
      step = 0.01
    ),
# 修改结束 #####################################################################
    tags$div(
      class = "mt-auto",
      card(
        fill = FALSE,
        class = "w-100",
        card_header("引用"),
        card_body(
          tags$p(
# 修改citation #################################################################
            "张庆垚. (2026).",
            tags$i("t分布"),
            ". 单车欲问边. https://stat.psych.pub/t_distribution/"
# 修改结束 #####################################################################
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
# 修改Output ###################################################################
  card(card_header("比较t与z的H0分布"), plotOutput(outputId = "plot")),
  layout_columns(
    widths = c(6, 6),
    card(card_header("H0分布中t与z的临界值"), tableOutput(outputId = "table_xs")),
    card(card_header("实际t值(H1)的精确p值"), tableOutput(outputId = "table_ps"))
    )
# 修改结束 #####################################################################
) # ui结束

# Define server logic required to draw a histogram ####

server <- function(input, output) {
  
# 修改计算 #####################################################################
  output$plot <- renderPlot({
    
    # 视野x轴下限与上限
    LS_coord <- range(qt(c(0.0005, 0.9995), df = input$df), input$t)
    
    # 临界值
    LLz <- qnorm(0.025)
    ULz <- qnorm(0.975)
    LLt <- qt(0.025, df = input$df)
    ULt <- qt(0.975, df = input$df)
    
    # x
    x <- seq(LS_coord[1], LS_coord[2], 0.001)
    length_x <- length(x)
    
    # 正态概率密度曲线数据
    datz <- data.frame(
      x = x,
      Density = dnorm(x),
      Distribution = rep("H0标准正态分布", length_x))
    # 正态尾部数据
    datzTailLeft <- datz[datz$x <= LLz, ]
    datzTailLeft$Area <- "标准正态分布左尾2.5%"
    datzTailRight <- datz[datz$x >= ULz, ]
    datzTailRight$Area <- "标准正态分布右尾2.5%"
    datzTail <- rbind(datzTailLeft, datzTailRight)
    
    # t概率密度曲线数据
    datt <- data.frame(
      x = x,
      Density = dt(x, input$df),
      Distribution = rep(paste0("H0 t(", input$df, ")分布"), length_x))
    # t尾部数据
    dattTailLeft <- datt[datt$x <= LLt, ]
    dattTailLeft$Area <- paste0("t(", input$df, ")分布左尾2.5%")
    dattTailRight <- datt[datt$x >= ULt, ]
    dattTailRight$Area <- paste0("t(", input$df, ")分布右尾2.5%")
    dattTail <- rbind(dattTailLeft, dattTailRight)
    
    dat <- rbind(datz, datt)
    dat$Distribution <- factor(dat$Distribution, 
                               levels = c(paste0("H0 t(", input$df, ")分布"), "H0标准正态分布"))
    
    plot <- ggplot(dat, aes(x, Density)) +
      geom_path(aes(color = Distribution), alpha = 0.5) +
      geom_area(aes(fill = Area), data = datzTail, alpha = 0.5) + 
      geom_area(aes(fill = Area), data = dattTail, alpha = 0.5) + 
      geom_vline(xintercept = input$t, linetype = 3) +
      annotate("text", input$t, 0, label = round(input$t, 2), angle = 90) +
      theme_classic(base_size = 18)
    plot
  })
  
  output$table_xs <- renderTable(
    striped = TRUE,
    hover = TRUE,
    spacing = "s",
    align = "l",
    digits = 4,
    
    data.frame(
    "p_two_tailed" = c(0.200, 0.100, 0.050, 0.020, 0.010, 0.002, 0.001),
    "p_one_tailed" = c(0.100, 0.050, 0.025, 0.010, 0.005, 0.001, 0.0005),
    t = qt(p = c(0.100, 0.050, 0.025, 0.010, 0.005, 0.001, 0.0005), df = input$df),
    z = qnorm(p = c(0.100, 0.050, 0.025, 0.010, 0.005, 0.001, 0.0005)))
  )
  
  output$table_ps <- renderTable(
    striped = TRUE,
    hover = TRUE,
    spacing = "s",
    align = "l",
    digits = 4,
    data.frame(
      Statistic = c(paste0("t(", input$df, ") = ", input$t), paste0("z = ", input$t)),
      "p_two_tailed" = c(pt(-abs(input$t), input$df)*2, pnorm(-abs(input$t))*2),
      "p_one_tailed" = c(pt(-abs(input$t), input$df), pnorm(-abs(input$t)))
    ))
# 修改结束 #####################################################################
}
shinyApp(ui = ui, server = server)