# shiny与bslib必须加载
library(shiny)
library(bslib)

# 修改package ##################################################################
library(ggplot2)
# 修改结束 #####################################################################

# Define UI --------------------------------------------------------------------
ui <- page_sidebar(
  
# 修改标题 #####################################################################
  title = "常见数据分布",
# 修改结束 #####################################################################
  
  # Sidebar panel for inputs ---------------------------------------------------
  sidebar = sidebar(
    bg = "#EEEEEE",
    fillable = TRUE,
    
# 修改Input ####################################################################
    numericInput(
      inputId = "df1",
      label = "df1",
      value = 3,
      min = 1
    ),
    helpText("df1仅用于F分布。"),
    numericInput(
      inputId = "df2",
      label = "df2",
      value = 30,
      min = 2
    ),
    helpText("df2用于卡方分布、t分布与F分布。此处df2的有效最小值为2。"),
    textInput(
      inputId = "color",
      label = "颜色",
      value = "#1661AB"
    ),
    helpText("颜色的数值可以是RGB值、HEX值、R中657种颜色名称或R调色板中的颜色编号(1-8)。"),
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
            tags$i("常见数据分布"),
            ". 单车欲问边. https://stat.psych.pub/common_distribution/"
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
layout_column_wrap(
    width = 1/3,
    card(card_header("正态分布"), card_body(plotOutput(outputId = "norm_plot"))),
    card(card_header("卡方分布"), card_body(plotOutput(outputId = "chi_plot"))),
    card(card_header("t分布"), card_body(plotOutput(outputId = "t_plot"))),
    card(card_header("F分布"), card_body(plotOutput(outputId = "f_plot"))),
    )
# 修改结束 #####################################################################
) # ui结束

# Define server logic required to draw a histogram ####

server <- function(input, output) {
  
# 修改计算 #####################################################################
  
  
  output$norm_plot <- renderPlot({
    dat <- data.frame(x = seq(-4, 4, 0.01))
    ggplot(dat, aes(x)) +
      geom_function(fun = dnorm, color = input$color) +
      labs(x = "z", y = "Density") +
      theme_bw(base_size = 16)
  })
  output$chi_plot <- renderPlot({
    dat <- data.frame(x = seq(0.001, qchisq(0.0001, input$df2, lower.tail = FALSE), 0.001))
    ggplot(dat, aes(x)) +
      geom_function(fun = dchisq, args = list(df = input$df2), color = input$color) +
      labs(x = "Chi-square", y = "Density") +
      theme_bw(base_size = 16) +
      coord_cartesian(xlim = c(-4, qchisq(0.0001, input$df2, lower.tail = FALSE)))
  })
  output$t_plot <- renderPlot({
    dat <- data.frame(x = seq(-4, 4, 0.01))
    ggplot(dat, aes(x)) +
      geom_function(fun = dt, args = list(df = input$df2), color = input$color) +
      labs(x = "t", y = "Density") +
      theme_bw(base_size = 16)
  })
  output$f_plot <- renderPlot({
    dat <- data.frame(x = seq(0.001, qf(0.0001, input$df1, input$df2, lower.tail = FALSE), 0.001))
    ggplot(dat, aes(x)) +
      geom_function(fun = df, args = list(df1 = input$df1, df2 = input$df2), color = input$color) +
      labs(x= "F", y = "Density") +
      theme_bw(base_size = 16) +
      coord_cartesian(xlim = c(-4, qf(0.0001, input$df1, input$df2, lower.tail = FALSE)))
  })
# 修改结束 #####################################################################
}
shinyApp(ui = ui, server = server)