# shiny与bslib必须加载
library(shiny)
library(bslib)

# 修改package ##################################################################
library(ggplot2)
# 修改结束 #####################################################################

# Define UI --------------------------------------------------------------------
ui <- page_sidebar(
  
# 修改标题 #####################################################################
  title = "数据分布",
# 修改结束 #####################################################################
  
  # Sidebar panel for inputs ---------------------------------------------------
  sidebar = sidebar(
    bg = "#EEEEEE",
    fillable = TRUE,
    
# 修改Input ####################################################################
    numericInput(
      inputId = "mu",
      label = "均值",
      value = 100
    ),
    numericInput(
      inputId = "sigma",
      label = "标准差",
      value = 15
    ),
    numericInput(
      inputId = "n",
      label = "样本量",
      value = 1000,
      min = 1
    ),
    numericInput(
      inputId = "bins",
      label = "分组数量",
      value = 50,
      min = 1
    ),
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
            tags$i("数据分布"),
            ". 单车欲问边. https://stat.psych.pub/distribution/"
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
    card(card_header("分组频数分布图"), card_body(plotOutput(outputId = "freq_plot"))),
    card(card_header("箱线图"), card_body(plotOutput(outputId = "box_plot"))),
    card(card_header("累计频数分布图"), card_body(plotOutput(outputId = "cumsum_freq_plot"))),
    card(card_header("相对频数分布图"), card_body(plotOutput(outputId = "prop_plot"))),
    card(card_header("概率密度图"), card_body(plotOutput(outputId = "d_plot"))),
    card(card_header("累计概率分布图"), card_body(plotOutput(outputId = "cumsum_prob_plot")))
    )
# 修改结束 #####################################################################
) # ui结束

# Define server logic required to draw plots ####

server <- function(input, output) {
  
# 修改计算 #####################################################################
  dat_fun <- reactive(data.frame(x = rnorm(input$n, input$mu, input$sigma)))
  
  output$freq_plot <- renderPlot({
    dat <- dat_fun()
    ggplot(dat, aes(x)) +
      geom_histogram(bins = input$bins, fill = input$color) +
      theme_bw(base_size = 16)
  })
  output$d_plot <- renderPlot({
    dat <- dat_fun()
    ggplot(dat, aes(x)) +
      geom_function(fun = dnorm, args = list(mean = input$mu, sd = input$sigma), linewidth = 1) +
      geom_density(color = input$color) + 
      annotate("text", 
               label = c("正态分布", "实际分布"), 
               color = c("black", input$color), 
               x = 100, y = c(0.03, 0.02)) +
      theme_bw(base_size = 16)
  })
  output$prop_plot <- renderPlot({
    dat <- dat_fun()
    ggplot(dat, aes(x)) +
      geom_histogram(bins = input$bins, aes(y = after_stat(count/input$n)), fill = input$color) +
      labs(y = "Proportion") +
      theme_bw(base_size = 16)
  })
  output$cumsum_freq_plot <- renderPlot({
    dat <- dat_fun()
    ggplot(dat, aes(x)) +
      geom_histogram(bins = input$bins, aes(y = cumsum(after_stat(count))), fill = input$color) +
      labs(y = "Cumulative Frequency") +
      theme_bw(base_size = 16)
  })
  output$cumsum_prob_plot <- renderPlot({
    dat <- dat_fun()
    ggplot(dat, aes(x)) +
      geom_histogram(bins = input$bins, aes(y = cumsum(after_stat(count/input$n))), fill = input$color) +
      labs(y = "Cumulative Probability") +
      theme_bw(base_size = 16)
  })
  output$box_plot <- renderPlot({
    dat <- dat_fun()
    ggplot(dat, aes(y = x)) +
      geom_boxplot(color = input$color) +
      scale_x_continuous(labels  = NULL) +
      theme_bw(base_size = 16)
  })
# 修改结束 #####################################################################
}
shinyApp(ui = ui, server = server)