# shiny与bslib必须加载
library(shiny)
library(bslib)

# 修改package ##################################################################
library(ggplot2)
# 修改结束 #####################################################################

# Define UI --------------------------------------------------------------------
ui <- page_sidebar(
  
# 修改标题 #####################################################################
  title = "依恋倾向心理测验",
# 修改结束 #####################################################################
    
# 修改Input ####################################################################
  sidebar = sidebar(
    fillable = TRUE,
    width = "33%",
    tags$b("从这里开始"),
    tags$p("一个人在不同亲密关系中的依恋倾向是不同的。
             亲密关系包括你与父母、兄弟姐妹、配偶(恋人)、好朋友、老师(导师)等重要人物的关系。
             接下来，你将评估与谁的关系？请选择："),
    selectInput(inputId = "relationship", 
                "你要评估与谁的关系：", 
                choices = list("配偶(恋人)",
                            "母亲(或像母亲一样的人)",
                            "父亲(或像父亲一样的人)",
                            "兄弟姐妹",
                            "好朋友",
                            "老师(导师)")),

    tags$p("下面给出的句子描述了每个人在亲密关系中可能会有的感觉。
             接下来，请根据与上述人物交往过程中常常体验到的感受，
             从七个选项中选出最符合你实际情况的选项。"),
    radioButtons(inputId = "ecr1", 
            "1.我不喜欢向TA袒露自己内心深处的感受.", 
            choices = list("非常不符"=1,
                        "不符合"=2,
                        "比较不符"=3,
                        "不确定"=4,
                        "比较符合"=5,
                        "符合"=6,
                        "非常符合"=7)),
    radioButtons(inputId = "ecr2", 
            "2.我担心TA不会像我在乎她那样在乎我。", 
            choices = list("非常不符"=1,
                        "不符合"=2,
                        "比较不符"=3,
                        "不确定"=4,
                        "比较符合"=5,
                        "符合"=6,
                        "非常符合"=7)),
    radioButtons(inputId = "ecr3", 
            "3.我发现依靠TA是件容易的事。", 
            choices = list("非常不符"=1,
                        "不符合"=2,
                        "比较不符"=3,
                        "不确定"=4,
                        "比较符合"=5,
                        "符合"=6,
                        "非常符合"=7)),
    radioButtons(inputId = "ecr4", 
            "4.我会就某些事与TA进行协商。", 
            choices = list("非常不符"=1,
                        "不符合"=2,
                        "比较不符"=3,
                        "不确定"=4,
                        "比较符合"=5,
                        "符合"=6,
                        "非常符合"=7)),
    radioButtons(inputId = "ecr5", 
            "5.我害怕TA会抛弃我。", 
            choices = list("非常不符"=1,
                        "不符合"=2,
                        "比较不符"=3,
                        "不确定"=4,
                        "比较符合"=5,
                        "符合"=6,
                        "非常符合"=7)),
    radioButtons(inputId = "ecr6", 
            "6.在需要的时候, 我向TA求助是有用的。", 
            choices = list("非常不符"=1,
                        "不符合"=2,
                        "比较不符"=3,
                        "不确定"=4,
                        "比较符合"=5,
                        "符合"=6,
                        "非常符合"=7)),
    radioButtons(inputId = "ecr7", 
            "7.我常常担心TA不是真地喜欢我。", 
            choices = list("非常不符"=1,
                        "不符合"=2,
                        "比较不符"=3,
                        "不确定"=4,
                        "比较符合"=5,
                        "符合"=6,
                        "非常符合"=7)),
    radioButtons(inputId = "ecr8", 
            "8.向TA敞开心扉会让我觉得不舒服。", 
            choices = list("非常不符"=1,
                        "不符合"=2,
                        "比较不符"=3,
                        "不确定"=4,
                        "比较符合"=5,
                        "符合"=6,
                        "非常符合"=7)),
    radioButtons(inputId = "ecr9", 
            "9.我经常与TA谈论我所遇到的问题以及我关心的事情。", 
            choices = list("非常不符"=1,
                        "不符合"=2,
                        "比较不符"=3,
                        "不确定"=4,
                        "比较符合"=5,
                        "符合"=6,
                        "非常符合"=7)),
    actionButton("update", "点击更新测评报告", icon = icon("refresh")),

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
            "张礼娟，张庆垚. (2026).",
            tags$i("依恋倾向心理测验"),
            ". 单车欲问边. https://stat.psych.pub/attachment/"
            # 修改结束 #####################################################################
          ),
          tags$p(
            # 修改citation #################################################################
            "Zhang, Q., Hou, Z. J., Fraley, R. C., Hu, Y., Zhang, X., Zhang, J., & Guo, X. (2022). Validating the Experiences in Close Relationships–Relationship Structures Scale among Chinese Children and Adolescents.", 
            tags$i("Journal of Personality Assessment, 104"),
            "(3), 347-358.",
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
  )),
# 修改Output ###################################################################
    card(card_header("测评报告"), 
         card_body(
           layout_sidebar(
             fillable = TRUE,
             sidebar = sidebar(
               bg = "#EEEEEE",
               position = "right",
               # "ECR_RS_report"包含html tag，需要用htmlOutput输出。
               htmlOutput(outputId = "ECR_RS_report")
             ),
             plotOutput(outputId = "attachment_plot")
             )
           )
         ),
# 修改结束 #####################################################################
) # ui结束

# Define server logic required to draw a histogram ####

# Plot the attachment_quadrant that never change.

attachment_quadrant  <- ggplot() +
  geom_rect(aes(xmin = 1, xmax = 4, ymin = 1, ymax = 4),
            fill = "springgreen", alpha = 0.3) +
  geom_rect(aes(xmin = 1, xmax = 2, ymin = 1, ymax = 2),
            fill = "springgreen") +
  geom_rect(aes(xmin = 1, xmax = 4, ymin = 4, ymax = 7),
            fill = "skyblue", alpha = 0.3) +
  geom_rect(aes(xmin = 1, xmax = 2, ymin = 6, ymax = 7),
            fill = "skyblue") +
  geom_rect(aes(xmin = 4, xmax = 7, ymin = 1, ymax = 4),
            fill = "wheat", alpha = 0.3) +
  geom_rect(aes(xmin = 6, xmax = 7, ymin = 1, ymax = 2),
            fill = "wheat") +
  geom_rect(aes(xmin = 4, xmax = 7, ymin = 4, ymax = 7),
            fill = "pink", alpha = 0.3) +
  geom_rect(aes(xmin = 6, xmax = 7, ymin = 6, ymax = 7),
            fill = "pink") +
  annotate("text", label = "安全型", x = 1.5, y = 1.5) +
  annotate("text", label = "回避型", x = 1.5, y = 6.5) +
  annotate("text", label = "焦虑型", x = 6.5, y = 1.5) +
  annotate("text", label = "矛盾型", x = 6.5, y = 6.5) +
  geom_hline(yintercept = 4, linewidth = 0.5) +
  geom_vline(xintercept = 4, linewidth = 0.5) +
  scale_x_continuous(name = "依恋焦虑",
                     breaks = 1:7,
                     labels = 1:7,
                     limits = c(1,7)) + 
  scale_y_continuous(name = "依恋回避",
                     breaks = 1:7,
                     labels = 1:7,
                     limits = c(1,7)) +
  coord_cartesian(ratio = 1) +
  theme_bw(base_size = 16)


  server <- function(input, output) {
# 修改计算 #####################################################################
    dat_fun <- reactive(
        data.frame(
          anxiety = mean(c(
            as.numeric(input$ecr2),
            as.numeric(input$ecr5),
            as.numeric(input$ecr7)
          )),
          avoidance = mean(c(as.numeric(input$ecr1),
              8 - as.numeric(input$ecr3),
              8 - as.numeric(input$ecr4),
              8 - as.numeric(input$ecr6),
              as.numeric(input$ecr8),
              8 - as.numeric(input$ecr9)))
    ))|> 
      # 点击update，则开始计算数据
      bindEvent(input$update)
    
    output$ECR_RS_report <- renderText({
      dat <- dat_fun()
      paste0(
        "<b>解读</b>",
        "在与",
        input$relationship,
        "的关系中，<br>",
        "你的依恋焦虑得分：",
        round(dat$anxiety, 2),
        "，<br>",
        "你的依恋回避得分：",
        round(dat$avoidance, 2),
        "，<br><br>",
        "左图中点的位置反映了你的依恋风格，横坐标为你的依恋焦虑得分，纵坐标为你的依恋回避得分。
         若该点落在深绿色区间内，这表明你的依恋风格为典型的安全型。
         若该点落在深绿色区间附近、浅绿色区间内，这表明你的依恋风格接近安全型。
         其他依此类推。<br><br>更多信息，请咨询专业人士。")
    })|> 
      # 检查两个输入缓存的变化，若缓存发生变化，则bindEvent可激发新计算
      # bindCache可避免用户反复点击刷新按钮造成的重复计算
      bindCache(dat_fun(), input$relationship)|> 
      # dat_fun()或input$relationship变化的前提下，点击update，重新生成ECR_RS_report
      bindEvent(input$update)
      
    output$attachment_plot <- renderPlot({
      dat <- dat_fun()
      attachment_quadrant + 
        geom_point(aes(anxiety, avoidance), data = dat, size = 5)})|> 
      # 检查dat_fun()缓存的变化，若缓存变化，则bindEvent可激发新计算
      # bindCache可避免用户反复点击刷新按钮造成的重复计算
      bindCache(dat_fun())|>
      # dat_fun()变化的前提下，点击update，重新生成统计图
      bindEvent(input$update)
    }
# 修改结束 #####################################################################

shinyApp(ui = ui, server = server)