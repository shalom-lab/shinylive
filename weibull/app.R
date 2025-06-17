library(shiny)
library(bslib)
library(ggplot2)
library(latex2exp)

ui <- page_fluid(
  theme = bs_theme(bootswatch = "flatly"),
  
  h1("威布尔分布"),
  
  layout_sidebar(
    sidebar = sidebar(
      title = "参数设置",
      
      sliderInput("shape", "形状参数 (k):", 
                  min = 0.1, max = 5, value = 1.5, step = 0.1),
      
      sliderInput("scale", "尺度参数 (λ):", 
                  min = 0.1, max = 5, value = 1, step = 0.1),
      
      sliderInput("xlim", "x轴范围:", 
                  min = 0, max = 10, value = c(0, 5), step = 0.1),
      
      # 显示均值和方差
      verbatimTextOutput("meanvar")
    ),
    
    # 使用navset_card_tab创建三个标签页
    navset_card_tab(
      # 第一个标签页：分布信息
      nav_panel(
        title = "基本信息",
        card(
          card_body(
            p("威布尔分布(Weibull Distribution)是可靠性工程与生存分析中常用的概率分布。它能够描述各种形态的失效率函数，非常适合模拟系统的寿命数据。"),
            p("威布尔分布有两个参数："),
            tags$ul(
              tags$li("形状参数 k (shape): 决定了分布的形状"),
              tags$li("尺度参数 λ (scale): 决定了分布的尺度")
            ),
            
            h4("数学公式"),
            withMathJax(),
            p("概率密度函数 (PDF):"),
            p("$$f(t) = \\frac{k}{\\lambda}\\left(\\frac{t}{\\lambda}\\right)^{k-1}e^{-(t/\\lambda)^k}, \\quad t \\geq 0$$"),
            
            p("累积分布函数 (CDF):"),
            p("$$F(t) = 1 - e^{-(t/\\lambda)^k}, \\quad t \\geq 0$$"),
            
            p("生存函数 (Survival function):"),
            p("$$S(t) = e^{-(t/\\lambda)^k}, \\quad t \\geq 0$$"),
            
            p("风险函数/危险率函数 (Hazard function):"),
            p("$$h(t) = \\frac{k}{\\lambda}\\left(\\frac{t}{\\lambda}\\right)^{k-1}, \\quad t \\geq 0$$"),
            
            h4("统计特性"),
            p("均值 (Mean):"),
            p("$$E(T) = \\lambda \\Gamma(1 + \\frac{1}{k})$$"),
            p("其中Γ是伽马函数。"),
            
            p("方差 (Variance):"),
            p("$$Var(T) = \\lambda^2 \\left[ \\Gamma(1 + \\frac{2}{k}) - \\Gamma^2(1 + \\frac{1}{k}) \\right]$$")
          )
        ),
      ),
      nav_panel(
        title='生存分析中的应用',
        card(
          p("在生存分析中，威布尔分布具有重要意义："),
          
          h4("1. 生存函数 S(t)"),
          p("表示个体在时间t后仍然存活的概率。威布尔分布的生存函数为："),
          p("$$S(t) = e^{-(t/\\lambda)^k}$$"),
          
          h4("2. 危险率函数 h(t)"),
          p("表示在时间t存活的个体在下一时刻发生事件的瞬时概率。威布尔分布的危险率函数为："),
          p("$$h(t) = \\frac{k}{\\lambda}\\left(\\frac{t}{\\lambda}\\right)^{k-1}$$"),
          p("当k < 1时，危险率随时间减少，表示早期故障；"),
          p("当k = 1时，危险率恒定，简化为指数分布；"),
          p("当k > 1时，危险率随时间增加，表示磨损故障。"),
          
          h4("3. 概率密度函数 f(t)"),
          p("表示事件发生时间的概率密度。威布尔分布的概率密度函数为："),
          p("$$f(t) = \\frac{k}{\\lambda}\\left(\\frac{t}{\\lambda}\\right)^{k-1}e^{-(t/\\lambda)^k}$$"),
          
          h4("应用场景"),
          p("威布尔分布在生存分析中广泛应用于："),
          tags$ul(
            tags$li("医学研究：分析患者存活时间"),
            tags$li("工程可靠性：预测设备失效时间"),
            tags$li("工业制造：研究产品寿命"),
            tags$li("保险精算：寿险和健康保险模型")
          )
        )
      ),
      
      # 第二个标签页：图形可视化
      nav_panel(
        title = "分布图形",
        layout_column_wrap(
          width = 1/2,
          card(
            card_header("概率密度函数 (PDF)"),
            plotOutput("pdfPlot")
          ),
          card(
            card_header("生存函数 (Survival Function)"),
            plotOutput("survivalPlot")
          )
        ),
        
        layout_column_wrap(
          width = 1/2,
          card(
            card_header("危险率函数 (Hazard Function)"),
            plotOutput("hazardPlot")
          ),
          card(
            card_header("累积分布函数 (CDF)"),
            plotOutput("cdfPlot")
          )
        )
      ),
      
      # 第三个标签页：参数影响
      nav_panel(
        title = "参数影响分析",
        layout_column_wrap(
          width = 1/3,
          card(
            card_header("形状参数(k)的影响"),
            card_body(
              p("形状参数k对威布尔分布有显著影响:"),
              tags$ul(
                tags$li(strong("k < 1: "), "危险率随时间减少。分布呈现出较高的初始失效率，适用于描述早期故障或婴儿期故障。"),
                tags$li(strong("k = 1: "), "危险率恒定，威布尔分布简化为指数分布。适用于描述随机故障。"),
                tags$li(strong("k > 1: "), "危险率随时间增加，适用于描述磨损故障。"),
                tags$li(strong("k ≈ 3.5: "), "威布尔分布近似正态分布。")
              ),
              plotOutput("shapeEffect")
            )
          ),
          card(
            card_header("尺度参数(λ)的影响"),
            card_body(
              p("尺度参数λ主要影响分布的尺度（拉伸或压缩）:"),
              tags$ul(
                tags$li("λ增大：分布向右拉伸，均值增加"),
                tags$li("λ减小：分布向左压缩，均值减小"),
                tags$li("λ等效于时间的单位变换")
              ),
              plotOutput("scaleEffect")
            )
          ),
          card(
            card_header("参数与统计特性的关系"),
            card_body(
              p("参数变化对均值和方差的影响:"),
              withMathJax(),
              p("当k固定时，λ增大会导致均值和方差同比例增加。"),
              p("当λ固定时，k的变化对均值和方差的影响更为复杂："),
              tags$ul(
                tags$li("k增大通常会使分布更加集中（方差减小）"),
                tags$li("当k接近3.6时，分布的偏度接近于0，最接近正态分布")
              ),
              plotOutput("momentPlot")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  # 创建x轴数据点
  x_values <- reactive({
    seq(input$xlim[1], input$xlim[2], length.out = 500)
  })
  
  # 计算威布尔PDF
  weibull_pdf <- reactive({
    dweibull(x_values(), shape = input$shape, scale = input$scale)
  })
  
  # 计算威布尔CDF
  weibull_cdf <- reactive({
    pweibull(x_values(), shape = input$shape, scale = input$scale)
  })
  
  # 计算威布尔生存函数
  weibull_survival <- reactive({
    1 - weibull_cdf()
  })
  
  # 计算威布尔危险率函数
  weibull_hazard <- reactive({
    k <- input$shape
    lambda <- input$scale
    t <- x_values()
    k/lambda * (t/lambda)^(k-1)
  })
  
  # 绘制PDF图
  output$pdfPlot <- renderPlot({
    df <- data.frame(x = x_values(), y = weibull_pdf())
    
    ggplot(df, aes(x = x, y = y)) +
      geom_line(color = "#1f77b4", size = 1.2) +
      labs(
        x = "时间 (t)",
        y = "概率密度 f(t)",
        title = paste0("威布尔PDF (k=", input$shape, ", λ=", input$scale, ")")
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5)) +
      coord_cartesian(xlim = input$xlim)
  })
  
  # 绘制CDF图
  output$cdfPlot <- renderPlot({
    df <- data.frame(x = x_values(), y = weibull_cdf())
    
    ggplot(df, aes(x = x, y = y)) +
      geom_line(color = "#ff7f0e", size = 1.2) +
      labs(
        x = "时间 (t)",
        y = "累积概率 F(t)",
        title = paste0("威布尔CDF (k=", input$shape, ", λ=", input$scale, ")")
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5)) +
      coord_cartesian(xlim = input$xlim)
  })
  
  # 绘制生存函数图
  output$survivalPlot <- renderPlot({
    df <- data.frame(x = x_values(), y = weibull_survival())
    
    ggplot(df, aes(x = x, y = y)) +
      geom_line(color = "#2ca02c", size = 1.2) +
      labs(
        x = "时间 (t)",
        y = "生存概率 S(t)",
        title = paste0("威布尔生存函数 (k=", input$shape, ", λ=", input$scale, ")")
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5)) +
      coord_cartesian(xlim = input$xlim)
  })
  
  # 绘制危险率函数图
  output$hazardPlot <- renderPlot({
    df <- data.frame(x = x_values(), y = weibull_hazard())
    
    ggplot(df, aes(x = x, y = y)) +
      geom_line(color = "#d62728", size = 1.2) +
      labs(
        x = "时间 (t)",
        y = "危险率 h(t)",
        title = paste0("威布尔危险率函数 (k=", input$shape, ", λ=", input$scale, ")")
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5)) +
      coord_cartesian(xlim = input$xlim)
  })
  
  # 绘制形状参数影响图
  output$shapeEffect <- renderPlot({
    x <- seq(0, 5, length.out = 500)
    shapes <- c(0.5, 1, 1.5, 3, 5)
    
    df <- data.frame()
    for(shape in shapes) {
      temp_df <- data.frame(
        x = x,
        y = dweibull(x, shape = shape, scale = input$scale),
        shape = paste("k =", shape)
      )
      df <- rbind(df, temp_df)
    }
    
    ggplot(df, aes(x = x, y = y, color = shape)) +
      geom_line(size = 1) +
      labs(
        x = "时间 (t)",
        y = "概率密度 f(t)",
        title = "不同形状参数(k)的影响",
        color = "形状参数"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom"
      ) +
      scale_color_brewer(palette = "Set1")
  })
  
  # 绘制尺度参数影响图
  output$scaleEffect <- renderPlot({
    x <- seq(0, 5, length.out = 500)
    scales <- c(0.5, 1, 1.5, 2, 3)
    
    df <- data.frame()
    for(scale in scales) {
      temp_df <- data.frame(
        x = x,
        y = dweibull(x, shape = input$shape, scale = scale),
        scale = paste("λ =", scale)
      )
      df <- rbind(df, temp_df)
    }
    
    ggplot(df, aes(x = x, y = y, color = scale)) +
      geom_line(size = 1) +
      labs(
        x = "时间 (t)",
        y = "概率密度 f(t)",
        title = "不同尺度参数(λ)的影响",
        color = "尺度参数"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom"
      ) +
      scale_color_brewer(palette = "Set2")
  })
  
  # 绘制均值和方差与参数关系图
  output$momentPlot <- renderPlot({
    # 创建数据
    k_values <- seq(0.2, 5, by = 0.1)
    lambda_values <- c(0.5, 1, 2, 3)
    
    df <- data.frame()
    
    for(lambda in lambda_values) {
      means <- sapply(k_values, function(k) lambda * gamma(1 + 1/k))
      variances <- sapply(k_values, function(k) lambda^2 * (gamma(1 + 2/k) - (gamma(1 + 1/k))^2))
      
      mean_df <- data.frame(
        x = k_values,
        y = means,
        parameter = paste0("λ = ", lambda),
        type = "均值"
      )
      
      var_df <- data.frame(
        x = k_values,
        y = variances,
        parameter = paste0("λ = ", lambda),
        type = "方差"
      )
      
      df <- rbind(df, mean_df, var_df)
    }
    
    # 绘图
    ggplot(df, aes(x = x, y = y, color = parameter, linetype = type)) +
      geom_line(size = 1) +
      labs(
        x = "形状参数 (k)",
        y = "值",
        title = "形状参数(k)对均值和方差的影响",
        color = "尺度参数",
        linetype = "统计量"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom"
      ) +
      scale_color_brewer(palette = "Dark2")+
      scale_y_log10()
  })
}

shinyApp(ui = ui, server = server)
