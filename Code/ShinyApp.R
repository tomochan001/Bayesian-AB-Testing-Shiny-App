library(DT)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(plotly)
library(scales)

#### Shiny UI ####

options(scipen = 999)

header <- dashboardHeader(title = "メール施策A/Bテストアプリ", titleWidth = 400)
                          
sidebar <- dashboardSidebar(disable = TRUE)

body <- dashboardBody(
    
    fluidRow(
        column(width = 2,
            box(title = '使用法',
                status = "primary", solidHeader = TRUE,
                p('1. 統制群（青色）＞「今回のA/Bテストの結果」のフォームに、統制群（前回と同じパターン）のクリック数と開封者数を入力してください。'),
                p('2. 実験群（オレンジ色）＞「今回のA/Bテストの結果」のフォームに、実験群（今回実験してみたパターン）のクリック数と開封者数を入力してください。'),
                p('※CTRだけわかっていてクリック数がわからない場合は、それぞれの開封者数にCTRをかけた値をクリック数に入れてください。'),
                p('※このテストについて事前知識がある場合は、「事前分布」にそれぞれの値を入力してください。ない場合は0のままにしておいてください。'),
                p('3. シミュレーションの回数を指定してください。回数を増やすと精度が少し上がりますが、処理負荷が増えます。'),
                p('4. グラフのタイプを選択してください。'),
                p('5. 準備ができたら、「実行」を押してください。'),
                # p('-------------'),
                p('翻訳・改変：林　智彦'),
                p(a('LinkedIn', href="https://www.linkedin.com/in/thayashi36/", target="_blank")),
                p('Code posted on'),
                p(a("Github",href="https://github.com/tomochan001/Bayesian-AB-Testing-Shiny-App", target="_blank")),
                p(''),
                p('Originally Made by Adam Vagner'),
                p(a("LinkedIn", href="https://www.linkedin.com/in/adamvagner/", target="_blank")),
                width = NULL
            )
        ),
        
        column(width = 4,
               
               fluidRow(
                   column(width = 6,
                        box(title = '1. 統制群（Control）',
                            status = "info", solidHeader = TRUE,
                            
                            h5('今回のA/Bテストの結果'),
                            
                            numericInput("control.success", 
                                         p("クリック数"), 
                                         value = 15),
                            numericInput("control.trials", 
                                         p("開封者数"), 
                                         value = 100),
                            p(' '),
                            p(' '),
                            
                            h5('事前分布（通常は0のままで）'),
                            
                            numericInput("control.prior.success", 
                                         p("クリック数"), 
                                         value = 0),
                            numericInput("control.prior.trials", 
                                         p("開封者数"), 
                                         value = 0),
                            width = NULL
                        )
                    ),
                
                    column(width = 6,
                           box(title = '2. 実験群（Test）',
                               status = "warning", solidHeader = TRUE,
                               

                               h5('今回のA/Bテストの結果'),
                               
                               numericInput("test.success", 
                                            p("クリック数"), 
                                            value = 25),
                               numericInput("test.trials", 
                                            p("開封者数"), 
                                            value = 100),
                               p(' '),
                               p(' '),
                               
                               h5('事前分布（通常は0のままで）'),
                               
                               
                               numericInput("test.prior.success", 
                                            p("クリック数"), 
                                            value = 0),
                               numericInput("test.prior.trials", 
                                            p("開封者数"), 
                                            value = 0),
                               
                               width = NULL
                           )
                        )
                   ),
               
               fluidRow(
                   column(width = 12,
                          box(title = '3. 4. 5. シミュレーション',
                              status = "danger", solidHeader = TRUE,
                              
                              numericInput("ntrials", 
                                           p("シミュレーションの回数（10,000を推奨）"), 
                                           value = 10000,
                                           min = 1,
                                           max = 100000),
                              
                              selectInput("radio", p("プロットの種類"),
                                           choices = list("Lift Curve" = 1, 
                                                          "ヒストグラムの比較" = 2,
                                                          "確率密度グラフの比較" = 3), 
                                           selected = 1),
                              
                              actionButton(inputId = "button", 
                                           label = "シミュレーションを実行"),
                              
                              width = NULL
                          )
                   )
               )
               ),
        
        column(width = 6,
             fluidRow( 
               column(width = 6,
                      box(title = 'シミュレーションによるベイズ確率',
                            status = "info", solidHeader = TRUE,
                          p('2. 実験群が1. 統制群を上回る確率: '),
                          textOutput('superior'),
                            width = NULL
                      )
               ),
               column(width = 6,
                      box(title = '伝統的なt検定',
                           status = "info", solidHeader = TRUE,
                          p('片側検定のP-値: '),
                          textOutput('ttest'),
                           width = NULL
                      )
               )
             ),
             
             fluidRow(
                 column(width = 12,
                       box(title = 'Interactive Plot Output',
                           status = "info", solidHeader = TRUE,
                           plotlyOutput('plotly', height = '540px'),
                           width = NULL
                       )
                 )
             )
        )
    )
)

ui <- dashboardPage(
    header, 
    sidebar,
    body,
    skin = 'blue'
    
)

#### Server #### 

server <- function(input, output) { 
    
    df <- eventReactive(input$button, {
        
        n.trials <- input$ntrials
        a.prior.alpha <- input$control.prior.success
        a.prior.beta <- input$control.prior.trials - a.prior.alpha
        
        b.prior.alpha <- input$test.prior.success
        b.prior.beta <- input$test.prior.trials - b.prior.alpha
        
        a.success <- input$control.success
        a.failure <- input$control.trials - a.success # Total views/impressions (denominator) minus success
        
        b.success <- input$test.success
        b.failure <- input$test.trials - b.success
        
        # Sample from beta distribution based on results
        a.samples <- rbeta(n.trials, a.success + a.prior.alpha, a.failure + a.prior.beta)
        b.samples <- rbeta(n.trials, b.success + b.prior.alpha, b.failure + b.prior.beta)
        
        df <- data.frame(Lift = b.samples/a.samples - 1, Control = a.samples, Test = b.samples)
        
        # Remove extreme outliers for nicer plot
        outlier_values <- boxplot.stats(df$Lift, coef = 3)$out
        df <- df %>% filter(!Lift %in% outlier_values)
        df
        
        
    })
    
    output$superior <- renderText({
        
        p.b_superior <- sum(df()$Lift > 0)/nrow(df())
        scales::percent(p.b_superior)
    })
    
    pvalue <- eventReactive(input$button, {
        
        a.success <- input$control.success
        a.failure <- input$control.trials - a.success # Total views/impressions (denominator) minus success
        
        b.success <- input$test.success
        b.failure <- input$test.trials - b.success
        
        p_hat <- (a.success + b.success) / (a.success + a.failure + b.success + b.failure) # pooled p
        
        z_score <- ((b.success / (b.success + b.failure) - a.success / (a.success + a.failure)) - 0) /
            sqrt((p_hat)*(1 - p_hat)*(1/(a.success + a.failure) + 1/(a.success + a.failure))) 
        
        pnorm(z_score, lower.tail = FALSE) 
        
    })
    
    output$ttest <- renderText({
        
       round(pvalue(), 20)
        
    })
    
    output$plotly <- renderPlotly({
            
        # ggplot base histogram
        p1 <- ggplot(df(), aes(x = Lift)) + 
            geom_histogram(fill = 'steelblue4', alpha = .6, bins = 30) + 
            geom_vline(xintercept = 0, linetype = 'dashed')
        
        # Extract out ggplot data object for 2nd axis transformation
        ggplot_df <- ggplot_build(p1)$data[[1]]
        
        transform <- max(ggplot_df$ymax)
        median1 <- median(df()$Lift)
        
        xmin <- min(ggplot_df$x)
        xmax <- max(ggplot_df$x)
        
        y_data <- data.frame(x = c(xmin, median1), y = c(.5 * transform, .5 * transform))
        x_data <- data.frame(x = c(median1, median1), y = c(0, .5 * transform))
    
        plotly_text <- 'Cumulative Probability'
        
        p1 <- p1 + geom_line(aes_string(y = paste0('..y.. * ', transform), text = 'plotly_text', label = '..y..'), stat='ecdf', size = 1.3) + 
            geom_line(data = y_data, aes(x = x, y = y), linetype = 'dotted') + #, color = '#33CCFF') + # horizontal line
            geom_line(data = x_data, aes(x = x, y = y), linetype = 'dotted') + #, color = '#33CCFF') + # vert line
            scale_y_continuous(breaks = round(seq(0, transform, transform/4), 0), # Main axis
                               minor_breaks = NULL,
                               labels = scales::comma,
                               sec.axis = sec_axis(~./transform, breaks = seq(0, 1, .25), 
                                                   name = "Cumulative Probability (line)", labels = scales::percent)) + # 2nd axis
            scale_x_continuous(breaks = seq((xmin - xmin %% .5), (xmax + xmax %% .5), .5), # x-axis breaks at .5
                               labels = scales::percent) +
            labs(x = 'Lift', y = 'Count (bar)') +
            theme_light()
        
        p1 <- ggplotly(p1, tooltip = c('plotly_text', 'Lift', 'count','label'), height = 540)
        
        median_control <- median(df()$Control)
        median_test <- median(df()$Test)
        
        p2 <- ggplot(df()) + 
            geom_histogram(aes(x = Control, fill = 'Control '), alpha = .5) +
            geom_histogram(aes(x = Test, fill = 'Test '), alpha = .5) + 
            geom_vline(xintercept = median_control, color = '#0072B2', linetype = 'dashed') +
            geom_vline(xintercept = median_test, color = '#D55E00', linetype = 'dashed') +
            labs(x = 'CTR', y = 'Count') +
            scale_fill_manual(name = NULL, guide = 'legend', 
                              values = c('Control ' = '#0072B2', 'Test ' = '#D55E00'), labels = c('Control ', 'Test ')) +
            scale_y_continuous(labels = scales::comma) +
            scale_x_continuous(labels = scales::percent) +
            theme_light()
        
        p2 <- ggplotly(p2, tooltip = c('Control', 'Test', 'count'))
        
        p3 <- ggplot(df()) + 
            geom_density(aes(x = Control, fill = 'Control ', color = 'Control '), alpha = .5) +
            geom_density(aes(x = Test, fill = 'Test ', color = 'Test '), alpha = .5) +
            geom_vline(xintercept = median_control, color = '#0072B2', linetype = 'dashed') +
            geom_vline(xintercept = median_test, color = '#D55E00', linetype = 'dashed') +
            labs(x = 'CTR', y = 'Density') +
            scale_fill_manual(name = NULL, guide = 'legend', 
                              values = c('Control ' = '#0072B2', 'Test ' = '#D55E00'), labels = c('Control ','Test ')) +
            scale_colour_manual(name = NULL, guide = 'legend',
                                values = c('Control ' = '#0072B2', 'Test ' = '#D55E00'), labels = c('Control ','Test ')) +
            scale_y_continuous(labels = scales::comma) +
            scale_x_continuous(labels = scales::percent) +
            theme_light()
        
        p3 <- ggplotly(p3, tooltip = c('Control', 'Test', 'count'))
        
        if(input$radio == 1){
            p1
        } else if(input$radio == 2){
            p2
        } else {
            p3
        }
        
        
    })
    
}

shinyApp(ui, server)

