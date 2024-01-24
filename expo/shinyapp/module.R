#' UI of ExposureAssessment
ExposureAssessmentUI <- function(id) {
  require(DT)
  require(plotly)
  ns <- NS(id)
  
  tabBox(
    tabPanel("吸入曝露量の推計",
      fluidRow(
        column(width = 8,
          shinydashboard::box(
            plotlyOutput(ns("ExposurePlot"), height = 650),
            title = "推計結果",
            status = "primary",
            width = NULL
          )
        ),
        column(width = 4,
          shinydashboard::box(
            numericInput(ns("InitialEmission"), "化学物質の空気中濃度 (ppm)", 10),
            numericInput(ns("Duration"), "曝露時間 (h)", 10),
            selectInput(ns("Inhalation"),
              "活動の強度",
              choices = c("睡眠時" = "Sleep",
                          "着席時" = "Sedentary and Passive Activities",
                          "軽度の運動" = "Light Intensity Activities",
                          "中程度の運動" = "Moderate Intensity Activities",
                          "活発な運動" = "High Intensity")
            ),
            title = "パラメータ設定",
            status = "warning",
            width = NULL
          )
        )
      )
    ),
    tabPanel("経口曝露量の推計",
      fluidRow(
        column(width = 12,
          h3("開発中")
        )
      )
    ),
    tabPanel("経皮曝露量の推計",
             fluidRow(
               column(width = 12,
                      h3("開発中")
               )
             )
    ),
    width = "100%",
    title = actionLink(inputId = ns("Help"), label = NULL, icon = icon("question-circle"))
  )
}

#' Server logic of ExposureAssessment
ExposureAssessmentServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      require(shiny)
      require(shinydashboard)
      require(DT)
      require(ggplot2)
      require(ggsci)
      require(plotly)
      
      dataset <- readRDS("data/InhalationRate.rds")
      
      inhalation_rates <- reactive({
        dataset %>%
          filter(ActivityCategory == input$Inhalation) ->
          retval

        return(retval)
      })
      
      output$ExposurePlot <- renderPlotly({
        data <- inhalation_rates()
        fig <- plot_ly(data = data,
                       type = "box",
                       q1 = ~Percentile25th * 60 * input$InitialEmission * input$Duration,
                       median = ~Percentile50th * 60 * input$InitialEmission * input$Duration,
                       q3 = ~Percentile75th * 60 * input$InitialEmission * input$Duration,
                       upperfence = ~Percentile95th * 60 * input$InitialEmission * input$Duration,
                       lowerfence = ~Percentile5th * 60 * input$InitialEmission * input$Duration,
                       split = ~Sex, x = ~factor(Age, unique(data$Age))) %>%
          layout(boxmode = "group",
                 xaxis = list(title = "年代", tickfont = 30),
                 yaxis = list(title = "曝露量 (mg)"))
        
        return(fig)
      })
      
      observeEvent(input$Help, {
        showModal(modalDialog(
          title = "曝露量推計ツールについて", size = "l", footer = modalButton("閉じる"),
          p("このツールでは米国Environment Protection Agency (EPA)「Guidelines for Human Exposure Assessment」(Oct 2019, EPA/100/B-1/001)に基づいて曝露量を推計します。"),
          h5("Guidelines for Human Exposure Assessmentより抜粋"),
          wellPanel(
            style = "overflow-y:scroll; max-height: 600px",
            h5("2.4. 暴露推定値の算出"),
            p("曝露評価者は、曝露シナリオ、濃度、活動パターン、その他の曝露要因を記述した情報やデータを組み合わせることで、
            個人や集団の曝露量を定量的に推定することができます。前述のように、曝露の特徴を明らかにするには、質量と時間の定義が必要です。"),
            p("このセクションでは、最も一般的な曝露経路である吸入、摂取、経皮の各経路での曝露量を推定するための経路別の式と関連する入力変数を示します。
              ここでは一般的な式を示しているが、詳細は「Draft Protocol for Measuring Children's Non-Occupational Exposure to Pesticides by All Relevant Pathways」（U.S. EPA 2001b）に記載されており、吸入および経皮経路による曝露を推定するための式も含まれている。
              これらの曝露方程式の様々な形式の詳細（様々なヒト曝露モデルで使用されるモデルのデフォルト値を含む）は、Williamsら（2010）に記載されている。"),
            h6("2.4.1. 吸入による曝露"),
            p("化学物質を吸い込むことにより、曝露は吸入経路で発生します。化学物質は、気道に直接影響を与えるか（侵入点影響）、気道組織を経由して血流に入り、身体の他の器官に影響を与える可能性がある（標的器官影響）。
            単純化した仮定として、ガス、エアロゾル、2.5マイクロメートル以下の微細な（「呼吸可能な」）粒子については、吸入曝露が線量に等しいとされています。
            より詳細な線量の推定には、ストレス因子のADMEパラメータを考慮した別の方程式やモデルが必要となる。大きな粒子を吸い込んだ場合、肺の最下部（肺胞）に到達する可能性は低い。
            肺の中の繊毛の動きによってそのような粒子が取り除かれることもあり、その場合は飲み込むことになります。
            ナノメートルサイズの粒子は、上気道に堆積し、標的臓器に到達する可能性があります（Oberdörster et al.2007）。吸入曝露による摂取に関連する線量の推定は、呼吸器系の複雑な性質のために複雑である。
            入口としての呼吸器系の複雑な性質のため、吸入曝露による摂取に関連する線量の推定は複雑である（U.S. EPA 1994b; U.S. EPA 2009f）。"),
            p("最も簡単に言えば、ある曝露事象における吸入曝露は、人がいる場所の空気中の平均的な化学物質 濃度と等しい。人の呼吸域の空気中の化学物質の平均濃度に吸入率を乗じたものである。以下の式のようになる（U.S. EPA 2001b）。"),
            p("E", tags$sub("inh"), " = (C", tags$sub("a"), ")(IR)"),
            tags$ul(
              tags$li("E", tags$sub("inh"), "= 吸入曝露量（単位時間あたりの質量）"),
              tags$li("C", tags$sub("a"), "= 化学物質の空気中濃度（呼吸域の空気量あたりの化学物質の質量）"),
              tags$li("IR = 吸入率（単位時間当たりに呼吸した空気の量）")
            ),
            p("特定の化学物質への曝露をより詳細に推定するためには、追加の要因が考慮される。例えば、粒子状物質への曝露を含む複雑な状況では、肺への沈着と吸入も考慮されます。
              肺への沈着と呼気も考慮する。あるいは、該当する曝露期間の曝露濃度から曝露量を推定することもできます。
              また、EPA基準濃度（米国）を用いて危険度指数を算出する際には、該当する曝露期間の曝露濃度で曝露量を推定する。
              EPA 基準濃度（U.S. EPA 2004a; U.S. EPA 2009f）を用いて危険度指数を算出する際には、該当暴露期間の暴露濃度で推定することもできる。")
          )
        ))
      })
    }
  )
}

