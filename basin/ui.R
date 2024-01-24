pacman::p_load(shiny,shinyjs,ggplot2,rgdal,DT,dygraphs,leaflet,shinydashboard,crosstalk,GGally,plotly,shinycssloaders,RColorBrewer,readr,devtools,here,sf,dplyr,tidyr,geojsonio,mpoly,mapdeck,data.table,colourpicker,geodist,shinyscreenshot,xlsx,elevatr)
pacman::p_load(farver,magrittr,uuid)
pacman::p_load(data.table, tidyverse, mapview,rjson)
# pacman::p_load(xts)
options(shiny.maxRequestSize = 100 * 1024 ^ 2)
## mapdeck API token
MAPBOX_TOKEN <- 'pk.eyJ1Ijoia29qaW1vcmkiLCJhIjoiY2sxcnU3enlyMDk0MDNicDQ0NHNzbjg2dCJ9.JJ8KdQvY7wTuAjm5fuaaJQ'
set_token(MAPBOX_TOKEN)
Sys.setlocale("LC_CTYPE", locale="Japanese")

# ui object
ui <- navbarPage(
  "迅速拡散予測データエクスプローラ",
  tabPanel("マップ", value='A', icon = icon("map"),
    tags$div(
      tags$canvas(
        id="container-contour",
        width="900",
        height="900",
      )
    ),
    div(
      class="outer",
      useShinyjs(),
      tags$head(
        includeCSS("assets/vendors/leaflet.draw.css"),
        includeCSS("assets/css/styles.css"),
        includeScript("assets/vendors/leaflet.hotline.js"),
        includeScript("assets/vendors/Leaflet.ImageOverlay.Rotated.js"),
        includeScript("assets/vendors/d3.v4.js"),
        includeScript("assets/vendors/d3-contour.v1.min.js"),
        includeScript("assets/vendors/chroma.min.js"),
        includeScript("assets/vendors/shp.js"),
        includeScript("assets/vendors/turf.min.js"),
        includeScript("assets/vendors/plotly-locale-ja.js"),
        includeScript("assets/vendors/leaflet.draw.js"),
        includeScript("assets/js/hotline.js"),
        includeScript("assets/js/app.js"),
        includeScript("assets/js/groundwater.js")
      ),

      leafletOutput(outputId = "map", width="100%", height="100%"),
      absolutePanel(
        id="controls",
        class = "panel panel-default panel-map",
        fixed = TRUE,            
        draggable = TRUE,
        top = 60,
        left = "auto",
        right = 20,
        bottom = "auto",
        width = 350,
        height = "auto",
        tabsetPanel(id="tabs",
          tabPanel("河川", value="ctat",
            div(class="accordion", style="margin-top: 15px;",
              div(class="accordion-item show",
                div(class="accordion-button", span("データの読込み", class="font-weight-bold"), icon("chevron-up")),
                div(class="accordion-collapse",
                  div(class="accordion-body",
                    fileInput(inputId = "csvfile", label = h5("観測データ(csv)の読込み"), accept = c(".csv")),
                    fileInput(inputId = "shpfile", label = h5("観測点(csv/shp)+河川データ(shp/zip)の読込み"), multiple = TRUE, accept = c('.shp', '.dbf', '.sbn', '.sbx', '.shx', '.prj', '.zip', '.geojson', '.json', '.csv')),
                    fileInput(inputId = "shpfile2", label = h5("他のデータ(shp)の読込み"), multiple = TRUE, accept = c('.shp', '.dbf', '.sbn', '.sbx', '.shx', '.prj', '.zip', '.geojson', '.json')),
                    fileInput(inputId = "csvfile2", label = h5("計算パラメータ(csv)の読込み"), accept = c(".csv"))
                  )
                )
              ),
              div(class="accordion-item",
                div(class="accordion-button", span("表示関連", class="font-weight-bold"), icon("chevron-up")),
                div(class="accordion-collapse",
                  div(class="accordion-body",
                    selectInput(inputId = "variableselected", label = h6("表示データ"), choices = c()),
                    dateInput("currentDate",
                        label = tags$div(
                            h5("日付を選択する", style='display: inline-block; margin-right: 6px;'),
                            actionButton('runAnimationRiver', 'Play', icon = icon('play', 'fa-sm'), style='padding:4px;')
                        ),
                        '2008-01-04',
                    ),
                    plotOutput("histRiver", height = 200),
                    # Screenshot button
                    screenshotButton(label="プロットを出力する", id="histRiver",filename ="TMDU_HIST") ,
                    
                    #plotOutput("histSupply", height = 200) %>% withSpinner(),
                    # for now
                    h5("発災地情報"),
                    numericInput("srcROI", "影響半径(m)", 0.0),
                    fluidRow(
                      column(6, infoBoxOutput("latbox", width = NULL)),
                      column(6, infoBoxOutput("lngbox", width = NULL))
                    )
                  )
                )
              ),
              div(class="accordion-item",
                div(class="accordion-button", span("詳細設定", class="font-weight-bold"), icon("chevron-up")),
                div(class="accordion-collapse",
                  div(class="accordion-body",
                    #hr(),
                    h4("漏洩源情報"),
                    # actionButton('srcCT', '漏洩源を指定'),
                    numericInput("CT_thrs", "閾値(mg/L)", 10.),
                    h4("パラメータ"),
                    numericInput("CT_tmax", "日数(d):",          20., min = 0),
                    tags$div(
                      class="input-group",
                      numericInput("CT_mass", "漏洩量(kg):", 5000.)
                      # selectInput("CT_mass", "漏洩量(kg):", choices = c(5000.), selected = 5000.)
                    ),
                    tags$div(
                      class="input-group",
                      numericInput("CT_area", "河道断面積(m2):",1500.),
                      # selectInput("CT_area", "河道断面積(m2):", choices = c(1500.), selected = 1500.),
                    ),
                    tags$div(
                      class="input-group",
                      numericInput("CT_disp", "分散係数(m2/s):", 1000.),
                      # selectInput("CT_disp", "分散係数(m2/s):", choices = c(1000.), selected = 1000.),
                    ),
                    tags$div(
                      class="input-group",
                      numericInput("CT_velo", "流速(m/s):", 0.05),
                      # selectInput("CT_velo", "流速(m/s):", choices = c(0.15), selected = 0.15),
                    ),
                    tags$div(
                      class="input-group",
                      numericInput("CT_gamm", "歪度(-):", 0.00),
                      # selectInput("CT_gamm", "歪度(-):", choices = c(0.00), selected = 0.00),
                    )
                  )
                )
              ),
              br(),
              actionButton('calcATCT', '移流分散濃度を計算する', class="btn-block btn-success"),
              br(),
              div(class="accordion-item",
                  div(class="accordion-button", span("出力", class="font-weight-bold"), icon("chevron-up")),
                  div(class="accordion-collapse",
                      div(class="accordion-body",
                          #hr(),
                          # br(),
                          # actionButton('calcATCT', '移流分散濃度を計算する', class="btn-block btn-success"),
                          downloadButton('saveATCT', '計算結果を出力する(CSV形式)', class="btn-block btn-success"), 
                          downloadButton('downloadshp', '地図を出力する(GeoJSON形式)', class="btn-block btn-success"),
                          # Screenshot button
                          br(),
                          screenshotButton(label="地図のスクリーンショット", id="map", filename ="TMDU_MAP_RIVER",), #  selector="body") <- this takes screenshot of everything
                      )
                  )
              )
            ),
            # hr(),
            
            selectInput(inputId = "mapselected", label = h5("背景地図"), choices = c()),
            hr(),
            h5(a("TMDU", href = "http://www.tmd.ac.jp/"), "と"),
            h5(a("NC", href = "https://www.nagoya-cu.ac.jp/"), "との共同制作。"),
            img(src = "logo-tmd.png", height = "40px"),
            img(src = "nagoyacity_logo.png", height = "48px")
          ),
          tabPanel('地下水',
            value="groundwater",
            div(class="accordion", style="margin-top: 15px;",
              div(class="accordion-item",
                div(class="accordion-button", span("計算タイプ＆データの読込み", class="font-weight-bold"), icon("chevron-up")),
                div(class="accordion-collapse",
                  div(class="accordion-body",
                    # Read input file automatically based on selected menu -> use radio buttons
                    fileInput(inputId = "csvfile3", label = h5("計算パラメータ(csv)の読込む"), accept = c(".csv")),
                    radioButtons(
                      inputId = "methodselected",
                      label = h6("計算タイプ"),
                      selected = "デフォルト",              
                      choices = c("デフォルト","マイ設定１","マイ設定２")
                    )
                  )
                )
              ),
              div(class="accordion-item show",
                div(class="accordion-button", span("漏洩源", class="font-weight-bold"), icon("chevron-up")),
                div(class="accordion-collapse",
                  div(class="accordion-body",
                    h5("メッシュの設定：", class="font-weight-bold"),
                    div(
                      class="row",
                      div(class="col-sm-6", numericInput("noX", "グリッド横", 1)),
                      div(class="col-sm-6", numericInput("noY", "グリッド縦", 1))
                    ),
                    br(),
                    numericInput("noK", "K係数", 5),
                    br(),
                    div(
                      class="row",
                      div(class="col-sm-6", numericInput("aquiferWidth", "帯水層幅", 0)),
                      div(class="col-sm-6", numericInput("boxLength", "ボックスの長さ", 0))
                    ),
                    br(),
                    hidden(textInput('gwFocusOn', '', value="")),
                    textInput("sourceLatlng", "漏洩源", ''),
                    br(),
                    hidden(numericInput("direction_elev", "方向(度)、標高基準", 0.0)), #CH#
                    checkboxInput("ignoreAltitude","標高を使用しない",FALSE), 
                    numericInput("direction", "方向(度)", 0.0), #CH#
                    hidden(textInput('destinationLatlng', '', '')),
                    br()
                  )
                )
              ),
              div(class="accordion-item",
                div(class="accordion-button", span("物性値", class="font-weight-bold"), icon("chevron-up")),
                div(class="accordion-collapse",
                  div(class="accordion-body",
                    div(
                      class="physical-properties",
                      div(
                        class="pp-option",
                        div(class='pp-option-label small', span('気孔率')),
                        div(class='pp-option-value', numericInput("porosity", "", value=0.0, step=0.01)),
                      ),
                      div(
                        class="pp-option",
                        div(class='pp-option-label small', span('透水係数')),
                        div(class='pp-option-value', numericInput("hydraulicCond", "", 0.0)),
                      ),
                      div(
                        class="pp-option",
                        div(class='pp-option-label small', span('縦方向の分散性')),
                        div(class='pp-option-value', numericInput("longDisp", "", 0.0)),
                      ),
                      div(
                        class="pp-option",
                        div(class='pp-option-label small', span('横方向の分散性')),
                        div(class='pp-option-value', numericInput("latDisp", "", 0.0)),
                      ),
                      div(
                        class="pp-option",
                        div(class='pp-option-label small', span('垂直分散性')),
                        div(class='pp-option-value', numericInput("vertDisp", "", 0.0)),
                      ),
                      div(
                        class="pp-option",
                        div(class='pp-option-label small', span('油圧勾配')),
                        div(class='pp-option-value', numericInput("hydraulicGrad", "", 0.0)),
                      ),
                      div(
                        class="pp-option",
                        div(class='pp-option-label small', span('分配係数')),
                        div(class='pp-option-value', numericInput("distCoef", "", 0.0)),
                      ),
                    )
                  )
                )
              ),
              div(class="accordion-item",
                div(class="accordion-button", span("その他のプロパティ", class="font-weight-bold"), icon("chevron-up")),
                div(class="accordion-collapse",
                  div(class="accordion-body",
                    div(
                      class="physical-properties",
                      div(
                        class="pp-option",
                        div(class='pp-option-label small', span('分子拡散係数')),
                        div(class='pp-option-value', numericInput("diffusionCoef", "", 0.0)),
                      ),
                      div(
                        class="pp-option",
                        div(class='pp-option-label small', span('減衰定数')),
                        div(class='pp-option-value', numericInput("decayConst", "", 0.0)),
                      ),
                      div(
                        class="pp-option",
                        div(class='pp-option-label small', span('固体の容積密度')),
                        div(class='pp-option-value', numericInput("bulkDen", "", 0.0)),
                      ),
                      div(
                        class="pp-option",
                        div(class='pp-option-label small', span('水の密度')),
                        div(class='pp-option-value', numericInput("denOfWater", "", 0.0)),
                      ),
                      div(
                        class="pp-option",
                        div(class='pp-option-label small', span('誤差の許容範囲')),
                        div(class='pp-option-value', numericInput("errorTole", "", 0.0)),
                      ),
                      div(
                        class="pp-option",
                        div(class='pp-option-label small', span('時間間隔サイズ')),
                        div(class='pp-option-value', numericInput("timeInterval", "", 0.0)),
                      ),
                      div(
                        class="pp-option",
                        div(class='pp-option-label small', span('排出時間')),
                        div(class='pp-option-value', numericInput("distTime", "", 0.0)),
                      ),
                      div(
                        class="pp-option",
                        div(class='pp-option-label small', span('廃棄物排出量')),
                        div(class='pp-option-value', numericInput("releaseRate", "", 0.0)),
                      ),
                    )
                  )
                )
              )
            ),
            br(),
            hidden(checkboxInput('isPlayRiver', '', value=FALSE)),
            div(
              class="display-container",
              div(
                class="display-icon",
                conditionalPanel(
                  'input.isPlayRiver',
                  tags$i(class = "far fa-pause-circle")
                ),
                conditionalPanel(
                  '!input.isPlayRiver',
                  tags$i(class = "far fa-play-circle")
                )
              ),
              h5("スライスを選択:"),
              div(
                class="display-slice",
                numericInput("sliceContour", "", 1),
              )
            ),
            br(),
            # div(
            #   class='output-container',
            #   div(
            #     class='output-label',
            #     span('Save AT123 output')
            #   ),
            #   div(
            #     class='output-path',
            #     textInput("outputPath", "", "")
            #   )
            # ),
            actionButton('calculate', '地下水中物質移行を計算する', class = 'btn-block btn-warning'),
            br(),
            # downloadButton('downloadat123_e', '計算結果を出力(XLSX形式)'),
            # br(),
            div(class="accordion", style="margin-top: 15px;",
                div(class="accordion-item",
                    div(class="accordion-button", span("出力", class="font-weight-bold"), icon("chevron-up")),
                    div(class="accordion-collapse",
                        div(class="accordion-body",
                            br(),
                            downloadButton('downloadat123', '計算結果を出力する(CSV形式)', class="btn-block btn-success"),
                            br(),
                            actionButton('downloadshp', '地図を出力する(GeoJSON形式)', class="btn-block btn-success"),
                            br(),
                            # Screenshot button
                            screenshotButton(label="地図のスクリーンショット", id="map", filename="TMDU_MAP_GROUNDWATER") 
                        )
                    )
                ),
            ),
            
            selectInput(inputId = "mapselected_gw", label = h5("背景地図"), choices = c()),
            hr(),
            h5(a("TMDU", href = "http://www.tmd.ac.jp/"), "と"),
            h5(a("NC", href = "https://www.nagoya-cu.ac.jp/"), "との共同制作。"),
            img(src = "logo-tmd.png", height = "40px"),
            img(src = "nagoyacity_logo.png", height = "48px")
          )
          # tabPanel('AT123',
          #   value="at123",
          #   h2('AT123')
          # )
        )
      ),
      
      # Params on Map
      div(id="params-on-map",
        div(class="params-river params-container",
          div(class="params-item w-100", strong("漏洩源情報")),
          div(class="params-item w-100", "閾値(mg/L): ", span(from="CT_thrs", 10)),
        ),
        div(class="params-river params-container",
          div(class="params-item w-100", strong("パラメータ")),
          div(class="params-item", "日数(d): ", span(from="CT_tmax", 20)),
          div(class="params-item", "漏洩量(kg): ", span(from="CT_mass", 5000)),
          div(class="params-item", "河道断面積(m2): ", span(from="CT_area", 1500)),
          div(class="params-item", "分散係数(m2/s): ", span(from="CT_disp", 1000)),
          div(class="params-item", "流速(m/s): ", span(from="CT_velo", 0.05)),
          div(class="params-item", "歪度(-): ", span(from="CT_gamm", 0)),
        ),
        div(class="params-ground-water params-container", style="display: none",
          div(class="params-item w-100", strong("漏洩源")),
          div(class="params-item", "グリッド横: ", span(from="noX", 16)),
          div(class="params-item", "グリッド縦: ", span(from="noY", 16)),
          div(class="params-item", "K係数: ", span(from="noK", 5)),
          div(class="params-item", "帯水層幅: ", span(from="aquiferWidth", 150)),
          div(class="params-item", "ボックスの長さ: ", span(from="boxLength", 530)),
          div(class="params-item", "方向(度): ", span(from="direction", 0)),
          div(class="params-item w-100", "漏洩源: ", span(from="sourceLatlng", "")),
        ),
        div(class="params-ground-water params-container", style="display: none",
          div(class="params-item w-100", strong("物性値")),
          div(class="params-item", "気孔率: ", span(from="porosity", 0.25)),
          div(class="params-item", "透水係数: ", span(from="hydraulicCond", 25)),
          div(class="params-item", "縦方向の分散性: ", span(from="longDisp", 30)),
          div(class="params-item", "横方向の分散性: ", span(from="latDisp", 15)),
          div(class="params-item", "垂直分散性: ", span(from="vertDisp", 2.5)),
          div(class="params-item", "油圧勾配: ", span(from="hydraulicGrad", 0)),
          div(class="params-item", "分配係数: ", span(from="distCoef", 0)),
        ),
        div(class="params-ground-water params-container", style="display: none",
          div(class="params-item w-100", strong("その他のプロパティ")),
          div(class="params-item", "分子拡散係数: ", span(from="diffusionCoef", 0)),
          div(class="params-item", "減衰定数: ", span(from="decayConst", 0)),
          div(class="params-item", "固体の容積密度: ", span(from="bulkDen", 1400)),
          div(class="params-item", "水の密度: ", span(from="denOfWater", 0)),
          div(class="params-item", "誤差の許容範囲: ", span(from="errorTole", 0.001)),
          div(class="params-item", "時間間隔サイズ: ", span(from="timeInterval", 30)),
          div(class="params-item", "排出時間: ", span(from="distTime", 100000)),
          div(class="params-item", "廃棄物排出量: ", span(from="releaseRate", 6.28)),
        )
      )
    ),
    ),
  tabPanel(
    "時系列データ",
    icon = icon("chart-line"),
          
    fluidRow(
      plotlyOutput(outputId = "ts_Q", height = 400) %>% withSpinner()
    ),
    fluidRow(
      hr(),
      column(
        12,
        plotlyOutput(outputId = "boxp_Q", height = 600) %>% withSpinner()
      )
    )
  ),
  tabPanel(
    "データテーブル",
    DTOutput(outputId = "table"), icon = icon("table"),
    downloadButton('downloadDataTable', 'ファイルを出力する'),
  ),
  tabPanel(
    "移流分散理論解",
    icon = icon("calculator"),
    class="tab-analysis",
    column(
      3,
#      numericInput("dist", "距離(m):",       100.e3, min = 0),
      numericInput("tmax", "時間(d):",          20., min = 0),
      shinyjs::hidden(textInput('modalName', '')),
      tags$div(
        class="input-group",
        selectInput("dist", "距離(m):", choices = c(100000.), selected = 100000.),
        actionButton(inputId = 'openDistanceModal', label = '', icon = icon('tasks')),
        shinyjs::hidden(textInput('distOptions', ''))
      ),
      tags$div(
        class="input-group",
        selectInput("mass", "漏洩量(kg):", choices = c(5000.), selected = 5000.),
        actionButton(inputId = 'openLeakageModal', label = '', icon = icon('tasks')),
        shinyjs::hidden(textInput('massOptions', ''))
      ),
      tags$div(
        class="input-group",
        selectInput("area", "河道断面積(m2):", choices = c(1500.), selected = 1500.),
        actionButton(inputId = 'openAreaRiverModal', label = '', icon = icon('tasks')),
        shinyjs::hidden(textInput('areaOptions', ''))
      ),
      tags$div(
        class="input-group",
        selectInput("disp", "分散係数(m2/s):", choices = c(1000.), selected = 1000.),
        actionButton(inputId = 'openDispersionModal', label = '', icon = icon('tasks')),
        shinyjs::hidden(textInput('dispOptions', ''))
      ),
      tags$div(
        class="input-group",
        selectInput("velo", "流速(m/s):", choices = c(0.15), selected = 0.15),
        actionButton(inputId = 'openFlowModal', label = '', icon = icon('tasks')),
        shinyjs::hidden(textInput('veloOptions', ''))
      ),
      tags$div(
        class="input-group",
        selectInput("gamm", "歪度(-):", choices = c(0.00), selected = 0.00),
        actionButton(inputId = 'openSkewnessModal', label = '', icon = icon('tasks')),
        shinyjs::hidden(textInput('gammOptions', ''))
      ),
      downloadButton('downloadData', 'ファイルを出力する'),
      downloadButton('downloadAll', 'すべてダウンロード'),
      tags$br(),
      actionButton('execute', '感度解析の実行', icon = icon('chart-line')),
      actionButton('refresh', 'リフレッシュ', class = 'btn-danger')
    ),
    column(
      9,
      plotlyOutput(outputId = "solve", height = 600) %>% withSpinner()
    )
  ),
  tabPanel(
    '設定',
    icon = icon('cog'),
    navlistPanel(
      "設定",
      tabPanel(
        "ホットライン",
        wellPanel(
          fluidRow(
            column(
              6,
              sliderInput('hotlineOutlineWidth', 'アウトライン幅', 0, 8, 0)
            ),
            column(
              6,
              sliderInput('hotlineWeight', '重量', -1, 16, 12)
            )
          ),

          fluidRow(
            column(
              6,
              sliderInput('hotlineSmooth', '平滑化', 0, 10, 0)
            ),
            column(
              6,
              colourInput('hotlineOutlineColor', 'アウトラインカラー', '#95F505')
            ),
          ),
          fluidRow(
            column(
              6,
              numericInput("bufferDistance", "ホットライン・バッファ(m)", 100)
            )
          ),
          fluidRow(
            column(
              12,
              h3('カラーパレット'),
              shinyjs::hidden(
                textInput(
                  'rainbowPalletSelected',
                  '',
                )
              ),
              tags$div(
                class='color-palette-container',
                tags$div(
                  class='color-palette-item',
                  tags$div(
                    class='color-palette',
                    style='
                      --color1:#ffbaba;
                      --color2:#ff7b7b;
                      --color3:#ff5252;
                      --color4:#ff0000;
                      --color5:#a70000;
                     '
                  )
                ),
                tags$div(
                  class='color-palette-item',
                  tags$div(
                    class='color-palette',
                    style='
                      --color1:#adff00;
                      --color2:#00FF00;
                      --color3:#16DE16;
                      --color4:#00d27f;
                      --color5:#028900;
                     '
                  )
                ),
                tags$div(
                  class='color-palette-item',
                  tags$div(
                    class='color-palette',
                    style='
                      --color1:#6497b1;
                      --color2:#267CAB;
                      --color3:#005b96;
                      --color4:#03396c;
                      --color5:#011f4b;
                     '
                  )
                ),
                tags$div(
                  class='color-palette-item',
                  tags$div(
                    class='color-palette',
                    style='
                      --color1:#C4C4C4;
                      --color2:#999999;
                      --color3:#777777;
                      --color4:#555555;
                      --color5:#333333;
                     '
                  )
                ),
                tags$div(
                  class='color-palette-item',
                  tags$div(
                    class='color-palette',
                    style='
                      --color1:#00FFFF;
                      --color2:#00FF00;
                      --color3:#FFFF00;
                      --color4:#FF7F00;
                      --color5:#FF0000;
                     '
                  )
                ),
                tags$div(
                  class='color-palette-item',
                  tags$div(
                    class='color-palette',
                    style='
                      --color1:#FF0000;
                      --color2:#FF7F00;
                      --color3:#FFFF00;
                      --color4:#00FF00;
                      --color5:#00FFFF;
                     '
                  )
                )
              )
            ),
            column(
              12,
              colourInput('color1', '')
            ),
            column(
              12,
              colourInput('color2', '')
            ),
            column(
              12,
              colourInput('color3', '')
            ),
            column(
              12,
              colourInput('color4', '')
            ),
            column(
              12,
              colourInput('color5', '')
            ),
          ),
          actionButton("saveHotlineSettings", "設定を保存する")
        )
      ),
      tabPanel(
        "影響半径",
        wellPanel(
          numericInput("radiusInfluence", "影響半径(m)", 100),
          actionButton("saveDistance", "保存")
        )
      ),
      tabPanel(
        "地下水",
        wellPanel(
          h5('グリッドの設定', class="font-weight-bold"),
          checkboxInput("gridTransparency", "透明性有効", F),
          fluidRow(
            column(
              6,
              colourInput('gridColor', '色', '#95F505')
            ),
            column(
              6,
              sliderInput('gridWeight', '太さ', value=1, min=1, max=10, step=1)
            )
          ),
          h5('漏洩源の位置設定', class="font-weight-bold"),
          fluidRow(
            column(
              4,
              colourInput('sourceLocationColor', '色', '#fc0f03')
            ),
            column(
              4,
              numericInput('sourceLocationRadius', '半径(m)', 20)
            ),
            column(
              4,
              sliderInput('sourceLocationOpacity', '不透明度', value=0.8, min=0, max=1, step=0.1)
             )
           ),
          h5('漏洩源の方向設定', class="font-weight-bold"),
          fluidRow(
            column(
              6,
              colourInput('arrowColor', '色', '#fc0f03')
            ),
            column(
              6,
              sliderInput('arrowOpacity', '不透明度', value=0.8, min=0, max=1, step=0.1)
            )
          ),
          fluidRow(
            column(
              6,
              numericInput('arrowWeight', '太さ', 4)
            ),
            column(
              6,
              sliderInput('arrowLength', '長さ', value=0.2, min=0, max=1, step=0.1)
            )
          ),
          
          h5('コンターの設定', class="font-weight-bold"),
          checkboxInput("contourSmoothAuto", "自動平滑化", T),
          fluidRow(
            column(
              6,
              numericInput('contourSmooth', '平滑化係数', 5)
            ),
            column(
              6,
              sliderInput('contourOpacity', '不透明度', value=0.9, min=0, max=1, step=0.1)
            )
          ),
          actionButton("saveGroundwaterSettings", "保存")
        )
      )
    ),
  )           
)
