ChemicalGISUI <- function(id) {
  ns <- NS(id)
  
  require(leaflet.extras2)
  require(dipsaus)
  require(shinyWidgets)
  require(shinyBS)
  require(shinycssloaders)
  
  PRTR_type <- readRDS("data/PRTR_type.rds")
  PRTR_chemical <- readRDS("data/PRTR_chemical.rds")
  
  navbarPage(
    "基盤地図",
    tabPanel(
      "地図", icon = icon("map"),
      tags$head(includeCSS("styles.css")),
      useShinyjs(),
      fluidRow(
        column(
          9,
          div(
            style = "height: calc(100vh - 100px)",
            leafletOutput(ns("Map"), height = "100%")
          )
        ),
        column(
          3,
          bsCollapse(
            id = ns("Collapse"),
            bsCollapsePanel(
              title = "施設の表示",
              style = "primary",
              fluidRow(
                column(10, checkboxInput(ns("PRTR"), label = "PRTR事業所")),
                column(2, h5(actionLink(ns("PRTR_show_advanced"), label = NULL, icon = icon("plus")))),
                column(12, hidden(
                  div(
                    id = ns("PRTR_advanced"),
                    style = "margin-left: 1em;",
                    checkboxInput(
                      ns("PRTR_chem_select"),
                      label = "取扱物質を指定",
                      value = FALSE
                    ),
                    selectInput(
                      ns('PRTR_chem'),
                      label = NULL,
                      PRTR_chemical,
                      multiple=TRUE,
                      selectize=TRUE
                    ),
                    checkboxInput(
                      ns("PRTR_site_select"),
                      label = "業種を指定",
                      value = FALSE
                    ),
                    selectInput(
                      ns("PRTR_site"),
                      label = NULL,
                      choices = PRTR_type$choice,
                      multiple = TRUE,
                      selectize = TRUE
                    )
                  )
                ))
              ),
              fluidRow(
                column(10, checkboxInput(ns("AirMonitoring"), label = "大気環境測定局")),
                column(2, h5(actionLink(ns("AirMonitoring_show_advanced"), label = NULL, icon = icon("plus"))))
              ),
              checkboxInput(ns("WaterMonitoring"), label = "公共用水域水質測定点"),
              checkboxInput(ns("PurificationPlant"), label = "浄水場"),
              fluidRow(
                column(10, checkboxInput(ns("Hospital"), label = "病院")),
                column(2, h5(actionLink(ns("Hospital_show_advanced"), label = NULL, icon = icon("plus")))),
                column(12, hidden(
                  div(id = ns("Hospital_advanced"),
                      radioButtons(ns("Hospital_all"), label = NULL, choiceValues = c("all", "select"), choiceNames = c("すべての病院", "分類を選択"), inline = TRUE),
                      selectInput(ns('Hospital_type'), label = NULL, c("救急告示病院", "基幹災害拠点病院", "地域災害拠点病院", "その他の病院"), multiple=TRUE, selectize=TRUE))
                )
                )
              ),
              fluidRow(
                column(10, checkboxInput(ns("Welfare"), label = "福祉施設")),
                column(2, h5(actionLink(ns("Welfare_show_advanced"), label = NULL, icon = icon("plus")))),
                column(12, hidden(
                  div(id = ns("Welfare_advanced"),
                      style = "margin-left: 1em;",
                      checkboxInput(
                        ns("Welfare_select"),
                        label = "分類を指定",
                        value = FALSE
                      ),
                      selectInput(
                        ns('Welfare_type'),
                        label = NULL,
                        multiple=TRUE,
                        selectize=TRUE,
                        choices = c("保護施設" = "hogo",
                                    "老人福祉施設" = "roujin",
                                    "障害者支援施設等" = "shogai",
                                    "身体障害者社会参加支援施設" = "shintai",
                                    "児童福祉施設等" = "jidou",
                                    "母子・父子福祉施設" = "boshihushi",
                                    "その他の社会福祉施設等" = "sonota")
                      )
                  )
                )
                )
              ),
              checkboxInput(ns("Hokenjo"), label = "保健所"),
              checkboxInput(ns("School"), label = "学校"),
              fluidRow(
                column(8, actionButtonStyled(ns("ShowHereButton"), label = "地図に表示", type = "primary", width = "100%")),
                column(4, actionButtonStyled(ns("ShowHereClear"), label = "クリア", type = "danger", width = "100%"))
              )
            ),
            bsCollapsePanel(
              title = "マップの表示",
              style = "primary",
              checkboxGroupInput(
                ns("AreaMap"),
                label = NULL,
                choices = c("メッシュ人口", "洪水浸水想定区域", "津波浸水想定区域", "高潮浸水想定区域", "土砂災害警戒区域（土石流）")
              ),
              fluidRow(
                column(8, actionButtonStyled(ns("AreaMapButton"), label = "地図に表示", type = "primary", width = "100%")),
                column(4, actionButtonStyled(ns("AreaMapClear"), label = "クリア", type = "danger", width = "100%"))
              )
            ),
            bsCollapsePanel(
              title = "大気拡散シミュレーション",
              style = "primary",
              h5("流出地点"),
              fluidRow(
                column(6, numericInput(ns("Latitude"), value = NULL, label = "緯度")),
                column(6, numericInput(ns("Longitude"), value = NULL, label = "経度"))
              ),
              p("※:地図をクリックすると自動的に入力されます。"),
              selectInput(
                ns("AirChemical"),
                label = "物質名称",
                choices = c("トルエン", "1,2-ジクロロエタン"),
                selected = "トルエン",
                width = "100%"
              ),
              selectInput(
                ns("AirAmount"),
                label = "流出速度（kg/s）",
                choices = c(2, 4, 6, 8, 10, 20, 40, 60, 80, 100, 200, 400, 600, 800, 1000),
                selected = NULL,
                width = "100%"
              ),
              selectInput(
                ns("AirElapse"),
                label = "流出継続時間（分）",
                choices = c("10", "60"),
                selected = NULL,
                width = "100%"
              ),
              h5("気象条件"),
              selectInput(
                ns("AirStability"),
                label = "大気安定度",
                choices = c("D（昼間、曇天時）", "F（夜間等）"),
                selected = NULL,
                width = "100%"
              ),
              selectInput(
                ns("AirVelocity"),
                label = "風速（m/s）",
                choices = c("1.5", "3"),
                selected = NULL,
                width = "100%"
              ),
              selectInput(
                ns("AirDirection"),
                label = "風向",
                choices = list(
                  "北" = 90,
                  "北東" = 45,
                  "東" = 0,
                  "南東" = 315,
                  "南" = 270,
                  "南西" = 225,
                  "西" = 180,
                  "北西" = 135
                ),
                selected = NULL,
                width = "100%"
              ),
              selectInput(
                ns("AirTemperature"),
                label = "気温（℃）",
                choices = c(15, 30),
                selected = NULL,
                width = "100%"
              ),
              fluidRow(
                column(8,
                       actionButtonStyled(
                         ns("ShowAirSimulation"),
                         label = "地図に表示",
                         type = "primary",
                         width = "100%"
                       )),
                column(4, actionButtonStyled(ns("ShowAirSimulationClear"), label = "クリア", type = "danger", width = "100%"))
              )
            ),
            bsCollapsePanel(
              title = "河川流出シミュレーション",
              style = "primary",
              selectInput(
                ns("RiverChemical"),
                label = "化学物質",
                choices = c("", "トリエタノールアミン4級塩", "アミンオキシド", "エステルアミド型ジアルキルアミン塩", "クロロホルム"),
                selected = "トリエタノールアミン4級",
                width = "100%"
              ),
              selectInput(
                ns("RiverAmount"),
                label = "排出量",
                choices = c("", "0.1t", "1t", "10t"),
                selected = "0.1t",
                width = "100%"
              ),
              selectInput(
                ns("River"),
                label = "河川",
                choices = c("", "多摩川", "淀川", "日光川"),
                selected = NULL,
                width = "100%"
              ),
              selectInput(
                ns("RiverPoint"),
                label = "流出地点",
                choices = c("上流", "中流", "下流"),
                selected = "上流",
                width = "100%"
              ),
              selectInput(
                ns("RiverWeather"),
                label = "天候",
                choices = c("晴天", "雨天", "集中豪雨"),
                selected = "晴天",
                width = "100%"
              ),
              selectInput(
                ns("RiverElapse"),
                label = "流出からの経過時間",
                choices = list(
                  "1時間" = "1h後",
                  "3時間" = "3h後",
                  "12時間" = "12h後",
                  "24時間" = "24h後",
                  "48時間" = "48h後"),
                selected = "1時間",
                width = "100%"
              ),
              fluidRow(
                column(8,
                       actionButtonStyled(
                         ns("ShowRiverSimulation"),
                         label = "地図に表示",
                         type = "primary",
                         width = "100%"
                       )),
                column(4, actionButtonStyled(ns("ShowRiverSimulationClear"), label = "クリア", type = "danger", width = "100%"))
              )
            ),
            bsCollapsePanel(
              title = "人口の推計",
              style = "primary"
            )#,
            #bsCollapsePanel(
            #  title = "インポートデータの表示",
            #  style = "primary"
            #)
            #,
            # bsCollapsePanel(
            #   title = "入力データの表示",
            #   style = "primary",
            #   uiOutput(ns("ImportedData"))
            # )
          )
        )
      )
    ),
    tabPanel(
      "データのインポート",
      icon = icon("save"),
      fileInput(ns("FileInput"),
                label = "ファイルを開く", accept = c(".kml")
      ),
      div(
        box(
          uiOutput(ns("Preview")),
          title = "プレビュー",
          width = "100%", height = "100%"
        ), 
        style = "height: calc(100vh - 230px)"
      ),
      actionButton(ns("Import"), label = "インポート")
    ),
    tabPanel(
      "表示設定",
      icon = icon("pencil")
    ),
    id = ns("tabBox")
  )
}