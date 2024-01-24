server <- function(input, output, clientData, session) {
  Sys.setlocale("LC_ALL", locale="English")

  # Using Eval instead of source to fix encoding error
  # Reference: https://stackoverflow.com/questions/5031630/how-to-source-r-file-saved-using-utf-8-encoding

  eval(parse("constants.R", encoding="UTF-8"))

  eval(parse("utils.R", encoding="UTF-8"))

  eval(parse("mkMaps.R", encoding="UTF-8"))

  # Tab 1: マップ
  eval(parse("components/Map.R", encoding="UTF-8"))

  # Tab 2: 時系列データ
  eval(parse("components/Plot.R", encoding="UTF-8"))

  # Tab 3: データテーブル
  eval(parse("components/DataTable.R", encoding="UTF-8"))

  # Tab 4: 移流分散理論解
  eval(parse("components/Calculator.R", encoding="UTF-8"))

  # Tab 5: セッティング
  eval(parse("components/Settings.R", encoding="UTF-8"))

  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
}
# 