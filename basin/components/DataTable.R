######################################################################
#
# Datatable of data location
#
######################################################################

# OUTPUT ---------------------

output$table <- renderDT({
  data <- data()
  if (is.null(data)) return(NULL)

  datatable(data, options = list(
      language = list(url = '//cdn.datatables.net/plug-ins/1.12.1/i18n/ja.json')
    )
  )
})

datasetInput <- reactive({
  data <- data()
  if (is.null(data)) return(NULL)
  return(data)
})

# Downloadable csv of selected dataset ----
output$downloadDataTable <- downloadHandler(
  filename = "TMDU_DATA_TABLE.csv",
  content = function(file) {
    fwrite(datasetInput(), file, row.names = FALSE,bom=T)
  }
)

