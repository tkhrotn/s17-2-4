######################################################################
#
# Exact solution of contaminant transport for river spill
#
######################################################################

# HELPER FUNCTIONS ---------------------

getSolc <- function(sec2day, dist, maxt, mass, area, disp, velo, gamm) {
    solc <- array(0:as.integer(maxt / sec2day), dim = c(1))
    tim <- seq(0, maxt, SECONDS_ONE_DAY)
    k = 1
    
    for (l in tim) {
        mbya <- mass / area
        val1 <- mbya / sqrt(4 * pi * disp * l)
        val2 <- -1.0 * (dist - velo * l) * (dist - velo * l) / (4 * disp * l)
        hfnc <- hermite(3, kind = "h")
        z <- (dist - velo * l) / sqrt(2 * disp * l)
        vtmp <- data.frame(1, as.function(hfnc)(z))
        val3 <- 1 - (gamm * vtmp)
        solc[k] <- val1 * exp(val2) * val3[1, 1]
        k <- k + 1  
    }
    return(solc)
}

modal <- function (inputId = '', initialValue = 0, range) {
    modalDialog(
        class = inputId,
        fluidRow(
            column(4, numericInput(inputId = "minDistance", label = "下限", value = ifelse(length(range) > 1, range[1], initialValue), min = 0.0, step = 1.0)),
            column(4, numericInput( inputId = "maxDistance", label = "上限", value = ifelse(length(range) > 1, range[length(range)], initialValue), min = 0.0, step = 1.0 )),
            column(4, numericInput( inputId = "divisonDistance", label = "分割数", value = ifelse(length(range) > 1, length(range) - 1, 1), min = 0.0, step = 1.0 )),
            column(12, class="options", tags$table(class="table table-bordered", tags$thead(tags$tr(tags$th(align = "center", strong('Case')), tags$th(align = "center", strong('Value')))), tags$tbody()))
        ),
        footer = tagList(actionButton("ok", "OK", class="btn-success"), modalButton("閉じる"))
    )
}

getDataBreakthrough <- function () {
    dist <- distance()
    maxt <- maxtime() * SECONDS_ONE_DAY
    mass <- releasedmass()
    area <- sectionarea()
    disp <- dispersion() #* SECONDS_ONE_DAY
    velo <- rivervelocity() #* SECONDS_ONE_DAY
    gamm <- skewness()
    distOptions <- distOptions()  
    massOptions <- massOptions()  
    areaOptions <- areaOptions()
    dispOptions <- dispOptions()
    veloOptions <- veloOptions()
    gammOptions <- gammOptions()
    
    distLength <- length(distOptions)
    massLength <- length(massOptions)
    areaLength <- length(areaOptions)
    dispLength <- length(dispOptions)
    veloLength <- length(veloOptions)
    gammLength <- length(gammOptions)
    # unit: s
    tim <- seq(0, maxt, SECONDS_ONE_DAY)
    # unit: kg/m3
    
    currentSolc = list()
    currentSolcValue <- getSolc(SECONDS_ONE_DAY, dist, maxt, mass, area, disp, velo, gamm)
    currentSolcName <- sprintf('L%s_M%s_A%s_D%s_V%s_G%s', dist, mass, area, disp, velo, gamm)
    currentSolc[[currentSolcName]] = currentSolcValue
    
    solcList <- list()
    solcList$x = tim / SECONDS_ONE_DAY
    
    # getIndex <- function (distIndex=1, massIndex=1, areaIndex=1, dispIndex=1, veloIndex=1, gammIndex=1) {
    #     loop0 <- gammIndex - 1
    #     loop1 <- loop0 * veloLength  + (veloIndex - 1)
    #     loop2 <- loop1 * dispLength + (dispIndex - 1)
    #     loop3 <- loop2 * areaLength + (areaIndex - 1)
    #     loop4 <- loop3 * massLength + (massIndex - 1)
    #     loop5 <- loop4 * distLength + (distIndex - 1)
    #     index <- loop5 + 1
    #     return(index)
    # }
    
    for (di in 1:distLength) {
        newDist <- ifelse(distLength <= 1, dist, distOptions[di])
        for (m in 1:massLength) {
            newMass <- ifelse(massLength <= 1, mass, massOptions[m])
            for (a in 1:areaLength) {
                newArea <- ifelse(areaLength <= 1, area, areaOptions[a])
                for (d in 1:dispLength) {
                    newDisp <- ifelse(dispLength <= 1, disp, dispOptions[d])
                    for (v in 1:veloLength) {
                        newVelo <- ifelse(veloLength <= 1, velo, veloOptions[v])
                        for (g in 1:gammLength) {
                            newGamm <- ifelse(gammLength <= 1, gamm, gammOptions[v])
                            newSolc <- getSolc(SECONDS_ONE_DAY, newDist, maxt, newMass, newArea, newDisp, newVelo, newGamm)
                            # index <- getIndex(m, a, d, v, g)
                            # solcList[[index]] <- newSolc
                            newCol <- sprintf('L%s_M%s_A%s_D%s_V%s_G%s', newDist, newMass, newArea, newDisp, newVelo, newGamm)
                            solcList[[newCol]] <- newSolc
                            if (gammLength <= 1) { break }
                        }
                        if (veloLength <= 1) { break }
                    }
                    if (dispLength <= 1) { break }
                }
                if (areaLength <= 1) { break }
            }
            if (massLength <= 1) { break }
        }
        if (distLength <= 1) { break }
    }
    
    return(list(tim=tim, sec2day=SECONDS_ONE_DAY, currentSolc=currentSolc, solcList=solcList))
}

getDefaultPlot <- function () {
    p <- renderPlotly({
        data <- getDataBreakthrough()
        name <- names(data$currentSolc)[1]
        plot_ly(y = data$currentSolc[[name]], name = name, text = name, type = "scatter", mode = "lines") %>%
            layout(title = "Estimated Breakthrough Curve", xaxis = list(showgrid = T, title = "経過時間(日)"), yaxis = list(showgrid = F, title = "物質濃度(mg/L)", side = "left"), showlegend = TRUE) %>%
            config(locale = 'ja')
    })
    return(p)
}

# REACTIVE ---------------------

distance <- reactive(as.numeric(input$dist))
maxtime <- reactive(as.numeric(input$tmax))
releasedmass <- reactive(as.numeric(input$mass))
sectionarea <- reactive(as.numeric(input$area))
dispersion <- reactive(as.numeric(input$disp))
rivervelocity <- reactive(as.numeric(input$velo))
skewness <- reactive(as.numeric(input$gamm))
distOptions <- reactive(as.numeric(input$distOptions))
massOptions <- reactive(as.numeric(input$massOptions))
areaOptions <- reactive(as.numeric(input$areaOptions))
dispOptions <- reactive(as.numeric(input$dispOptions))
veloOptions <- reactive(as.numeric(input$veloOptions))
gammOptions <- reactive(as.numeric(input$gammOptions))


# OBSERVE ---------------------

observe({
    dist <- distance()
    #    maxt <- maxtime()
    mass <- releasedmass()
    area <- sectionarea()
    disp <- dispersion() #* sec2day
    velo <- rivervelocity() #* sec2day
    gamm <- skewness()
    distOptions <- distOptions()  
    #    maxtOptions <- maxtOptions()  
    massOptions <- massOptions()  
    areaOptions <- areaOptions()
    dispOptions <- dispOptions()
    veloOptions <- veloOptions()
    gammOptions <- gammOptions()
    output$solve <- getDefaultPlot()
})

# OBSERVE EVENT ---------------------

# Reset data
observeEvent(input$refresh, {
    updateSelectInput(session, 'dist', choices = c(10000.))
    updateTextInput(session, 'distOptions', '')
    updateSelectInput(session, 'mass', choices = c(5000.))
    updateTextInput(session, 'massOptions', '')
    updateSelectInput(session, 'area', choices = c(1500.))
    updateTextInput(session, 'areaOptions', '')
    updateSelectInput(session, 'disp', choices = c(1000.))
    updateTextInput(session, 'dispOptions', '')
    updateSelectInput(session, 'velo', choices = c(0.15))
    updateTextInput(session, 'veloOptions', '')
    updateSelectInput(session, 'gamm', choices = c(0.00))
    updateTextInput(session, 'gammOptions', '')
    output$solve <- getDefaultPlot()
})

observeEvent(input$execute, {
    data <- getDataBreakthrough()
    plotLength <- length(data$solcList) - 1
    if (plotLength <= 1) {
        output$solve <- getDefaultPlot()
    } else {
        output$solve <- renderPlotly({
            p <- plot_ly() %>% config(locale = 'ja')
            for (i in 1:plotLength) {
                name <- names(data$solcList)[i + 1]
                p <- add_trace(p, y = data$solcList[[name]], name = name, text = name, type = "scatter", mode = "lines")
            }
            p %>%
            layout(title = "Estimated Breakthrough Curve", xaxis = list(showgrid = T, title = "経過時間(日)"), yaxis = list(showgrid = F, title = "物質濃度(mg/L)", side = "left"), showlegend = TRUE)
            return(p)
        })
    }
})

observeEvent(input$openDistanceModal, {
    showModal(modal('dist', 5000., as.numeric(input$distOptions)))
})

observeEvent(input$openLeakageModal, {
    showModal(modal('mass', 5000., as.numeric(input$massOptions)))
})

observeEvent(input$openAreaRiverModal, {
    showModal(modal('area', 1500., as.numeric(input$areaOptions)))
})

observeEvent(input$openDispersionModal, {
    showModal(modal('disp', 1000., as.numeric(input$dispOptions)))
})

observeEvent(input$openFlowModal, {
    showModal(modal('velo', 0.15, as.numeric(input$veloOptions)))
})

observeEvent(input$openSkewnessModal, {
    showModal(modal('gamm', 0.00, as.numeric(input$gammOptions)))
})

observeEvent(input$ok, {
    modalName <- input$modalName
    nameInputOptions <- paste0(modalName, 'Options')
    options <- input[[nameInputOptions]]
    updateSelectInput(session, modalName, choices = options)
})


# OUTPUT ---------------------

output$solve <- getDefaultPlot()

output$downloadData = downloadHandler(
    filename = "TMDU_BREAKTHROUGH.csv",
    content = function(file) {
        data <- getDataBreakthrough()
        name <- names(data$solcList)[2]
        output <- data.frame(x = data$tim / data$sec2day, y = data$solcList[[name]])
        write.csv(output, file)
    }
)

output$downloadAll = downloadHandler(
    filename = "TMDU_BREAKTHROUGH-ALL.csv",
    content = function(file) {
        data <- getDataBreakthrough()
        output <- data$solcList
        write.csv(output, file)
    }
)

