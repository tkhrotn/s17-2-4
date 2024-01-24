ChemicalGISServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      require(htmltools)
      require(leaflet)
      require(leaflet.extras)
      require(htmlwidgets)
      require(dplyr)
      require(leafgl)
      require(sf)
      require(RColorBrewer)
      require(dipsaus)
      require(stringr)
      
      source("module/utils.R", local = TRUE, encoding = "UTF8")
      
      rvals <- reactiveValues()
      
      base_groups <- c("OpenStreetMap", "淡色地図", "航空写真", "白地図")
      overlay_groups <- c("メッシュ人口", "洪水浸水想定区域", "津波浸水想定区域", "高潮浸水想定区域", "土砂災害警戒区域（土石流）")
      default_marker_style <- data.frame()
      
      # 医療機関
      P04.hospital <- readRDS("data/P04.hospital.rds")
      P04.hospital.param <- readRDS("data/P04.hospital.param.rds")
      P04.emergency <- readRDS("data/P04.emergency.rds")
      P04.emergency.param <- readRDS("data/P04.emergency.param.rds")
      P04.disaster1 <- readRDS("data/P04.disaster1.rds")
      P04.disaster1.param <- readRDS("data/P04.disaster1.param.rds")
      P04.disaster2 <- readRDS("data/P04.disaster2.rds")
      P04.disaster2.param <- readRDS("data/P04.disaster2.param.rds")
      P04.others <- readRDS("data/P04.others.rds")
      P04.others.param <- readRDS("data/P04.others.param.rds")
      P04.clinic <- readRDS("data/P04.clinic.rds")
      #P04.clinic.param <- readRDS("data/P04.clinic.param.rds")
      P04.dental <- readRDS("data/P04.dental.rds")
      #P04.dental.param <- readRDS("data/P04.dental.param.rds")
      
      # 保健所
      hokenjo <- readRDS("data/hokenjo.rds")
      hokenjo.param <- readRDS("data/hokenjo.param.rds")
      
      # 学校
      P29 <- readRDS("data/P29.rds")
      P29.param <- readRDS("data/P29.param.rds")
      
      # 上水道関連施設
      P21B <- readRDS("data/P21B.rds")
      P21B.param <- readRDS("data/P21B.param.rds")
      
      # 給水区域
      P21A <- readRDS("data/P21A.rds")
      
      # ポンプ場施設
      P22A <- readRDS("data/P22A.rds")
      #P22A.param <- readRDS("data/P22A.param.rds")
      
      # 処理場施設
      P22B <- readRDS("data/P22B.rds")
      #P22B.param <- readRDS("data/P22B.param.rds")
      
      # 水道取水口
      sluice_gate <- readRDS("data/sluice_gate.rds")
      
      # 大気環境測定局
      TM20160023 <- readRDS("data/TM20160023.rds")
      TM20160023.param <- readRDS("data/TM20160023.param.rds")
      
      # 公共用水域水質測定点
      MM20090000 <- readRDS("data/MM20090000.rds")
      #MM20090000.param <- readRDS("data/MM20090000.param.rds")
      
      # 集客施設
      P33 <- readRDS("data/P33.rds")
      #P33.param <- readRDS("data/P33.param.rds")
      
      # PRTR施設
      PRTR <- readRDS("data/PRTR.rds")
      PRTR.param <- readRDS("data/PRTR.param.rds")
      
      # 福祉施設
      P14.hogo <- readRDS("data/P14.hogo.rds")
      P14.roujin <- readRDS("data/P14.roujin.rds")
      P14.shogai <- readRDS("data/P14.shogai.rds")
      P14.shintai <- readRDS("data/P14.shintai.rds")
      P14.jidou <- readRDS("data/P14.jidou.rds")
      P14.boshihushi <- readRDS("data/P14.boshihushi.rds")
      P14.sonota <- readRDS("data/P14.sonota.rds")
      P14.param <- readRDS("data/P14.param.rds")
      
      # 二次医療圏
      #SMA <- readRDS("data/SMA.rds")
      
      output$Map <- renderLeaflet({
        leaflet(options = leafletOptions(minZoom = 5, maxZoom = 18)) %>%
          addMapPane("baseMapPane", 0) %>%
          addTiles(group = base_groups[1]) %>%
          addTiles(
            "https://cyberjapandata.gsi.go.jp/xyz/pale/{z}/{x}/{y}.png",
            group = base_groups[2],
            attribution = "<a href='http://maps.gsi.go.jp/development/ichiran.html' target='_blank'>国土地理院</a>",
            options = tileOptions(pane = "baseMapPane", fadeAnimation = FALSE)) %>%
          addTiles(
            "https://cyberjapandata.gsi.go.jp/xyz/seamlessphoto/{z}/{x}/{y}.jpg",
            group = base_groups[3],
            attribution = "<a href='http://maps.gsi.go.jp/development/ichiran.html' target='_blank'>国土地理院</a>",
            options = tileOptions(pane = "baseMapPane", fadeAnimation = FALSE)) %>%
          addTiles(
            "https://cyberjapandata.gsi.go.jp/xyz/blank/{z}/{x}/{y}.png",
            group = base_groups[4],
            attribution = "<a href='http://maps.gsi.go.jp/development/ichiran.html' target='_blank'>国土地理院</a>",
            options = tileOptions(pane = "baseMapPane", fadeAnimation = FALSE)) %>%
          #addTiles("tile/sma/{z}/{x}/{y}.png", group = "二次医療圏",
          #         options = tileOptions(pane = "overlayPane", maxNativeZoom = 12)) %>%
          addTiles(
            "tile/500mesh_pop/{z}/{x}/{y}.png", group = "メッシュ人口",
            options = tileOptions(pane = "overlayPane", opacity = 0.50, maxNativeZoom = 10)) %>%
          hideGroup("メッシュ人口") %>%
          addTiles(
            "https://disaportaldata.gsi.go.jp/raster/01_flood_l2_shinsuishin_data/{z}/{x}/{y}.png", group = "洪水浸水想定区域",
            options = tileOptions(pane = "overlayPane", opacity = 0.50, maxNativeZoom = 17)) %>%
          hideGroup("洪水浸水想定区域") %>%
          addTiles(
            "https://disaportaldata.gsi.go.jp/raster/04_tsunami_newlegend_data/{z}/{x}/{y}.png", group = "津波浸水想定区域",
            options = tileOptions(pane = "overlayPane", opacity = 0.50, maxNativeZoom = 17)) %>%
          hideGroup("津波浸水想定区域") %>%
          addTiles(
            "https://disaportaldata.gsi.go.jp/raster/03_hightide_l2_shinsuishin_data/{z}/{x}/{y}.png", group = "高潮浸水想定区域",
            options = tileOptions(pane = "overlayPane", opacity = 0.50, maxNativeZoom = 17)) %>%
          hideGroup("高潮浸水想定区域") %>%
          addTiles(
            "https://disaportaldata.gsi.go.jp/raster/05_dosekiryukeikaikuiki/{z}/{x}/{y}.png", group = "土砂災害警戒区域（土石流）",
            options = tileOptions(pane = "overlayPane", opacity = 0.50, maxNativeZoom = 17)) %>%
          hideGroup("土砂災害警戒区域（土石流）") %>%
          setView(140, 37, zoom = 5) %>%
          addScaleBar(position="bottomright") %>%
          addMeasure(
            position = "bottomright",
            primaryLengthUnit = "meters",
            primaryAreaUnit = "sqmeters",
            activeColor = "#3D535D",
            completedColor = "#7D4479") %>%
          addStyleEditor(
            position = "topleft",
            openOnLeafletDraw = FALSE,
          ) %>%
          addDrawToolbar(
            position = "topleft",
            circleMarkerOptions = FALSE,
            editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())) %>%
          addMiniMap(position = "bottomleft") %>%
          addLayersControl(
            baseGroups = base_groups,
            #overlayGroups = overlay_groups,
            position = "topright",
            options=layersControlOptions(autoZIndex = FALSE, collapsed = FALSE))
      })
      
      cropMarkers <- function(x, bounds = input$Map_bounds) {
        x %>% filter(lng > bounds$west & lng < bounds$east & lat > bounds$south & lat < bounds$north)
      }
      
      # 医療機関の表示 ---------------------------------------------------------
      Hospital_show_advanced <- reactiveVal(FALSE)
      observeEvent(input$Hospital_show_advanced, {
        if (Hospital_show_advanced()) {
          hideElement("Hospital_advanced", anim = TRUE)
          updateActionLink(session = session, inputId = "Hospital_show_advanced", icon = icon("plus"))
          Hospital_show_advanced(FALSE)
        } else {
          showElement("Hospital_advanced", anim = TRUE)
          updateActionLink(session = session, inputId = "Hospital_show_advanced", icon = icon("minus"))
          Hospital_show_advanced(TRUE)
        }
      })
      
      observeEvent(input$Hospital_all, {
        if (input$Hospital_all == "all") {
          disable("Hospital_type")
        } else {
          enable("Hospital_type")
        }
      })
      
      # 福祉施設の表示 ---------------------------------------------------------
      Welfare_show_advanced <- reactiveVal(FALSE)
      observeEvent(input$Welfare_show_advanced, {
        if (Welfare_show_advanced()) {
          hideElement("Welfare_advanced", anim = TRUE)
          updateActionLink(session = session, inputId = "Welfare_show_advanced", icon = icon("plus"))
          Welfare_show_advanced(FALSE)
        } else {
          showElement("Welfare_advanced", anim = TRUE)
          updateActionLink(session = session, inputId = "Welfare_show_advanced", icon = icon("minus"))
          Welfare_show_advanced(TRUE)
        }
      })
      
      observeEvent(input$Welfare_select, {
        if (input$Welfare_select) {
          enable("Welfare_type")
        } else {
          disable("Welfare_type")
        }
      })
      
      updateWelfareMarkers <- function(map) {
        map %>% clearGroup(P14.param$group) -> map
        
        if (input$Welfare) {
          P14.hogo.crop <- P14.hogo %>% cropMarkers()
          P14.roujin.crop <- P14.roujin %>% cropMarkers()
          P14.shogai.crop <- P14.shogai %>% cropMarkers()
          P14.jidou.crop <- P14.jidou %>% cropMarkers()
          P14.shintai.crop <- P14.shintai %>% cropMarkers()
          P14.boshihushi.crop <- P14.boshihushi %>% cropMarkers()
          P14.sonota.crop <- P14.sonota %>% cropMarkers()
          
          
          if (!input$Welfare_select) {
            if (nrow(P14.hogo.crop) > 0) {
              map %>% drawFunction(P14.hogo.crop, P14.param) -> map
            }
            if (nrow(P14.roujin.crop) > 0) {
              map %>% drawFunction(P14.roujin.crop, P14.param) -> map
            }
            if (nrow(P14.shogai.crop) > 0) {
              map %>% drawFunction(P14.shogai.crop, P14.param) -> map
            }
            if (nrow(P14.jidou.crop) > 0) {
              map %>% drawFunction(P14.jidou.crop, P14.param) -> map
            }
            if (nrow(P14.shintai.crop) > 0) {
              map %>% drawFunction(P14.shintai.crop, P14.param) -> map
            }
            if (nrow(P14.boshihushi.crop) > 0) {
              map %>% drawFunction(P14.boshihushi.crop, P14.param) -> map
            }
            if (nrow(P14.sonota.crop) > 0) {
              map %>% drawFunction(P14.sonota.crop, P14.param) -> map
            }
          } else {
            if (length(grep("hogo", input$Welfare_type)) > 0 & nrow(P14.hogo.crop) > 0) {
              map %>% drawFunction(P14.hogo.crop, P14.param) -> map
            }
            if (length(grep("roujin", input$Welfare_type)) > 0 & nrow(P14.roujin.crop) > 0) {
              map %>% drawFunction(P14.roujin.crop, P14.param) -> map
            }
            if (length(grep("shogai", input$Welfare_type)) > 0 & nrow(P14.shogai.crop) > 0) {
              map %>% drawFunction(P14.shogai.crop, P14.param) -> map
            }
            if (length(grep("jidou", input$Welfare_type)) > 0 & nrow(P14.jidou.crop) > 0) {
              map %>% drawFunction(P14.jidou.crop, P14.param) -> map
            }
            if (length(grep("shintai", input$Welfare_type)) > 0 & nrow(P14.shintai.crop) > 0) {
              map %>% drawFunction(P14.shintai.crop, P14.param) -> map
            }
            if (length(grep("boshihushi", input$Welfare_type)) > 0 & nrow(P14.boshihushi.crop) > 0) {
              map %>% drawFunction(P14.boshihushi.crop, P14.param) -> map
            }
            if (length(grep("sonota", input$Welfare_type)) > 0 & nrow(P14.sonota.crop) > 0) {
              map %>% drawFunction(P14.sonota.crop, P14.param) -> map
            }
          }
        }
        
        return(map)
      }
      
      # PRTR施設の表示 ---------------------------------------------------------
      PRTR_show_advanced <- reactiveVal(FALSE)
      observeEvent(input$PRTR_show_advanced, {
        if (PRTR_show_advanced()) {
          hideElement("PRTR_advanced", anim = TRUE)
          updateActionLink(session = session, inputId = "PRTR_show_advanced", icon = icon("plus"))
          PRTR_show_advanced(FALSE)
        } else {
          showElement("PRTR_advanced", anim = TRUE)
          updateActionLink(session = session, inputId = "PRTR_show_advanced", icon = icon("minus"))
          PRTR_show_advanced(TRUE)
        }
      })
      
      observeEvent(input$PRTR_chem_select, {
        if (input$PRTR_chem_select) {
          enable("PRTR_chem")
        } else {
          disable("PRTR_chem")
        }
      })
      
      observeEvent(input$PRTR_site_select, {
        if (input$PRTR_site_select) {
          enable("PRTR_site")
        } else {
          disable("PRTR_site")
        }
      })
      
      updatePRTRMarkers <- function(map) {
        map %>% clearGroup(PRTR.param$group) -> map
        
        if (input$PRTR) {
          PRTR.crop <- PRTR %>% cropMarkers()
          
          if (!input$PRTR_site_select) {
            if (input$PRTR_chem_select) {
              if (input$PRTR_chem != "") {
                PRTR.crop %>% filter(str_detect(popup, paste0(">", str_flatten(input$PRTR_chem, collapse = "<|>"), "<"))) -> PRTR.crop
              } else {
                list() -> PRTR.crop
              }
            }
          } else {
            if (!input$PRTR_chem_select) {
              if (input$PRTR_site != "") {
                PRTR.crop %>% filter(str_detect(popup, paste0(">", str_flatten(input$PRTR_site, collapse = "<|>"), "<"))) -> PRTR.crop
              } else {
                list() -> PRTR.crop
              }
            } else {
              if (input$PRTR_site != "" & input$PRTR_chem != "") {
                PRTR.crop %>% 
                  filter(str_detect(popup, paste0(">", str_flatten(input$PRTR_chem, collapse = "<|>"), "<"))) %>%
                  filter(str_detect(popup, paste0(">", str_flatten(input$PRTR_site, collapse = "<|>"), "<"))) -> PRTR.crop
              } else {
                list() -> PRTR.crop
              }
            }
          }
          
          if (nrow(PRTR.crop) > 0) {
            map %>% drawFunction(PRTR.crop, PRTR.param) -> map
          }
        }
        
        return(map)
      }
      
      # 大気環境測定局の表示 ---------------------------------------------------
      updateAirMonitoring <- function(map) {
        map %>% clearGroup(TM20160023.param$group) -> map
        
        if (input$AirMonitoring) {
          TM20160023.crop <- TM20160023 %>% cropMarkers()
          
          if (nrow(TM20160023.crop) > 0) {
            map %>% drawFunction(TM20160023.crop, TM20160023.param) -> map
          }
        }
        
        return(map)
      }
      
      # 上水道施設の表示 -------------------------------------------------------
      observeEvent(input$Map_marker_click, {
        if (input$Map_marker_click$group == "上水道関連施設") {
          ids <- str_split(input$Map_marker_click$id, ":", simplify = TRUE)
          areas <- P21A %>% filter(P21A_001 == ids[1] & P21A_002 == ids[2])
          
          map <- leafletProxy("Map", session)
          map %>%
            clearGroup("P21A") %>%
            addPolygons(data = areas, popup = ~popup, label = ~P21A_002, group = "P21A") ->
            map
        }
      })
      
      # この地域の施設を表示 ---------------------------------------------------
      
      observeEvent(input$ShowHereButton, {
        P04.emergency.crop <- P04.emergency %>% filter(lng > input$Map_bounds$west & lng < input$Map_bounds$east & lat > input$Map_bounds$south & lat < input$Map_bounds$north)
        P04.disaster1.crop <- P04.disaster1 %>% filter(lng > input$Map_bounds$west & lng < input$Map_bounds$east & lat > input$Map_bounds$south & lat < input$Map_bounds$north)
        P04.disaster2.crop <- P04.disaster2 %>% filter(lng > input$Map_bounds$west & lng < input$Map_bounds$east & lat > input$Map_bounds$south & lat < input$Map_bounds$north)
        P04.others.crop <- P04.others %>% filter(lng > input$Map_bounds$west & lng < input$Map_bounds$east & lat > input$Map_bounds$south & lat < input$Map_bounds$north)
        hokenjo.crop <- hokenjo %>% filter(lng > input$Map_bounds$west & lng < input$Map_bounds$east & lat > input$Map_bounds$south & lat < input$Map_bounds$north)
        P29.crop <- P29 %>% filter(lng > input$Map_bounds$west & lng < input$Map_bounds$east & lat > input$Map_bounds$south & lat < input$Map_bounds$north)
        P21B.crop <- P21B %>% filter(lng > input$Map_bounds$west & lng < input$Map_bounds$east & lat > input$Map_bounds$south & lat < input$Map_bounds$north)
        
        map <- leafletProxy("Map", session)
        map %>% 
          clearGroup(P04.emergency.param$group) %>%
          clearGroup(P04.disaster1.param$group) %>%
          clearGroup(P04.disaster2.param$group) %>%
          clearGroup(P04.others.param$group) %>%
          clearGroup(P29.param$group) %>%
          clearGroup(hokenjo.param$group) %>%
          clearGroup(P21B.param$group) -> map

        # 病院
        if (input$Hospital) {
          if (input$Hospital_all == "all") {
            if (nrow(P04.emergency.crop) > 0) {
              map %>% drawFunction(P04.emergency.crop, P04.emergency.param) -> map
            }
            if (nrow(P04.disaster1.crop) > 0) {
              map %>% drawFunction(P04.disaster1.crop, P04.disaster1.param) -> map
            }
            if (nrow(P04.disaster2.crop) > 0) {
              map %>% drawFunction(P04.disaster2.crop, P04.disaster2.param) -> map
            }
            if (nrow(P04.others.crop) > 0) {
              map %>% drawFunction(P04.others.crop, P04.others.param) -> map
            }
          } else {
            if (length(grep("救急告示病院", input$Hospital_type)) > 0 & nrow(P04.emergency.crop) > 0) {
              map %>% drawFunction(P04.emergency.crop, P04.emergency.param) -> map
            }
            if (length(grep("基幹災害拠点病院", input$Hospital_type)) > 0 & nrow(P04.disaster1.crop) > 0) {
              map %>% drawFunction(P04.disaster1.crop, P04.disaster1.param) -> map
            }
            if (length(grep("地域災害拠点病院", input$Hospital_type)) > 0 & nrow(P04.disaster2.crop) > 0) {
              map %>% drawFunction(P04.disaster2.crop, P04.disaster2.param) -> map
            }
            if (length(grep("その他の病院", input$Hospital_type)) > 0 & nrow(P04.others.crop) > 0) {
              map %>% drawFunction(P04.others.crop, P04.others.param) -> map
            }
          }
        }

        # 保健所
        if (input$Hokenjo & nrow(hokenjo.crop) > 0) {
          map %>% drawFunction(hokenjo.crop, hokenjo.param) -> map
        }
        
        # PRTR施設
        
        # 浄水場
        if (input$PurificationPlant & nrow(P21B.crop) > 0) {
          map %>% drawFunction(P21B.crop, P21B.param) -> map
        }
        
        # 学校
        if (input$School & nrow(P29.crop) > 0) {
          map %>% drawFunction(P29.crop, P29.param) -> map
        }
        
        map %>% 
          updatePRTRMarkers() %>%
          updateAirMonitoring() %>%
          updateWelfareMarkers() -> map
      })
      
      observeEvent(input$ShowHereClear, {
        map <- leafletProxy("Map", session)
        map %>% 
          clearGroup(PRTR.param$group) %>% 
          clearGroup(TM20160023.param$group) %>%
          clearGroup(P14.param$group) %>%
          clearGroup(P04.emergency.param$group) %>%
          clearGroup(P04.disaster1.param$group) %>%
          clearGroup(P04.disaster2.param$group) %>%
          clearGroup(P04.others.param$group) %>%
          clearGroup(P29.param$group) %>%
          clearGroup(hokenjo.param$group) %>%
          clearGroup(P21B.param$group) %>%
          clearGroup("P21A") -> map
      })
      
      # マップの表示 -----------------------------------------------------------
      observeEvent(input$AreaMapButton, {
        print(input$AreaMap)
        map <- leafletProxy("Map")
        
        for (i in overlay_groups) {
          map %>% hideGroup(i) -> map
        }
        
        for (i in input$AreaMap) {
          map %>% showGroup(i) -> map
        }
      })
      
      observeEvent(input$AreaMapClear, {
        map <- leafletProxy("Map")
        
        for (i in overlay_groups) {
          map %>% hideGroup(i) -> map
        }
      })

      
      # 河川流出シミュレーション -----------------------------------------------
      observeEvent(input$Collapse, {
        if (input$Collapse != "河川流出シミュレーション") {
          map <- leafletProxy("Map")
          map %>%
            clearGroup("ReleasePoint") -> map
        }
      })
      
      observe({
        if (input$River != "") {
          release_point <- readRDS(paste0("data/", input$River, "_事故地点位置.rds")) %>%
            filter(str_detect(Name, input$RiverPoint))
          
          map <- leafletProxy("Map")
          
          map %>%
            clearGroup("ReleasePoint") %>%
            addAwesomeMarkers(label = ~Name, data = release_point, group = "ReleasePoint", icon = makeAwesomeIcon("times", library = "fa", markerColor = "red", iconColor = '#FFFFFF')) -> map
        }
      })
      
      observeEvent(input$ShowRiverSimulation, {
        layer <- readRDS(paste0("data/",
                                input$River, "_",
                                input$RiverWeather, "時_",
                                input$RiverPoint, "_",
                                input$RiverChemical,
                                input$RiverAmount, "排出_",
                                input$RiverElapse, ".rds"))
        
        map <- leafletProxy("Map")
        
        map %>%
          clearGroup("RiverSimulation") -> map
        
        for (i in 1:(length(layer) - 1)) {
          map %>%
            addPolygons(
              weight = 0,
              color = NULL,
              data = layer[[i]][[1]],
              fillColor = layer[[i]]$color,
              fillOpacity = layer[[i]]$opacity,
              group = "RiverSimulation"
            ) -> map
        }
      })
      
      observeEvent(input$ShowRiverSimulationClear, {
        map <- leafletProxy("Map")
        
        map %>%
          clearGroup("ReleasePoint") %>%
          clearGroup("RiverSimulation") -> map
      })
      
      
      # データのインポート -----------------------------------------------------
      
      output$PreviewMap <- renderLeaflet({
        m <- leaflet() %>%
          addTiles() %>%
          addLayersControl(
            overlayGroups = rvals$preview_layer_name
          )
        
        pal <- brewer.pal(length(rvals$preview_layer_name), "Set1")
        for (i in 1:length(rvals$preview_layer_name)) {
          m <- m %>%
            addPolygons(weight = 0, color = NULL, fillColor = pal[i], fillOpacity = 0.8, data = st_zm(st_geometry(rvals$preview_layer[[i]])), group = rvals$preview_layer_name[i])
        }
        
        return(m)
      })
      
      observeEvent(input$FileInput, {
        layer <- st_layers(input$FileInput$datapath)
        name <- layer$name
        
        layer <- list()
        for(i in name) {
          layer <- c(layer, list(st_union(st_read(input$FileInput$datapath, layer = i))))
        }
        rvals$preview_layer <- layer
        rvals$preview_layer_name <- name
        
        output$Preview <- renderUI({
          leafletOutput(session$ns("PreviewMap"), width = "100%", height = "500")
        })
      })
      
      observeEvent(input$Import, {
        if (!is.null(rvals$preview_layer_name) & !is.null(rvals$preview_layer)) {
          rvals$imported_layer <- c(rvals$imported_layer, rvals$preview_layer)
          rvals$imported_layer_name <- c(rvals$imported_layer_name, rvals$preview_layer_name)
          
          pal <- brewer.pal(length(rvals$preview_layer_name), "Set1")
          m <- leafletProxy("Map", session)
          for (i in 1:length(rvals$preview_layer_name)) {
            m <- m %>%
              addPolygons(weight = 0, color = NULL, fillColor = pal[i], fillOpacity = 0.8, data = st_zm(st_geometry(rvals$preview_layer[[i]])), group = rvals$preview_layer_name[i])
          }
          
          showNotification("データをインポートしました。", type = "message")
          
          output$ImportedData <- renderUI({
            checkboxGroupInput(session$ns("ImportedDataCheckbox"), label = NULL, choices = rvals$imported_layer_name, selected = rvals$imported_layer_name)
          })
        }
      })
      
      observeEvent(input$ImportedDataCheckbox, {
        m <- leafletProxy("Map", session)
        m <- m %>%
          hideGroup(rvals$imported_layer_name) %>%
          showGroup(input$ImportedDataCheckbox)
      })
      
      
      # 図形描画処理 -----------------------------------------------------------
      features <- reactiveVal(list())
      
      observeEvent(input$Map_draw_all_features, {
        features(input$Map_draw_all_features$features)
        print(length(features()))
      })
      
      observeEvent(input$Map_shape_click, {
        p <- input$Map_shape_click
        
        print(p)
      })
      
      
      # URLパラメータの処理 ----------------------------------------------------
      observe({
        query <- getQueryString()
        map <- leafletProxy("Map", session)
        
        if (!is.null(query$lng) & !is.null(query$lat) & !is.null(query$zoom)) {
          map <- map %>% setView(as.numeric(query$lng), as.numeric(query$lat), as.numeric(query$zoom))
        }
      })
    }
  )
}
