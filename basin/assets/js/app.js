/*--------------------------------
  CACHE VARIABLES
---------------------------------*/
var options = {};
var geojsonData = null;
var _hotlineRiver = null;
var STOP = false;
var timers = [];
var drawGroup = null;
var addLocationPopup = null;

/*--------------------------------
  CONSTANTS VARIABLES
---------------------------------*/
const iconHTML = '<i class="far fa-stop-circle"></i> Stop';
const playHTML = '<i class="fas fa-sm fa-play"></i> Play';
const milliseconds_per_day = 500;
const DEFAULT_COLOR = ["#00FFFF", "#00FF00", "#FFFF00", "#FF7F00", "#FF0000"];
const NA_COLOR = '#808080';

function saveLocationId() {
  let locationId = $('#newLocationId').val();
  let drawings = drawGroup.getLayers();
  let addLayer = drawings[drawings.length - 1];
  addLayer.options.layerId = locationId;
  addLayer.options.label = locationId;
  addLayer.options.group = "g_circle_markers";
  addLayer.bindTooltip(locationId, { direction: 'bottom', className: 'black-tooltip' });
  
  if (addLocationPopup != null) {
    let map = document.getElementById('map')._leaflet_map;
    map.closePopup();
  }
}

$(function () {
  /*--------------------------------
    CLIENT EVENTS
  ---------------------------------*/
  /** Create an element to hide/show legends */
  var $legendToggle = $(document.createElement('a'));
  $legendToggle.addClass('icon-toggle').html('<i class="fas fa-arrow-left"></i>').appendTo($('#map'));
  $legendToggle.hide();

  /** Init hook to get Leaflet map instance */ 
  L.Map.addInitHook(function () {
    // Only hook main map, skip mini-map
    if (this.getContainer().id != "map")
      return;

    // Store map instance
    let map = this;
    this.getContainer()._leaflet_map = map;
    this.whenReady(function () {
      $legendToggle.show();

      setTimeout(function() {
        drawGroup = new L.FeatureGroup();
        map.addLayer(drawGroup);

        // Init leaflet Draw control
        let drawControl = new L.Control.Draw({
          draw: {
            polyline: false,
            polygon: false,
            rectangle: false,
            circle: false,
            marker: false,
            circlemarker: {
              fillColor: "#000000",
              fillOpacity: 0.3,
              stroke: true,
              weight: 4,
              color: '#00FF00',
              opacity: 0.5,
            }
          },
          edit: { featureGroup: drawGroup },
          position: 'topright'
        });
        map.addControl(drawControl);

        // Draw:created event add new layer to group
        map.on(L.Draw.Event.CREATED, function (e) {
          let layer = e.layer;
          drawGroup.addLayer(layer);

          // Add popup to created layer
          addLocationPopup = L.popup({ closeOnClick: false });
          let content = `<span><b>Location Id</b></span><br/>
                         <input id="newLocationId" class="form-control" type="text"/><br/>
                         <input type="button" class="btn btn-xs" value="Save" onclick="saveLocationId()"/>`;
          addLocationPopup.setContent(content);
          addLocationPopup.setLatLng(layer.getLatLng());
          addLocationPopup.openOn(map);
          $("#newLocationId").focus();
        });

        // Init download river geojson button
        L.Control.Button = L.Control.extend({
          options: { position: 'topright' },
          onAdd: function (map) {
              let container = L.DomUtil.create('div', 'leaflet-bar leaflet-control');
              container.title = "Download edited locations";
              let button = L.DomUtil.create('a', 'leaflet-control-button', container);
              button.innerHTML = '<img class="customClass" src="images/download.png" width="16">';
              L.DomEvent.disableClickPropagation(button);
              L.DomEvent.on(button, 'click', function(){
                // downloadLayersAsGeojson("g_circle_markers", "river_stations.geojson");
                downloadRiverStationAsCsv("river_stations.csv");
              });
              return container;
          },
        });
        let control = new L.Control.Button()
        control.addTo(map);
      
        addCircleToDrawGroup();
      }, 500)
    });
  });


  /** Tab show/hide Params on Map */
  $('#tabs a').on('shown.bs.tab', function (e) {
    let activeTab = $(e.target).text().trim();
    if (activeTab == '河川') {
      $('.params-river').fadeIn();
      $('.params-ground-water').hide();
    } else if (activeTab == '地下水') {
      $('.params-river').hide();
      $('.params-ground-water').fadeIn();
    }
  });

  /** Water Setting change Params on Map text */
  $('#controls input').on('change', function(e) {
    const id = $(this).attr('id');
    let bindElements = $('span[from=' + id + ']');
    if (bindElements.length == 0)
      return;
    bindElements.text($(this).val());
  });

  /** Accordions show/hide */
  $('.accordion').on('click', '.accordion-button', function () {
    const accordionItem = $(this).closest('.accordion-item')
    const accordionCollapse = accordionItem.find('.accordion-collapse')
    const isShow = accordionItem.hasClass('show')
    // accordionCollapse.slideToggle()
    if (isShow) {
      accordionItem.removeClass('show')
      accordionCollapse.slideUp()
    } else {
      accordionItem.addClass('show')
      accordionCollapse.slideDown()
    }
  });
  $('.accordion-item.show').find('.accordion-collapse').slideDown();

  /** Read geojson data from shapefile. Assign data to geojsonData variable */
  document.getElementById('shpfile').addEventListener('change', readGeojsonData, false);

  // Click On Toggle Icon to show/hide legends
  $('body').on('click', '.icon-toggle', function (e) {
    e.stopPropagation();
    let toggleElements = $('.leaflet-top').first().children().not(":nth-child(1)");
    toggleElements.toggle('slow');
    $('.icon-toggle').toggleClass('rotate');
  });

  // Enable/disable user manual input Direction
  $('body').on('change', '#ignoreAltitude', function() {
    $("#direction").prop('disabled', !this.checked);
  });
  $("#direction").prop('disabled', true);

  // Download map data as GeoJSON
  $('body').on('click', '#downloadshp', function(e) {
    e.preventDefault();
    downloadLayersAsGeojson();
  });



  // Set Select Hotline Pallet
  $('.color-palette').on('click', function() {
    Shiny.setInputValue('rainbowPalletSelected', [
      $(this).css('--color1'),
      $(this).css('--color2'),
      $(this).css('--color3'),
      $(this).css('--color4'),
      $(this).css('--color5')
    ]);
  });

  // Tab 4: modal
  $('body').on('keyup input', '#shiny-modal input', function () {
    let shinyModal = $(this).closest('#shiny-modal');
    const options = getOptions(shinyModal);
    addCasesToModal(options);
    return false;
  });

  $('body').on('click', '#shiny-modal button#ok', function () {
    let shinyModal = $(this).closest('#shiny-modal');
    let selectInputID = getSelectInputID(shinyModal);
    let nameInputOptions = `${selectInputID}Options`;

    shinyModal.modal('hide');
    const options = getOptions(shinyModal);

    Shiny.setInputValue('modalName', selectInputID);
    Shiny.setInputValue(nameInputOptions, options);
  });

  $('body').on('shown.bs.modal', '#shiny-modal', function () {
    let shinyModal = $(this);
    const options = getOptions(shinyModal);
    addCasesToModal(options);
  });


  /*--------------------------------
    SERVER EVENTS
  ---------------------------------*/
  
  // Add json data to Map
  Shiny.addCustomMessageHandler('onAddJson', handlerAddJson)
  function handlerAddJson(data) {
    checkMap().then((map) => {
      try {
        L.geoJSON(data).addTo(map);
      } catch(err) {
        console.error('Error handlerAddJson: ', err)
      } 
    });
  }

  //  Check to show/hide legends
  Shiny.addCustomMessageHandler('onShowLegend', handlerShowLegend)
  function handlerShowLegend(data) {
    let isLegendHidden = $('.icon-toggle').hasClass('rotate')
    if (isLegendHidden) {
      $('.icon-toggle').trigger('click')
    }
  }

  // Clear all layers by group name
  Shiny.addCustomMessageHandler('onClearLayerByGroupName', handlerClearLayerByGroupName)
  function handlerClearLayerByGroupName(groupName) {
    let map = document.getElementById('map')._leaflet_map;
    if (!map) return;
    map.eachLayer(function (layer) {
      if (layer.options.group == groupName) {
        map.removeLayer(layer)
      }
    });

    // Clear draw group
    drawGroup.clearLayers();
  }

  // Re-init Draw group
  Shiny.addCustomMessageHandler('onAddedLayerByGroupName', handlerAddedLayerByGroupName)
  function handlerAddedLayerByGroupName(groupName) {
    let map = document.getElementById('map')._leaflet_map;
    if (!map) return;

    // Add all river circles to group
    setTimeout(function(){
      addCircleToDrawGroup();
    }, 500)
  }

  // Update color of Circle Markers
  Shiny.addCustomMessageHandler("onUpdateColorMarkers", handlerUpdateColorMarkers);
  function handlerUpdateColorMarkers(payload) {
    let data = payload.data
    let summary = payload.summary
    // Get Colors list. From Default or Palette setting
    let colors = DEFAULT_COLOR
    if (options.palette) {
      colors = []
      let colorsPalette = options.palette
      Object.keys(colorsPalette).sort().forEach(key => {
        colors.push(colorsPalette[key])
      })
    }
    const values = data.VALUE

    checkMap().then(map => {
      map.eachLayer(layer => {
        try {
          var color = NA_COLOR
          if (layer.options.labelOptions && layer.options.labelOptions.layerId) {
            const ID = layer.options.labelOptions.layerId
            const indexLayer = data.NAME.indexOf(ID)
            const value = values[indexLayer]
            let tooltip = `${ID}: ${value}`

            if (value == null) {
              tooltip = `${ID}: NA`
            }

            if (indexLayer !== -1) {
              if (value !== null && value !== 'NaN' && value !== NaN) {
                const indexColor = getIndexColor(value, summary[0], summary[1])
                if (indexColor == -1) {
                  color = NA_COLOR
                } else {
                  if (Array.isArray(colors)) {
                    color = colors[indexColor]
                  } else {
                    color = Object.values(colors)[indexColor]
                  }
                }
              }
              layer.setStyle({ fillColor: color }).bindTooltip(tooltip)
            } else {
              layer.setStyle({ fillColor: NA_COLOR }).bindTooltip(tooltip)
            }
          }
        } catch (error) {}
      })
    })
  }

  // Update hotline settings
  Shiny.addCustomMessageHandler("onUpdateHotlineSettings", handlerUpdateHotlineSettings);
  function handlerUpdateHotlineSettings(hotlineSettings) {
    hotlineSettings.palette = {};
    if (hotlineSettings.rainbowPallet) {
      var paletteJumpStep = 1 / (hotlineSettings.rainbowPallet.length - 1);
      for (let i = 0; i < hotlineSettings.rainbowPallet.length; i++) {
        let paletteBreak =  i * paletteJumpStep;
        hotlineSettings.palette[paletteBreak] = hotlineSettings.rainbowPallet[i];
      }
    }
    options = hotlineSettings;
    return options;
  }

  // Clear hotline
  Shiny.addCustomMessageHandler("onClearHotline", handlerClearHotline);
  function handlerClearHotline(data) {
    try {
      _hotlineRiver.remove();
    } catch (error) {}
    _hotlineRiver = null;
  }

  function addHCircleMarkers(points, color) {
    let map = document.getElementById('map')._leaflet_map;
    for (const point of points) {
      const circle = L.circleMarker(point.slice(0, 2), { radius: 10, color: color }).bindTooltip(`${point[0]}, ${point[1]}`)
      circle.addTo(map)
    }
  }

  function removeUpstream(arr, from) {
    const indexFrom = arr.findIndex(o => o[0] === from[0] && o[1] === from[1])
    if (indexFrom !== -1) {
      arr = arr.map((o, index) => {
        if (index >= indexFrom) {
          return [o[0], o[1], NA_VALUE]
        }
        return o
      })
    }
    return arr
  }
  
  function removeAllUpstream(arr) {
    return arr.map(o => [o[0], o[1], NA_VALUE])
  }

  Shiny.addCustomMessageHandler("onUpdateHotLineBuffer", handlerUpdateHotLineBuffer);
  function handlerUpdateHotLineBuffer(data) {
    let hotlineList = buildHotlineDataBuffer(geojsonData, data);

    // Remove upstream for "SampleRiverValue_wgs84" file only
    if (geojsonData != null && geojsonData.fileName != null && geojsonData.fileName == "SampleRiverValue_wgs84") {
      hotlineList[0] = removeUpstream(hotlineList[0], [34.807823888888954, 135.61901972222233])
      hotlineList[1] = removeUpstream(hotlineList[1], [34.818640555555646, 135.63931972222224])
      hotlineList[3] = removeUpstream(hotlineList[3], [34.8514405555556, 135.65704472222228])
      hotlineList[4] = removeAllUpstream(hotlineList[4])
      hotlineList[5] = removeAllUpstream(hotlineList[5])
    }

    // Add hotline
    addHotline(hotlineList, data.min, data.max, false)
  }

  Shiny.addCustomMessageHandler('onBuildHotlineOriginTest', handlerBuildHotlineOrigin)
  function handlerBuildHotlineOrigin(_) {
    let hotlineList = buildHotlineDataTest(geojsonData)
    addHotlineTest(hotlineList);
  }

  Shiny.addCustomMessageHandler('onAddHotlineNAValue', handlerAddHotlineNAValue);
  function handlerAddHotlineNAValue(_) {
    if (_hotlineRiver) {
      _hotlineRiver.setStyle({
        'palette': {
          0: NA_COLOR,
        }
      })
    } else {
      let hotlineList = buildHotlineDataNA(geojsonData)
      addHotline(hotlineList, 0, 0, false);
    }
  }

  Shiny.addCustomMessageHandler('onAddCircleNAValue', handlerAddCircleNAValue)
  function handlerAddCircleNAValue(_) {
    checkMap().then(map => {
      map.eachLayer(layer => {
        try {
          if (layer.options.labelOptions && layer.options.labelOptions.layerId) {
            const ID = layer.options.labelOptions.layerId
            layer.setStyle({ fillColor: NA_COLOR }).bindTooltip(`${ID}: NA`)
          }
        } catch (error) {}
      })
    })
  }

  Shiny.addCustomMessageHandler("onAddHotLine", handlerAddHotLine);
  function handlerAddHotLine(hotlineData) {
    addHotline(hotlineData.data, hotlineData.min, hotlineData.max);
  }

  // Click to Time Player
  Shiny.addCustomMessageHandler("runAnimationRiver", runAnimationRiver);
  function runAnimationRiver(data) {
    const html =  STOP ? playHTML : iconHTML;
    $("#runAnimationRiver").html(html);
    if (STOP) {
      clearTimer();
    } else {
      const indexStart = data.dates.indexOf(data.start);
      for (let i = indexStart; i < data.dates.length; i++) {
        const date = data.dates[i];
        setDelayRiver(indexStart, i, date, data);
      }
    }
    STOP = !STOP;
  }

  // Click to Map and find nearest of point
  Shiny.addCustomMessageHandler('onFindNearestPoint', handlerFindNearestPoint)
  function handlerFindNearestPoint(data) {
    let minDistance = Number.POSITIVE_INFINITY
    let nearestPoint = null
    let fromLatlng = data.fromLatlng

    if (fromLatlng) {
      for (const point of data.mapPoints) {
        distance = findDistance([fromLatlng.lng, fromLatlng.lat], point)
        if (distance < minDistance) {
          minDistance = distance
          nearestPoint = point
        }
      }
      if (nearestPoint) {
        if (fromLatlng.lng.toFixed(6) !== nearestPoint[0].toFixed(6) || fromLatlng.lat.toFixed(6) !== nearestPoint[1].toFixed(6)) {
          let map = document.getElementById('map')._leaflet_map;
          map.fire('click', {
            latlng: L.latLng([nearestPoint[1], nearestPoint[0]])
          })
        }
      }
    }
  }

  // Fire Click map
  Shiny.addCustomMessageHandler('onClickMap', handlerClickMap)
  function handlerClickMap(latlng) {
    let map = document.getElementById('map')._leaflet_map;
    map.fire('click', {
      latlng: L.latLng([latlng.lat, latlng.lng])
    })
  }

  Shiny.addCustomMessageHandler('onToggleCalculateCTAT', handlerToggleCalculateCTAT)
  function handlerToggleCalculateCTAT(isInProcess) {
    let actionButton = document.getElementById('calcATCT')
    if (actionButton) {
      if (isInProcess) {
        actionButton.textContent = 'Calculating...'
        actionButton.disabled = true
      } else {
        actionButton.textContent = '移流分散を計算'
        actionButton.disabled = false
      }
    } 
  }

  Shiny.addCustomMessageHandler('onBringToBackClickablePoint', handlerBringToBackClickablePoint)
  function handlerBringToBackClickablePoint(_) {
    setTimeout(() => {
      let map = document.getElementById('map')._leaflet_map;
      let srclatlngLayer = null;
      let srcROILayer = null;
      map.eachLayer(function (layer) {
        if (layer.options) {
          // Bring all markers to front
          if (layer.options.group === 'g_circle_markers')
            layer.bringToFront()

          if (layer.options.layerId == 'srclatlng')
            srclatlngLayer = layer;
          if (layer.options.layerId == 'srcROI')
            srcROILayer = layer;
        }
      })
      // Bring 2 red dots to front later
      if (srclatlngLayer) srclatlngLayer.bringToFront();
      if (srcROILayer) srcROILayer.bringToFront();
    }, 500)
  }

  /*--------------------------------
    HELPER FUNCTIONS
  ---------------------------------*/

  function clearTimer() {
    for (const timer of timers) {
      clearTimeout(timer);
    }
  }

  function setDelayRiver(start, i, date, data) {
    let timer = setTimeout(() => {
      $.fn.bsDatepicker
      Shiny.setInputValue('currentDate', date)
      $('#currentDate input').val(date)
      $('#currentDate input').trigger('keyup')

      if (i == data.dates.length - 1) {
        setTimeout(() => {
          $("#runAnimationRiver").html(playHTML)
        }, milliseconds_per_day)
      }
    }, milliseconds_per_day * (i - start))
    timers.push(timer)
  }

  // TAB 4
  function convertToFloat(value) {
    return parseFloat(parseFloat(value).toFixed(1))
  } 

  function getOptions(shinyModal) {
    let inputs = shinyModal.find('input');
    let min = $(inputs[0]).val();
    let max = $(inputs[1]).val();
    let division = $(inputs[2]).val();

    min = convertToFloat(min)
    max = convertToFloat(max)
    division = convertToFloat(division)

    if (isNaN(min) || min < 0) return null;
    if (isNaN(max) || max < 0) return null;
    if (isNaN(division) || division < 0) return null;
    if (min > max || division < 1) return null;
    let results = [];
    let delta = (max - min) / division;
    while (convertToFloat(min) < max) {
      if (isNaN(min)) return null;
      results.push(convertToFloat(min))
      min += delta;
    }
    results.push(max)
    return results;
  }

  function getOptionsHTML(options) {
    let elementsHTML = [];
    if (options) {
      for (const [index, option] of options.entries()) {
        let optionElement = document.createElement('tr');
        let caseElement = document.createElement('td');
        let valueElement = document.createElement('td');

        caseElement.textContent = `Case ${index + 1}`; 
        valueElement.textContent = `${option}`;

        optionElement.appendChild(caseElement)
        optionElement.appendChild(valueElement)

        elementsHTML.push(optionElement);
      }
      return elementsHTML
    }
    return null
  }

  function getSelectInputID(shinyModal) {
    return shinyModal.find('.modal-body').attr("class").split(/\s+/)[1];
  }

  function addCasesToModal(options) {
    const optionsHTML = getOptionsHTML(options);
    $('#shiny-modal .options tbody').html('')
    if (optionsHTML) {
      optionsHTML.forEach(function (element, i) {
        $('#shiny-modal .options tbody').append(element.outerHTML).hide().fadeIn(300);
      })
    }
  }

  // Add hotline to map
  function addHotline(data, min, max, fitBounds=true) {
    if (data && data.length) {
      checkMap().then((map) => {
        handlerClearHotline()
        let hotlineOptions = options
        if (min == undefined || max == undefined) {
          // Find min, max
          let dataDropNa = data.map(item => item[2]).filter(v => !isNaN(v))
          min = Math.min(...dataDropNa)
          max = Math.max(...dataDropNa)
        }
        hotlineOptions = Object.assign(hotlineOptions, { min, max });

        _hotlineRiver = L.hotline(data, hotlineOptions).addTo(map)
        if (fitBounds) map.fitBounds(_hotlineRiver.getBounds())
      })
    }
  }

  // ONlY FOR TEST
  function addHotlineTest(hotlines) {
    if (hotlines && hotlines.length) {
      checkMap().then((map) => {
        handlerClearHotline()
        for (const line of hotlines) {
          L.hotline(line, {
            outlineWidth: 5,
            outlineColor: `#${Math.floor(Math.random()*16777215).toString(16)}`,
          }).addTo(map)
        }
      })
    }
  }

  function getIndexColor(value, min, max) {
    if (value < min || value > max || min == max) return -1;
    const N = 6;
    const delta = (max - min) / N

    for (let i = 0; i < N; i++) {
      if (min + delta*i > value) {
        return i - 1
      }
    }
    return N - 2
  }
  
  // Request Animation Frame for time player
  function rafAsync() {
    return new Promise(resolve => {
      requestAnimationFrame(resolve);
    });
  }

  // Waiting for map loaded
  function checkMap() {
    let map = document.getElementById('map')._leaflet_map;
    if (!map) {
      return rafAsync().then(() => checkMap());
    } else {
      return Promise.resolve(map);
    }
  }
  
  // Add Polylines data from ZIP file
  function readGeojsonData(evt) {
    let files = evt.target.files;
    geojsonData = null;

    for (const file of files) {
      let isZipFile = file.name.split('.').pop().toLowerCase() == 'zip'
      if (isZipFile) {
        var reader = new FileReader();
        reader.onload = function () {
          shp(this.result).then(function(geojson) {
            geojsonData = geojson;
          });
        };
        reader.readAsArrayBuffer(file);
      }
    }
  }

  // Download layers on map as geojson
  function downloadLayersAsGeojson(group = "", filename = "TMDU.geojson") {
    let geojson = {
      "type": "FeatureCollection",
      "name": "TMDU Map Data",
      "crs": { "type": "name", "properties": { "name": "urn:ogc:def:crs:EPSG::4326" } },
      "features": []
    };

    // Iterate the layers of the map
    let map = document.getElementById('map')._leaflet_map;
    map.eachLayer(function (layer) {
      // Skip layer is group and unable to convert to GeoJSON
      if (typeof layer.toGeoJSON !== 'function' || layer instanceof L.FeatureGroup || layer instanceof L.LayerGroup)
        return;
      // Skip this group
      if (layer.options.group == '河川➔PRTR事業所' || layer.groupname == '河川➔PRTR事業所')
        return;
      // If group blank then download all, otherwise download selected group
      if (group != "" && layer.options.group != group)
        return;
      
      let layerGeo = layer.toGeoJSON();
      layerGeo.properties = {};
      layerGeo.properties.ID = (layer.options.layerId) ? layer.options.layerId : "";
      layerGeo.properties.Name = (layer.options.layerId) ? layer.options.layerId : "";
      // ID_No
      let IdNo = (layer.options.layerId == null) ? NaN : Number(layer.options.layerId.substring(1));
      layerGeo.properties.ID_No = isNaN(IdNo) ? 0 : IdNo;
      // lat/lng
      if (typeof layer.getLatLng === 'function') {
        const latlng = layer.getLatLng();
        layerGeo.properties.POINT_X = latlng.lng;
        layerGeo.properties.POINT_Y = latlng.lat;
      }
      // Optional
      if (layer.options.label) layerGeo.properties.LABEL = layer.options.label;
      if (layer.options.group) layerGeo.properties.GROUP = layer.options.group;

      geojson.features.push(layerGeo);
    });
    
    // Create export button
    var element = document.createElement('a');
    element.setAttribute('href', 'data:text/json;charset=utf-8,' + encodeURIComponent(JSON.stringify(geojson)));
    element.setAttribute('download', filename);
    element.style.display = 'none';
    document.body.appendChild(element);
    element.click();
    document.body.removeChild(element);
  }

  function downloadRiverStationAsCsv(filename) {
    let group = 'g_circle_markers';
    let csvText = [
      ['ID','Name','ID_No','LABEL','POINT_X','POINT_Y']
    ];

    // Iterate the layers of the map
    let map = document.getElementById('map')._leaflet_map;
    map.eachLayer(function (layer) {
      // Skip layer is group and unable to convert to GeoJSON
      if (typeof layer.toGeoJSON !== 'function' || layer instanceof L.FeatureGroup || layer instanceof L.LayerGroup)
        return;
      // Skip this group
      if (layer.options.group == '河川➔PRTR事業所' || layer.groupname == '河川➔PRTR事業所')
        return;
      // If group blank then download all, otherwise download selected group
      if (group != "" && layer.options.group != group)
        return;
      
      let layerGeo = layer.toGeoJSON();
      let row = [];
      layerGeo.properties = {};
      layerGeo.properties.ID = (layer.options.layerId) ? layer.options.layerId : "";
      layerGeo.properties.Name = (layer.options.layerId) ? layer.options.layerId : "";
      // ID_No
      let IdNo = (layer.options.layerId == null) ? NaN : Number(layer.options.layerId.substring(1));
      layerGeo.properties.ID_No = isNaN(IdNo) ? 0 : IdNo;
      // lat/lng
      if (typeof layer.getLatLng === 'function') {
        const latlng = layer.getLatLng();
        layerGeo.properties.POINT_X = latlng.lng;
        layerGeo.properties.POINT_Y = latlng.lat;
      }
      // Optional
      if (layer.options.label) layerGeo.properties.LABEL = layer.options.label;
      if (layer.options.group) layerGeo.properties.GROUP = layer.options.group;

      row.push(layerGeo.properties.ID);
      row.push(layerGeo.properties.Name);
      row.push(layerGeo.properties.ID_No);
      row.push(layerGeo.properties.LABEL);
      row.push(layerGeo.properties.POINT_X);
      row.push(layerGeo.properties.POINT_Y);
      csvText.push(row);
    });
    
    // Create export button
    var element = document.createElement('a');
    element.setAttribute('href', 'data:text/csv;charset=utf-8,' + csvText.map(row => row.join(",")).join("\n"));
    element.setAttribute('download', filename);
    element.style.display = 'none';
    document.body.appendChild(element);
    element.click();
    document.body.removeChild(element);
  }

  // Add river circles to draw group
  function addCircleToDrawGroup() {
    let map = document.getElementById('map')._leaflet_map;
    if (!map) return;
    map.eachLayer(function (layer) {
      if (layer instanceof L.FeatureGroup == false &&
        layer instanceof L.LayerGroup == false &&
        layer.options.group == "g_circle_markers") {
          drawGroup.addLayer(layer);
      }
    });
  }

});
