$(function () {   
    /*--------------------------------
        GLOBAL VARIABLES
    ---------------------------------*/
    var _grid = null
    var _gridCoordinates = null
    var _gridBounds = null
    var _gwSliceData = []
    var _timeStep = 0
    var _heatLayer = null
    var _heatLayer2 = null
    var _timeIntervalContour = null
    var _contourSettings = null
    
    // Disable pp-option
    $('.pp-option-value.disabled').each((_, e) => { $(e).find('input').prop('disabled', true) })

    // Create focus button
    $("#sourceLatlng").after(`<div class="input-group-btn" style="vertical-align: bottom;"><button class="btn btn-default" onclick="setGwFocusOn('sourceLatlng')"><span class="glyphicon glyphicon-map-marker"></span></button></div>`)
    $("#sourceLatlng").closest(".form-group").addClass("input-group")
    $("#direction").after(`<div class="input-group-btn" style="vertical-align: bottom;"><button class="btn btn-default" onclick="setGwFocusOn('direction')"><span class="glyphicon glyphicon-arrow-right"></span></button></div>`)
    $("#direction").closest(".form-group").addClass("input-group")

    /*--------------------------------
        CLIENT EVENTS
    ---------------------------------*/    
    $("#sourceLatlng").focus(function () {
        setGwFocusOn('sourceLatlng')
    })
    $("#direction").focus(function () {
        setGwFocusOn('direction')
    })
    $('input').focus(function () {
        if (!['sourceLatlng', 'direction'].includes($(this).attr('id'))) {
            setGwFocusOn('')
        }
    })
    
    // Update destination location
    $("#destinationLatlng").change(function () {
        if ($("#ignoreAltitude").is(":checked") == false || // Not in user-pick mode
            $('#sourceLatlng').val() == '' ||               // Blank Source point
            $(this).val() == '')                            // Blank Target point
            return;

        const sourceLocation = $('#sourceLatlng').val().split(' - ');
        const targetLocation = $(this).val().split(' - ');
        const fromPoint = turf.point([parseFloat(sourceLocation[1]), parseFloat(sourceLocation[0])]);
        const toPoint = turf.point([parseFloat(targetLocation[1]), parseFloat(targetLocation[0])]);
        let direction = turf.bearing(fromPoint, toPoint);
        $('#direction').val(direction);
        $('#direction').trigger('change');
    })

    $(document).on('click', '.display-icon i', function () {
        const isPlay = $('#isPlayRiver').prop('checked')
        $('#isPlayRiver').prop('checked', !isPlay)
        $('#isPlayRiver').trigger('change')
    })

    $(document).on('change', '#isPlayRiver', function () {
        let el = $(this)
        if (el.prop('checked')) {
            setGwFocusOn('')
            _timeIntervalContour = setInterval(function () {
                _timeStep += 1
                drawContour()
            }, 1000)
        } else {
            clearInterval(_timeIntervalContour) 
        }
    })

    /*--------------------------------
        SERVER EVENTS
    ---------------------------------*/
    
    // Add json data to Map
    Shiny.addCustomMessageHandler('onDrawGrid', handlerDrawGrid)
    function handlerDrawGrid(data) {
        initialTimePlayer()
        
        let { from, direction: bearing, distance, noX, noY, settings } = data
        const distanceKM = distance / 1000
        settings = settings || {}

        const polylineOptions = {
            color: settings.gridColor || '#1B5EDB',
            weight: settings.gridWeight || 2,
        }
        const arrowOptions = {
            weight: settings.arrowWeight || 4,
            color: settings.arrowColor || '#fc0f03',
            opacity: settings.arrowOpacity || 0.8,
        }
        const arrowLength = settings.arrowLength || 0.2
        const soruceLocationOptions = {
            radius: settings.sourceLocationRadius || 20,
            color: settings.sourceLocationColor || '#fc0f03',
            opacity: settings.sourceLocationOpacity || 0.8,
        }
        
        if (from) {
            let map = document.getElementById('map') ? document.getElementById('map')._leaflet_map : null;
            if (map) {
                const fromPoint = turf.point([from.lng, from.lat])
                let point1 = turf.destination(fromPoint, distanceKM, bearing - 45, {units: 'miles'}).geometry.coordinates
                let point2 = turf.destination(fromPoint, distanceKM * Math.sqrt(2), bearing, {units: 'miles'}).geometry.coordinates
                let point3 = turf.destination(fromPoint, distanceKM, bearing + 45, {units: 'miles'}).geometry.coordinates
                let groupGrid = []
                let polylines1 = [[from.lat, from.lng], [point1[1], point1[0]], [point2[1], point2[0]], [point3[1], point3[0]], [from.lat, from.lng]]
                groupGrid.push(L.polyline(polylines1, polylineOptions))
                for (let i = 1; i < noX; i++) {
                    let pfrom1 = turf.destination(fromPoint, distanceKM / noX * i, bearing - 45, {units: 'miles'}).geometry.coordinates
                    let pto1 = turf.destination(turf.point(pfrom1), distanceKM, bearing + 45, {units: 'miles'}).geometry.coordinates
                    let pl1 = [[pfrom1[1], pfrom1[0]], [pto1[1], pto1[0]]]
                    groupGrid.push(L.polyline(pl1, polylineOptions))
                }
                for (let i = 1; i < noY; i++) {
                    let pfrom2 = turf.destination(fromPoint, distanceKM / noY * i, bearing + 45, {units: 'miles'}).geometry.coordinates
                    let pto2 = turf.destination(turf.point(pfrom2), distanceKM, bearing - 45, {units: 'miles'}).geometry.coordinates
                    let pl2 = [[pfrom2[1], pfrom2[0]], [pto2[1], pto2[0]]]
                    groupGrid.push(L.polyline(pl2, polylineOptions))
                }

                const pointArrowHead = turf.destination(fromPoint, distanceKM * Math.sqrt(2) * arrowLength, bearing, {units: 'miles'}).geometry.coordinates
                const pointArrowHeadPoint = turf.point(pointArrowHead)
                const pointArrowHeadLeft = turf.destination(pointArrowHeadPoint, distanceKM * Math.sqrt(2) * arrowLength / 4, -180 + bearing - 30, {units: 'miles'}).geometry.coordinates
                const pointArrowHeadRight = turf.destination(pointArrowHeadPoint, distanceKM * Math.sqrt(2) * arrowLength / 4, -180 + bearing + 30, {units: 'miles'}).geometry.coordinates

                groupGrid.push(L.polyline([[from.lat, from.lng], [pointArrowHead[1], pointArrowHead[0]]], arrowOptions))
                groupGrid.push(L.polyline([[pointArrowHead[1], pointArrowHead[0]], [pointArrowHeadLeft[1], pointArrowHeadLeft[0]]], arrowOptions))
                groupGrid.push(L.polyline([[pointArrowHead[1], pointArrowHead[0]], [pointArrowHeadRight[1], pointArrowHeadRight[0]]], arrowOptions))
                groupGrid.push(L.circle([from.lat, from.lng], {
                    radius: soruceLocationOptions.radius,
                    fillColor: soruceLocationOptions.color,
                    color: soruceLocationOptions.color,
                    fillOpacity: soruceLocationOptions.opacity,
                    opacity: soruceLocationOptions.opacity,
                }))

                removeGrid()
                _grid = L.layerGroup(groupGrid).addTo(map)
                _gridBounds = [
                    L.latLng([from.lat, from.lng]),
                    L.latLng(point3.reverse()),
                    L.latLng(point1.reverse()),
                ]
            }
        }
    }

    Shiny.addCustomMessageHandler('onUpdateGwFocusOn', handlerUpdateGwFocusOn)
    function handlerUpdateGwFocusOn(value) {
        setGwFocusOn(value)
    }

    Shiny.addCustomMessageHandler('onUpdateGridCoordinates', handlerUpdateGridCoordinates)
    function handlerUpdateGridCoordinates(data) {
        _gridCoordinates = []
        const { from, direction: bearing, distance, noX, noY } = data
        const distanceKM = distance / 1000

        if (from) {
            const originPoint = turf.destination(turf.point([from.lng, from.lat]), Math.sqrt(Math.pow(distanceKM / noX, 2) + Math.pow(distanceKM / noY, 2)) / 2, bearing, {units: 'miles'}).geometry.coordinates
            for (let i = 0; i < noX; i++) {
                _gridCoordinates[i] = []
                const nextXPoint = turf.destination(originPoint, distanceKM / noX * i, bearing - 45, {units: 'miles'}).geometry.coordinates
                for (let j = 0; j < noY; j++) {
                    _gridCoordinates[i].push(turf.destination(nextXPoint, distanceKM / noY * j, bearing + 45, {units: 'miles'}).geometry.coordinates)
                }
            }
        }
    }

    Shiny.addCustomMessageHandler('onUpdateContour', handlerUpdateContour)
    function handlerUpdateContour(data) {
        setGwFocusOn('')
        if (_gridCoordinates.length) {
            const { data: contourData, ui, settings, redraw } = data
            if (!redraw) {
                _gwSliceData = []
                for (let z = 0; z < ui.noZ; z++) {
                    _gwSliceData[z] = []
                    for (const cdata of contourData) {
                        let cData = []
                        for (const [i, xd] of cdata.entries()) {
                            cData.push(...xd.map((yd, j) => [_gridCoordinates[i][j][1], _gridCoordinates[i][j][0], yd[z]]))
                        }
                        _gwSliceData[z].push(cData)
                    }
                }
            }
            if (settings.gridTransparency) {
                removeGrid()
            }
            updateContourSettings(ui, settings)
            drawContour()
        }
    }

    Shiny.addCustomMessageHandler('onCalculateAT123', handlerCalculateAT123)
    function handlerCalculateAT123(_) {
        initialTimePlayer()
    }

    Shiny.addCustomMessageHandler('onRemoveContour', handleRemoveContour)
    function handleRemoveContour(_) {
        removeGrid()
        clearHeatMap()
    }

    function removeGrid() {
        try {
            _grid.remove()
            _grid = null
        } catch (error) {}
    }

    function drawContour() {
        let map = document.getElementById('map') ? document.getElementById('map')._leaflet_map : null;
        const _timestepsData = _gwSliceData[$('#sliceContour').val() - 1]

        if (_timestepsData && map) {
            if (_timeStep >= _timestepsData.length) {
                _timeStep = 0
            }
            addImageContour(_timestepsData[_timeStep])
        } else {
            $('#isPlayRiver').prop('checked', false)
            $('#isPlayRiver').trigger('change')
        }
    }

    function addImageContour(data) {
        const canvas = document.getElementById('container-contour'),
            map = document.getElementById('map')._leaflet_map,
            context = canvas.getContext('2d'),
            projection = d3.geoIdentity().scale(canvas.height / _contourSettings.noY),
            path = d3.geoPath(projection, context),
            dataContourMap = data.map(o => o[2])
        
        context.clearRect(0, 0, canvas.width, canvas.height)
        var cntrs = _contourSettings.contours(dataContourMap)
        for (const contour of cntrs) {
            context.beginPath()
            context.fillStyle = _contourSettings.scaleColor(contour.value)
            path(contour)
            context.fill()
        }
        if (_heatLayer) {
            _heatLayer2 = L.imageOverlay.rotated(canvas.toDataURL(), ..._gridBounds, {
                opacity: _contourSettings.opacity,
            }).addTo(map);
            setTimeout(() => {
                removeLayer(_heatLayer)
                _heatLayer = null
            }, 50)
        } else {
            _heatLayer = L.imageOverlay.rotated(canvas.toDataURL(), ..._gridBounds, {
                opacity: _contourSettings.opacity,
            }).addTo(map);
            setTimeout(() => {
                removeLayer(_heatLayer2)
                _heatLayer2 = null
            }, 50)
        }
    }

    function removeLayer(layer) {
        try {
            layer.remove()
        } catch (e) {}
    }

    function updateContourSettings(ui, settings) {
        _contourSettings = {}
        let minValue = Infinity
        let maxValue = -Infinity
        const _timestepsData = _gwSliceData[$('#sliceContour').val() - 1]
        if (_timestepsData) {

            for (const data of _timestepsData) {
                let min = Math.min(...data.map(o => o[2]))
                let max = Math.max(...data.map(o => o[2]))
                if (min < minValue) {
                    minValue = min
                }
                if (max > maxValue) {
                    maxValue = max
                }
            }
            const steps = (settings || {}).contourSmoothAuto ? Math.round((maxValue - minValue) / 7) : (settings || {}).contourSmooth

            _contourSettings.noX = ui.noX
            _contourSettings.noY = ui.noY
            _contourSettings.colors = ui.colors
            _contourSettings.scaleColor = chroma.scale(_contourSettings.colors).domain([minValue, maxValue])
            _contourSettings.summary = { min: minValue, max: maxValue }
            _contourSettings.contours = d3.contours().size([ui.noX, ui.noY]).thresholds(d3.range(minValue, maxValue, steps))
            _contourSettings.opacity = (settings || {}).contourOpacity || 0.9
        }
    }

    function clearHeatMap() {
        removeLayer(_heatLayer)
        removeLayer(_heatLayer2)
        _heatLayer = null
        _heatLayer2 = null
    }

    function initialTimePlayer() {
        clearHeatMap()
        _timeStep = 0
        stopPlayer()
    }

    function stopPlayer(_) {
        $('#isPlayRiver').prop('checked', false)
        $('#isPlayRiver').trigger('change')
    }
})

function setGwFocusOn(value) {
    $('#gwFocusOn').val(value)
    $('#gwFocusOn').trigger('keyup')
    $("#sourceLatlng").removeClass('is-focus')
    $("#direction").removeClass('is-focus')
    if (value !== '') {
        $(`#${value}`).addClass('is-focus')
    }
}
