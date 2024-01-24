const NA_VALUE = -999
const TO_VALUE = 0

// function buildHotlineDataBuffer1(geojsonData, data) {
//     let hotlineList = []

//     if (geojsonData) {
//         for (const feature of geojsonData.features) {
//             let hotlineData = [];
//             let value = getBufferValue(feature.geometry.coordinates, data.mapPoints, data.bufferDistance)
//             if (value == null || value == NaN || value == 'NaN') { value = NA_VALUE }
//             feature.geometry.coordinates.forEach(coordinate => {
//                 let hotlineItem = [coordinate[1], coordinate[0], parseFloat(value)];
//                 hotlineData.unshift(hotlineItem);
//             });
//             hotlineList = sortHotlineItem(hotlineList, hotlineData);
//         }
//     }
//     return hotlineList
// }
function buildHotlineDataTest(geojsonData) {
    let hotlineList = []
    let c = -1

    if (geojsonData) {
        for (const feature of geojsonData.features) {
            let hotlineData = [];
            feature.geometry.coordinates.forEach(coordinate => {
                c += 1
                let hotlineItem = [coordinate[1], coordinate[0], c];
                hotlineData.unshift(hotlineItem);
            })
            hotlineList = matchHotlines(JSON.parse(JSON.stringify(hotlineList)), hotlineData)
        }
    }

    return hotlineList
}

function buildHotlineDataBuffer(geojsonData, data) {
    let hotlineList = []
    if (geojsonData) {
        for (const feature of geojsonData.features) {
            let hotlineData = [];
            feature.geometry.coordinates.forEach((coordinate, index) => {
                const isSetNAValue = data.isTopDown && data.lat && coordinate[1] > data.lat
                let value = NA_VALUE
                if (!isSetNAValue) {
                    let nearestPoint = findNearestPoint(coordinate, data.mapPoints, data.bufferDistance)
                    value = nearestPoint ? nearestPoint[2] : null
                    if (value == null || value == NaN || value == 'NaN') { value = NA_VALUE }
                }
                let hotlineItem = [coordinate[1], coordinate[0], parseFloat(value)]
                hotlineData.unshift(hotlineItem);
            })

            hotlineList = matchHotlines(JSON.parse(JSON.stringify(hotlineList)), hotlineData)
        }
    }
    if (data.isTopDown) {
        hotlineList = hotlineList.map(lines => buildHotlineLines(lines, data.lat))
    } else {
        hotlineList = hotlineList.map(lines => buildHotlineLines(lines))
    }
    return hotlineList
}

function buildHotlineLines(lines, topLat) {
    lines = JSON.parse(JSON.stringify(lines))
    let preIndex = null
    let startNullIndex = null
    
    for (const [index, line] of lines.entries()) {
        if (line[2] !== NA_VALUE && startNullIndex === null) {
            startNullIndex = index
        }

        if (line[2] !== NA_VALUE) {
            if (preIndex !== null && index - 1 > preIndex) {
                const preVal = lines[preIndex][2]
                const currentVal = lines[index][2]
                const diff = currentVal - preVal
                const eachStep = diff / (index - preIndex)
                for (let i = 1; i < index - preIndex; i++) {
                    if (topLat && lines[preIndex + i][0] > topLat) {
                        lines[preIndex + i][2] = NA_VALUE
                    } else {
                        lines[preIndex + i][2] = preVal + eachStep * i
                    }
                }                
            }
            preIndex = index
        }
    }

    let endNullIndex = null
    const lineLength = lines.length

    for (let i = 0; i < lineLength; i++) {
        const line = lines[lineLength - i - 1]
        if (line[2] !== NA_VALUE && endNullIndex === null) {
            endNullIndex = lineLength - i
        }
    }
    // if (startNullIndex !== null && startNullIndex > 1 && startNullIndex < lineLength - 1) {
    //     let eachStep = (lines[startNullIndex + 1][2] - TO_VALUE) / startNullIndex
    //     for (let i = 0; i < startNullIndex - 1; i++) {
    //         if (topLat && lines[i + 1][0] > topLat) {
    //             lines[i + 1][2] = NA_VALUE
    //         } else {
    //             lines[i + 1][2] = TO_VALUE + eachStep * i
    //         }
    //     }
    // }
    // if (endNullIndex !== null && endNullIndex < lineLength - 2 && endNullIndex > 0) {
    //     const startVal = lines[endNullIndex - 1][2]
    //     let eachStep = (startVal - TO_VALUE) / (lineLength - endNullIndex - 1)
    //     for (let i = endNullIndex; i < lineLength - 1; i++) {
    //         if (topLat && lines[i][0] > topLat) {
    //             lines[i][2] = NA_VALUE
    //         } else {
    //             lines[i][2] = startVal - eachStep*(i + 1 - endNullIndex)
    //         }
    //     }
    // }

    return lines
}

function buildHotlineDataNA(geojsonData) {
    let hotlineList = []
    if (geojsonData) {
        for (const feature of geojsonData.features) {
            let hotlineData = [];
            feature.geometry.coordinates.forEach(coordinate => {
                let hotlineItem = [coordinate[1], coordinate[0], NA_VALUE];
                hotlineData.unshift(hotlineItem);
            });
            hotlineList.push(hotlineData)
        }
    }
    return hotlineList
}

function buildGradientValueHotline(hotlineList) {
    let gradientHotlinesValue = []
    for (const hotlines of hotlineList) {
        let gradient = hotlines
        let groupValue = groupHotlineByValue(hotlines)
        if (groupValue.length > 1) {
            for (let i = 0; i < groupValue.length - 1; i++) {
                gradientValueHotlines(groupValue[i], groupValue[i + 1])
            }
            gradient = groupValue.flat()
        }
        gradientHotlinesValue.push(gradient)
    }
    return gradientHotlinesValue
}

function matchHotlines(hotlineList, hotlineData) {
    if (hotlineData.length > 0) {
        let startPoint = hotlineData[0];
        let endPoint = hotlineData[hotlineData.length - 1]
        let matchedStart = checkPointMatch(hotlineList, startPoint, 'end')
        let matchedEnd = checkPointMatch(hotlineList, endPoint, 'start')

        if (matchedStart && !matchedEnd) {
            hotlineList[matchedStart[1]] = [...hotlineList[matchedStart[1]], ...hotlineData]
        } else if (matchedEnd && !matchedStart) {
            hotlineList[matchedEnd[1]].unshift(...hotlineData)
        } else if (matchedStart && matchedEnd) {
            hotlineData = [...hotlineData, ...hotlineList[matchedEnd[1]]]
            hotlineList[matchedStart[1]] = [...hotlineList[matchedStart[1]], ...hotlineData]
            hotlineList.splice(matchedEnd[1], 1)
        } else {
            hotlineList.push(hotlineData)
        }
    }
    return hotlineList
}


function groupHotlineByValue(hotlines) {
    hotlines = JSON.parse(JSON.stringify(hotlines))
    let group = []
    let trackValue = hotlines[0][2]
    for (const [i, point] of hotlines.entries()) {
        if (i == 0) {
            group[0] = [point]
            continue
        }
        if (point[2] != trackValue) {
            trackValue = point[2]
            group[group.length] = [point]
        } else {
            group[group.length - 1].push(point)
        }
    }
    return group
}

function gradientValueHotlines(hotline1, hotline2) {
    if (hotline1.length && hotline2.length) {
        let l1 = hotline1.length
        let l2 = hotline2.length
        if (l1 > 1 && l2 > 1) {
            let halfIndex1 = l1 !== 2 ? Math.floor(l1 / 2) + 1 : 1
            let halfIndex2 = l2 !== 2 ? Math.floor(l2 / 2) - 1 : 1
            const value1 = hotline1[halfIndex1][2]
            const value2 = hotline2[halfIndex2][2]
            if (value1 !== NA_VALUE && value2 !== NA_VALUE) {
                const L = (l1 - halfIndex1) + (halfIndex2 + 1)
                const step = (value2 - value1) / L
                if (step !== 0) {
                    let count = 1
        
                    while (halfIndex1 < l1) {
                        hotline1[halfIndex1][2] = value1 + step * count
                        halfIndex1 += 1
                        count += 1
                    }
                    count = 1
                    while (halfIndex2 >= 0) {
                        hotline2[halfIndex2][2] = value2 - step * count
                        halfIndex2 -= 1
                        count += 1
                    }
                }
            }  
        }
    }
}

function getBufferValue(coordinates, mapPoints, bufferDistance) {
    let minDistance = Number.POSITIVE_INFINITY
    let value = null

    for (const circle of mapPoints) {
        for (const point of coordinates) {
            const distance = findDistance(circle.slice(0, 2), point) // KM
            const isBufferPoint = distance < minDistance && (distance * 1000) <= bufferDistance
            if (isBufferPoint && circle[2] !== null) {
                minDistance = distance
                value = circle[2]
            }
        }
    }
    return value
}

function pDistance(M, A, B) {
    const MA = findDistance(M, A) * 1000
    const MB = findDistance(M, B) * 1000
    const AB = findDistance(A, B) * 1000
    const P = (MA + MB + AB) / 2
    const S = Math.sqrt((P * (P - MA) * (P - MB) * (P - AB)))
    const G1 = Math.asin(S * 2 / (MA * MB)) * 180 / Math.PI
    const G2 = Math.asin(S * 2 / (MA * AB)) * 180 / Math.PI
    const G3 = 180 - G1 - G2
    const collectionSides = [
        { name: 'MA', value: MA },
        { name: 'MB', value: MB },
        { name: 'AB', value: AB },
    ].sort((a, b) =>  a.value - b.value)
    const indexOfAB = collectionSides.findIndex(o => o.name === 'AB')
    const collectionG = [G1, G2, G3].sort((a, b) => a - b)
    for (let i = 0; i < collectionG.length; i++) {
        if (i !== indexOfAB && collectionG[i] > 90) return null
    }
    return S * 2 / AB
}

function findNearestPoint2(coordinate1, coordinate2, mapPoints, bufferDistance) {
    let minDistance = Number.POSITIVE_INFINITY
    let point = null

    for (const mp of mapPoints) {
        const distance = pDistance(mp.slice(0, 2), coordinate1, coordinate2) // m
        const isBufferPoint = distance < minDistance && distance<= bufferDistance
        if (isBufferPoint) {
            minDistance = distance
            point = mp
        }
    }
    return point
}

function findNearestPoint(coordinate, mapPoints, bufferDistance) {
    let minDistance = Number.POSITIVE_INFINITY
    let point = null

    for (const mp of mapPoints) {
        const distance = findDistance(mp.slice(0, 2), coordinate)
        const isBufferPoint = distance < minDistance && (distance * 1000) <= bufferDistance
        if (isBufferPoint) {
            minDistance = distance
            point = mp
        }
    }
    return point
}

function sortHotlineItem(hotlineList, hotlineData) {
    if (hotlineData.length > 0) {
        let startPoint = hotlineData[0];
        let endPoint = hotlineData[hotlineData.length - 1]
        let matchStart = checkPointMatch(hotlineList, startPoint, 'end')
        let matchEnd = checkPointMatch(hotlineList, endPoint, 'start')
        if (matchStart && !matchEnd) {
            hotlineList[matchStart[1]] = updateHotLineDataStart(hotlineList[matchStart[1]], hotlineData)
        } else if (matchEnd && !matchStart) {
            hotlineList[matchEnd[1]] = updateHotLineDataEnd(hotlineList[matchEnd[1]], hotlineData)
        } else if (matchStart && matchEnd) {
            hotlineData = updateHotLineDataStart(hotlineData, hotlineList[matchEnd[1]])
            hotlineList[matchStart[1]] = updateHotLineDataStart(hotlineList[matchStart[1]], hotlineData)
            hotlineList.splice(matchEnd[1], 1)
        } else {
            hotlineList.push(hotlineData)
        }
    }
    return hotlineList
}

function checkPointMatch(data, point, position='end') {
    if (point) {
        for (const [i, line] of data.entries()) {
            if (line.length > 0) {
                let index = position == 'start' ? 0 : line.length - 1;
                let isMatch = line[index][0] == point[0] && line[index][1] == point[1]
                if (isMatch) {
                    return [position, i]
                }
            }
        }
    }
    return false;
}

// Insert a feature to the begin of features
function updateHotLineDataStart(currentHotlineData, insertHotlineData) {
    const startPoint = currentHotlineData[currentHotlineData.length - 1];
    insertHotlineData = updateHotLineData(insertHotlineData, startPoint[2])
    currentHotlineData.push(...insertHotlineData)
    return currentHotlineData
}

// Insert a feature to the end of features
function updateHotLineDataEnd(currentHotlineData, insertHotlineData) {
    const insertHotlineDataValue = insertHotlineData[insertHotlineData.length - 1][2];
    const startPointValue = currentHotlineData[0][2];
    var changePoints = [];

    for (let i = 0; i < currentHotlineData.length; i++) {
        let point = currentHotlineData[i];
        if (point[2] == startPointValue) {
            changePoints.push(point);
        } else {
            break;
        }
    }
    changePoints = updateHotLineData(changePoints, insertHotlineDataValue)
    changePoints.forEach((point, index) => {
        currentHotlineData[index] = point;
    })
    currentHotlineData.unshift(...insertHotlineData);
    return currentHotlineData
}

function updateHotLineData(insertHotlineData, startPointValue) {
    if (startPointValue != NA_VALUE) {
        let distanceData = getDistanceData(insertHotlineData, startPointValue)
        insertHotlineData.forEach((point, index) => {
            point[2] = distanceData[index]
        })
    }
    return insertHotlineData
}

function getDistanceData(hotlineData, startValue) {
    const currentValue = hotlineData[0][2];
    var distanceStartToEnd = 0;
    var distances = [];
    for (let i = 0; i < hotlineData.length - 1; i++) {
        let startPoint = hotlineData[i];
        let endPoint = hotlineData[i + 1];
        let distance = findDistance(startPoint, endPoint);
        distanceStartToEnd += distance
        distances.push(distanceStartToEnd);
    }
    distances.unshift(0);
    distances = distances.map(distance => {
        const percent = distance / distanceStartToEnd;
        return startValue + (currentValue - startValue) * percent;
    })
    return distances;
}

function findDistance(origin, destination) {
    const lat1 = origin[1],
        lon1 = origin[0],
        lat2 = destination[1],
        lon2 = destination[0]
    const R = 6371; // KM
    const φ1 = lat1 * Math.PI/180; // φ, λ in radians
    const φ2 = lat2 * Math.PI/180;
    const Δφ = (lat2-lat1) * Math.PI/180;
    const Δλ = (lon2-lon1) * Math.PI/180;

    const a = Math.sin(Δφ/2) * Math.sin(Δφ/2) +
                Math.cos(φ1) * Math.cos(φ2) *
                Math.sin(Δλ/2) * Math.sin(Δλ/2);
    const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));
    const d = R * c; // in KM
    return d
}
