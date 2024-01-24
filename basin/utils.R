######################################################################
#'
#' UTILS FUNCTIONS
#'
######################################################################

getIndexColor <- function(value, min, max) {
    if (is.na(value)) return(-1)
    if (value < min || value > max) return(0)

    parts <- 5
    index <- -1
    startValue <- min
    delta <- (max - min) / (parts - 1)

    while (startValue <= value) {
        index = index + 1
        startValue = startValue + delta
    }

    return(index)
}

getColor <- function(index) {
    if (index == -1) return('#808080')
    return(toString(rainbowPallet[index + 1]))
}

getResults <- function(dates, dateInput, step) {
    start <- as.Date(dateInput)
    dates <- as.Date(dates)
    dates <- unique(dates)
    minDate <- min(dates)
    maxDate <- max(dates)

    if (start < minDate || start > maxDate) {
        start = minDate
    }
    results <- list(
        dates = dates,
        start = start,
        min = minDate,
        max = maxDate,
        step = step
    )
    return(results)
}
