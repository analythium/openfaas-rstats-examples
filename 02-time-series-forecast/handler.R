library(forecast)

covid_forecast <- function(region, cases="confirmed", window=14) {
    ## check arguments
    cases <- match.arg(cases, c("confirmed", "deaths"))
    window <- round(window)
    if (window < 1)
        stop("window must be > 0")
    ## API endpoint for region in global data set
    ## available values: https://hub.analythium.io/covid-19/api/v1/regions/
    u <- paste0("https://hub.analythium.io/covid-19/api/v1/regions/", region)
    x <- jsonlite::fromJSON(u) # will throw error if region is not found
    ## time series: daily new cases
    y <- pmax(0, diff(x$rawdata[[cases]]))
    ## last date
    l <- as.Date(x$rawdata$date[length(x$rawdata$date)])
    ## fit ETS
    m <- ets(y)
    ## forecaset based on model and window
    f <- forecast(m, h=window)
    ## process forecast
    p <- cbind(Date=seq(l+1, l+window, 1), as.data.frame(f))
    p[p < 0] <- 0
    as.list(p)
}

#* COVID
#* @get /
function(region, cases, window) {
  if (missing(cases))
    cases <- "confirmed"
  if (missing(window))
    window <- 14
  covid_forecast(region, cases, as.numeric(window))
}
