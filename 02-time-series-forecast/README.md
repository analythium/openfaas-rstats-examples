# Time Series Forecast

> This examples was presented in the OpenFaaS blog: ["Functions for data science with R templates for OpenFaaS"](https://www.openfaas.com/blog/r-templates/)

[Exponential smoothing](https://en.wikipedia.org/wiki/Exponential_smoothing) is a forecasting method.

We will create a function that pulls JSON data from public [COVID-19 data API](https://github.com/analythium/covid-19#readme), fits exponential smoothing model and returns the forecast results as JSON.

> You will learn how to consume data from external APIs, fit exponential smoothing model, and forecast, use the request body, URL parameters, or dynamic routing. We'll also learn about API annotation and HTTP statuses.

You'll need the prerequisites listed [here](https://github.com/analythium/openfaas-rstats-templates/tree/master/examples).

- [Time Series Forecast](#time-series-forecast)
  - [Create a new function using a template](#create-a-new-function-using-a-template)
  - [Customize the function](#customize-the-function)
  - [Build, push, deploy the function](#build-push-deploy-the-function)
  - [Testing](#testing)
  - [Use URL parameters](#use-url-parameters)
  - [Use dynamic routing](#use-dynamic-routing)
  - [Use r-minimal base image](#use-r-minimal-base-image)

## Create a new function using a template

Create a new function called `r-covid`.

```bash
faas-cli new --lang rstats-base-plumber r-covid
```

## Customize the function

Edit the `./r-covid/DESCRIPTION` file:

```yaml
Package: OpenFaaStR
Version: 0.0.1
Imports:
  forecast
Remotes:
SystemRequirements:
VersionedPackages:
```

Change the `./r-covid/handler.R` file:

```R
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
#* @post /
function(req) {
  x <- jsonlite::fromJSON(paste(req$postBody))
  if (is.null(x$cases))
    x$cases <- "confirmed"
  if (is.null(x$window))
    x$window <- 14
  covid_forecast(x$region, x$cases, x$window)
}
```

Note: if we want to make some of the arguments optional, we need to evaluating if those are `NULL` or not and set appropriate defaults (see how using URL parameters would simplify this below). We expect the properties to be arrays, therefore we did not specify the serializer and thus we use the default (boxed JSON) serializer of Plumber.

Edit the `r-covid.yml` file as required, see [configuration](https://docs.openfaas.com/reference/yaml/) options.

## Build, push, deploy the function

`faas-cli up` is a [shorthand](https://docs.openfaas.com/cli/templates/)
for automating `faas-cli build`, `faas-cli push`, and `faas-cli deploy`.

```bash
faas-cli up -f r-covid.yml
```

## Testing

The key-value pairs we have to pass are as follows:

- region: a region slug value for the API endpoint in global data set (see [available values](https://hub.analythium.io/covid-19/api/v1/regions/)),
- cases: one of `"confirmed"` or `"deaths"`,
- windows: a positive integer giving the forecast horizon in days.

Test the Docker image locally after `docker run -p 5000:8080 $OPENFAAS_PREFIX/r-covid`:

```bash
curl http://localhost:5000/ -H \
  "Content-Type: application/json" -d \
  '{"region": "canada-combined", "cases": "confirmed", "window": 4}'
```

Test the deployed instance:

```bash
curl $OPENFAAS_URL/function/r-covid -H \
  "Content-Type: application/json" -d \
  '{"region": "canada-combined", "cases": "confirmed", "window": 4}'
```

The output should include the 4-day forecast including 80% and 95% forecast intervals
(the exact results change daily):

```bash
{
  "Date": ["2021-01-16", "2021-01-17", "2021-01-18", "2021-01-19"],
  "Point Forecast": [7954.9403, 7995.8122, 8036.6842, 8077.5561],
  "Lo 80": [6782.2353, 6815.0554, 6847.1382, 6878.4625],
  "Hi 80": [9127.6454, 9176.569, 9226.2301, 9276.6497],
  "Lo 95": [6161.4427, 6190.0005, 6217.4306, 6243.7007],
  "Hi 95": [9748.4379, 9801.6239, 9855.9377, 9911.4114]
}
```

## Use URL parameters

Edit the `./r-covid/handler.R` file:

```R
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
```

This will use a parameter from the URL instead of parsing the request body. Specifying default values as part of the handle function arguments simplifies making some URL parameters optional (need to treat missing parameters as `missing()`). However, URL form encoded parameters will be of character type, thus checking type and making appropriate type conversions is necessary.

Note: we have changed the function argument (from `req` to the arguments of the `covid_forecast` function) and the HTTP request method from `@post` to `@get`.

Build, push, deploy the function:

```bash
faas-cli up -f r-covid.yml
```

Test the Docker image locally after `docker run -p 5000:8080 $OPENFAAS_PREFIX/r-covid`:

```bash
curl -X GET -G \
  localhost:5000 \
  -d region=canada-combined \
  -d cases=confirmed \
  -d window=4
```

Test the deployed instance:

```bash
curl -X GET -G \
  $OPENFAAS_URL/function/r-covid \
  -d region=canada-combined
```

The output should still be same as above.

Note: only the `region` parameter is mandatory, the the other two defaults to
`cases="confirmed"` and `window=14`.
`OPENFAAS_URL/function/r-covid?region=us` will be the same as
`OPENFAAS_URL/function/r-covid?region=us&window=14`.

## Use dynamic routing

Edit the `./r-covid/handler.R` file. Note: we also include better error handling. `stop()` messages only show up in logs. HTTP response status code and message will be displayed by the browser and aid troubleshooting on the client side. We also use more [annotations](https://www.rplumber.io/articles/annotations.html) to aid the OpenAPI info returned on the `/__docs__/` endpoint (see below). Dynamic routing for Plumber is explained [here](https://www.rplumber.io/articles/routing-and-input.html#dynamic-routes-1).

```R
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
#* @param region:string region name
#* @param cases:string cases (confirmed or deaths)
#* @param window:integer forecasting horizon in days
#* @get /<region>/<cases>/<window>
function(region, cases, window, res) {
  if (missing(cases))
    cases <- "confirmed"
  if (missing(window))
    window <- 14
  out <- try(covid_forecast(region, cases, as.numeric(window)))
  if (inherits(out, "try-error")) {
    res$status <- 400 # Bad Request
    return(list(error=as.character(out)))
  }
  out
}
```

Build, push, deploy the function:

```bash
faas-cli up -f r-covid.yml
```

Test the Docker image locally after `docker run -p 5000:8080 $OPENFAAS_PREFIX/r-covid`,
then visit `http://localhost:5000/us/deaths/4`

Test the deployed instance: `OPENFAAS_URL/function/r-covid/us/deaths/4`.

Check out and explore the OpenAPI documentation at `OPENFAAS_URL/function/r-covid/__docs__`.

Note: all parameters are required with dynamic routing.

## Use r-minimal base image

Using the r-minimal base image to build the forecasting function halves the image size.

The handler is the same, this is how the `DESCRIPTION` file looks like:

```yaml
Package: OpenFaaStR
Version: 0.0.1
Imports:
  forecast
Remotes:
SystemRequirements:
  gcc,
  musl-dev,
  g++,
  gfortran,
  linux-headers
VersionedPackages:
```
