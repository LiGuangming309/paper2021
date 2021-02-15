#https://docs.openaq.org/#/
#https://github.com/ropensci/ropenaq/tree/master/R
#"RCurl"

base_url <- "https://api.openaq.org/v2/"

client <- crul::HttpClient$new(url = paste0(base_url,"locations"))
status <- client$retry("get")
status$status_code

argsList <- list(country_id = c("US")#,
                 #parameter_id = 2
                 )

#argsList <- Filter(Negate(is.null), argsList)
#argsList <- argsList[argsList != ""]

res <- client$retry(
  verb = "get",
  query = argsList,
  pause_base = 1,
  pause_cap = 60,
  pause_min = 1,
  times = 5,
  terminate_on = NULL,
  retry_only_on = NULL,
  limit = 1000#,
  #onwait = onwait
)


treat_res <- function(res){
  contentPage <- suppressMessages(res$parse(encoding = "UTF-8"))
  # parse the data
  output <- jsonlite::fromJSON(contentPage)
  
  coordinates <- output$results$coordinates
  date <- output$results$date
  averagingPeriod <- output$results$averagingPeriod
  
  if(!is.null(date)){
    date <- dplyr::rename(date, date.utc = .data$utc)
    if (!is.null(date[["local"]])) {
      date <- dplyr::rename(date, date.local = .data$local)
    }
  }
  if(!is.null(averagingPeriod)){
    
    averagingPeriod <- dplyr::rename(
      averagingPeriod,
      averagingPeriod.unit = .data$unit,
      averagingPeriod.value = .data$value
    )
    
  }
  results <- output$results
  
  if("averagingPeriod" %in% names(results)){
    results <- dplyr::select(results, - .data$averagingPeriod)
  }
  if("coordinates" %in% names(results)){
    results <- dplyr::select(results, - .data$coordinates)
  }
  if("date" %in% names(results)){
    results <- dplyr::select(results, - .data$date)
  }
  results <- dplyr::bind_cols(results, coordinates)
  results <- dplyr::bind_cols(results, date)
  results <- dplyr::bind_cols(results, averagingPeriod)
  
  results <- dplyr::as_tibble(results)
  
  
  # get the meta
  meta <- dplyr::as_tibble(
    as.data.frame(output$meta))
  #get the time stamps
  timestamp <- dplyr::as_tibble(data.frame(
    queriedAt = func_date_headers(res$response_headers$date))) 
  
  attr(results, "meta") <- meta
  attr(results, "timestamp") <- timestamp
  attr(results, "url") <- res$url
  return(results)
}

# dates abbreviation
func_date_headers <- function(date){
  date <- strsplit(date, ",")[[1]][2]
  date <- gsub("Jan", "01", date)
  date <- gsub("Feb", "02", date)
  date <- gsub("Mar", "03", date)
  date <- gsub("Apr", "04", date)
  date <- gsub("May", "05", date)
  date <- gsub("Jun", "06", date)
  date <- gsub("Jul", "07", date)
  date <- gsub("Aug", "08", date)
  date <- gsub("Sep", "09", date)
  date <- gsub("Oct", "10", date)
  date <- gsub("Nov", "11", date)
  date <- gsub("Dec", "12", date)
  lubridate::dmy_hms(date, tz = "GMT")
}

results <- treat_res(res)
