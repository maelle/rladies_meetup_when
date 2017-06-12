# code for getting meetups from Lucy
doc.raw <- RCurl::getURL("https://raw.githubusercontent.com/rladies/starter-kit/master/Current-Chapters.md")
meetups <- stringr::str_match_all(doc.raw, "www.meetup.com/(.*?)/")[[1]][,2]
meetups <- unique(meetups)
meetups <- meetups[!meetups %in% c("Spotkania-Entuzjastow-R-Warsaw-R-Users-Group-Meetup",
                                   "R-Ladies-Cape-Town",
                                   "rladies-montevideo",
                                   "Taiwan-R")]

get_event_starts <- function(chapter){
  print(chapter)
  upcoming <- try(meetupr::get_events(chapter,
                                  api_key = Sys.getenv("MEETUP_KEY"),
                                  event_status = "upcoming"),
                  silent = TRUE)
  
  if(is(upcoming, "try-error")){
    upcoming <- try(meetupr::get_events(chapter,
                                        api_key = Sys.getenv("MEETUP_KEY"),
                                        event_status = "upcoming"),
                    silent = TRUE)
  }
  
  past <- try(meetupr::get_events(chapter,
                                  api_key = Sys.getenv("MEETUP_KEY"),
                                  event_status = "past"),
              silent = TRUE)
  
  if(is(past, "try-error")){
    past <- try(meetupr::get_events(chapter,
                                    api_key = Sys.getenv("MEETUP_KEY"),
                                    event_status = "past"),
                silent = TRUE)
  }

  if(is(upcoming, "try-error")){
    events <- past
  }else{
    events <- c(upcoming, past)
  }
  
  times <- purrr::map_dbl(events, get_time)
  offsets <- purrr::map_dbl(events, get_utc_offset)
  starts <- as.POSIXct(times/1000 + offsets / 1000,
             origin = "1970-01-01")
  lubridate::with_tz(starts, "UTC")
}


get_time <- function(list){
  return(list$time)
}

get_utc_offset <- function(list){
  return(list$utc_offset)
}

starts <- purrr::map(meetups, get_event_starts)
