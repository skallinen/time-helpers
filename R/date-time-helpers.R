#' \%+\% Operator Infix Function For String Concatenation
#'
#' Add string operation
#'
#' @param x strings
#' @param y strings
#' @return a concatenated string
#' @author Sami Kallinen
#' @export

`%+%`                  <- function(x, y) {paste0(x, y)}


#' Stores The Default Time Zone
#'
#' Rerturns the default time zone.
#'
#' @return a string containing "EET" eastern european time code.
#' @author Sami Kallinen
#' @export


get_time_zone              <- function(){"EET"}

#' The Current Year
#'
#' Returns the current year.
#'
#' @return a numeric value containing the current year
#' @author Sami Kallinen
#' @export
#' @importFrom magrittr "%>%"

get_current_year           <- function(){lubridate::now(tzone = get_time_zone()) %>% lubridate::year}

#' The Current Quarter
#'
#' Returns the current quarter.
#'
#' @return a numeric value containing the current quarter.
#' @author Sami Kallinen
#' @export
#' @importFrom magrittr "%>%"

get_current_quarter        <- function(){lubridate::now(tzone = get_time_zone()) %>% lubridate::quarter}

#' The Quarter Start and End dates
#'
#' Data frame with quarter dates.
#'
#' @return a data.frame with start dates and end dates for all
#' @author Sami Kallinen
#' @export
#' @importFrom magrittr "%>%"

get_quarter_dates          <- function(){
  dates <- data.frame(start = c("01-01", "04-01", "07-01", "10-01"),
                      end   = c("03-31", "06-30", "09-30", "12-31"))
  lapply(dates, function(x)
    (lubridate::now(tzone = get_time_zone()) %>%
       lubridate::year %+% "-" %+% x) %>%
      lubridate::ymd(tz = get_time_zone()) )
}

#' The Current Week number
#'
#' The current week number
#'
#' @return a number with the current week
#' @author Sami Kallinen
#' @export

get_current_week           <- function(){
  lubridate::isoweek(lubridate::now(tzone = get_time_zone()))
}

#' The first week number of current quarter
#'
#' The first week number of current quarter
#'
#' @return a number
#' @author Sami Kallinen
#' @export

get_first_quarter_week     <- function(){
  week <- lubridate::isoweek(get_quarter_dates()$start[get_current_quarter()])
  week <- ifelse(week == 53, 1, week)
  week
}

#' The last week number of current quarter
#'
#' The last week number of current quarter
#'
#' @return a number
#' @author Sami Kallinen
#' @export

get_last_quarter_week      <- function(){
  lubridate::isoweek(get_quarter_dates()$end[get_current_quarter()])
}

#' Duration of Current Quarter
#'
#' The duration of current quarter in weeks
#'
#' @return a number
#' @author Sami Kallinen
#' @export

get_quarter_duration_weeks <- function(){
  as.numeric(get_last_quarter_week()) - (get_first_quarter_week() - 1)
}

#' Today as a String
#'
#' Returns today's date as a string
#'
#' @return a string
#' @author Sami Kallinen
#' @export
#' @importFrom magrittr "%>%"

get_today_string           <- function(){
  lubridate::today() %>% as.character # todays date as string
}

#' First Day of Current Quarter
#'
#' The date when current quarter started
#'
#' @return the first day of the quarter in POSIXct
#' @author Sami Kallinen
#' @export

get_quarter_start              <- function(){
  get_quarter_dates()$start[get_current_quarter()]
}

#' Last Day of Current Quarter
#'
#' The date when current quarter ends
#'
#' @return the last day of the quarter in POSIXct
#' @author Sami Kallinen
#' @export

get_quarter_end                <- function(){
  get_quarter_dates()$end[get_current_quarter()]
}

#' Sequence the Dates in Current Quarter
#'
#' Produces the sequence of dates in the current quarter
#'
#' @return a sequence of POSIXct dates
#' @author Sami Kallinen
#' @export

get_time_seq                <- function(){
  seq(get_quarter_dates()$start[get_current_quarter()],
      get_quarter_dates()$end[get_current_quarter()],
      by = '1 day')
}

#' Sequence Dates
#'
#'
#'
#' @return a sequence of POSIXct dates
#' @author Sami Kallinen
#' @param start POSIXct date
#' @param end POSIXct date
#' @export
#' @importFrom magrittr "%>%"
#'
seq_date <- function(start, end){
  if(difftime(start, end, units="days") %>% abs < 1){
    x <- start
  } else {
    x <- seq(start, end, by = "1 day")
  }
  x
}


# get_timeseq_to_now  <- function(){
#   seq_date(get_quarter_start(),
#            today() %>% ymd(tz = get_time_zone()))
# }

# get_timeseq_to_end  <- function(){
#   seq_date(today() %>% ymd(tz = get_time_zone()),
#            get_quarter_end())
# }

#' Date of the Monday when This Quarter Started
#'
#' Date of the starting monday of the week when this quarter started.
#'
#' @return a POSIXct date
#' @author Sami Kallinen
#' @export
#' @importFrom magrittr "%>%"
#'
get_period_start_monday      <- function(){
  as.Date(paste(get_current_year(),
                get_first_quarter_week(), 1,
                sep = "-"),
          "%Y-%U-%u") %>%  lubridate::ymd(tz = get_time_zone())

}
#' Dummy Week Data Frame
#'
#' A dummy data.frame with week numbers, month and year for the current year.
#'
#' @return a data.frame
#' @author Sami Kallinen
#' @export
#' @import dplyr
#' @importFrom magrittr "%>%"

get_dummy_weeks <- function(){
  week <- rep(1:52)
  data.frame(week = week) %>%
    mutate(date  = paste(get_current_year(), week, "1", sep = "-"),
           date  = as.POSIXct(date, format = "%Y-%U-%u"),
           month = lubridate::month(date),
           year  = lubridate::year(date))
}


