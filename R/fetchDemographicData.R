#' Fetch NHANES demographics data for a given survey cycle.
#'
#' @name fetchDemographicData
#' @author Jack Leary
#' @description This function pulls demographic survey data from the CDC's NHANES survey from a user-provided year.
#' @import magrittr
#' @importFrom rvest read_html html_element html_text2
#' @importFrom stringr str_match
#' @importFrom haven read_xpt
#' @param start.year The year for which the user desires data. Must not be later than 2017 (the last year currently available). Defaults to "2017".
#' @return A data.frame containing the requested survey data.
#' @export
#' @examples
#' fetchDemographicData(start.year = "2017")

fetchDemographicData <- function(start.year = "2017") {
  # check inputs
  if (!start.year %in% c("1999", "2001", "2003", "2005", "2007", "2009", "2011", "2013", "2015", "2017")) {
    stop("start.year must be between 1999 and 2017, and must be an odd year.")
  }
  # build url to XPT file
  base_url <- "https://wwwn.cdc.gov/Nchs/Nhanes/"
  end_year <- as.character(as.numeric(start.year) + 1)
  year_range <- paste0(start.year, "-", end_year)
  # dynamically determine filename by parsing HTML of the year-specific CDC site
  html_url <- paste0(tolower("https://wwwn.cdc.gov/Nchs/Nhanes/"),
                     "search/datapage.aspx?Component=Demographics&CycleBeginYear=",
                     start.year)
  html_table <- rvest::read_html(html_url) %>%
                rvest::html_element("tbody") %>%
                rvest::html_text2()
  table_text <- gsub("\r", "", html_table)
  table_text <- gsub("\t", "", table_text)
  file_name <- as.character(stringr::str_match(table_text, "DEMO_+."))
  total_url <- paste0(base_url, year_range, "/", file_name, ".XPT")
  # fetch XPT file
  demo_file <- tryCatch(
    haven::read_xpt(total_url),
    error = function(e) "Could not read XPT file"
  )
  if (any(class(demo_file) == "character")) {
    if (demo_file == "Could not read XPT file") {
      stop(sprintf("There was an error reading the XPT file from the following URL: %s", total_url))
    }
  }
  return(demo_file)
}
