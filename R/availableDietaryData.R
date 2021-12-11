#' Fetch a table of the different NHANES dietary datasets available for a given year.
#'
#' @name availableDietaryData
#' @author Jack Leary
#' @description This function generates a table of the available NHANES dietary datasets and their abbreviated names e.g., "Dietary Supplement Database - Ingredient Information" and "DSII" for 2017-2018. The desired abbreviated values can then be fed into \code{\link{fetchDietaryData}}, which will return the dietary data itself.
#' @import magrittr
#' @importFrom polite bow nod scrape
#' @importFrom rvest html_element html_table
#' @importFrom stats setNames
#' @importFrom dplyr mutate
#' @param start.year The year for which the user desires data. Must not be later than 2017 (the last year currently available). Defaults to "2017".
#' @return A data.frame containing the names of the available dietary datasets for the given year and their abbreviated names.
#' @seealso \code{\link{fetchDietaryData}}
#' @export
#' @examples
#' availableDietaryData(start.year = "2013")

availableDietaryData <- function(start.year = "2017") {
  # check inputs
  if (!start.year %in% c("1999", "2001", "2003", "2005", "2007", "2009", "2011", "2013", "2015", "2017")) {
    stop("start.year must be between 1999 and 2017, and must be an odd year.")
  }
  # scrape table of dietary data
  cdc_url <- paste0("https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Dietary&CycleBeginYear=", start.year)
  cdc_bow <- polite::bow(url = cdc_url, force = TRUE)
  cdc_session <- polite::nod(bow = cdc_bow, path = cdc_url, verbose = FALSE)
  cdc_scraped <- polite::scrape(cdc_session)
  diet_table <- cdc_scraped %>%
                rvest::html_element("tbody") %>%
                rvest::html_table() %>%
                stats::setNames(c("DIET_TYPE", "DIET_ABRV", "DIET_FILE", "DIET_DATE")) %>%
                dplyr::mutate(DIET_ABRV = gsub(" Doc", "", DIET_ABRV))
  if (nrow(diet_table) == 0) {
    stop(sprintf("An error occurred when pulling dietary dataset names from %s", cdc_url))
  }
  return(diet_table)
}
