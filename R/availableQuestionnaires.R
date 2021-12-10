#' Fetch a table of the different NHANES questionnaires available for a given year.
#'
#' @name availableQuestionnaires
#' @description This function generates a table of the available NHANES questionnaires and their abbreviated names e.g., "Cardiovascular Health" and "CDQ_J" for 2017-2018. The desired abbreviated values can then be fed into \code{\link{fetchQuestionnaires}}, which will return the questionnaire data itself.
#' @import magrittr
#' @importFrom rvest read_html html_element html_table
#' @importFrom stats setNames
#' @importFrom dplyr mutate
#' @param start.year The year for which the user desires data. Must not be later than 2017 (the last year currently available). Defaults to "2017".
#' @return A data.frame containing, namely, the names of the available for the given year and their abbreviated names.
#' @seealso fetchQuestionnaires
#' @export
#' @examples
#' availableQuestionnaires(start.year = "2011")

availableQuestionnaires <- function(start.year = "2017") {
  # check inputs
  if (!start.year %in% c("1999", "2001", "2003", "2005", "2007", "2009", "2011", "2013", "2015", "2017")) {
    stop("start.year must be between 1999 and 2017, and must be an odd year.")
  }
  # get questionnaire names
  html_url <- paste0("https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Questionnaire&CycleBeginYear=", start.year)
  html_table <- rvest::read_html(html_url) %>%
                rvest::html_element("tbody") %>%
                rvest::html_table() %>%
                stats::setNames(c("SURVEY_TYPE", "SURVEY_ABRV", "SURVEY_FILE", "SURVEY_DATE")) %>%
                dplyr::mutate(SURVEY_ABRV = gsub(" Doc", "", SURVEY_ABRV))
  if (nrow(html_table) == 0) {
    stop(sprintf("An error occurred when pulling questionnaire names from %s", html_url))
  }
  return(html_table)
}
