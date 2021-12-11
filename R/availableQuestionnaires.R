#' Fetch a table of the different NHANES questionnaires available for a given year.
#'
#' @name availableQuestionnaires
#' @author Jack Leary
#' @description This function generates a table of the available NHANES questionnaires and their abbreviated names e.g., "Cardiovascular Health" and "CDQ_J" for 2017-2018. The desired abbreviated values can then be fed into \code{\link{fetchQuestionnaires}}, which will return the questionnaire data itself.
#' @import magrittr
#' @importFrom polite bow nod scrape
#' @importFrom rvest html_element html_table
#' @importFrom stats setNames
#' @importFrom dplyr mutate
#' @param start.year The year for which the user desires data. Must not be later than 2017 (the last year currently available). Defaults to "2017".
#' @return A data.frame containing the full and abbreviated names of the available questionnaires for the given year.
#' @seealso \code{\link{fetchQuestionnaires}}
#' @export
#' @examples
#' availableQuestionnaires(start.year = "2011")

availableQuestionnaires <- function(start.year = "2017") {
  # check inputs
  if (!start.year %in% c("1999", "2001", "2003", "2005", "2007", "2009", "2011", "2013", "2015", "2017")) {
    stop("start.year must be between 1999 and 2017, and must be an odd year.")
  }
  # fetch table of questionnaire names
  cdc_url <- paste0("https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Questionnaire&CycleBeginYear=", start.year)
  cdc_bow <- polite::bow(url = cdc_url, force = TRUE)
  cdc_session <- polite::nod(bow = cdc_bow, path = cdc_url, verbose = FALSE)
  cdc_scraped <- polite::scrape(cdc_session)
  question_table <- cdc_scraped %>%
                    rvest::html_element("tbody") %>%
                    rvest::html_table() %>%
                    stats::setNames(c("SURVEY_TYPE", "SURVEY_ABRV", "SURVEY_FILE", "SURVEY_DATE")) %>%
                    dplyr::mutate(SURVEY_ABRV = gsub(" Doc", "", SURVEY_ABRV))
  if (nrow(question_table) == 0) {
    stop(sprintf("An error occurred when pulling questionnaire names from %s", cdc_url))
  }
  return(question_table)
}
