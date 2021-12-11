#' Fetch NHANES questionnaire data for a given year.
#'
#' @name fetchQuestionnaires
#' @author Jack Leary
#' @description This function pulls more granular questionnaire data from the CDC's NHANES survey for a user-provided year.
#' @import magrittr
#' @importFrom dplyr pull
#' @importFrom haven read_xpt
#' @param start.year The year for which the user desires data. Must not be later than 2017 (the last year currently available). Defaults to "2017".
#' @param survey.list A character vector containing the names of specific questionnaires the user would like to analyze. If none are provided, all questionnaires are returned. Defaults to NULL.
#' @return A named list containing the requested questionnaire data.
#' @seealso \code{\link{availableQuestionnaires}}
#' @export
#' @examples
#' fetchQuestionnaires(start.year = "2017")
#' lapply(c("2015", "2017"), fetchQuestionnaires)

fetchQuestionnaires <- function(start.year = "2017", survey.list = NULL) {
  # check inputs
  if (!start.year %in% c("1999", "2001", "2003", "2005", "2007", "2009", "2011", "2013", "2015", "2017")) {
    stop("start.year must be between 1999 and 2017, and must be an odd year.")
  }
  if (is.null(survey.list)) {
    survey.list <- availableQuestionnaires(start.year = start.year) %>% pull(SURVEY_ABRV)
  }
  # dynamically generate URLs & fetch data
  end_year <- as.character(as.numeric(start.year) + 1)
  year_range <- paste0(start.year, "-", end_year)
  data_urls <- sapply(survey.list,
                      function(x) paste0("https://wwwn.cdc.gov/Nchs/Nhanes/", year_range, "/", x, ".XPT"),
                      USE.NAMES = FALSE)
  read_quest_fun <- function(x) {
    Sys.sleep(1)
    res <- tryCatch(
      haven::read_xpt(x),
      error = function(e) "Error reading questionnaire data"
    )
    return(res)
  }
  questionnaire_data <- lapply(data_urls, read_quest_fun)
  names(questionnaire_data) <- survey.list
  return(questionnaire_data)
}
