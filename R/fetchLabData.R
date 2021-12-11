#' Fetch NHANES lab data for a given year.
#'
#' @name fetchLabData
#' @author Jack Leary
#' @description This function pulls various types of laboratory data from the CDC's NHANES survey for a user-provided year.
#' @import magrittr
#' @importFrom dplyr pull
#' @importFrom progress progress_bar
#' @importFrom haven read_xpt
#' @param start.year The year for which the user desires data. Must not be later than 2017 (the last year currently available). Defaults to "2017".
#' @param lab.dataset.list A character vector containing the names of specific lab datasets the user would like to analyze. If none are provided, all lab datasets are returned. Defaults to NULL.
#' @return A named list containing the requested lab datasets.
#' @seealso \code{\link{availableLabData}}
#' @export
#' @examples
#' fetchLabData(start.year = "2017", lab.dataset.list = c("TCHOL_J", "PBCD_J"))

fetchLabData <- function(start.year = "2017", lab.dataset.list = NULL) {
  # check inputs
  if (!start.year %in% c("1999", "2001", "2003", "2005", "2007", "2009", "2011", "2013", "2015", "2017")) {
    stop("start.year must be between 1999 and 2017, and must be an odd year.")
  }
  if (is.null(lab.dataset.list)) {
    lab.dataset.list <- availableLabData(start.year = start.year) %>% dplyr::pull(LAB_ABRV)
  }
  # dynamically generate URLs & fetch data
  end_year <- as.character(as.numeric(start.year) + 1)
  year_range <- paste0(start.year, "-", end_year)
  data_urls <- sapply(lab.dataset.list,
                      function(x) paste0("https://wwwn.cdc.gov/Nchs/Nhanes/", year_range, "/", x, ".XPT"),
                      USE.NAMES = FALSE)
  # set up progress bar
  pb <- progress::progress_bar$new(format = "  downloading (:rate) [:bar] :percent eta: :eta",
                                   total = length(lab.dataset.list),
                                   clear = FALSE,
                                   width = 60)
  read_lab_fun <- function(x) {
    pb$tick()
    res <- tryCatch(
      haven::read_xpt(x),
      error = function(e) "Error reading lab data"
    )
    return(res)
  }
  lab_data <- lapply(data_urls, read_lab_fun)
  names(lab_data) <- lab.dataset.list
  return(lab_data)
}
