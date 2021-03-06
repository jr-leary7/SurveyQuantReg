% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetchExaminationData.R
\name{fetchExaminationData}
\alias{fetchExaminationData}
\title{Fetch NHANES physician exam data for a given survey cycle.}
\usage{
fetchExaminationData(start.year = "2017", exam.dataset.list = NULL)
}
\arguments{
\item{start.year}{The year for which the user desires data. Must not be later than 2017 (the last year currently available). Defaults to "2017".}

\item{exam.dataset.list}{A character vector containing the names of specific exam datasets the user would like to analyze. If none are provided, all exam datasets are returned. Defaults to NULL.}
}
\value{
A named list containing the requested exam datasets.
}
\description{
This function pulls various types of examination data from the CDC's NHANES survey for a user-provided year.
}
\examples{
fetchExaminationData(start.year = "2017", exam.dataset.list = c("AUX_J", "AUXAR_J"))
}
\seealso{
\code{\link{fetchExaminationData}}
}
\author{
Jack Leary
}
