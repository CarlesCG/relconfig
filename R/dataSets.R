#' Life data with one failure mode
#'
#' A dataset containing right censored life data for the purpose of constructing weibull plots and 
#' make predictions
#' 
#' @format A data frame with 16 rows and 5 variables:
#' \describe{
#'   \item{row_id}{unique identifier of the rows}
#'   \item{part_id}{unique identifyir of each entity in the pouplation i.e. a wind turbine}
#'   \item{time}{unit of time (or cycles) measured }
#'   \item{event}{indicates if at the moment of the measurement the component failed or was 
#'   survivor (rigth censored data)}
#'   \item{failure_mode}{indicates if in the data set there is one or more failure modes}
#' }
"onefm"


#' Life data with two failure modes
#'
#' A dataset containing right censored life data for the purpose of constructing weibull plots and 
#' make predictions
#' 
#' @format A data frame with 16 rows and 5 variables:
#' \describe{
#'   \item{row_id}{unique identifier of the rows}
#'   \item{part_id}{unique identifyir of each entity in the pouplation i.e. a wind turbine}
#'   \item{time}{unit of time (or cycles) measured }
#'   \item{event}{indicates if at the moment of the measurement the component failed or was 
#'   survivor (rigth censored data)}
#'   \item{failure_mode}{indicates if in the data set there is one or more failure modes}
#' }
"twofm"

# twofmTruncated <- read.csv("./inst/NewWeibullApp/data/sample_df_2FM_truncated.csv")
# devtools::use_data(twofmTruncated)
