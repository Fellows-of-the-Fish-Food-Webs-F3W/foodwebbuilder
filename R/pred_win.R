#' Predation window dataset: `pred_win`
#'
#' Dataset summarizing interspecific variation in size-dependent piscivory
#' used to model fish–fish interactions in trophic networks.
#' Each row corresponds to one fish species.
#'
#' @format A data frame with 61 rows and 9 columns:
#' \describe{
#'   \item{species_code}{Three-letter species code.}
#'   \item{alpha_min}{Intercept of the lower boundary of the predation window.}
#'   \item{beta_min}{Slope of the lower boundary of the predation window.}
#'   \item{alpha_max}{Intercept of the upper boundary of the predation window.}
#'   \item{beta_max}{Slope of the upper boundary of the predation window.}
#'   \item{alpha_mean}{SIntercept of the mean predator–prey body-size relationship.}
#'   \item{beta_mean}{Slope of the mean predator–prey body-size relationship.}
#' }
#'
#' @source Bonnaffé, C., Danet, A., et al. (2021).
#' *Comparison of size-structured and species-level trophic networks reveals
#' antagonistic effects of temperature on vertical trophic diversity at the
#' population and species level.* \emph{Oikos}, 130, 1297–1309.
#' \doi{10.1111/oik.08173}
#'
#' @examples
#' data(pred_win)
#' head(pred_win)
#'
#' @keywords datasets
#' @name pred_win
#' @docType data
"pred_win"
