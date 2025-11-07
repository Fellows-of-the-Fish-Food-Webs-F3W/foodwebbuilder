#' Predation window dataset: `pred_win`
#'
#' Dataset summarizing interspecific variation in size-dependent piscivory
#' used to model fish–fish interactions in trophic networks.
#' Each row corresponds to one fish species.
#'
#' @format A data frame with 61 rows and 9 columns:
#' \describe{
#'   \item{species_code}{Three-letter species code.}
#'   \item{alpha_min}{Lower slope coefficient defining the minimum predation window.}
#'   \item{beta_min}{Intercept defining the minimum predation window.}
#'   \item{alpha_max}{Upper slope coefficient defining the maximum predation window.}
#'   \item{beta_max}{Intercept defining the maximum predation window.}
#'   \item{alpha_mean}{Slope coefficient for the mean predation window.}
#'   \item{beta_mean}{Intercept for the mean predation window.}
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
