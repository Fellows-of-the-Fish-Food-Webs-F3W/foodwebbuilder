#' Individual fish measurements dataset: `ind_measure`
#'
#' Example dataset containing individual fish measurements collected from
#' freshwater monitoring stations. Each row corresponds to one measured fish.
#'
#' @format A data frame with 282 rows and 7 columns:
#' \describe{
#'   \item{site_id}{Station identifier.}
#'   \item{operation_id}{Identifier of the fishing operation.}
#'   \item{prelevement_id}{Sampling point identifier.}
#'   \item{batch_id}{Batch identifier.}
#'   \item{measure_id}{Individual measurement identifier.}
#'   \item{species_code}{Three-letter species code following the ASPE database convention.}
#'   \item{size}{Individual total length (mm).}
#' }
#'
#' @source Example dataset generated using the
#'   \emph{fishdatabuilder} package
#'   (\url{https://github.com/Fellows-of-the-Fish-Food-Webs-F3W/fishdatabuilder}),
#'   based on data from Irz P, Vigneron T, Poulet N, Cosson E, Point T,
#'   Baglinière E, Porcher JP. (2022). *A long-term monitoring database on fish
#'   and crayfish species in French rivers*. Knowledge & Management of Aquatic
#'   Ecosystems, 423:25. \doi{10.1051/kmae/2022021}.
#'
#' @examples
#' data(ind_measure)
#' head(ind_measure)
#'
#' @keywords datasets
#' @name ind_measure
#' @docType data
"ind_measure"
