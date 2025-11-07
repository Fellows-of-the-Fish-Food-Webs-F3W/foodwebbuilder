#' Ontogenetic diet shift dataset: `fish_diet_shift`
#'
#' Dataset summarizing the assumed diet composition changes with body size
#' that were used to build the trophic networks. Each row represents one fish
#' species at a specific size class.
#'
#' @format A data frame with 154 rows and 14 columns:
#' \describe{
#'   \item{species_code}{Three-letter species code.}
#'   \item{species_name}{Scientific (Latin) name of the species.}
#'   \item{size_min}{Minimum body size of the size class (mm).}
#'   \item{size_max}{Maximum body size of the size class (mm).}
#'   \item{stage}{Dietary stage corresponding to the size class.}
#'   \item{light}{Light-based food source.}
#'   \item{det}{Detritus food source.}
#'   \item{biof}{Biofilm food source.}
#'   \item{phytob}{Phytobenthos food source.}
#'   \item{macroph}{Macrophyte food source.}
#'   \item{phytopl}{Phytoplankton food source.}
#'   \item{zoopl}{Zooplankton food source.}
#'   \item{zoob}{Zoobenthos food source.}
#'   \item{fish}{Fish food source.}
#' }
#'
#' @source Bonnaffé, C., Danet, A., et al. (2021).
#' *Comparison of size-structured and species-level trophic networks reveals
#' antagonistic effects of temperature on vertical trophic diversity at the
#' population and species level.* \emph{Oikos}, 130, 1297–1309.
#' \doi{10.1111/oik.08173}
#'
#' @examples
#' data(fish_diet_shift)
#' head(fish_diet_shift)
#'
#' @keywords datasets
#' @name fish_diet_shift
#' @docType data
"fish_diet_shift"
