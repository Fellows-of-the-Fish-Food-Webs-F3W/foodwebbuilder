#' Resource diet dataset: `resource_diet_shift`
#'
#' Dataset summarizing the assumed resource diet composition used to build
#' the trophic networks. Each row represents a resource taxon.
#'
#' @format A data frame with 9 rows and 14 columns:
#' \describe{
#'   \item{species_code}{Resource code.}
#'   \item{taxon_name}{Resource taxon name.}
#'   \item{size_min}{Minimum body size of the size class (set to 0 due to lack of data; resource taxa are assumed to occur at all sampling sites).}
#'   \item{size_max}{Maximum body size of the size class (set to 0 due to lack of data; resource taxa are assumed to occur at all sampling sites).}
#'   \item{stage}{Dietary stage of the size class (set to 0 for all resource taxa).}
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
#' data(resource_diet_shift)
#' head(resource_diet_shift)
#'
#' @keywords datasets
#' @name resource_diet_shift
#' @docType data
"resource_diet_shift"
