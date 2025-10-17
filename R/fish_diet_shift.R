#'  Ontogenetic diet shift table
#'
#' Table summarizing the diet changes with body size that were assumed to build the trophic networks.
#'
#' @format “A data frame with 154 rows and 14 columns:
#' \describe{
#'   \item{species_code}{Species code}
#'   \item{species_name}{Species latin name}
#'   \item{size_min}{Minimum body size of a given size class}
#'   \item{size_max}{Maximum body size of a given size class}
#'   \item{stage}{Dietary stage of the size class}
#'   \item{light}{Light food source}
#'   \item{det}{Detritus food source}
#'   \item{biof}{Biofilm food source}
#'   \item{phytob}{Phytobenthos food source}
#'   \item{macrop}{Macrophyte food source}
#'   \item{phytopl}{Phytoplankton food source}
#'   \item{zoopl}{Zooplankton food source}
#'   \item{zoob}{Zoobenthos food source}
#'   \item{fish}{Fish food source}
#' }
#' @source Bonnaffé, Danet et al. (2021). Comparison of size-structured and species-level trophic networks reveals antagonistic effects of temperature on vertical trophic diversity at the population and species level. Oikos, 130, 1297–1309. https://doi.org/10.1111/oik.08173
"fish_diet_shift"
