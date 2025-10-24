#'  Resource diet table
#'
#' Table summarizing the resource diet that were assumed to build the trophic networks.
#'
#' @format “A data frame with 9 rows and 14 columns. Each row represents a resource taxon:
#' \describe{
#'   \item{species_code}{Species code}
#'   \item{taxon_name}{Taxon name}
#'   \item{size_min}{Minimum body size of a given size class; set to 0 due to the lack of data for resource taxa, which are assumed to occur at all sampling sites.}
#'   \item{size_max}{Maximum body size of a given size class; set to 0 due to the lack of data for resource taxa, which are assumed to occur at all sampling sites.}
#'   \item{stage}{Dietary stage of the size class; set to 0 due to the lack of data for resource taxa, which are assumed to occur at all sampling sites.}
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
#' @source * Bonnaffé, Danet et al. (2021). Comparison of size-structured and species-level trophic networks reveals antagonistic effects of temperature on vertical trophic diversity at the population and species level. Oikos, 130, 1297–1309. doi: [10.1111/oik.08173](https://doi.org/10.1111/oik.08173)
"fish_diet_shift"
