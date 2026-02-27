
<!-- README.md is generated from README.Rmd. Please edit that file -->

# foodwebbuilder

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/foodwebbuilder)](https://CRAN.R-project.org/package=foodwebbuilder)
<!-- badges: end -->

<div align="justify">

The goal of `foodwebbuilder` is to reconstruct local fish food-web
structures that account for intraspecific diet variation, by integrating
both taxonomic identity and body size. This package provides tools and
example datasets to reproduce and explore trophic network construction
as described in Bonnaffé *et al.* (2021) and Danet *et al.* (2021).

## Installation

You can install the **development version** of *foodwebbuilder* from
GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("Fellows-of-the-Fish-Food-Webs-F3W/foodwebbuilder")
```

*note that the package is not available on CRAN.*

## Included datasets

The package includes several datasets used to illustrate trophic network
reconstruction.

| Dataset | Description | Rows × Cols |
|----|----|----|
| `ind_measure` | Individual fish measurements | 282 × 7 |
| `fish_diet_shift` | Ontogenetic diet shifts by fish species and size class | 154 × 14 |
| `pred_win` | Predation window parameters defining size-dependent piscivory | 61 × 9 |
| `resource_diet_shift` | Resource diet composition assumed for basal taxa | 9 × 14 |

The `ind_measure` dataset originates from the **ASPE** database (via the
`fishdatabuilder` package), while `fish_diet_shift`, `pred_win`, and
`resource_diet_shift` are derived from Bonnaffé *et al.* (2021).  
All datasets are provided as reproducible examples for building
size-structured trophic networks.

## Quick look at the datasets

You can load and explore the datasets directly using `data()`:

``` r
library(foodwebbuilder)

data(ind_measure)
head(ind_measure)
#>   site_id operation_id prelevement_id batch_id measure_id species_code size
#> 1       4        39444          77045  4536843    6964173          CHE  440
#> 2       4        39444          77045  4536845    6964175          LPX  161
#> 3       4        39444          77045  4536847    6964177          LPX  180
#> 4       4        39444          77045  4536849    6964179          ROT   32
#> 5       4        39444          77045  4536851    6964181          TAN   62
#> 6       4        39444          77045  4536852    6964182          TAN   66

data(fish_diet_shift)
head(fish_diet_shift)
#>   species_code        species_name size_min size_max stage light det biof
#> 1          ANG   Anguilla_anguilla        0     19.9     1     0   0    0
#> 2          ANG   Anguilla_anguilla       20    324.9     2     0   0    0
#> 3          ANG   Anguilla_anguilla      325      Inf     3     0   0    0
#> 4          BLE Salaria_fluviatilis        0     19.9     1     0   0    0
#> 5          BLE Salaria_fluviatilis       20      Inf     2     0   1    0
#> 6          LOR      Cobitis_taenia        0     19.9     1     0   0    0
#>   phytob macroph phytopl zoopl zoob fish
#> 1      0       0       0     1    0    0
#> 2      0       0       0     0    1    0
#> 3      0       0       0     0    1    1
#> 4      0       0       0     1    0    0
#> 5      0       0       0     1    1    0
#> 6      0       0       0     1    0    0

data(pred_win)
head(pred_win)
#>   species_code alpha_min beta_min alpha_max beta_max alpha_mean beta_mean
#> 1          BRO         0     0.03         0    0.575          0         0
#> 2          ANG         0     0.03         0    0.310          0         0
#> 3          LOT         0     0.03         0    0.600          0         0
#> 4          BBG         0     0.03         0    0.400          0         0
#> 5          PER         0     0.03         0    0.450          0         0
#> 6          SAN         0     0.03         0    0.450          0         0

data(resource_diet_shift)
head(resource_diet_shift)
#>   species_code    taxon_name size_min size_max stage light det biof phytob
#> 1          det      detritus        0        0     0     0   0    0      0
#> 2         biof      biofilms        0        0     0     1   0    0      0
#> 3       phytob  phytobenthos        0        0     0     1   1    0      0
#> 4      macroph    macrophyte        0        0     0     1   1    0      0
#> 5      phytopl phytoplankton        0        0     0     1   0    0      0
#> 6        zoopl   zooplankton        0        0     0     0   0    0      0
#>   macroph phytopl zoopl zoob fish
#> 1       0       0     0    0    0
#> 2       0       0     0    0    0
#> 3       0       0     0    0    0
#> 4       0       0     0    0    0
#> 5       0       0     0    0    0
#> 6       0       1     1    0    0
```

## Workflow overview

The `foodwebbuilder` package follows a two-step approach:

1.  **Reconstruction of the trophic metaweb** – the comprehensive
    network of potential feeding links among all taxa in the dataset,
    accounting for intraspecific diet variation.

2.  **Extraction of local food-webs** – subnetworks derived from the
    metaweb, restricted to the species and size classes observed at each
    local unit.

## Step 1 – Reconstructing the trophic metaweb

The first step consists in building a size-structured trophic metaweb
from the datasets included in the package.

This workflow illustrates how to:

1.  filter species to retain only those with complete information across
    all input tables;

2.  define size classes for each fish species based on observed body
    sizes;

3.  assemble the metaweb, combining fish–fish, resource–fish, and
    resource–resource interactions into a single adjacency matrix.

``` r
## 1. Remove species with incomplete information
ind_clean <- remove_missing_species(
  ind_measure     = ind_measure,
  fish_diet_shift = fish_diet_shift,
  pred_win        = pred_win
)
#> missing species found and removed:
#> LPX LPP

## 2. Define size classes for each species (here: 5 classes)
size_classes <- compute_size_classes(
  ind_measure = ind_clean,
  num_classes = 5
)

head(size_classes)
#>   species_code lower_bound upper_bound_1 upper_bound_2 upper_bound_3
#> 1          CHE           0            88           176           264
#> 2          ROT           0           6.4          12.8          19.2
#> 3          TAN           0          19.2          38.4          57.6
#> 4          CHA           0            21            42            63
#> 5          EPT           0             9            18            27
#> 6          EPI           0          13.8          27.6          41.4
#>   upper_bound_4 upper_bound_5
#> 1           352           440
#> 2          25.6            32
#> 3          76.8            96
#> 4            84           105
#> 5            36            45
#> 6          55.2            69

## 3. Build the trophic metaweb (adjacency matrix)
metaweb <- build_metaweb(
  tab_size_classes    = size_classes,
  pred_win            = pred_win,
  fish_diet_shift     = fish_diet_shift,
  resource_diet_shift = resource_diet_shift,
  num_classes         = 5,
  selected_resources  = c("zoopl", "phytopl")
)

## The metaweb is a square matrix: rows = prey, columns = consumers
dim(metaweb)
#> [1] 37 37

## Total number of potential interactions
sum(metaweb)
#> [1] 205
```

## Step 2 – Reconstructing local food-webs

The second step consists in extracting, for each sampling unit, the
subnetwork of the global metaweb that includes only the trophic species
(species × size classes) represented by locally observed individuals,
along with the selected basal resources.

``` r
local_foodwebs <- build_local_foodweb(
  ind_measure       = ind_clean,
  local_id          = "operation_id",         # column in ind_measure
  metaweb           = metaweb,
  tab_size_classes  = size_classes,
  num_classes         = 5,
  selected_resources  = c("zoopl", "phytopl")
)

## Inspect how many local food webs were built
length(local_foodwebs)
#> [1] 3

## Extract one local food web
local_fw <- local_foodwebs[[1]]
```

## References

- Bonnaffé, C., Danet, A., *et al.* (2021). Comparison of
  size-structured and species-level trophic networks reveals
  antagonistic effects of temperature on vertical trophic diversity at
  the population and species level. *Oikos*, 130, 1297–1309. **doi:**
  [10.1111/oik.08173](https://doi.org/10.1111/oik.08173)
- Danet, A., Bonnaffé, C., *et al.* (2021). Species richness and
  food-web structure jointly drive community biomass and its temporal
  stability in fish communities. *Ecology Letters*, 24, 2364–2377.
  **doi:** [10.1111/ele.13857](https://doi.org/10.1111/ele.13857)

## License

GPL-3 © Fellows of the Fish Food Webs (F<sup>3</sup>W) Project

</div>
