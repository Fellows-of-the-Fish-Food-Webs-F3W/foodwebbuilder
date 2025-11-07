
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

2.  **Extraction of local food-webs** – site- and time-specific networks
    derived from the metaweb, including only species recorded at each
    sampling event.

## Step 1 – Reconstructing the trophic metaweb

*To complete*

## Step 2 – Reconstructing local food-webs

*To complete*

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
