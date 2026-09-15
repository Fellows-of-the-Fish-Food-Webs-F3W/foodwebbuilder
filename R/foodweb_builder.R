#######################
## foodweb_builder.R ##
#######################

## Goal: functions file for building local food webs


########################
## INTERNAL UTILITIES ##
########################

.assert_has_cols <- function(df, cols, df_name = deparse(substitute(df))) {
  missing <- setdiff(cols, colnames(df))
  if (length(missing) > 0) {
    stop(
      sprintf(
        "%s must contain the following columns: %s",
        df_name, paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.infer_num_classes <- function(tab_size_classes) {
  if (!("species_code" %in% colnames(tab_size_classes))) {
    stop(
      "tab_size_classes must contain a 'species_code' column.",
      call. = FALSE
    )
  }

  if (ncol(tab_size_classes) < 3) {
    stop(
      "tab_size_classes must have at least three columns: ",
      "species_code, lower_bound, upper_bound_1",
      call. = FALSE
    )
  }

  as.integer(ncol(tab_size_classes) - 2L)
}

.build_trophic_species <- function(tab_size_classes, num_classes) {
  paste(
    rep(tab_size_classes$species_code, each = num_classes),
    rep(seq_len(num_classes), times = nrow(tab_size_classes)),
    sep = "_"
  )
}

.flatten_bounds <- function(tab_size_classes) {
  #Expect columns: species_code, lower_bound, upper_bound_1..upper_bound_K
  num_classes <- .infer_num_classes(tab_size_classes)

  #Lower bounds per class: lower_bound + upper_bound_1..upper_bound_(K-1)
  lb <- as.numeric(
    t(
      as.matrix(
        tab_size_classes[
          ,
          -c(1, ncol(tab_size_classes)),
          drop = FALSE
        ]
      )
    )
  )

  ub <- as.numeric(
    t(
      as.matrix(
        tab_size_classes[
          ,
          -c(1, 2),
          drop = FALSE
        ]
      )
    )
  )

  list(
    lb = lb,
    ub = ub,
    num_classes = num_classes
  )
}

## Assign each individual fish to a trophic species
##
## Trophic species are species-size-class combinations such as
## "XXX_1", "XXX_2", etc.
##
## Size classes are left-closed and right-open [lower, upper),
## except for the final size class of each species, which is
## closed on both sides [lower, upper]. This ensures that an
## individual whose size is exactly equal to the maximum size
## is assigned to the final class.
.assign_trophic_species <- function(ind_measure,
                                    tab_size_classes) {

  ##Validate inputs

  .assert_has_cols(
    ind_measure,
    c(
      "species_code",
      "size"
    ),
    "ind_measure"
  )

  ##Infer size-class structure
  bounds <- .flatten_bounds(tab_size_classes)
  num_classes <- bounds$num_classes

  trophic_species <- .build_trophic_species(
    tab_size_classes,
    num_classes
  )

  lb <- bounds$lb
  ub <- bounds$ub

  if (
    length(trophic_species) != length(lb) ||
    length(lb) != length(ub)
  ) {
    stop(
      "Internal inconsistency: trophic species codes ",
      "and bounds are misaligned. ",
      "Check tab_size_classes formatting.",
      call. = FALSE
    )
  }

  ##Map trophic species to fish species

  species_of_ts <- sub(
    "_.*$",
    "",
    trophic_species
  )

  ts_index_by_species <- split(
    seq_along(trophic_species),
    species_of_ts
  )

  ##Assign individuals
  assigned <- rep(
    NA_character_,
    nrow(ind_measure)
  )

  for (i in seq_len(nrow(ind_measure))) {

    sp <- ind_measure$species_code[i]
    size <- ind_measure$size[i]

    ##Missing species code or size cannot be assigned
    if (is.na(sp) || is.na(size)) {
      next
    }

    ##Identify size classes belonging to this species
    idx <- ts_index_by_species[[sp]]

    ##Species absent from tab_size_classes
    if (is.null(idx)) {
      next
    }

    ##All classes except the last one use [lower, upper)
    if (length(idx) > 1L) {

      non_final_idx <- idx[-length(idx)]

      matched <- non_final_idx[
        size >= lb[non_final_idx] &
          size < ub[non_final_idx]
      ]

    } else {

      matched <- integer(0)
    }

    ##Final class uses [lower, upper]
    final_idx <- idx[length(idx)]

    if (
      size >= lb[final_idx] &&
      size <= ub[final_idx]
    ) {
      matched <- c(
        matched,
        final_idx
      )
    }

    ##A valid individual should belong to exactly one size class
    if (length(matched) == 1L) {
      assigned[i] <- trophic_species[matched]
    }
  }

  assigned
}


#########################
## BUILD LOCAL FOODWEB ##
#########################

#' Build a local food web from the metaweb for all local units
#'
#' @description
#' Builds a local food web (subnetwork) from a global metaweb **for each**
#' local sampling unit present in `ind_measure`, as defined by the column
#' given in `local_id`.
#'
#' For each local unit, the function identifies which trophic species
#' (species–size-class combinations) are represented locally based on
#' individual sizes, then subsets the global metaweb accordingly.
#'
#' @param ind_measure A data frame of individual-level measurements containing
#'   at least the columns specified by `local_id`, `species_code`, and `size`.
#'   Each row represents an observed individual in the local community.
#' @param local_id A character string giving the name of the column in
#'   `ind_measure` that defines the local sampling units (e.g. `"site_id"`,
#'   `"operation_id"`).
#' @param metaweb A global metaweb adjacency matrix (as produced by
#'   [build_metaweb()]) representing all potential interactions among
#'   trophic species and resources.
#' @param tab_size_classes A data frame of size-class boundaries for each
#'   species, typically generated by [compute_size_classes()]. Must include
#'   a `species_code` column and class bounds.
#'   The number of size classes **is inferred** from this object.
#' @param selected_resources Character vector of resource nodes to keep in each
#'   local food web.
#'
#' @return
#' A named list of local food webs.
#' Each element is a square adjacency matrix, and names correspond to unique
#' values of `ind_measure[[local_id]]`.
#'
#' @details
#' For each local unit, the function:
#' 1. Extracts the individuals belonging to that unit.
#' 2. Assigns each individual to a trophic species using its body size.
#' 3. Identifies the trophic species represented locally.
#' 4. Subsets the metaweb to keep only present trophic species + resources.
#'
#' Size classes are interpreted as left-closed and right-open intervals
#' `[lower, upper)`, except for the final size class of each species, which
#' includes its upper bound.
#'
#' The number of size classes is inferred from `tab_size_classes`.
#' Users wishing to change the number of size classes should
#' regenerate `tab_size_classes` with [compute_size_classes()]
#' and re-run [build_metaweb()].
#'
#' @examples
#' \dontrun{
#' # Suppose you already built:
#' #  - an individual measurement table `ind_measure`
#' #  - a table of size classes named `tab_size_classes`
#' #  - a global metaweb object named `metaweb`
#'
#' local_foodwebs <- build_local_foodweb(
#'   ind_measure        = ind_measure,
#'   local_id           = "operation_id",
#'   metaweb            = metaweb,
#'   tab_size_classes   = tab_size_classes,
#'   selected_resources = c("zoopl", "phytopl")
#' )
#'
#' names(local_foodwebs)
#' dim(local_foodwebs[[1]])
#' }
#'
#' @seealso [build_metaweb()], [compute_size_classes()]
#'
#' @export
build_local_foodweb <- function(ind_measure,
                                local_id,
                                metaweb,
                                tab_size_classes,
                                selected_resources) {

  ##Validate inputs

  if (
    !is.character(local_id) ||
    length(local_id) != 1L ||
    is.na(local_id)
  ) {
    stop(
      "local_id must be a single non-missing character string.",
      call. = FALSE
    )
  }

  required_cols <- c(
    local_id,
    "species_code",
    "size"
  )

  .assert_has_cols(
    ind_measure,
    required_cols,
    "ind_measure"
  )

  if (
    is.null(rownames(metaweb)) ||
    is.null(colnames(metaweb))
  ) {
    stop(
      "metaweb must have rownames and colnames (node names).",
      call. = FALSE
    )
  }

  if (!identical(rownames(metaweb), colnames(metaweb))) {
    stop(
      "metaweb must be square with identical row and column names.",
      call. = FALSE
    )
  }

  if (
    !is.character(selected_resources) ||
    length(selected_resources) == 0L
  ) {
    stop(
      "selected_resources must be a non-empty character vector.",
      call. = FALSE
    )
  }

  missing_res <- setdiff(
    selected_resources,
    rownames(metaweb)
  )

  if (length(missing_res) > 0L) {
    stop(
      "The following selected_resources are not present ",
      "in metaweb node names: ",
      paste(missing_res, collapse = ", "),
      call. = FALSE
    )
  }

  ##Identify local units
  local_units <- unique(
    ind_measure[[local_id]]
  )


  ##Build local food webs
  results <- lapply(
    local_units,
    function(id) {

      ##Extract individuals belonging to the local unit
      df_local <- ind_measure[
        ind_measure[[local_id]] == id,
        c(
          "species_code",
          "size"
        ),
        drop = FALSE
      ]

      ##Assign individuals using the common trophic-species assignment function
      assigned_trophic_species <- .assign_trophic_species(
        ind_measure = df_local,
        tab_size_classes = tab_size_classes
      )

      ##Identify trophic species represented locally
      ts_present <- unique(
        assigned_trophic_species[
          !is.na(assigned_trophic_species)
        ]
      )

      ##Keep locally present fish trophic species and resources
      keep <- c(
        ts_present,
        selected_resources
      )

      ##Keep only nodes that exist in the metaweb
      keep <- intersect(
        keep,
        rownames(metaweb)
      )

      ##Extract local subnetwork
      metaweb[
        keep,
        keep,
        drop = FALSE
      ]
    }
  )

  ##Name output list
  names(results) <- as.character(
    local_units
  )

  ##Return
  results
}
