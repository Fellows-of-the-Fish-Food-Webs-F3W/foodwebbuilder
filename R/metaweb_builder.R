#######################
## metaweb_builder.R ##
#######################

## Goal: functions file for the build_metaweb function

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

.as_01_numeric <- function(x, name = "value") {
  if (is.logical(x)) {
    y <- x
    storage.mode(y) <- "numeric"  # conserve dimensions/dimnames
    return(y)
  }
  if (is.numeric(x)) return(as.numeric(x))
  stop(sprintf("%s must be logical or numeric (0/1).", name), call. = FALSE)
}

#####################################
## CHECK FOR MISSING DATA AND TRIM ##
#####################################

#' Remove Species with missing data across input tables
#'
#' @description
#' Cleans the individual measurement dataset by removing species that are not
#' present in the auxiliary datasets (`fish_diet_shift` or `pred_win`).
#' This ensures that all species retained have complete information across
#' all relevant data sources.
#'
#' @param ind_measure A data frame containing individual-level measurements
#'   with a column `species_code` identifying each species.
#' @param fish_diet_shift A data frame containing species dietary information,
#'   with a column `species_code`.
#' @param pred_win A data frame containing predator window or prey availability
#'   data, with a column `species_code`.
#'
#' @return
#' A filtered version of `ind_measure` containing only species that appear in
#' both `fish_diet_shift` and `pred_win`.
#'
#' @details
#' The function checks whether each species in `ind_measure` is represented in
#' both reference datasets. Any species missing from either is removed, and a
#' message is printed listing which species were excluded.
#'
#' @examples
#' \dontrun{
#' filtered <- remove_missing_species(ind_measure, fish_diet_shift, pred_win)
#' }
#'
#' @export
remove_missing_species <- function(ind_measure, fish_diet_shift, pred_win) {

  .assert_has_cols(ind_measure,     "species_code", "ind_measure")
  .assert_has_cols(fish_diet_shift, "species_code", "fish_diet_shift")
  .assert_has_cols(pred_win,        "species_code", "pred_win")

  sp_ind  <- unique(ind_measure$species_code)
  sp_diet <- unique(fish_diet_shift$species_code)
  sp_pred <- unique(pred_win$species_code)

  missing <- setdiff(sp_ind, intersect(sp_diet, sp_pred))

  if (length(missing) > 0) {
    message("Missing species found and removed: ",
            paste(missing, collapse = " "))
    ind_measure <- ind_measure[!(ind_measure$species_code %in% missing), ,
                               drop = FALSE]
  } else {
    message("No missing species found (nothing removed).")
  }

  ind_measure
}

##########################
## COMPUTE SIZE CLASSES ##
##########################

#' Compute size class boundaries for each species
#'
#' @description
#' Divides the observed body length range of each species into a specified
#' number of size classes. This is typically used to discretize continuous
#' size data for subsequent analysis or modeling.
#'
#' @param ind_measure A data frame of individual-level measurements
#'   containing at least the columns `species_code` and `size`.
#' @param num_classes Integer indicating the number of size classes
#'   to divide each species' observed size range into.
#'
#' @return
#' A data frame listing each species with the corresponding size class
#' boundaries. The output includes a column for the species code,
#' a lower bound, and upper bound columns for each class.
#'
#' @details
#' For each species, the function identifies the maximum observed size and
#' constructs evenly spaced size class intervals from 0 to that maximum.
#' These intervals are returned in a tabular form suitable for further use
#' in population or trophic modeling.
#'
#' @examples
#' \dontrun{
#' size_classes <- compute_size_classes(ind_measure, num_classes = 5)
#' head(size_classes)
#' }
#'
#' @export
compute_size_classes <- function(ind_measure, num_classes) {

  .assert_has_cols(ind_measure, c("species_code", "size"), "ind_measure")

  if (length(num_classes) != 1 || is.na(num_classes) || num_classes < 1) {
    stop("num_classes must be a single integer >= 1.",
         call. = FALSE)
  }
  num_classes <- as.integer(num_classes)

  species <- unique(ind_measure$species_code)

  out <- lapply(species, function(sp) {

    x <- ind_measure$size[ind_measure$species_code == sp]
    x <- x[!is.na(x)]

    if (length(x) == 0) {
      stop("Species '", sp, "' has no non-missing size values in ind_measure.",
           call. = FALSE)
    }

    size_max <- max(x)

    # breaks length = K+1 ; upper bounds are breaks[-1]
    if (size_max == 0) {
      breaks <- rep(0, num_classes + 1L)
    } else {
      breaks <- seq(0, size_max, length.out = num_classes + 1L)
    }

    upper <- breaks[-1]

    c(species_code = sp, lower_bound = 0,
      stats::setNames(upper, paste0("upper_bound_", seq_len(num_classes))))
  })

  out <- do.call(rbind, out)
  out <- as.data.frame(out, stringsAsFactors = FALSE)

  # coerce numeric columns
  for (j in seq(2, ncol(out))) out[[j]] <- as.numeric(out[[j]])

  out
}

###################
## BUILD METAWEB ##
###################

#' Build a trophic metaweb
#'
#' @description
#' Constructs a complete metaweb—an integrated species interaction network—
#' by combining size‐class information, predator–prey relationships, dietary
#' data, and resource links. The resulting matrix describes all potential
#' trophic interactions among size‐structured fish species and their resources.
#'
#' @param tab_size_classes A data frame of size‐class boundaries for each
#'   species, typically produced by [compute_size_classes()]. Must contain a
#'   `species_code` column and the lower and upper bounds for each size class.
#' @param pred_win A data frame describing the predator–prey window for each
#'   species, including columns `species_code`, `beta_min`, and `beta_max`,
#'   defining the lower and upper ratios of prey to predator body size.
#' @param fish_diet_shift A data frame describing ontogenetic diet shifts for
#'   each fish species. Must include `species_code`, `size_min`, `size_max`,
#'   and columns indicating dietary components (e.g., `fish`, `benthos`, etc.).
#'   Used to determine piscivory status and resource consumption.
#' @param resource_diet_shift A data frame describing resource‐to‐resource
#'   interactions (e.g., basal resource dependencies). Must include one row per
#'   resource and columns corresponding to the resources listed in
#'   `selected_resources`.
#' @param num_classes Optional integer.
#'   If provided, it is used ONLY as a consistency check
#'   against `tab_size_classes` (source of truth).
#'   If inconsistent, the function errors.
#' @param selected_resources Character vector giving the names of resource
#'   types (columns) to include in the metaweb.
#' @param method_resource_fish String indicating which method to use for building resource-fish
#'   interactions There are two options: `midpoint`, which uses the midpoint of size classes,
#'   or `bounds`, which uses the lower and upper bounds.
#' @param method_predation_window String indicating which method to use for building predation
#'   windows. There are two options: `midpoint`, which uses the midpoint of size classes,
#'   or `bounds`, which uses the lower and upper bounds.
#' @param method_fish_fish String indicating which method to use for building fish-fish
#'   interactions. There are two options: `midpoint`, which uses the midpoint of size classes,
#'   or `bounds`, which uses the lower and upper bounds. By default, the midpoint method is used.
#'
#' @return
#' A square adjacency matrix (data frame or matrix) representing all potential
#' trophic interactions among trophic species (size classes) and resources.
#' Rows correspond to prey items and columns to consumers.
#'
#' @details
#' The function proceeds through several steps:
#' 1. Defines *trophic species* as combinations of species and size classes.
#' 2. Computes prey size‐class limits using the predator–prey window parameters.
#' 3. Builds fish–fish interaction matrices based on size overlap and
#'    piscivory status from `fish_diet_shift`.
#' 4. Builds resource–fish and resource–resource interaction matrices using
#'    `fish_diet_shift` and `resource_diet_shift`.
#' 5. Combines all matrices into a single metaweb adjacency matrix.
#'
#' The metaweb thus represents the complete potential trophic network
#' integrating all modeled size classes and resource categories.
#'
#' @seealso [remove_missing_species()], [compute_size_classes()]
#'
#' @examples
#' \dontrun{
#' # 1) Load packaged example datasets
#' data(ind_measure)
#' data(fish_diet_shift)
#' data(pred_win)
#' data(resource_diet_shift)
#'
#' # 2) Ensure consistency of species across inputs
#' ind_clean <- remove_missing_species(
#'   ind_measure     = ind_measure,
#'   fish_diet_shift = fish_diet_shift,
#'   pred_win        = pred_win
#' )
#'
#' # 3) Choose number of size classes once
#' tab_size_classes <- compute_size_classes(ind_clean, num_classes = 5)
#'
#' # 4) Build metaweb
#'
#' metaweb <- build_metaweb(
#'   tab_size_classes      = tab_size_classes,
#'   pred_win              = pred_win,
#'   fish_diet_shift       = fish_diet_shift,
#'   resource_diet_shift   = resource_diet_shift,
#'   num_classes           = 5, # optional consistency check
#'                              # (must match tab_size_classes)
#'   selected_resources    = c("zoopl", "phytopl"),
#'   method_resource_fish  = "midpoint",
#'   method_predation_window = "midpoint",
#'   method_fish_fish        = "midpoint"
#' )
#'
#' dim(metaweb)
#' metaweb[1:5, 1:5]
#' }
#'
#' @seealso [remove_missing_species()], [compute_size_classes()]
#'
#' @export
build_metaweb <- function(tab_size_classes,
                          pred_win,
                          fish_diet_shift,
                          resource_diet_shift,
                          num_classes = NULL,
                          selected_resources,
                          method_resource_fish = "midpoint",
                          method_predation_window="midpoint",
                          method_fish_fish="midpoint") {
  
  ## ---- Basic validation ----
  .assert_has_cols(tab_size_classes, c("species_code", "lower_bound"),
                   "tab_size_classes")
  .assert_has_cols(pred_win, c("species_code", "beta_min", "beta_max"),
                   "pred_win")
  .assert_has_cols(fish_diet_shift,
                   c("species_code", "size_min", "size_max", "fish"),
                   "fish_diet_shift")
  .assert_has_cols(resource_diet_shift, "species_code", "resource_diet_shift")

  if (ncol(tab_size_classes) < 3) {
    stop(
      "tab_size_classes must have at least 3 columns: ",
      "species_code, lower_bound, upper_bound_1",
      call. = FALSE
    )
  }

  inferred_num_classes <- ncol(tab_size_classes) - 2L

  if (!is.null(num_classes)) {
    if (length(num_classes) != 1 || is.na(num_classes) || num_classes < 1) {
      stop("num_classes must be a single integer >= 1 (or NULL).",
           call. = FALSE)
    }
    num_classes <- as.integer(num_classes)

    if (!identical(num_classes, inferred_num_classes)) {
      stop(
        "Inconsistent 'num_classes': you passed ", num_classes,
        ", but tab_size_classes implies ", inferred_num_classes, ". ",
        "Rebuild tab_size_classes with compute_size_classes(",
        "..., num_classes = ", num_classes, ") ",
        "or omit num_classes here.",
        call. = FALSE
      )
    }
  }

  num_classes <- inferred_num_classes

  if (length(selected_resources) == 0) {
    stop("selected_resources must be a non-empty character vector.",
         call. = FALSE)
  }
  if (!is.character(selected_resources)) {
    stop("selected_resources must be a character vector.",
         call. = FALSE)
  }

  # Verify selected_resources exist in both diet tables (fish and resource)
  available_cols <- intersect(colnames(fish_diet_shift),
                              colnames(resource_diet_shift))
  missing_cols <- setdiff(selected_resources, available_cols)
  if (length(missing_cols) > 0) {
    stop(
      "The following selected_resources are not present as columns in both ",
      "'fish_diet_shift' and 'resource_diet_shift': ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Verify selected_resources are also present
  # as resource nodes (rows) in resource_diet_shift
  missing_nodes <- setdiff(selected_resources, resource_diet_shift$species_code)
  if (length(missing_nodes) > 0) {
    stop(
      "The following selected_resources are present as columns ",
      "but not as resource nodes in ",
      "resource_diet_shift$species_code: ",
      paste(missing_nodes, collapse = ", "),
      call. = FALSE
    )
  }

  ## ---- Define trophic species ----
  species_code <- tab_size_classes$species_code

  # Lower bounds per class are: lower_bound + all but the last upper bounds
  lb_size_classes <- as.numeric(t(as.matrix(
    tab_size_classes[, -c(1, ncol(tab_size_classes)), drop = FALSE])))
  ub_size_classes <- as.numeric(t(as.matrix(
    tab_size_classes[, -c(1, 2), drop = FALSE])))

  if (length(lb_size_classes) != length(ub_size_classes)) {
    stop("tab_size_classes has inconsistent lower/upper bounds formatting.",
         call. = FALSE)
  }

  mp_size_classes <- 0.5 * (ub_size_classes + lb_size_classes)

  trophic_species_code <- paste(
    rep(species_code, each = num_classes),
    rep(seq_len(num_classes), times = length(species_code)),
    sep = "_"
  )

  ## ---- Prey size classes from predation window ----
  s <- match(species_code, pred_win$species_code)
  if (any(is.na(s))) {
    miss <- species_code[is.na(s)]
    stop(
      "The following species_code are not found in pred_win$species_code: ",
      paste(miss, collapse = ", "),
      call. = FALSE
    )
  }
  pred_win_ <- pred_win[s, , drop = FALSE]

  if (method_predation_window == "midpoint"){
    # reshape class midpoints as matrix (species x classes)
    mp_mat <- matrix(mp_size_classes, ncol = num_classes, byrow = TRUE)
    lb_prey_mat <- mp_mat * pred_win_$beta_min
    ub_prey_mat <- mp_mat * pred_win_$beta_max
  } else {
    if (method_predation_window == "bounds")
    {
      # reshape class bounds as matrix (species x classes)
      lb_mat <- matrix(lb_size_classes, ncol = num_classes, byrow = TRUE)
      ub_mat <- matrix(ub_size_classes, ncol = num_classes, byrow = TRUE)
      lb_prey_mat <- lb_mat * pred_win_$beta_min
      ub_prey_mat <- ub_mat * pred_win_$beta_max
    } else {
      stop(
        "The following predation window method is not recognised (method_predation_window): ",
        paste(method_predation_window, ".\n"),
        "Choose either midpoint or bounds.",
        call. = FALSE
      )
    }
  }

  ## Compute prey size ranges
  lb_prey <- as.numeric(t(lb_prey_mat))
  ub_prey <- as.numeric(t(ub_prey_mat))

  ## ---- Potential fish-fish interactions (size window overlap) ----
  # Matrix prey x predator
  if (method_fish_fish == "midpoint"){
    ff <- (
      outer(mp_size_classes, lb_prey, `>=`) &
        outer(mp_size_classes, ub_prey, `<`)
    )
  } else {
    if (method_fish_fish == "bounds"){
      ff <- (
        outer(ub_size_classes, lb_prey, `>=`) &
          outer(lb_size_classes, ub_prey, `<`)
      )
    } else {
      stop(
        "The following fish-fish interaction method is not recognised (method_fish_fish): ",
        paste(method_fish_fish, ".\n"),
        "Choose either midpoint or bounds.",
        call. = FALSE
      )
    }
  }
  ff <- .as_01_numeric(ff, "ff_interactions")

  dimnames(ff) <- list(prey = trophic_species_code,
                       predator = trophic_species_code)

  ## ---- Piscivory status per trophic species (predator filter) ----
  diet_by_sp <- split(fish_diet_shift, fish_diet_shift$species_code)
 
  get_row_for_size_midpoint <- function(df, size_val, sp, ts_code) {
    # choose row where size_min <= size <= size_max (using greater or equal for both poses no problem as the upper bound of a size class is always lower than the lower bound of the adjacent larger size class)
    idx <- which(size_val >= df$size_min & size_val <= df$size_max)
    if (length(idx) == 0) {
      stop(
        "No matching diet interval found for trophic species ", ts_code,
        " (species_code=", sp, ", size midpoint=", signif(size_val, 6), "). ",
        "Check fish_diet_shift size_min/size_max coverage.",
        call. = FALSE
      )
    }
    df[idx[1], , drop = FALSE]
  }

  get_row_for_size_bounds <- function(df, size_val_lb, size_val_hb, sp, ts_code) {
    # choose row where size_min <= size <= size_max (using greater or equal for both poses no problem as the upper bound of a size class is always lower than the lower bound of the adjacent larger size class)
    idx <- which(size_val_lb <= df$size_max & size_val_hb >= df$size_min)
    if (length(idx) == 0) {
      stop(
        "No matching diet interval found for trophic species ", ts_code,
        " (species_code=", sp, ", size midpoint=", signif(size_val, 6), "). ",
        "Check fish_diet_shift size_min/size_max coverage.",
        call. = FALSE
      )
    }
    row_i_ = df[idx, , drop = FALSE] # Can be multiple matches
    if (length(which(colnames(row_i_) == "species_code")) == 0 | length(which(colnames(row_i_) == "species_name")) == 0){
      stop(
        "No matching columns named species_code or species_name found in diet table. Unable to safely collapse diet across multiple size classes."
      )
    }
    s = which(colnames(row_i_) == "species_code" | colnames(row_i_) == "species_name")
    row_i_rhs = apply(row_i_[,-s], 2, sum) # Collapse into a single row
    row_i_rhs = (row_i_rhs > 0)*1 # Binarise
    row_i_ = data.frame(c(row_i_[1,c("species_code", "species_name")], row_i_rhs))
    return(row_i_)
  }

  piscivory <- numeric(length(trophic_species_code))
  rf_mat <- matrix(0,
                   nrow = length(selected_resources),
                   ncol = length(trophic_species_code))
  rownames(rf_mat) <- selected_resources
  colnames(rf_mat) <- trophic_species_code

  for (i in seq_along(trophic_species_code)) {

    sp <- sub("_.*$", "", trophic_species_code[i])
    df <- diet_by_sp[[sp]]
    if (is.null(df)) {
      stop(
        "Species '", sp, "' not found in fish_diet_shift. ",
        "Run remove_missing_species() upstream or check input tables.",
        call. = FALSE
      )
    }

    ## DEV: Add option to switch between midpoint and bounds.
    if (method_resource_fish == "midpoint"){
        row_i <- get_row_for_size_midpoint(df,
                                           mp_size_classes[i],
                                           sp,
                                           trophic_species_code[i])
    } else {
        if (method_resource_fish == "bounds"){
            row_i <- get_row_for_size_bounds(df,
                                             lb_size_classes[i],
                                             ub_size_classes[i],
                                             sp,
                                             trophic_species_code[i])
        } else {
          stop(
            "The following resource-fish interaction method is not recognised (method_resource_fish): ",
            paste(method_resource_fish, ".\n"),
            "Choose either midpoint or bounds.",
            call. = FALSE
          )
        }
    }
         
    piscivory[i] <- .as_01_numeric(row_i[["fish"]], "fish_diet_shift$fish")

    # resource-fish diet proportions / flags for selected resources
    rf_mat[, i] <- as.numeric(row_i[1, selected_resources, drop = TRUE])
  }

  # Apply piscivory filter to predator columns
  ff <- ff %*% diag(piscivory)
  ff <- as.matrix(ff)
  dimnames(ff) <- list(trophic_species_code, trophic_species_code)

  ## ---- Resource-fish interactions ----
  rf <- rf_mat
  rf <- as.matrix(rf)
  dimnames(rf) <- list(selected_resources, trophic_species_code)

  ## ---- Resource-resource interactions ----
  row_idx <- match(selected_resources, resource_diet_shift$species_code)
  if (any(is.na(row_idx))) {
    missing_res <- selected_resources[is.na(row_idx)]
    stop(
      "The following selected_resources are not found ",
      "in resource_diet_shift$species_code: ",
      paste(missing_res, collapse = ", "),
      call. = FALSE
    )
  }

  rr <- resource_diet_shift[row_idx, selected_resources, drop = FALSE]
  rr <- t(as.matrix(rr))
  storage.mode(rr) <- "numeric"
  dimnames(rr) <- list(selected_resources, selected_resources)

  ## ---- Fish-resource interactions (zeros) ----
  fr <- matrix(
    0,
    nrow = ncol(ff),
    ncol = ncol(rr),
    dimnames = list(trophic_species_code, selected_resources)
  )

  ## ---- Assemble metaweb ----
  metaweb <- cbind(
    rbind(ff, rf),
    rbind(fr, rr)
  )

  # Final sanity: square + consistent dimnames
  if (nrow(metaweb) != ncol(metaweb)) {
    stop("Internal error: assembled metaweb is not square.",
         call. = FALSE)
  }
  if (!identical(rownames(metaweb), colnames(metaweb))) {
    # enforce identical ordering
    nodes <- union(rownames(metaweb), colnames(metaweb))
    metaweb <- metaweb[nodes, nodes, drop = FALSE]
  }

  metaweb
}
