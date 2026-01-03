#' Adaptive Class Balancing Recipe Step
#'
#' A recipe step that adaptively balances class distributions using NearMiss
#' for downsampling and Borderline-SMOTE (with SMOTE fallback) for upsampling.
#' The step uses a single tunable target_ratio parameter that controls the
#' degree of class rebalancing.
#'
#' @param recipe A recipe object
#' @param ... Column selector for the outcome variable (typically the class)
#' @param role Not used by this step
#' @param trained Logical, has the step been trained
#' @param target_ratio Numeric, controls class balance (1 = perfect balance,
#'   higher values retain more original imbalance)
#' @param seed Random seed for reproducibility
#' @param skip Logical, should step be skipped when baking new data (default
#'   TRUE - sampling should only apply to training data)
#' @param id Unique identifier for the step
#' @return An updated recipe object
#'
#' @details
#' The algorithm:
#' 1. Computes a reference count from class frequencies using an adaptive
#'    quantile (median for ≤10 classes, 60th percentile for 11-20 classes,
#'    65th percentile for >20 classes)
#' 2. Downsamples classes exceeding reference × target_ratio using NearMiss
#' 3. Upsamples classes below reference ÷ target_ratio using Borderline-SMOTE
#'    (falls back to regular SMOTE if Borderline-SMOTE fails)
#'
#' With skip = TRUE (default), the sampling is only applied during prep()
#' on training data, not when bake() is called on new data.
#'
#' @export
step_adaptive_sampling <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  target_ratio = 5,
  seed = NULL,
  skip = TRUE,
  id = recipes::rand_id("adaptive_sampling")
) {
  recipes::add_step(
    recipe,
    step_adaptive_sampling_new(
      terms = rlang::enquos(...),
      trained = trained,
      role = role,
      target_ratio = target_ratio,
      seed = seed,
      skip = skip,
      id = id
    )
  )
}

step_adaptive_sampling_new <- function(
  terms,
  role,
  trained,
  target_ratio,
  seed,
  skip,
  id,
  columns = NULL
) {
  recipes::step(
    subclass = "adaptive_sampling",
    terms = terms,
    role = role,
    trained = trained,
    target_ratio = target_ratio,
    seed = seed,
    skip = skip,
    id = id,
    columns = columns
  )
}

#' @exportS3Method recipes::prep
prep.step_adaptive_sampling <- function(x, training, info = NULL, ...) {
  col_name <- recipes::recipes_eval_select(x$terms, training, info)
  recipes::check_type(training[, col_name], types = c("factor", "string"))

  step_adaptive_sampling_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    target_ratio = x$target_ratio,
    seed = x$seed,
    skip = x$skip,
    id = x$id,
    columns = col_name
  )
}

#' @exportS3Method recipes::bake
bake.step_adaptive_sampling <- function(object, new_data, ...) {
  col_name <- object$columns

  if (!is.factor(new_data[[col_name]])) {
    new_data[[col_name]] <- as.factor(new_data[[col_name]])
  }

  class_counts <- table(new_data[[col_name]])
  n_classes <- length(class_counts)

  reference_quantile <- compute_reference_quantile(n_classes)
  reference_count <- as.numeric(stats::quantile(
    class_counts,
    reference_quantile
  ))
  target_max <- reference_count * object$target_ratio
  target_min <- reference_count / object$target_ratio

  classes_to_downsample <- names(class_counts[class_counts > target_max])
  classes_to_upsample <- names(class_counts[class_counts < target_min])

  result_data <- new_data

  result_data <- apply_downsampling(
    result_data,
    col_name,
    classes_to_downsample,
    class_counts,
    target_max,
    object$seed
  )

  result_data <- apply_upsampling(
    result_data,
    col_name,
    classes_to_upsample,
    target_min,
    object$seed
  )

  result_data
}

compute_reference_quantile <- function(n_classes) {
  if (n_classes <= 10) {
    0.5
  } else if (n_classes <= 20) {
    0.6
  } else {
    0.65
  }
}

apply_downsampling <- function(
  data,
  col_name,
  classes_to_downsample,
  class_counts,
  target_max,
  seed
) {
  if (length(classes_to_downsample) == 0) {
    return(data)
  }

  max_count <- max(class_counts[classes_to_downsample])
  under_ratio <- max_count / target_max

  if (!is.null(seed)) {
    set.seed(seed)
  }

  themis:::nearmiss(
    df = data,
    var = col_name,
    k = 5,
    under_ratio = under_ratio
  )
}

apply_upsampling <- function(
  data,
  col_name,
  classes_to_upsample,
  target_min,
  seed
) {
  if (length(classes_to_upsample) == 0) {
    return(data)
  }

  updated_counts <- table(data[[col_name]])
  max_count <- max(updated_counts)
  over_ratio <- target_min / max_count

  if (over_ratio >= 1) {
    return(data)
  }

  min_class_size <- min(updated_counts[classes_to_upsample])
  neighbors_to_use <- min(5, max(1, min_class_size - 1))

  if (!is.null(seed)) {
    set.seed(seed)
  }

  tryCatch(
    {
      themis:::bsmote(
        df = data,
        var = col_name,
        k = neighbors_to_use,
        over_ratio = over_ratio,
        all_neighbors = FALSE
      )
    },
    error = function(e) {
      tryCatch(
        {
          themis:::smote(
            df = data,
            var = col_name,
            k = neighbors_to_use,
            over_ratio = over_ratio
          )
        },
        error = function(e2) {
          warning("Both BSMOTE and SMOTE failed. Returning original data.")
          data
        }
      )
    }
  )
}

#' @exportS3Method base::print
print.step_adaptive_sampling <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- paste0(
    "Adaptive class balancing (target_ratio = ",
    x$target_ratio,
    ")"
  )
  recipes::print_step(
    tr_obj = x$terms,
    untr_obj = x$terms,
    trained = x$trained,
    title = title,
    width = width
  )
}

#' @exportS3Method generics::tidy
tidy.step_adaptive_sampling <- function(x, ...) {
  res <- tibble::tibble(
    terms = if (recipes::is_trained(x)) {
      x$columns
    } else {
      recipes::sel2char(x$terms)
    },
    target_ratio = x$target_ratio,
    id = x$id
  )
  res
}

#' @exportS3Method recipes::required_pkgs
required_pkgs.step_adaptive_sampling <- function(x, ...) {
  c("themis", "GenreDiscourseAnalysis")
}

#' Create a target_ratio dials parameter
#' @param range Numeric vector of length 2 with min and max values
#' @param trans Transformation to apply (NULL for identity)
#' @return A dials parameter object
#' @export
target_ratio <- function(range = c(1, 20), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(target_ratio = "Target Class Ratio"),
    finalize = NULL
  )
}

#' @exportS3Method tune::tunable
tunable.step_adaptive_sampling <- function(x, ...) {
  tibble::tibble(
    name = "target_ratio",
    call_info = list(list(
      pkg = "GenreDiscourseAnalysis",
      fun = "target_ratio"
    )),
    source = "recipe",
    component = "step_adaptive_sampling",
    component_id = x$id
  )
}
