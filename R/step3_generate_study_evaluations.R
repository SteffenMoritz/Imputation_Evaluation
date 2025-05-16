#' Evaluate Imputation Simulation Runs Across Multiple Algorithms
#'
#' Calculates the mean absolute error (MAE) between each imputed dataset
#' and the original complete data—**only** at the entries newly made missing.
#' Supports one or more `imputations` objects (e.g. from different algorithms)
#' and returns per‐algorithm MAEs as well as combined metrics.
#'
#' @param imps_obj  A single object of class \sQuote{imputations} or a **list**
#'                  of such objects (e.g. different algorithms).
#' @param original  A complete \code{data.frame} used as the base for simulation
#'                  (must contain no \code{NA}s).
#'
#' @return An object of class \sQuote{evaluation} with components:
#'   \describe{
#'     \item{metadata}{List with
#'       \describe{
#'         \item{alg_names}{Character vector of algorithm names.}
#'         \item{n_runs}{Integer vector of runs per algorithm.}
#'         \item{total_runs}{Total number of runs across all algorithms.}
#'       }
#'     }
#'     \item{results}{Named list (one element per algorithm), each containing:
#'       \describe{
#'         \item{mae}{Named list of run‑level MAEs (mae1, \dots).}
#'         \item{mean_mae}{Mean of those MAEs.}
#'         \item{var_mae}{Variance of those MAEs.}
#'       }
#'     }
#'     \item{combined}{List with
#'       \describe{
#'         \item{mean_mae}{Mean of *all* MAEs across all algorithms.}
#'         \item{var_mae}{Variance of *all* MAEs.}
#'       }
#'     }
#'   }
#' @export
evaluate_simulation_runs <- function(imps_obj, original) {
  # 1. validate original
  if (!is.data.frame(original)) {
    stop("`original` must be a data.frame.")
  }
  # 2. collect imputations objects into a list
  if (inherits(imps_obj, "imputations")) {
    imps_list <- list(imps_obj)
  } else if (is.list(imps_obj) &&
             all(sapply(imps_obj, function(x) inherits(x, "imputations")))) {
    imps_list <- imps_obj
  } else {
    stop("`imps_obj` must be an 'imputations' object or a list of them.")
  }
  # 3. for each algorithm, compute run‑level MAEs
  alg_names <- vapply(imps_list,
                      function(x) x$metadata$alg_name,
                      character(1))
  n_algs    <- length(imps_list)
  results   <- vector("list", n_algs)
  names(results) <- alg_names
  all_maes  <- numeric(0)
  n_runs_vec <- integer(n_algs)
  
  for (j in seq_along(imps_list)) {
    obj    <- imps_list[[j]]
    sims   <- obj$simulations   # sim1…simN
    imps   <- obj$imputations   # imp1…impN
    n_runs <- length(imps)
    n_runs_vec[j] <- n_runs
    
    maes <- numeric(n_runs)
    for (i in seq_len(n_runs)) {
      sim_df <- sims[[paste0("sim", i)]]
      imp_df <- imps[[paste0("imp", i)]]
      # positions where we *introduced* missingness:
      idx     <- is.na(sim_df) & !is.na(original)
      diffs   <- abs(imp_df[idx] - original[idx])
      maes[i] <- mean(diffs, na.rm = TRUE)
    }
    
    results[[j]] <- list(
      mae      = setNames(as.list(maes), paste0("mae", seq_len(n_runs))),
      mean_mae = mean(maes, na.rm = TRUE),
      var_mae  = var(maes, na.rm = TRUE)
    )
    all_maes <- c(all_maes, maes)
  }
  
  combined <- list(
    mean_mae = mean(all_maes, na.rm = TRUE),
    var_mae  = var(all_maes, na.rm = TRUE)
  )
  
  out <- list(
    metadata = list(
      alg_names  = alg_names,
      n_runs     = n_runs_vec,
      total_runs = sum(n_runs_vec)
    ),
    results  = results,
    combined = combined
  )
  class(out) <- "evaluation"
  out
}

#' @export
print.evaluation <- function(x, ...) {
  cat(">> Evaluation of Imputation Simulations Across Algorithms <<\n")
  cat("Algorithms:     ", paste(x$metadata$alg_names, collapse = ", "),   "\n")
  cat("Runs per alg.:  ", paste(x$metadata$n_runs,    collapse = ", "),   "\n")
  cat("Total runs:     ", x$metadata$total_runs,      "\n\n")
  
  for (alg in names(x$results)) {
    res <- x$results[[alg]]
    cat("─ Algorithm:", alg, "\n")
    cat("  MAEs:      ", paste0(format(unlist(res$mae), digits=4), collapse = ", "), "\n")
    cat(sprintf("  Mean MAE:  %.4f   Var MAE: %.6f\n\n",
                res$mean_mae, res$var_mae))
  }
  
  cat(">> Combined (all runs) <<\n")
  cat(sprintf("Mean MAE: %.4f\n", x$combined$mean_mae))
  cat(sprintf("Var MAE : %.6f\n", x$combined$var_mae))
  invisible(x)
}
