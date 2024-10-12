#' Manage imported standalone files
#'
#' @param dest_dir A string indicating the directory where the downloaded file
#'   is to be saved. Tilde-expansion is performed.
#' @param ... elements passed to [`standalone_urls()`][`standalone_urls()`]
#' @param names_prefix A prefix for the imported file. If `NULL` (default), the
#'   prefix will be `"standalone-"` (e.g. `standalone-xyz.R`).
#'
#' @return
#' `standalone_paths_get()` returns the paths of the standalone files in `dest_dir`.
#'
#' `standalone_paths_remove()` removes all standalone paths from `dest_dir`.
#' @name standalone-paths
NULL
#' @rdname standalone-paths
#' @export
standalone_paths_get <- function (dest_dir = "./R", ..., names_prefix = NULL) {
  if(is.null(names_pre)) names_pre <- "standalone-"
  dest_dir <- file.path(getwd(), basename(dest_dir))
  out <- list.files(path = dest_dir,
                    pattern = paste0("^", names_prefix),
                    full.names = TRUE)
  if(length(out)) return(out)
  NULL
}
#' @rdname standalone-paths
#' @export
standalone_paths_remove <- function (dest_dir = "./R", ..., names_prefix = NULL) {
  dest_dir <- file.path(getwd(), basename(dest_dir))
  paths <- standalone_paths_get(dest_dir = dest_dir, names_pre = names_prefix)
  if(is.null(paths)) {
    cat("There are no standalone files in:",
        sprintf("  \"%s\"", dest_dir),
        sep = "\n")
  } else {
    out <- do.call(file.remove, list(paths))
    if(all(out)) {
      msg <- sprintf("Success!!! - All standalone files removed from:\n  \"%s\"", dest_dir)
      cat(msg)
    }
  }
  return(invisible())
}
