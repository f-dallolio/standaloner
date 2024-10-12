#' Get standalone files standalone urls.
#'
#' @inheritParams usethis::use_standalone
#' @param error_call Defaults to `caller_env()`. Useful for reporting errors.
#'
#' @return A named vector of download urls of standalone files.
#' @export
standalone_urls <- function(repo_spec, ..., ref = NULL, host = NULL, error_call = caller_env()) {
  json <- gh::gh("/repos/{repo_spec}/contents/{path}",
    repo_spec = repo_spec,
    ref = ref, .api_url = host, path = "R/"
  )
  names <- vapply(json, getElement, "", "name")
  urls <- vapply(json, getElement, "", "download_url")
  urls <- setNames(urls, names)[grepl("^standalone-", names)]

  names(urls) <- gsub("^standalone-|.[Rr]$", "", names(urls))
  urls
}

#' Import standalone files.
#'
#' `standalone_import()` allows the choice of individual standalone files.
#'
#' `standalone-import_all()` imports all stadalone files available in the
#' selected rpository.
#'
#' @inheritParams usethis::use_standalone
#' @param file 	Name of standalone file. The ⁠standalone-⁠ prefix and file
#'   extension are optional. If omitted, will allow you to choose from the
#'   standalone files offered by that repo.
#' @param ... elements passed to [`standalone_urls()`][`standalone_urls()`]
#' @param dest_dir 	A string indicating the directory where the downloaded file
#'   is to be saved. Tilde-expansion is performed.
#' @param names_prefix A prefix for the imported file. If `NULL` (default), the
#'   prefix will be "standalone-" (e.g. `standalone-xyz.R`).
#'
#' @return The function returns `NULL` invisibly while saving the selected
#'   standalone files in `dest-dir`.
#' @name standalone-import
NULL
#' @rdname standalone-import
#' @export
standalone_import <- function(repo_spec, file = NULL, ..., dest_dir = "./R", names_prefix = NULL) {
  urls <- standalone_urls(repo_spec = repo_spec, ...)
  if (is.null(file)) urls <- standalone_choose(repo_spec = repo_spec, ..., urls = NULL)
  standalone_import_urls(urls, dest_dir = dest_dir, names_prefix = names_prefix)
}
#' @rdname standalone-import
#' @export
standalone_import_all <- function(repo_spec, ..., dest_dir = "./R", names_prefix = NULL) {
  urls <- standalone_urls(repo_spec = repo_spec, ...)
  standalone_import_urls(urls, dest_dir = dest_dir, names_prefix = names_prefix)
}



standalone_choose <- function(repo_spec, ..., urls = NULL) {
  match.arg(
    arg = names(list(...)),
    choices = names(formals(standalone_urls)),
    several.ok = TRUE
  )

  if (is.null(urls)) urls <- standalone_urls(repo_spec = repo_spec, ...)

  choices <- names(urls)
  if (is.null(choices)) {
    choices <- gsub("^standalone-|.[Rr]$", "", basename(urls))
    names(urls) <- choices
  }

  sel <- select.list(c("ALL", choices), multiple = TRUE)
  if (tolower(sel) == "all") sel <- choices
  urls[sel]
}

standalone_import_urls <- function(urls, ..., dest_dir = "./R", names_prefix = NULL) {
  dest_dir <- file.path(getwd(), basename(dest_dir))

  dest_file <- basename(urls)
  if (length(names_prefix)) dest_file <- gsub("^standalone-$", paste0(names_prefix, "-"), x = dest_file)

  dest_path <- file.path(dest_dir, dest_file)

  out <- mapply(download.file, url = urls, destfile = dest_path)
  invisible(NULL)
}
