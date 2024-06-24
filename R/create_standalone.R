#' Create a single unified standalione file
#'
#' @param dir The directory where the individual standalone files are stored.
#' @param file_name The name of the single standalone file.
#'
#' @return The function is called for its side effects only.
#' @export
#'
create_standalone <- function(dir = "standalone", file_name = "import-standalone") {
  paths <- list.files(path = dir, full.names = TRUE)
  paths <- normalizePath(paths)

  nms <- gsub("[.]R$", "", basename(paths))
  names(paths) <- nms

  standalones <- vapply(
    paths,
    function(x) {
      sub_section(readLines(x))
    },
    character(1)
  )

  heads <- sprintf("# %s-standalone ", names(standalones))
  wdt <- getOption("width")
  txt0 <- paste0(
    heads,
    strrep("-", wdt - nchar(heads, "width")),
    "\n\n",
    standalones,
    "\n\n\n"
  )

  txt <- paste(txt0, collapse = "\n")

  file <- system.file("R", file_name)
  if (!grepl("[.]R$", file)) {
    file <- paste0(file, ".R")
  }

  cat(txt, sep = "", file = file)
}
