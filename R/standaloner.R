# standalone_urls <- function (repo_spec, ..., ref = NULL, host = NULL, error_call = caller_env()) {
#   json <- gh::gh("/repos/{repo_spec}/contents/{path}", repo_spec = repo_spec,
#                  ref = ref, .api_url = host, path = "R/")
#   names <- vapply(json, getElement, "", "name")
#   urls <- vapply(json, getElement, "", "download_url")
#   urls <- setNames(urls, names)[grepl("^standalone-", names)]
#
#   names(urls) <- gsub("^standalone-|.[Rr]$", "", names(urls))
#   urls
# }
#
# standalone_choose <- function(repo_spec, ..., urls = NULL) {
#
#   match.arg(arg = names(list(...)),
#             choices = names(formals(standalone_urls)),
#             several.ok = TRUE)
#
#   if(is.null(urls)) urls <- standalone_urls(repo_spec = repo_spec, ...)
#
#   choices <- names(urls)
#   if(is.null(choices)) {
#     choices <- gsub("^standalone-|.[Rr]$", "", basename(urls))
#     names(urls) <- choices
#   }
#
#   sel <- select.list(c("ALL", choices), multiple = TRUE)
#   if(tolower(sel) == "all") sel <- choices
#   urls[sel]
# }
#
# standalone_import_urls <- function (urls, ..., dest_dir = "./R", names_pre = NULL) {
#   dest_dir <- file.path(getwd(), basename(dest_dir))
#
#   dest_file <- basename(urls)
#   if(length(names_pre)) dest_file <- gsub("^standalone-$", paste0(names_pre, "-"), x = dest_file)
#
#   dest_path <- file.path(dest_dir, dest_file)
#
#   out <- mapply(download.file, url = urls, destfile = dest_path)
#   invisible(NULL)
# }
#
# standalone_import_all <- function (repo_spec, ..., dest_dir = "./R", names_pre = NULL) {
#   urls <- standalone_urls(repo_spec = repo_spec, ...)
#   standalone_import_urls(urls, dest_dir = dest_dir, names_pre = names_pre)
# }
#
# standalone_import <- function (repo_spec, nms = NULL, ..., dest_dir = "./R", names_pre = NULL) {
#   urls <- standalone_urls(repo_spec = repo_spec, ...)
#   if(is.null(nms)) urls <- standalone_choose(repo_spec = repo_spec, ..., urls = NULL)
#   standalone_import_urls(urls, dest_dir = dest_dir, names_pre = names_pre)
# }
#
# standalone_get_paths <- function (dest_dir = "./R", ..., names_pre = NULL) {
#   if(is.null(names_pre)) names_pre <- "standalone-"
#   dest_dir <- file.path(getwd(), basename(dest_dir))
#   out <- list.files(path = dest_dir,
#                     pattern = paste0("^", names_pre),
#                     full.names = TRUE)
#   if(length(out)) return(out)
#   NULL
# }
#
# standalone_remove_files <- function (dest_dir = "./R", ..., names_pre = NULL) {
#   dest_dir <- file.path(getwd(), basename(dest_dir))
#   paths <- standalone_get_paths(dest_dir = dest_dir, names_pre = names_pre)
#   if(is.null(paths)) {
#     cat("There are no standalone files in:",
#         sprintf("  \"%s\"", dest_dir),
#         sep = "\n")
#   } else {
#     out <- do.call(file.remove, list(paths))
#     if(all(out)) {
#       msg <- sprintf("Success!!! - All standalone files removed from:\n  \"%s\"", dest_dir)
#       cat(msg)
#     }
#   }
#   return(invisible())
# }
