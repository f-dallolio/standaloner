#' Select standalone files from another repo
#'
#' @inherit usethis::use_standalone description
#'
#' @inheritParams usethis::use_standalone
#' @param files 	Name of standalone file. The ⁠standalone-⁠ prefix and file
#'   extension are optional. If omitted, will allow you to choose from the
#'   standalone files offered by that repo. Unlike [usethis::use_standalone], selecting multiple standalone files from the same repo is allowed.
#' @param out_dir The directory where to download and store the individual "standalone" files. If the folder does not exist, it is automatically created. It defaults to the folder "standalone" in the root directory of the package found with [here::here].
#' @param error_call The execution environment of a currently running function,
#'   e.g. call = caller_env(). The corresponding function call is retrieved and
#'   mentioned in error messages as the source of the error.
#'
#' @return This function is mainly used for its side effects. That is, downloading and storing individual `"standalone"` files.
#' @export
#'
select_standalone <- function (repo_spec,
                               files = NULL,
                               out_dir = "standalone",
                               ref = NULL,
                               host = NULL,
                               error_call = caller_env()) {

  json <- gh::gh("/repos/{repo_spec}/contents/{path}", repo_spec = repo_spec,
                 ref = ref, .api_url = host, path = "R/")
  urls <- vapply(json, getElement, character(1), "download_url")

  names <- vapply(json, getElement, character(1), "name")
  id <- grepl("^standalone-", names)

  urls <- urls[id]
  names <- names[id]

  choices <- gsub("^standalone-|.[Rr]$", "", names)

  if (length(choices) == 0) {
    cli::cli_abort("No standalone files found in {repo_spec}.",
                   call = error_call)
  }

  if (!rlang::is_interactive()) {
    cli::cli_abort(c("`files` is absent, but must be supplied.",
                     i = "Possible options are {.or {choices}}."), call = error_call)
  }

  if(is.null(files)){
    choice <- utils::select.list(choices = choices,
                                 title = "Which standalone file do you want to use (0 to exit)?",
                                 multiple = TRUE)
    pos <- match(choice, choices)
  } else {
    choice <- match(files, choices)
    na_choice <- choice[is.na(choice)]
    if (length(na_choice) > 0) {
      msg <- "{.arg {na_choice}} are not among standalone files."
      cli::cli_warn(msg, call = error_call)
    }
    pos <- na.omit(choice)
  }

  if (length(pos) == 0) {
    cli::cli_abort("Selection cancelled", call = error_call)
  }

  standalone_names <- gsub("^standalone-|[.].+$" , "", names[pos])
  standalone_urls <- urls[pos]

  out_dir <- system.file(here::here, out_dir)
  if(!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

  standalone_paths <- sprintf("%s/%s.R", out_dir, standalone_names)

  invisible(mapply(download.file, standalone_urls, standalone_paths))
  out <- standalone_paths
  names(out) <- standalone_names

  out

}



sub_section <- function(x){#}, nms =){

  nocov_pos <- grep("^.*nocov", x)
  nocov_1 <-nocov_pos[[1]]
  nocov_2 <- nocov_pos[[2]]
  nocov_seq <- seq(nocov_1 + 1, nocov_2 - 1)

  code <- x[nocov_seq]
  code_last <- code[length(code)]
  while(code_last == ""){
    code <- code[-length(code)]
    code_last <- code[length(code)]
  }

  pos_section <- grep("^#+[[:space:]]*.+[-]{4, }", code)

  if(length(pos_section) > 0) {
    section <- code[pos_section]
    new_section <- paste0("#", section)
    new_section <- strtrim(new_section, nchar(section, "width"))

    code[pos_section] <- new_section
  }

  x <- c(x[1 : nocov_1],
         code,
         "",
         x[nocov_2 : length(x)])

  paste(x, collapse = "\n")

}
