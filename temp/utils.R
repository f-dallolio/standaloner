parse_to_list <- function(file = NULL,
                          n = NULL,
                          text = NULL,
                          prompt = NULL,
                          encoding = "unknown",
                          env = parent.frame()){
  x <- parse(file = file,
             n = n,
             text = text,
             prompt = prompt,
             keep.source = FALSE,
             srcfile = NULL,
             encoding = encoding)
  assign_call <- vapply(x, function(x) identical(x[[1]], quote(`<-`)), logical(1))
  x_assign <- x[assign_call]
  nms <- vapply(x_assign, function(x) format(x[[2]]), character(1))
  out <- lapply(
    x_assign,
    function(x, env = parent.frame()) {
      fn <- eval(x[[3]], env)
    }
  )
  names(out) <- nms
  out
}


is_assignment_call <- function(x){
  identical(as.list(x)[[1]], quote(`<-`))
}
assignment_values <- function(x){
  id <- vapply(x, is_assignment_call, logical(1))

  if(is_assignment_call(x)) {
    list(name = deparse(x[[2]]))
  }
  x[[1]] <- deparse(x[[1]])
  new_names(x, 1, ".fn")
}









