#' Mindware Sample Study
#'
#' @return path to sample MindWare study; character string
#' @export
#'
mindware_sample_study_1 <- function(){
  system.file("extdata/MW/Pilot1", package = "psyphr.read")
}

#' Recursively Print out the Structure of a List
#'
#' @references https://gist.github.com/iqis/926c189da89a32b9702c340c42401c67
#'
#' @param x a list or environment
#' @param limit recursive level limit
#' @param class show class of object
#' @param unclass expand list-like objects
#' @param compact don't show unnamed elements
#' @param rich show length & memory size of element
#' @param all.names show hidden objects in an environment
#'
#' @return character string of the screen output
#' @export
#'
tree <- function(x = .GlobalEnv, limit = 5, class = TRUE, unclass = FALSE, compact = TRUE, rich = FALSE, all.names = FALSE){
  if (!(mode(x) %in% c("list", "environment") & length(x) > 0)) {
    stop("Please provide a non-empty list or environment")
  }

  if (length(ls(x)) == 0) return(NULL)

  element_profile <- function(name, body){
    c(
      "$", name,
      `if`(class, c(" (", paste(class(body), collapse = ", "), ") ")), # class(es)
      `if`(rich,c(
        "|",
        ifelse(is.null(dim(body)), # length or dim
               length(body),
               paste(dim(body),
                     collapse = " by ")),
        ", ",
        format(utils::object.size(body), # size (in memory)
               units = "auto",
               standard = "SI"),
        "|"
      ))
    )
  }

  traverse <- function(x, level = 1, branches_end = c()){
    element_names <- # vec of names at current level
      if (is.list(x)) {
        if (!is.null(names(x))) {
          names(x)
        } else {
          rep("", length(x))
        }
      } else if (is.environment(x)) {
        objects(x, all.names = all.names)
      }

    for (i in 1:length(element_names)) {
      branches_end[level] <- i == length(element_names) # logical vec, branch ended in all levels?
      element <- if (is.list(x)) { # element itself
        x[[i]]
      } else if (is.environment(x)) {
        get(element_names[i], x)
      }

      indent <- if (level == 1) {
        NULL
      } else {
        sapply(branches_end[1:(level - 1)],
               function(x) `if`(x, "    ", "\u2502    "))
      }

      # print line
      if (compact & is.null(names(x))) { # compact when no-name
        NULL
      } else {
        cat(
          # rep("    ", level - 1),
          indent,
          `if`(branches_end[level], "\u2514\u2500\u2500 ", "\u251C\u2500\u2500 "), # branch
          element_profile(name = element_names[i],
                          body = element),
          "\n",
          sep = ""
        )
      }

      # Next level?
      element_class <- `if`(unclass,
                            mode(element),
                            class(element)
      )
      if (any(element_class %in% c("list", "environment")) & length(element) > 0 & level < limit) {
        traverse(x = element, level = level + 1, branches_end = branches_end)
      }
    }
  }

  catn <- function(...){
    cat(..., sep = "\n")
  }

  out <- utils::capture.output({
    cat(element_profile(name = deparse(substitute(x)),
                        body = x),"\n", sep = "")
    traverse(x)
  })

  catn(out)
  invisible(out)
}
