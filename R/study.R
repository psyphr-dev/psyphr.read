#' Read a study
#'
#' @param path path to a study directory; a character string
#' @param ... dot-dot-dot
#'
#' @return a data frame; psyphr study S3 object
#' @export
study <- function(path, ...){
  # this is now just a placeholder function
  # its job is to detect vendor and dispatch corresponding function
}


#' Read a study from a directory
#'
#' @param path path to a study directory; a character string
#' @param structure directory structure; "flat" or "recursive"
#' @param stash whether to cache data on disk; logical
#' @param stash_dir_path cache directory; a character string
#'
#' @return a data frame; psyphr study S3 object
#' @export
study_mindware <- function(path, structure = "flat", stash = FALSE, stash_dir_path = tempdir()){
  file_paths <- list.files(path = path, pattern = "\\.xlsx$", full.names = TRUE, recursive = TRUE)
  file_ids <-
    dplyr::case_when(
      structure == "flat" ~
        file_paths %>% bare_name() %>% stringr::str_split(pattern = "_"),
      structure == "recursive" ~
        list.files(path = path, pattern = "\\.xlsx$", recursive = TRUE) %>%
        gsub("\\..*$", "", .) %>%
        stringr::str_split(pattern = "/")
    )


  assertthat::assert_that(length(unique(lapply(file_ids, length))) == 1,
                          msg = "All file names must follow identical schema.")

  study <- file_ids %>%
    dplyr::bind_cols() %>%
    t() %>%
    tibble::as_tibble() %>%
    dplyr::rename_all(
      ~ .x %>% stringr::str_replace("V", "id_")
    )

  if (stash) {
    study$stash <-  as.list(vector(length = nrow(study)))
    for (i in 1:nrow(study)) {
      study$stash[[i]] <- stash(mindware(file_paths[i]),
                                dir_path = stash_dir_path,
                                file_name = paste(file_ids[[i]], collapse = "_"))
    }

    study$format <- vector(length = nrow(study))
    for (i in 1:nrow(study)) {
      study$format[[i]] <-  attr(study$stash[[i]](), "format")
    }
  } else {
    study <- study %>%
      dplyr::mutate(
        data = file_paths %>% purrr::quietly(purrr::map)(mindware) %>% `[[`("result"),
        format = .data$data %>% purrr::map( ~ attributes(.x)["format"]) %>% unlist()
      )
  }
  structure(study, class = c("psyphr_study", class(study)))
}

#' Print a Summary of a Psyphr Study
#'
#' @param x a psyphr study object
#' @param ... dot-dot-dot
#'
#' @return NULL
#' @export
print.psyphr_study <- function(x, ...){
  #


}


pick_sheets <- function(study, elements){
  purrr::map(elements,
      ~ purrr::map(study$data, .)
  ) %>%
    purrr::set_names(elements) %>%
    tibble::as_tibble()
}

#' Unnest Data in a Study
#'
#' @param study a psyphr study
#'
#' @return a data frame; psyphr study S3 object
#' @export
#'
unnest_data <- function(study){
  MW_format_profiles <- readRDS(system.file("extdata/MW/MW_format_profiles.rds", package = "psyphr.read"))
  study_formats <- intersect(unique(study$format), names(MW_format_profiles))
  study_sheet_names <- unique(unlist(MW_format_profiles[study_formats]))
  sheets <- pick_sheets(study, study_sheet_names)

  study %>%
    dplyr::select(-.data$data) %>%
    dplyr::bind_cols(sheets)
}


#' Flatten a Study with a Recursive File Structure
#'
#' @param origin a character string; origin path
#' @param dest a character string; destination path
#' @param delim a character string; file name delimiter
#'
#' @export
flatten_study_dir <- function(origin, dest, delim = "_"){
  `if`(dir.exists(dest),NULL,dir.create(dest))

  origin_file_name <- list.files(origin, recursive = TRUE)
  `if`(any(grepl(delim, origin_file_name)), stop("path should not contain delimiter"))
  dest_file_name <- gsub("/", delim, origin_file_name)

  origin_file_path <- file.path(origin, origin_file_name)
  dest_file_path <- file.path(dest, dest_file_name)

  for (i in seq_along(origin_file_path)) {
    file.copy(origin_file_path[i], dest_file_path[i])
  }
}

#' Lift a Study with a Flat File Structure
#'
#' @param origin a character string; origin path
#' @param dest a character string; destination path
#' @param delim a character string; file name delimiter
#'
#' @export
lift_study_dir <- function(origin, dest, delim = "_"){
  `if`(dir.exists(dest),NULL,dir.create(dest))

  origin_file_name <- list.files(origin, recursive = FALSE)
  dest_file_name <- gsub(delim, "/", origin_file_name)

  origin_file_path <- file.path(origin, origin_file_name)
  dest_file_path <- file.path(dest,dest_file_name)

  for (i in seq_along(origin_file_path)) {
    if (!dir.exists(dirname(dest_file_path[i]))) {
      dir.create(dirname(dest_file_path[i]), recursive = TRUE)
    }
    file.copy(origin_file_path[i], dest_file_path[i])
  }
}

