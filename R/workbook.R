#' Read a MindWare Workbook
#'
#' @param path file path to workbook
#'
#' @return a list of data frames, as a S3 object
#' @export
#' @importFrom magrittr %>%
read_MW <- function(path){
  workbook <- read_MW_workbook(path)
  workbook_format <- detect_MW_workbook_format(workbook)

  # some hard logic, LOL
  f <-
    if (workbook_format %in% c("BPV", "BPV_Interval")) {tidy_MW_BPV
    } else if (workbook_format %in% "EDA") {tidy_MW_EDA
    } else if (workbook_format %in% c("EMG", "EMG_Interval")) {tidy_MW_EMG
    } else if (workbook_format %in% c("HRV", "HRV_Interval")) {tidy_MW_HRV
    } else if (workbook_format %in% "IMP") {tidy_MW_IMP
    } else if (workbook_format %in% "Startle_EMG") {tidy_MW_Startle_EMG
    } else (stop("Input is not in a known format"))

  workbook <- f(workbook)

  # Preserve attributes, because purrr::map() will discard them
  # see: https://github.com/tidyverse/purrr/issues/104
  workbook_attributes <- attributes(workbook)

  # Convert types & Assign back attributes
    # This is done at last because all previous steps keep data verbatim as "character"
    # as a precaution to possible errors.
    # However, as "character" is more expensive than "numeric", it may be necessary to change this behavior
  workbook <- workbook %>%
    purrr::map(~ .x %>% readr::type_convert(col_types = readr::cols(readr::col_guess()))) %>%
    `attributes<-`(workbook_attributes)
}


#### Internal ####

# Read a MindWare Workbook in Excel format
read_MW_workbook <- function(path){
  # Check if file type is Excel
  `if`(is.na(readxl::excel_format(path)), stop("The input is not an Excel file"))

  sheet_names <- readxl::excel_sheets(path)

  # Read each sheet from workbook
  suppressMessages({
  workbook <- purrr::map(sheet_names,
                  ~ readxl::read_excel(path = path,
                               sheet = .,
                               na = c("", "N/A"),
                               col_names = FALSE,
                               col_types = "text")
  ) %>% magrittr::set_names(sheet_names)

  })

  structure(
    workbook,
    class = c("psyphr_workbook", class(workbook)),
    device_vendor = "MindWare",
    origin_path = path,
    origin_mtime = file.mtime(path)
    )
}

# Detect the workbook format as a string
detect_MW_workbook_format <- function(workbook){
  MW_format_profiles <- readRDS(system.file("extdata/MW_format/MW_format_profiles.rds",
                                            package = "psyphr",
                                            mustWork = TRUE))
  this_workbook_profile <- list(worksheets = workbook %>% rlang::squash() %>% names(),
                                settings = workbook %>% `[[`("Settings") %>% psyphr:::df_to_vector() %>% names()
  )
  names(MW_format_profiles)[purrr::map_lgl(MW_format_profiles, ~ identical(.x, this_workbook_profile))]
}

# Tidy Mindware workbooks
tidy_MW_BPV <- function(workbook){

  # BPV Stats
  workbook[[1]] <- workbook[[1]] %>%
    transpose_convert_colnames()

  # IBI
  workbook[[2]] <- workbook[[2]] %>%
    first_row_to_colnames()

  # Systolic Amplitudes
  workbook[[3]] <- workbook[[3]] %>%
    first_row_to_colnames()

  # Diastolic Amplitudes
  workbook[[4]] <- workbook[[4]] %>%
    first_row_to_colnames()

  # MAP
  workbook[[5]] <- workbook[[5]] %>%
    first_row_to_colnames()

  # HR Power Band Stats
  workbook[[6]] <- workbook[[6]] %>%
    transpose_convert_colnames()

  # BP Power Band Stats
  workbook[[7]] <- workbook[[7]] %>%
    transpose_convert_colnames()

  # BRS Stats
  workbook[[8]] <- workbook[[8]] %>%
    first_row_to_colnames()

  # Interval Stats
  # optional
  has_interval <- length(workbook) == 11

  if (has_interval){
    workbook[[8 + has_interval]] <- workbook[[8 + has_interval]] %>%
      first_row_to_colnames()
  }

  # Editing Stats
  workbook[[9 + has_interval]] <- workbook[[9 + has_interval]] %>%
    transpose_convert_colnames()

  # Settings
  workbook[[10 + has_interval]] <- workbook[[10 + has_interval]] %>%
    transpose_convert_colnames()

  attr(workbook, "format") <- "BPV"

  return(workbook)
}

tidy_MW_EDA <- function(workbook){
  # EDA Stats
  workbook[[1]] <- workbook[[1]] %>%
    transpose_convert_colnames()

  # SCR Stats
  workbook[[2]] <- workbook[[2]] %>%
    first_row_to_colnames()

  # Editing Stats
  workbook[[3]] <- workbook[[3]] %>%
    transpose_convert_colnames()

  # Settings
  workbook[[4]] <- workbook[[4]] %>%
    transpose_convert_colnames()

  attr(workbook, "format") <- "EDA"

  return(workbook)
}

tidy_MW_EMG <- function(workbook){

  # EMG Stats
  workbook[[1]] <- workbook[[1]] %>%
    transpose_convert_colnames()

  # Channel Stats
  workbook[[2]] <- workbook[[2]] %>%
    first_row_to_colnames()

  # Interval Stats
  # optional
  has_interval <- length(workbook) == 5

  if (has_interval){
    workbook[[2 + has_interval]] <- workbook[[2 + has_interval]] %>%
      first_row_to_colnames()
  }

  # Editing Stats
  workbook[[3 + has_interval]] <- workbook[[3 + has_interval]] %>%
    transpose_convert_colnames()

  # Settings
  workbook[[4 + has_interval]] <- workbook[[4 + has_interval]] %>%
    transpose_convert_colnames()

  attr(workbook, "format") <- "EMG"

  return(workbook)
}

tidy_MW_HRV <- function(workbook){
  # HRV Stats
  workbook[[1]] <- workbook[[1]] %>%
    transpose_convert_colnames()

  # IBI
  workbook[[2]] <- workbook[[2]] %>%
    first_row_to_colnames()

  # Power Band Stats
  workbook[[3]] <- workbook[[3]] %>%
    transpose_convert_colnames()

  # Heart Rate Time Series
  workbook[[4]] <- workbook[[4]] %>%
    first_row_to_colnames()

  # Heart Period Power Spectrum
  hr_delta_f <- workbook[[5]][1,1, drop = TRUE]
  workbook[[5]] <- workbook[[5]][2:nrow(workbook[[5]]), ] %>%
    first_row_to_colnames()
  attr(workbook[[5]], "HR Delta F") <- hr_delta_f


  # Respiration Time Series
  workbook[[6]] <- workbook[[6]] %>%
    first_row_to_colnames()

  # Respiration Power Spectrum
  resp_delta <- workbook[[7]][1,1, drop = TRUE]
  workbook[[7]] <- workbook[[7]][2:nrow(workbook[[7]]), ] %>%
    first_row_to_colnames()
  attr(workbook[[7]], "Resp Delta") <- resp_delta

  # Interval Stats
    # optional
  has_interval <- length(workbook) == 10

  if (has_interval){
    workbook[[7 + has_interval]] <- workbook[[7 + has_interval]] %>%
      first_row_to_colnames()
  }

  # Editing Stats
  workbook[[8 + has_interval]] <- workbook[[8 + has_interval]] %>%
    transpose_convert_colnames()

  # Settings
  workbook[[9 + has_interval]] <- workbook[[9 + has_interval]] %>%
    transpose_convert_colnames()

  attr(workbook, "format") <- "HRV"

  return(workbook)
}

tidy_MW_IMP <- function(workbook){
  # Impedance Stats
  workbook[[1]] <- workbook[[1]] %>%
    transpose_convert_colnames()

  # IBI
  workbook[[2]] <- workbook[[2]] %>%
    first_row_to_colnames()

  # Editing Stats
  workbook[[3]] <- workbook[[3]] %>%
    transpose_convert_colnames()

  # Settings
  workbook[[4]] <- workbook[[4]] %>%
    transpose_convert_colnames()

  attr(workbook, "format") <- "IMP"

  return(workbook)
}

tidy_MW_Startle_EMG <- function(workbook){

  # Left eye - Trials
  workbook[[1]] <- workbook[[1]] %>%
    first_row_to_colnames()

  # Left eye - Conditions
  workbook[[2]] <- workbook[[2]] %>%
    first_row_to_colnames()

  # Left ear - Trials
  workbook[[3]] <- workbook[[3]] %>%
    first_row_to_colnames()

  # Left ear - Conditions
  workbook[[4]] <- workbook[[4]] %>%
    first_row_to_colnames()

  # Right ear - Trials
  workbook[[5]] <- workbook[[5]] %>%
    first_row_to_colnames()

  # Right ear - Conditions
  workbook[[6]] <- workbook[[6]] %>%
    first_row_to_colnames()

  # Settings
  workbook[[7]] <- workbook[[7]] %>%
    transpose_convert_colnames()

  attr(workbook, "format") <- "Startle_EMG"

  return(workbook)
}

# Turn a data frame into vector
# Data frame's first column as vectors' names, the second column as values
df_to_vector <- function(.data){
  res <- .data[[2]]
  names(res) <- .data[[1]]
  res
}

transpose_convert_colnames <- function(.data) {
  .data %>%
    t() %>%
    first_row_to_colnames() %>%
    tibble::as_tibble()
}

first_row_to_colnames <- function(.data){
  colnames(.data) <- .data[1,]
  .data[-1,,drop = FALSE]
}

# Bare Name of a File, w.o. Path or Extension
bare_name <- function(path){
  gsub("(\\.+)(?!.*\\1).*$", "", basename(path), perl = TRUE)
}