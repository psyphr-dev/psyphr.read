# This is a regular R script
# It analyzes the sample data and generates its profile for format identification

require(tidyverse)

MW_file_paths <- list.files("inst/extdata/MW",
                            pattern = ".xlsx$",
                            full.names = TRUE)


MW_format_names <- psyphr.read:::bare_name(MW_file_paths)
MW_workbooks <- map(MW_file_paths, psyphr.read:::MW_workbook)


MW_worksheets_names <-
  MW_workbooks %>%
  map(~ rlang::squash(.x) %>% names())
MW_Settings_fields <-
  MW_workbooks %>%
  map(~ `[[`(.x, "Settings") %>% psyphr.read:::df_to_vector() %>% names())

MW_format_profiles <-
  map2(MW_worksheets_names,
       MW_Settings_fields,
       ~list(worksheets = .x
             # settings = .y)
       )) %>%
  `names<-`(MW_format_names)

saveRDS(MW_format_profiles, file = "inst/extdata/MW/MW_format_profiles.rds")

