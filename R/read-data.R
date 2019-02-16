#' Import Disclosure File D1-1
#'
#' This function will read in the D1-1 disclosure files
#' for a given calendar year.
#' @param year A 4-digit year
#' @return A tibble of disclosure data
#' @importFrom readr read_lines
#' @importFrom tibble tibble
#' @importFrom dplyr filter mutate
import_disclosure_file_d11 <- function(year) {
  if (!is.numeric(year))
    stop('Must supply a numeric value to import_disclosure_file_d11')
  if (length(year) != 1L)
    stop('year supplied to import_disclosure_file_d11 must be a scalar')

  zip_name <- sprintf('data/zipped/%02dexp_discl.zip', year %% 100)

  # Some zip files are archives of multiple files and
  # others have all of the data in a single file. This
  # determines which one and returns a comparable
  # character vector of file information.
  file_list <- unzip(zip_name, list = TRUE)
  if (length(file_list$Name) == 1) {
    in_tbl <- unzip(zip_name, files = file_list$Name[[1]]) %>%
      readr::read_lines() %>%
      tibble::tibble(V1 = .) %>%
      filter(substr(V1, 1, 4) == 'D1-1')
  } else {
    in_tbl <- filter(file_list, stringr::str_detect(Name, 'D11.dat')) %>%
      pull(Name) %>%
      unzip(zip_name, files = .) %>%
      readr::read_lines() %>%
      tibble::tibble(V1 = .)
  }

  # Some files have a 4-digit metro area and others have 5 digit.
  # This detects a 5-digit metro area and fixes it.
  if (all(unique(substr(in_tbl$V1, 36, 36)) %in% c('Y', 'N', ' '))) {
    in_tbl$V1 <- sprintf('%s %s',
                         substr(in_tbl$V1, 1, 31),
                         substring(in_tbl$V1, 32))
  }

  mutate(in_tbl,
         respondent = substr(V1, 6, 15),
           agency = factor(substr(V1, 16, 16),
                           levels = c('1', '2', '3', '4'),
                           labels = c('OCC', 'FRB', 'FDIC', 'OTS')),
           year = to_numeric(substr(V1, 17, 20)),
           loan_type = substr(V1, 21, 21),
           action = substr(V1, 22, 22),
           state = to_numeric(substr(V1, 23, 24)),
           county = to_numeric(substr(V1, 25, 27)),
           metro_area = to_numeric(substr(V1, 28, 32)),
           assessment_area = to_numeric(substr(V1, 33, 36)),
           partial_county_fl = to_logical(substr(V1, 37, 37)),
           split_county_fl = to_logical(substr(V1, 38, 38)),
           population_f = factor(substr(V1, 39, 39),
                                 levels = c('S', 'L', ' '),
                                 labels = c('<= 500,000 in population',
                                            '>500,000 in population',
                                            'Total')),
           income_group = factor(as.numeric(substr(V1, 40, 42)),
                                 levels = c(1:15, 101:106),
                                 labels = c('< 10% of MFI',
                                            '10% to 20% of MFI',
                                            '20% to 30% of MFI',
                                            '30% to 40% of MFI',
                                            '40% to 50% of MFI',
                                            '50% to 60% of MFI',
                                            '60% to 70% of MFI',
                                            '70% to 80% of MFI',
                                            '80% to 90% of MFI',
                                            '90% to 100% of MFI',
                                            '100% to 110% of MFI',
                                            '110% to 120% of MFI',
                                            '>= 120% of MFI',
                                            'Unknown MFI',
                                            'Unknown Tract',
                                            'Low Income',
                                            'Moderate Income',
                                            'Middle Income',
                                            'Upper Income',
                                            'Unknown Income',
                                            'Unknown Tract')),
           report_level = factor(as.numeric(substr(V1, 43, 45)),
                                 levels = c(4, 6, 8, 10, 20, 30, 40, 50, 60),
                                 labels = c('Total Inside & Outside Assessment Area (AA)',
                                            'Total Inside AA',
                                            'Total Outside AA',
                                            'State Total',
                                            'Total Inside AA in State',
                                            'Total Outside AA in State',
                                            'County Total',
                                            'Total Inside AA in County',
                                            'Total Outside AA in County')),
           num_le100k = as.numeric(substr(V1, 46, 51)),
           amt_le100k = as.numeric(substr(V1, 52, 59)),
           num_100to250k = as.numeric(substr(V1, 60, 65)),
           amt_100to250k = as.numeric(substr(V1, 66, 73)),
           num_250to1m = as.numeric(substr(V1, 74, 79)),
           amt_250to1m = as.numeric(substr(V1, 80, 87)),
           num_sbl = as.numeric(substr(V1, 88, 93)),
           amt_sbl = as.numeric(substr(V1, 94, 101)),
           num_affiliate = as.numeric(substr(V1, 102, 107)),
           amt_affiliate = as.numeric(substr(V1, 108, 115))) %>%
    select(-V1)
}

#' Convert Character to Numeric
#'
#' This function converts a character vector to a numeric
#' value. Blank or 'NA' values are first converted to missing
#' values.
#' @param x A character vector
#' @return A numeric vector
#' @importFrom stringr str_trim
to_numeric <- function(x) {
  if (!is.character(x))
    stop('Non-character vector supplied to to_numeric')

  ifelse(stringr::str_trim(x) %in% c('', 'NA'),
         as.character(NA),
         x) %>%
    as.numeric()
}

#' Convert Character to Logical
#'
#' This function converts a character vector into a
#' logical value, where 'Y' is TRUE, 'N' is FALSE, and
#' everything else is set to NA.
#' @param x A character vector
#' @return A logical vector
to_logical <- function(x) {
  if (!is.character(x))
    stop('Non-character submitted to to_logical')

  ifelse(x %in% c('Y', 'N'),
         x == 'Y',
         as.logical(NA))
}




