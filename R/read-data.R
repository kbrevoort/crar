#' Assemble Disclosure D1-1 Table
#'
#' This function reads in all available CRA data and assembles
#' a joint tibble with the available data.
#' @param geog The geography (or report_level) for which the data
#' are to be pulled. See the factor levels for that variable for
#' options.
#' @return A tibble of data
#' @importFrom purrr map_df
#' @importFrom dplyr filter
assemble_disclosure_d11 <- function(geog = 'County Total') {
  list.files(path = 'data/zipped', pattern = 'discl.zip') %>%
    substr(1, 2) %>%
    unique() %>%
    as.numeric() %>%
    purrr::map_df(import_disclosure_file_d11) %>%
    filter(report_level == geog)
}

#' Import Transmittal File
#'
#' This function imports the transmittal file for each year of CRA
#' data.
#' @param year The year of data to be read (can be 2- or 4-digit years)
#' @return A tibble of data
#' @importFrom readr read_fwf fwf_widths cols
#' @importFrom dplyr mutate
import_transmittal_file <- function(year) {
  zip_name <- sprintf('data/zipped/%02dexp_trans.zip', year %% 100)
  file_list <- unzip(zip_name, list = TRUE)

  in_chr <- unzip(zip_name, files = file_list$Name[[1]]) %>%
    readr::read_lines(progress = FALSE) %>%
    stringr::str_replace('\xb6', 'O') %>%  # This code appears in at least one archive
    stringr::str_pad(width = 152, side = 'right')

  tibble::tibble(respondent = to_numeric(substr(in_chr, 1, 10)),
                 agency = factor_agency(substr(in_chr, 11, 11)),
                 year = to_numeric(substr(in_chr, 12, 15)),
                 respondent_name = substr(in_chr, 16, 45),
                 respondent_address = substr(in_chr, 46, 85),
                 respondent_city = substr(in_chr, 86, 110),
                 respondent_state = substr(in_chr, 111, 112),
                 respondent_zip = substr(in_chr, 113, 122),
                 tax_id = substr(in_chr, 123, 132),
                 id_rssd = stringr::str_trim(substr(in_chr, 133, 142)),
                 assets = to_numeric(substr(in_chr, 143, 152))) %>%
    mutate(id_rssd = ifelse(id_rssd == '', -1 * respondent, as.numeric(id_rssd)))
}

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

  # Most files use 5 digits for the table name, but some only use 4
  # In those cases with 4, add a blank space
  if (!all(unique(substr(in_tbl$V1, 5, 5)) == ' '))
    in_tbl$V1 <- add_space_at(in_tbl$V1, 5)

  # Some files have a 4-digit metro area and others have 5 digit.
  # This detects a 5-digit metro area and fixes it.
  if (all(unique(substr(in_tbl$V1, 36, 36)) %in% c('Y', 'N', ' ')))
    in_tbl$V1 <- add_space_at(in_tbl$V1, 32)

  # Read in the transmittal data to get a respondent to id_rssd map
  idrssd_map <- import_transmittal_file(year) %>%
    select(respondent, id_rssd)

  ret_dt <- mutate(in_tbl,
                   respondent = to_numeric(substr(V1, 6, 15)),
                   agency = factor_agency(substr(V1, 16, 16)),
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
    left_join(idrssd_map, by = 'respondent') %>%
    select(-V1, -respondent)

  select(ret_dt, c('id_rssd', names(ret_dt)))
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

factor_agency <- function(x) {
  x <- as.character(x)

  factor(x,
         levels = c('1', '2', '3', '4'),
         labels = c('OCC', 'FRB', 'FDIC', 'OTS'))
}

add_space_at <- function(x, n) {
  sprintf('%s %s',
          substr(x, 1, n - 1),
          substring(x, n))
}

