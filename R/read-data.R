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

  in_chr <- unz(zip_name, file_list$Name[[1]]) %>%
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
    mutate(id_rssd = ifelse(id_rssd == '',
                            -1 * ((respondent * 10) + as.numeric(agency)),
                            as.numeric(id_rssd)))
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
    in_tbl <- unz(zip_name, file_list$Name[[1]]) %>%
      readr::read_lines(progress = FALSE) %>%
      tibble::tibble(V1 = .) %>%
      filter(substr(V1, 1, 4) == 'D1-1')
  } else {
    in_tbl <- filter(file_list, stringr::str_detect(Name, 'D11.dat')) %>%
      pull(Name) %>%
      unz(zip_name, filename = .) %>%
      readr::read_lines(progress = FALSE) %>%
      tibble::tibble(V1 = .)
  }

  w <- get_widths(year) # Widths by file year
  s <- cumsum(c(1, w))[1:length(w)] # starting point for each variable
  e <- cumsum(w)                    # end point for each variable

  # Read in the transmittal data to get a respondent to id_rssd map
  idrssd_map <- import_transmittal_file(year) %>%
    select(respondent, id_rssd)

  ret_dt <- mutate(in_tbl,
                   respondent = to_numeric(substr(V1, s[2], e[2])),
                   agency = factor_agency(substr(V1, s[3], e[3])),
                   year = to_numeric(substr(V1, s[4], e[4])),
                   loan_type = substr(V1, s[5], e[5]),
                   action = substr(V1, s[6], e[6]),
                   state = to_numeric(substr(V1, s[7], e[7])),
                   county = to_numeric(substr(V1, s[8], e[8])),
                   metro_area = to_numeric(substr(V1, s[9], e[9])),
                   assessment_area = to_numeric(substr(V1, s[10], e[10])),
                   partial_county_fl = to_logical(substr(V1, s[11], e[11])),
                   split_county_fl = to_logical(substr(V1, s[12], e[12])),
                   population_f = factor(substr(V1, s[13], e[13]),
                                         levels = c('S', 'L', ' '),
                                         labels = c('<= 500,000 in population',
                                                    '>500,000 in population',
                                                    'Total')),
                   income_group = factor(as.numeric(substr(V1, s[14], e[14])),
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
                   report_level = factor(as.numeric(substr(V1, s[15], e[15])),
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
                   num_le100k = as.numeric(substr(V1, s[16], e[16])),
                   amt_le100k = as.numeric(substr(V1, s[17], e[17])),
                   num_100to250k = as.numeric(substr(V1, s[18], e[18])),
                   amt_100to250k = as.numeric(substr(V1, s[19], e[19])),
                   num_250to1m = as.numeric(substr(V1, s[20], e[20])),
                   amt_250to1m = as.numeric(substr(V1, s[21], e[21])),
                   num_sbl = as.numeric(substr(V1, s[22], e[22])),
                   amt_sbl = as.numeric(substr(V1, s[23], e[23])),
                   num_affiliate = as.numeric(substr(V1, s[24], e[24])),
                   amt_affiliate = as.numeric(substr(V1, s[25], e[25]))) %>%
    left_join(idrssd_map, by = 'respondent') %>%
    select(-V1)

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

get_widths <- function(year) {
  short_year <- year %% 100L

  if (year == 96L) {
    ret_val <- c(4, 10, 1, 4, 1, 1, 2, 3, 4, 4,
                 1, 1, 1, 3, 3, 6, 8, 6, 8, 6,
                 8, 6, 8, 6, 8)
  } else if (year %in% (c(1997:2003) %% 100)) {
    ret_val <- c(5, 10, 1, 4, 1, 1, 2, 3, 4, 4,
                 1, 1, 1, 3, 3, 6, 8, 6, 8, 6,
                 8, 6, 8, 6, 8)
  } else {
    ret_val <- c(5, 10, 1, 4, 1, 1, 2, 3, 5, 4,
                 1, 1, 1, 3, 3, 10, 10, 10, 10, 10,
                 10, 10, 10, 10, 10)
  }

  ret_val
}

