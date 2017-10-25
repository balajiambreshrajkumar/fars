#' Create a dataframe table from the given csv filename.
#' Should the filename not exist, then, an error is raised.
#' @importFrom readr read_csv
#' @importFrom  dplyr tbl_df
#' @note No progress bar will be shown while reading the file.
#' @param filename The file to read from.
#' @return dplyr tbl_df instance of the file content.
#' @examples \dontrun{
#' csv <- fars_read('avalid_filename.csv')
#' > class(csv)
#' [1] "tbl_df"     "tbl"        "data.frame"
#' invalid <- fars_read('does_not_exist.csv')
#' Error in fars_read("does_not_exist.csv") :
#'  file 'does_not_exist.csv' does not exist
#' }
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Create a file name for reading using \code{fars_read} method
#' @param year Integer year.
#' @return Formatted filename of the form \code{accident_<year>.csv.bz2}
#' @example
#' make_filename(2017)
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Create a dataframe only for the specific years.
#' Each year would have a filename of the form \code{accident_<year>.csv.bz2}
#' @note This call assumes that the data files are
#' directly accessible under the current directory. Use \code{getwd()} to confirm.
#' @note MONTH refers to the numeric representation with January being 1.
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @param years A vector of integers
#' @return list of dplyr tbl_df instances each for the year. If data for the year
#' specified cannot be found, a warning is printed and NULL is returned for that entry
#' in the year.
#' @examples \dontrun{
#' data <- fars_read_years(c(2014, 2015, 2021))
#' Warning message:
#' In value[[3L]](cond) : invalid year: 2021
#'
#' > head(x=data[1])
#' [[1]]
#' A tibble: 30,056 x 2
#' MONTH  year
#' # <int> <dbl>
#'  1     1  2014
#' 2     1  2014
#' 3     1  2014
#' 4     1  2014
#' 5     1  2014
#' 6     1  2014
#' 7     1  2014
#' 8     1  2014
#' 9     1  2014
#' 10     1  2014
#' # ... with 30,046 more rows
#' > head(x=data[2])
#' [[1]]
#' # A tibble: 32,166 x 2
#' MONTH  year
#' <int> <dbl>
#'   1     1  2015
#' 2     1  2015
#' 3     1  2015
#' 4     1  2015
#' 5     1  2015
#' 6     1  2015
#' 7     1  2015
#' 8     1  2015
#' 9     1  2015
#' 10     1  2015
#' # ... with 32,156 more rows
#' > head(data[3])
#' [[1]]
#' NULL
#' }
fars_read_years <- function(years) {
  MONTH <- NULL
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}
