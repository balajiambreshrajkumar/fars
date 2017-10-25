#' Creates a summary of number of fars records per month for each year.
#' @importFrom  dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#' @param years: A vector of integer years
#' @return A data frame of the counts of number of fars records per year.
#' Months are numbered between 1 and 12 (both ends included) and the counts
#' are present under the years column. Invalid year entries are left out of the summary.
#' @examples \dontrun {
#' > ret <- fars_summarize_years(c(2013, 2015, 2021))
#' Warning message:
#'   In value[[3L]](cond) : invalid year: 2021
#' > ret
#' # A tibble: 12 x 3
#' MONTH `2013` `2015`
#' * <int>  <int>  <int>
#'   1     1   2230   2368
#' 2     2   1952   1968
#' 3     3   2356   2385
#' 4     4   2300   2430
#' 5     5   2532   2847
#' 6     6   2692   2765
#' 7     7   2660   2998
#' 8     8   2899   3016
#' 9     9   2741   2865
#' 10    10   2768   3019
#' 11    11   2615   2724
#' 12    12   2457   2781
#' }
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Shows a graphic of the points where accidents took place
#' for the given state number for the given year.
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom  graphics points
#' @param state.num: Integer ID of the state.
#' @param year: Integer year.
#' @return NULL
#' @note Rasies an error if the state number is absent in the
#' year fars dataset or the fars data file for the year is absent.
#' @examples \dontrun {
#' fars_map_state(1, 2021)
#' Error in fars_read(filename) :
#'  file 'accident_2021.csv.bz2' does not exist
#'
#' fars_map_state(1000, 2013)
#' Error in fars_map_state(1000, 2013) : invalid STATE number: 1000
#'
#' fars_map_state(1, 2013)
#' <shows image>
#'}
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
