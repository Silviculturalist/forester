#' is_monotonic
#'
#' @param df Tidy dataframe with one row for each plantid and observation.
#' @param id Unique identifier variable for each plant.
#' @param year Year variable.
#' @param height Numeric height variable.
#'
#' @return Returns the original dataframe with an additional column, monotonic_growth: TRUE if not strictly decreasing.
#' @export
#'
#' @examples
#' data(expected)
#' is_monotonic(df=expected,id=plantid,year=year,height=height)
#'
#'
is_monotonic <- function(df, id, year, height){
  id <- enquo(id)
  year <- enquo(year)

  df %>% group_by(!!id) %>% arrange(!!id, !!year) %>%
    tidyr::fill(!!height, .direction="downup") %>%
    mutate(monotonic_growth = cumall(c(TRUE, diff(height) > 0))) %>% ungroup()
}
