#' Create a standData object.
#'
#' @param siteData a siteData object.
#' @param treelistData a treelist object.
#' @param weatherData a weatherData object.
#'
#' @return standData object.
#' @export
#'
#' @examples
standData <- function(siteData,treelistData,weatherData){
  standData <- list(siteData, treelistData, weatherData, standSummary)
  class(standData) <- c("standData","list")
  return(standData)
}
