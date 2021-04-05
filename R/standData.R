#' Create a standData object.
#'
#' @param siteData a siteData object.
#' @param treelistData a treelist object.
#'
#' @return standData object.
#' @export
#'
#' @examples
standData <- function(siteData,treelistData){
  standData <- list(siteData, treelistData,standSummary)
  class(standData) <- c("standData","list")
  return(standData)
}
