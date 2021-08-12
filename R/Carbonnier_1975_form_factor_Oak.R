#' Calculate the form factor for Oak stands, from Carbonnier (1975)
#'
#' @source Carbonnier, C. (1975) Produktionen i kulturbestånd av ek i södra Sverige:
#' Yield of Oak plantations in southern Sweden. Studia Forestalia Suecica. Nr. 125.
#' Royal College of Forestry. Stockholm. ISBN 91-38-02278-8. p. 72.
#'
#' @param Lorey_mean_height Mean height of stand according to Lorey's formula, e.g. [forester::Lorey_mean_height]
#'
#' @return Form factor for stand.
#' @export
#'
#' @examples
Carbonnier_1975_form_factor_Oak <- function(
  Lorey_mean_height
){
  return(
    +0.4225+
    +0.7788*(1/Lorey_mean_height)
  )
}
