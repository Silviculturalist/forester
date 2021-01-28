#' Formula maker
#' @description Creates functions from data frame information. Data frame specifications is given in details.
#'
#' @param data A data.frame, more information on structure in details.
#' @param ID The ID of the column you are interested in.
#' @param lhs Your response variable, Default: "myVariable".
#' @param constant A string defining the 'constant term'. Default: 'constant'.
#'
#' @details The data frame must consist of:
#' * a column containing a row for each of the terms used in any of the functions (NA for not used).
#' * a column for each function with the respective value for each term.
#'
#' By default, If any term is called *constant* this rows value will not be multiplied by the term.
#'
#'
#' \tabular{lrrr}{
#' Term \tab F1 \tab \tab F2 \cr
#' constant \tab 1 \tab \tab 1.5 \cr
#' b1 \tab -3.23 \tab \tab NA \cr
#' b2 \tab NA \tab \tab 32
#' }
#'
#' @return A function in functional form.
#'
#' @export
#'
#' @examples
#' #Not run
#' formulator(data, "Beech", "`my Variable`")
#' myVariable ~ 0.2081 + 1.7491\*lndp1 + -0.2167\*d
formulator  <-  function(data, ID, lhs, constant = "constant", term="terms") {
     terms <-  structure(
         paste(data[[ID]], data[[term]], sep = "*"),
         names = data[[term]]
       )
     terms <-  terms[!is.na(data[[ID]])]
     cnst <-  which(names(terms) == constant)
     terms[cnst] <-  data[[ID]][cnst]
     rhs <-  paste(terms, collapse = " + ")
     textVersion <-  paste(lhs, "~", rhs)
     as.formula(textVersion, env = parent.frame())
   }
