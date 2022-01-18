#' @title Print dtametasa.fc and dtametasa.rc functions results
#'
#' @description Print results from function \code{\link{dtametasa.fc}} or \code{\link{dtametasa.rc}}
#'
#' @param x object from function \code{dtametasa.fc} or \code{dtametasa.rc}
#' @param digits digits of the results
#' @param ... other parameters in function \code{\link{print}}
#'
#' @importFrom stats integrate nlminb plogis pnorm qlogis uniroot qchisq qnorm
#' @importFrom mixmeta mixmeta
#'
#'
#' @seealso
#' \code{\link{dtametasa.fc}},
#' \code{\link{dtametasa.rc}},
#' \code{\link[base]{print}}.
#'
#' @rdname print.dtametasa
#'
#' @export

print.dtametasa <- function(x, digits = 3, ...){

  if(!inherits(x, "dtametasa")) stop("ONLY VALID FOR RESULTS FROM dtametasa.fc OR dtametasa.rc FUNCTIONS")

  print(list(par.all = x$par.all), digits = digits, ...)

}
