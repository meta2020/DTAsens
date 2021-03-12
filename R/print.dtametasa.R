#' @title Print dtametasa.fc and dtametasa.rc functions results
#'
#' @description Print results from function \code{\link{dtametasa.fc}} or \code{\link{dtametasa.rc}}
#'
#' @param x object from function \code{dtametasa.fc} or \code{dtametasa.rc}
#' @param digits digits of the results
#' @param ... other parameters in function \code{\link{print}}
#'
#' @examples
#'
#' (sa1 <- dtametasa.fc(IVD, p = 0.7))
#' (sa2 <- dtametasa.rc(IVD, p = 0.7))
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

  if(!inherits(x, "dtametasa")) stop("ONLY VALID FOR RESULTS OF dtasens1 OR dtasens2")

  print(list(par = x$par), ...)

}
