#' @title Print dtasens1 and dtasens2
#'
#' @description Print results from function \code{\link{dtasens1}} or \code{\link{dtasens2}}
#'
#' @param x object from function \code{dtasens1} or \code{dtasens2}
#' @param digits digits of the results
#' @param ... other parameters in function \code{\link{print}}
#'
#' @examples
#'
#' (opt1 <- dtasens1(IVD, p = 0.7))
#' (opt2 <- dtasens2(IVD, p = 0.7))
#' 
#' @seealso 
#' \code{\link{dtasens1}},
#' \code{\link{dtasens2}},
#' \code{\link[base]{print}}.
#'
#' @rdname print.dtasens
#' 
#' @export

print.dtasens <- function(x, digits = 3, ...){

  if(!inherits(x, "dtasens")) stop("ONLY VALID FOR RESULTS OF dtasens1 OR dtasens2")

  print(list(par = x$par, ci = x$ci), ...)

}
