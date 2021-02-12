#' Print DTAsens results
#'
#' @description Print DTAsens results
#'
#' @param object object
#' @param digits object
#' @param ... par
#'
#' @return par
#'
#' @export
#' @examples
#' (opt1 <- dtasens1(IVD, p = 0.7))
#' (opt2 <- dtasens2(IVD, p = 0.7))


print.DTAsens <- function(object, digits = 3, ...){

  if(!inherits(object, "DTAsens")) stop("Only valid for dtasens1 and dtasens2")

  print(list(par = object$par, ci = object$ci), digits = digits)

  #NextMethod("print")

}
