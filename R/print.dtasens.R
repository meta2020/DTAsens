#' Print DTAsens
#'
#' @description Print DTAsens
#'
#' @param x object
#' @param digits digits
#' @param ... graphical parameters to plot
#'
#' @examples
#'
#' (opt1 <- dtasens1(IVD, p = 0.7))
#' (opt2 <- dtasens2(IVD, p = 0.7))
#'
#' @rdname print.DTAsens
#' @export
print.DTAsens <- function(x, digits = 3, ...){

  if(!inherits(x, "DTAsens")) stop("Only valid for dtasens1 or dtasens2")

  print(list(par = x$par, ci = x$ci), ...)

}
