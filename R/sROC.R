#' @title Plot a single summary ROC plot
#'
#' @description Plot a single sROC plot
#'
#' @param object The object from function \code{dtasens1} or \code{dtasens2};
#' or a vector of \code{c(u1, u2, t1, t2, r)}
#' 
#' @param add Whether to add the plot into an existed plot.
#' Default is \code{FALSE}, to create a new plot.
#' 
#' @param roc.col The color of sROC.
#' Default is black.
#' 
#' @param roc.lty The line type of sROC.
#' Default is solid.
#' 
#' @param roc.lwd The line width of sROC.
#' Default is 1.
#' 
#' @param add.sum.point Whether to add the summary point in the sROC plot.
#' Default it not the add.
#' 
#' @param spoint.pch The type of the point.
#' Default is 19.
#' 
#' @param spoint.col The color of the point.
#' Default is black.
#' 
#' @param spoint.cex The size of the point.
#' Default is 1.
#' 
#' @param ... Other augments in function \code{\link{points}} or function \code{\link{curve}}
#'
#' @return sROC plot
#'
#' @importFrom grDevices gray.colors
#' @importFrom graphics curve lines points matplot
#' @importFrom stats qnorm
#' 
#' @seealso 
#' \code{\link[graphics]{points}},
#' \code{\link[graphics]{curve}},
#' \code{\link{dtasens1}},
#' \code{\link{dtasens2}}.
#' 
#' @examples
#'
#' opt1 <- dtasens1(IVD, p = 0.7)
#' sROC(opt1)
#' 
#' par.vec <- c(1,1, 0.5, 0.5, -0.6)
#' sROC(par.vec)
#'
#' @export

sROC <- function(object,
                 add = FALSE,
                 roc.col = 1,
                 roc.lty = 1,
                 roc.lwd = 1,
                 add.sum.point = TRUE,
                 spoint.pch = 19,
                 spoint.col = 1,
                 spoint.cex = 1,
                 ...) {

  if(inherits(object,"dtasens")) par.vec <- object$par else {

    if (is.vector(object) & length(object) >= 5) {

      par.vec <- object} else stop("PLEASE INPUT EITHER dtasens OBJECTS OR A VECTOR OF c(u1, u2, t1, t2, r)")

  }

    u1 <- par.vec[1]
    u2 <- par.vec[2]
    t1 <- par.vec[3]
    t2 <- par.vec[4]
    r  <- par.vec[5]

  roc   <- function(x) plogis(u1 - (r*t1/t2) * (qlogis(x) + u2))

  curve(roc, xlab = "FPR", ylab = "TPR", add = add, col = roc.col, lwd =roc.lwd,lty = roc.lty,
        xlim = c(0,1), ylim = c(0,1), ...)

  if(add.sum.point) points(plogis(-u2), plogis(u1), pch = spoint.pch, col = spoint.col, ...)


}


#' @title Plot multiple summary ROC curves
#'
#' @description Plot multiple ROC curves
#' 
#' @param par.matrix A matrix with 5 rows. 
#' Each column is the vector \code{c(u1, u2, t1, t2, r)}.
#' 
#' @param add Whether to add the plot into an existed plot.
#' Default is \code{FALSE}, to create a new plot.
#' 
#' @param ncols Set different colors for multiple sROC.
#' Defult is \code{NULL}, that uses different grey's colors.
#' 
#' @param roc.lty The line tyoe of sROC.
#' Default is solid lines.
#' 
#' @param roc.lwd The line width of sROC.
#' Default is 1.
#' 
#' @param add.sum.point Whether to add the summary point in the sROC plot.
#' Default it not the add.
#' 
#' @param legend Whether to add legend into the plot.
#' Default is not to add.
#' 
#' @param p.vec If add the legend (\code{legend = TRUE}),
#' define the probability sequence.
#' 
#' @param legend.text If add the legend (\code{legend = TRUE}),
#' define the legend context.
#' 
#' @param legend.cex The font size of legend.
#' 
#' @param spoint.pch The point type of the summary point in sROC.
#'
#' @param spoint.cex The point size of the summary point in sROC.
#' 
#' @param ... Other augments in function \code{\link{points}} or function \code{\link{curve}}
#'
#' @return sROC plot
#' 
#' @seealso 
#' \code{\link[graphics]{points}},
#' \code{\link[graphics]{curve}},
#' \code{\link{dtasens1}},
#' \code{\link{dtasens2}}.
#' 
#'
#' @examples
#'
#' par.matrix <-cbind(c(1,1.5,0.5, 0.5, -0.2),
#'                    c(1, 1, 1, 2, -0.6), 
#'                    c(1.8, 2, 1, 2, -0.6))
#' 
#' p.vec <- seq(0.2,0.6,0.2)
#' 
#' msROC(par.matrix, legend = TRUE, p.vec = p.vec, legend.cex = 0.9)
#' msROC(par.matrix, legend = TRUE, 
#'       legend.text = c("l1", "l2", "l3"), 
#'       legend.cex = 0.9, ncols = 1:3)
#'
#' @export

msROC <- function(par.matrix,  ## u1 u2 t12 t22
                 add = FALSE,
                 ncols = NULL,
                 roc.lty = 1,
                 roc.lwd = 1,
                 add.sum.point=TRUE,
                 legend = FALSE,
                 p.vec,
                 legend.text = paste0("p = ",p.vec),
                 legend.cex = 1,
                 spoint.pch = 19,
                 spoint.cex = 1,
                 ...) {

  if(nrow(par.matrix) < 5) stop("PLEASE CHECK THE INPUT MATRIX")

  if (!add) plot(NULL, xlim=c(0,1), ylim=c(0,1), xlab = "FPR", ylab = "TPR")

  if (is.null(ncols)) ncols <- gray.colors(ncol(par.matrix), gamma = 1, start = 0, end = 0.8)

  for (i in 1:ncol(par.matrix)) {

    u1 = par.matrix[1,i]
    u2 = par.matrix[2,i]
    t1 = par.matrix[3,i]
    t2 = par.matrix[4,i]
    r  = par.matrix[5,i]

    roc <- function(x) plogis(u1 - (r*t1/t2) * (qlogis(x) + u2))
    curve(roc, 0, 1, col = ncols[i], add = TRUE,
          lty = roc.lty, lwd = roc.lwd, ...)
  }

  if (legend) legend("bottomright",
                    legend = legend.text,
                    col = ncols,
                    lty = rep(roc.lty, ncol(par.matrix)),
                    cex = legend.cex,
                    ...)

  if (add.sum.point) {
    sens <- plogis(par.matrix[1,])
    spec <- plogis(par.matrix[2,])
    points(1-spec, sens, col=ncols, pch = spoint.pch, cex = spoint.cex, ...)
  }

}

