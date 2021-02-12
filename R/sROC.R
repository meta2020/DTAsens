#' Single ROC plot
#'
#' @description Single ROC plot
#'
#' @param object c(u1, u2, t1, t2, r)
#' @param add par
#' @param roc.col par
#' @param roc.lty par
#' @param roc.lwd par
#' @param add.sum.point par
#' @param spoint.pch par
#' @param spoint.col par
#' @param spoint.cex par
#' @param ... par
#'
#' @return plot
#'
#' @importFrom grDevices gray.colors
#' @importFrom graphics curve lines points
#' @importFrom stats qnorm


#' @examples
#'
#' par.vec <- c(1,1, 0.5, 0.5, -0.6)
#' sROC(par.vec)
#'
#' opt1 <- dtasens1(IVD, p = 0.7)
#' sROC(opt1)
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

  if(inherits(object,"DTAsens")) par.vec <- object$par else {

    if (is.vector(object) & length(object) >= 5) {

      par.vec <- object} else stop("Please input either DTAsens object or a vector of c(u1, u2, t1, t2, r)")

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

#' Multiple ROC curves
#'
#' @description Multiple ROC curves
#' @param par.matrix cbind(u1, u2, t1, t2, r)
#' @param add par
#' @param ncols ncols
#' @param roc.lty roc.lty
#' @param roc.lwd par
#' @param add.sum.point par
#' @param legend par
#' @param p.vec par
#' @param legend.text par
#' @param legend.cex par
#' @param spoint.pch par
#' @param spoint.cex par
#' @param ... par
#'
#' @return plot
#'
#' @examples
#'
#' par.matrix <-cbind(c(1,1.5,0.5, 0.5, -0.2),c(1, 1, 1, 2, -0.6), c(1.8, 2, 1, 2, -0.6))
#' p.vec <- seq(0.2,0.6,0.2)
#' mROC(par.matrix, legend = TRUE, p.vec = p.vec, legend.cex = 0.9)
#' mROC(par.matrix, legend = TRUE, legend.text = c("l1", "l2", "l3"), legend.cex = 0.9, ncols = 1:3)
#'
#' @export

mROC <- function(par.matrix,  ## u1 u2 t12 t22
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

  if(nrow(par.matrix) < 5) stop("Please check your parameter matrix")

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

