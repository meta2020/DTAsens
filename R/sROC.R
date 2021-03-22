#' @title Plot a summary ROC plot for dtametasa object
#'
#' @description Plot a single sROC plot
#'
#' @param object The object from function \code{dtametasa.fc} or \code{dtametasa.rc};
#' or a vector of \code{c(u1, u2, t1, t2, r)}
#'
#' @param add Whether to add the plot into an existed plot.
#' Default is \code{FALSE}, to create a new plot.
#'
#' @param sroc.col The color of sROC.
#' Default is black.
#'
#' @param sroc.lty The line type of sROC.
#' Default is solid.
#'
#' @param sroc.lwd The line width of sROC.
#' Default is 1.
#'
#' @param add.spoint Whether to add the summary point in the sROC plot.
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
#' @param xlab The label of x-axis.
#' Default is: 1-specificity.
#'
#' @param ylab The label of y-axis.
#' Default is Sensitivity.
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
#' \code{\link{dtametasa.fc}},
#' \code{\link{dtametasa.rc}}.
#'
#' @examples
#'
#' sa1 <- dtametasa.fc(IVD, p = 0.7)
#' sROC(sa1)
#'
#' @export

sROC <- function(object,
                 add = FALSE,
                 sroc.col = 1,
                 sroc.lty = 1,
                 sroc.lwd = 1,
                 add.spoint = TRUE,
                 spoint.pch = 19,
                 spoint.col = 1,
                 spoint.cex = 1,
                 xlab = "1-specificity",
                 ylab = "Sensitivity",
                 ...) {

  if(inherits(object,"dtametasa")) par.vec <- object$par[c(1,2,4,5)] else {

    if (is.vector(object) & length(object) >= 4) {

      par.vec <- object} else stop("PLEASE INPUT EITHER dtametasa OBJECTS OR A VECTOR OF c(u1, u2, t22, t12)")

  }

  u1  <- par.vec[1]
  u2  <- par.vec[2]
  t22 <- par.vec[3]
  t12 <- par.vec[4]

  roc   <- function(x) plogis(u1 - (t12/t22) * (qlogis(x) + u2))

  curve(roc, xlab = xlab, ylab = ylab, add = add, col = sroc.col, lwd =sroc.lwd,lty = sroc.lty,
        xlim = c(0,1), ylim = c(0,1), ...)

  if(add.spoint) points(plogis(-u2), plogis(u1), pch = spoint.pch, col = spoint.col, cex = spoint.cex, ...)


}




#' @title Plot summary ROC curves
#'
#' @description Plot multiple ROC curves
#'
#' @param par It can be a vector of (u1 u2 t22 t12),
#' or a matrix with 4 rows.
#' Each column is the vector \code{c(u1, u2, t22, t12)}.
#'
#' @param add Whether to add the plot into an existed plot.
#' Default is \code{FALSE}, to create a new plot.
#'
#' @param ncols Set different colors for multiple sROC.
#' Defult is \code{NULL}, that uses different grey's colors.
#'
#' @param sroc.lty The line tyoe of sROC.
#' Default is solid lines.
#'
#' @param sroc.lwd The line width of sROC.
#' Default is 1.
#'
#' @param add.spoint Whether to add the summary point in the sROC plot.
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
#' @param xlab The label of x-axis.
#' Default is: 1-specificity.
#'
#' @param ylab The label of y-axis.
#' Default is Sensitivity.
#'
#' @param ... Other augments in function \code{\link{points}} or function \code{\link{curve}}
#'
#' @importFrom grDevices gray.colors
#' @importFrom graphics curve points
#'
#' @return sROC plot
#'
#' @seealso
#' \code{\link[graphics]{points}},
#' \code{\link[graphics]{curve}},
#' \code{\link{dtametasa.fc}},
#' \code{\link{dtametasa.rc}}.
#'
#'
#' @examples
#'
#' # Use matrix
#'
#' par.matrix <-cbind(c(1, 1.5, 0.5, -0.2),
#'                    c(1, 1, 1, -0.6),
#'                    c(1.8, 2, 1,-0.6))
#'
#' sROC.matrix(par.matrix, legend = TRUE, p.vec = seq(0.2,0.6,0.2), legend.cex = 0.9)
#'
#' sROC.matrix(par.matrix, legend = TRUE,
#'       legend.text = c("l1", "l2", "l3"),
#'       legend.cex = 0.9, ncols = 1:3)
#'
#' # Use vector
#'
#' par.vec <- c(1,1.5,0.5,-0.2)
#'
#' sROC.matrix(par.vec)
#'
#' @export

sROC.matrix <- function(par,  ## u1 u2 t12 t22
                 add = FALSE,
                 ncols = NULL,
                 sroc.lty = 1,
                 sroc.lwd = 1,
                 add.spoint=TRUE,
                 legend = FALSE,
                 p.vec,
                 legend.text = paste0("p = ",p.vec),
                 legend.cex = 1,
                 spoint.pch = 19,
                 spoint.cex = 1,
                 xlab = "1 - specificity",
                 ylab = "Sensitivity",
                 ...) {

  if(length(par) < 4) stop("PLEASE CHECK THE INPUT VECTOR")

  if(length(par) == 4)  par <- as.matrix(par)

  if(nrow(par) < 4) stop("PLEASE CHECK THE INPUT MATRIX")

  if (!add) plot(NULL, xlim=c(0,1), ylim=c(0,1), xlab = xlab, ylab = ylab)

  if (is.null(ncols)) ncols <- gray.colors(ncol(par), gamma = 1, start = 0, end = 0.8)

  for (i in 1:ncol(par)) {

    u1  <- par[1,i]
    u2  <- par[2,i]
    t22 <- par[3,i]
    t12 <- par[4,i]

    roc <- function(x) plogis(u1 - (t12/t22) * (qlogis(x) + u2))
    curve(roc, 0, 1, col = ncols[i], add = TRUE,
          lty = sroc.lty, lwd = sroc.lwd, ...)
  }

  if (legend) legend("bottomright",
                    legend = legend.text,
                    col = ncols,
                    lty = rep(sroc.lty, ncol(par)),
                    cex = legend.cex,
                    ...)

  if (add.spoint) {
    sens <- plogis(par[1,])
    spec <- plogis(par[2,])
    points(1-spec, sens, col=ncols, pch = spoint.pch, cex = spoint.cex, ...)
  }

}

