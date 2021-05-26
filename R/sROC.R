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
#' Default is 1, solid.
#'
#' @param plot.ci Whether to plot confidence interval of sROC.
#' Default is \code{TRUE}, to plot the CI.
#'
#' @param ci.level The significance level of confidence interval of sROC.
#' Default is \code{0.95}
#'
#' @param sroc.ci.col The color of confidence interval of sROC.
#' Default is grey.
#'
#' @param sroc.ci.lwd The line width of confidence interval of sROC.
#' Default is 1.
#'
#' @param sroc.ci.lty The line type of confidence interval of sROC.
#' Default is 2, dashed.
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
                 xlab = "1-specificity",
                 ylab = "Sensitivity",
                 plot.ci = TRUE,
                 ci.level = 0.95,
                 sroc.ci.col = "grey48",
                 sroc.ci.lwd = 1,
                 sroc.ci.lty = 2,
                 add.spoint = TRUE,
                 spoint.pch = 18,
                 spoint.col = 1,
                 spoint.cex = 2,
                 ...) {

  if(inherits(object,"dtametasa")) par.vec <- object$par[1:5] else {

    if (is.vector(object) & length(object) >= 5) {

      par.vec <- object} else stop("PLEASE INPUT EITHER dtametasa OBJECT OR A VECTOR OF c(u1, u2, t1, t2, r)")

  }

  u1  <- par.vec[1]
  u2  <- par.vec[2]
  t1  <- par.vec[3]
  t2  <- par.vec[4]
  r   <- par.vec[5]


  f <- function(x) plogis(u1 - (t1*t2*r/(t2^2)) * (qlogis(x) + u2))

  curve(f, xlab = xlab, ylab = ylab, add = add, col = sroc.col, lwd =sroc.lwd,lty = sroc.lty,
        xlim = c(0,1), ylim = c(0,1), ...)

  if(inherits(object,"dtametasa") & plot.ci){

  f.lb <- function(x) plogis( u1 - (t1*t2*r/t2^2) * (qlogis(x) + u2) +
                                qnorm((1-ci.level)/2, lower.tail = TRUE)*
                                suppressWarnings(
                                  sqrt(QIQ(x, u1, u2, t1, t2, r, object$var.ml[1:5,1:5]))))

  f.ub <- function(x) plogis( u1 - (t1*t2*r/t2^2) * (qlogis(x) + u2) +
                                qnorm((1-ci.level)/2, lower.tail = FALSE)*
                                suppressWarnings(
                                  sqrt(QIQ(x, u1, u2, t1, t2, r, object$var.ml[1:5,1:5]))))

  curve(f.lb, add = TRUE, col = sroc.ci.col, lwd =sroc.ci.lwd, lty = sroc.ci.lty)

  curve(f.ub, add = TRUE, col = sroc.ci.col, lwd =sroc.ci.lwd, lty = sroc.ci.lty)

  }

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
#' @param ncols Set a vector of different colors for multiple sROC.
#' Defult uses different grey's colors.
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
#' p.seq <- seq(0.5, 0.9, 0.1)
#' sa1.seq <- sapply(p.seq, function (p) dtametasa.fc(IVD, p)$par)
#' sROC.matrix(sa1.seq)
#'
#'
#'

#'
#' @export

sROC.matrix <- function(par,  ## u1 u2 t12 t22
                 add = FALSE,
                 ncols = gray.colors(ncol(par), gamma = 1, start = 0.8, end = 0),
                 sroc.lty = 1,
                 sroc.lwd = 1,
                 add.spoint=TRUE,
                 spoint.pch = 18,
                 spoint.cex = 2,
                 xlab = "1 - specificity",
                 ylab = "Sensitivity",
                 ...) {

  if(nrow(par) < 5) stop("PLEASE CHECK THE INPUT MATRIX")

  if(length(par) == 5)  par <- as.matrix(par)

  if (!add) plot(NULL, xlim=c(0,1), ylim=c(0,1), xlab = xlab, ylab = ylab)

  for (i in 1:ncol(par)) {

    u1  <- par[1,i]
    u2  <- par[2,i]
    t1  <- par[3,i]
    t2  <- par[4,i]
    r   <- par[5,i]

    f <- function(x) plogis(u1 - (t1*t2*r/(t2^2)) * (qlogis(x) + u2))
    curve(f, 0, 1, col = ncols[i], add = TRUE,
          lty = sroc.lty, lwd = sroc.lwd, ...)
  }

  if (add.spoint) {
    sens <- plogis(par[1,])
    spec <- plogis(par[2,])
    points(1-spec, sens, col=ncols, pch = spoint.pch, cex = spoint.cex, ...)
  }

}

