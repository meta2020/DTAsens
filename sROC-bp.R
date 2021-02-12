#' Single ROC plot
#'
#' @description Single ROC plot
#'
#' @param par.vec c(u1, u2, t1, t2, r)
#' @param add par
#' @param add.ci par
#' @param ci.level ci.level
#' @param add.sum.point par
#' @param ... par
#'
#' @return plot
#'
#' @importFrom grDevices gray.colors
#' @importFrom graphics curve lines points
#' @importFrom stats qnorm


#' @examples
#' par.vec <- c(1,1, 0.5, 0.5, -0.6)
#' sROC(par.vec, ci.level = 0.90)
#'
#' @export

sROC <- function(par.vec = NULL,
                 add = FALSE,
                 add.ci = FALSE,
                 ci.level = 0.95,
                 add.sum.point = TRUE,
                 ...) {

  if (!is.null(par.vec)) {

    u1 <- par.vec[1]
    u2 <- par.vec[2]
    t1 <- par.vec[3]
    t2 <- par.vec[4]
    r  <- par.vec[5]

  }

  roc   <- function(x) plogis(u1 - (r*t1/t2) * (qlogis(x) + u2))
  roc.l <- function(x) plogis(u1 - (r*t1/t2) * (qlogis(x) + u2) - qnorm((1-ci.level)/2, lower.tail = FALSE) * t1* sqrt(1-r^2))
  roc.u <- function(x) plogis(u1 - (r*t1/t2) * (qlogis(x) + u2) + qnorm((1-ci.level)/2, lower.tail = FALSE) * t1* sqrt(1-r^2))

  curve(roc, xlab = "FPR", ylab = "TPR", add = add, ..., xlim = c(0,1), ylim = c(0,1))
  if(add.ci){

    curve(roc.l, xlab = "FPR", ylab = "TPR", add = TRUE, lty = 3, col = "grey")
    curve(roc.u, xlab = "FPR", ylab = "TPR", add = TRUE, lty = 3, col = "grey")

  }


  if(add.sum.point) points(plogis(-u2), plogis(u1),...)


}

#' Multiple ROC curves
#'
#' @description Multiple ROC curves
#' @param par.matrix cbind(u1, u2, t1, t2, r)
#' @param s.point s.point
#' @param s.line s.line
#' @param new.plot new.plot
#' @param legend legend
#' @param p.vec p.vec
#' @param cols set cols
#' @param ... par
#'
#' @return plot
#'
#' @examples
#' par.matrix <-matrix(c(1,1,0.5, 0.5, -0.6, 1,1,1, 2, -0.6), 5,2)
#' mROC(par.matrix, legend = TRUE, p.vec = c(0.9, 0.5), cols = 1:2)
#'
#' @export

mROC <- function(par.matrix,  ## u1 u2 t12 t22
                 s.point=TRUE, s.line = FALSE,
                 new.plot =TRUE,legend = FALSE, p.vec,
                 cols = NULL,...) {

  if (new.plot) plot(NULL, xlim=c(0,1), ylim=c(0,1), xlab = "FPR", ylab = "TPR")

  if (is.null(cols)) cols <- gray.colors(ncol(par.matrix), gamma = 1, start = 0, end = 0.8)

  for (i in 1:ncol(par.matrix)) {

    u1 = par.matrix[1,i]
    u2 = par.matrix[2,i]
    t1 = par.matrix[3,i]
    t2 = par.matrix[4,i]
    r  = par.matrix[5,i]

    auc <- function(x) plogis(u1 - (r*t1/t2) * (qlogis(x) + u2))
    curve(auc, 0, 1, col = cols[i], add = TRUE, xlab = "FPR", ylab = "TPR", ...)
  }

  if (legend) legend("bottomright",
                      legend = p.vec,
                      col = cols[1:ncol(par.matrix)],
                     lty = rep(1, ncol(par.matrix)))

  if (s.point) {
    sens <- plogis(par.matrix[1,])
    spec <- plogis(par.matrix[2,])
    points(1-spec, sens, col=cols, pch = 19)
  }

  if (s.line)  lines(1-spec, sens)

}


#' Single ROC plot
#'
#' @description Single ROC plot
#'
#' @param par.ci.mat c(u1, u2, t1, t2, r)
#' @param add par
#' @param add.cl par
#' @param ci.level ci.level
#' @param add.sum.point par
#' @param ... par
#'
#' @return plot
#'
#' @importFrom grDevices gray.colors
#' @importFrom graphics curve lines points
#' @importFrom stats qnorm


#' @examples
#' opt <- dtasens1(IVD, p = 0.5, c1 = sqrt(0.5), ci.level = 0.95)
#' par.ci.mat <- opt$prof.llk.ci
#' sROC.cl(par.ci.mat)
#'
#' @export

sROC.cl <- function(par.ci.mat = NULL,
                 add = FALSE,
                 add.cl = TRUE,
                 ci.level = 0.95,
                 add.sum.point = TRUE,
                 ...) {


  roc <- function(x) {

    u1 <- v[1]
    u2 <- v[2]
    t1 <- v[3]
    t2 <- v[4]
    r  <- v[5]

    return(plogis(u1 - (r*t1/t2) * (qlogis(x) + u2)))

  }

  v <- par.ci.mat[,2]
  curve(roc, xlab = "FPR", ylab = "TPR", add = add, ..., xlim = c(0,1), ylim = c(0,1))

  if(add.cl){

    v <- c(par.ci.mat[1:3,1], par.ci.mat[4:5,3])
    curve(roc, add = TRUE, lty = 3, col = "grey")
    v <- c(par.ci.mat[1:3,3], par.ci.mat[4:5,1])
    curve(roc, add = TRUE, lty = 3, col = "grey")

  }

  if(add.sum.point) {
    u1 <- par.ci.mat[1,2]
    u2 <- par.ci.mat[2,2]
    points(plogis(-u2), plogis(u1),...)

  }


}
