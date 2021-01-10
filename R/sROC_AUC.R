#' ROC plot
#'
#' @description ROC plot
#'
#' @param u1 u1
#' @param u2 u2
#' @param t22 t22
#' @param t12 t12
#' @param par c(u1, u2, t22, t12)
#' @param add par
#' @param s.point par
#' @param ... par
#'
#' @return plot
#'
#' @importFrom grDevices gray.colors
#' @importFrom graphics curve lines points

#' @examples
#' sROC(1,1,0.5, -0.5)
#'
#' @export

sROC <- function(u1, u2, t22, t12,
                 par = NULL,
                 add = FALSE,
                 s.point = TRUE,
                 ...) {

  if (!is.null(par)) {

    u1  <- par[1]
    u2  <- par[2]
    t12 <- par[4]
    t22  <- par[3]

  }

  auc <- function(x) plogis(u1 + (t12/t22) * (-qlogis(x) - u2))

  curve(auc, xlab = "FPR", ylab = "TPR", add = add, ...)

  if(s.point) points(plogis(-u2), plogis(u1),...)


}

#' ROC bunch plot
#'
#' @description ROC plot in bunch
#' @param par.matrix rbind(u1, u2, t22, t12)
#' @param s.point s.point
#' @param s.line s.line
#' @param new.plot new.plot
#' @param ... par
#'
#' @return plot
#'
#' @examples
#' par.matrix <-matrix(c(1,1,0.5, -0.5,
#'                    1,1,0.5, -0.3), 4,2)
#' sROC.bunch(par.matrix)
#'
#' @export

sROC.bunch <- function(par.matrix,  ## u1 u2 t22 t12
                       s.point=TRUE, s.line = FALSE,
                       new.plot =TRUE,legend = TRUE, p,...) {

  if (new.plot) plot(NULL, xlim=c(0,1), ylim=c(0,1), xlab = "FPR", ylab = "TPR")

  cols <- gray.colors(ncol(par.matrix), gamma = 1, start = 0, end = 0.8)

  for (i in 1:ncol(par.matrix)) {

    u1 = par.matrix[1,i]
    u2 = par.matrix[2,i]

    t22 = par.matrix[3,i]
    t12 = par.matrix[4,i]

    auc <- function(x) plogis(u1 + (t12/t22) * (-qlogis(x) - u2))
    curve(auc, 0, 1, col = cols[i], add = TRUE, xlab = "FPR", ylab = "TPR", ...)
  }

  if (legend) legend("bottomright",
                      legend = p,
                      col = cols[1:ncol(par.matrix)],
                     lty = rep(1, ncol(par.matrix)))

  if (s.point) {
    sens <- plogis(par.matrix[1,])
    spec <- plogis(par.matrix[2,])
    points(1-spec, sens, col=cols, pch = 19)
  }

  if (s.line)  lines(1-spec, sens)

}


