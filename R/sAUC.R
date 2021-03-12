#' @title Calculate summary AUC
#'
#' @description Calculate sAUC values from parameter vector or parameter matrix
#'
#' @param par A vector of (u1 u2 t22 t12),
#' or a matrix with 4 rows.
#' Each column is the vector of \code{c(u1, u2, t22, t12)}.
#'
#' @return sAUC values
#'
#' @examples
#'
#' #Use matrix
#'
#' par.matrix <-cbind(c(1,1,0.5, -0.6),
#'                    c(1,1,2, -0.6))
#'
#' sAUC(par.matrix)
#'
#' # Use vector
#'
#' par.vec <- c(1,1,0.5, -0.6)
#'
#' sAUC(par.vec)
#'
#' @export

sAUC <- function(par){

  if(length(par) < 4) stop("PLEASE CHECK THE INPUT VECTOR")

  if(length(par) == 4)  par <- as.matrix(par)

  if(nrow(par) < 4) stop("PLEASE CHECK THE INPUT MATRIX")

  sapply(1:ncol(par), function(i) {

    u1  <- par[1,i]
    u2  <- par[2,i]
    t22 <- par[3,i]
    t12 <- par[4,i]

    if (NA %in% par[,i]) {auc <- NA} else {

      auc.try <- try(integrate(function(x) { plogis(u1 - (t12/t22) * (qlogis(x) + u2)) }, 0, 1))

      if(!inherits(auc.try,"try-error")) auc.try$value else NA

    }

  })

}

