#' @title Calculate a single summary AUC value
#'
#' @description Calculate a single sAUC value
#'
#' @param object The object from function \code{dtasens1} or \code{dtasens2};
#' or a vector of \code{c(u1, u2, t1, t2, r)}
#'
#' @return sAUC value
#'
#' @importFrom grDevices gray.colors
#' @importFrom graphics curve lines points
#'
#' @seealso \code{\link[stats]{integrate}}
#'
#' @examples
#'
#' opt1 <- dtasens1(IVD, p = 0.7)
#' sAUC(opt1)
#'
#' par.vec <- c(1,1, 0.5, 0.5, -0.7)
#' sAUC(par.vec)
#'
#' @export

sAUC <- function(object){

  if(inherits(object, "dtasens")) par.vec <- object$par else {

    if (is.vector(object) & length(object) >= 5) {

      par.vec <- object} else stop("PLEASE INPUT EITHER dtasens OBJECTS OR A VECTOR OF c(u1, u2, t1, t2, r)")

  }

    u1  <- par.vec[1]
    u2  <- par.vec[2]
    t1  <- par.vec[3]
    t2  <- par.vec[4]
    r   <- par.vec[5]

  integrate(function(x) { plogis(u1 - (r*t1/t2) * (qlogis(x) + u2)) }, 0, 1)

}



#' @title Calculate multiple sAUC values
#'
#' @description Calculate multiple sAUC values
#'
#' @param par.matrix A matrix with 5 rows.
#' Each column is the vector \code{c(u1, u2, t1, t2, r)}.
#'
#' @return sAUC values
#'
#' @examples
#'
#' par.matrix <-cbind(c(1,1,0.5, 0.5, -0.6),
#'                    c(1,1,1, 2, -0.6))
#'
#' msAUC(par.matrix)
#'
#' @export

msAUC <- function(par.matrix){

  if(nrow(par.matrix) < 5) stop("PLEASE CHECK THE INPUT MATRIX")

  sapply(1:ncol(par.matrix), function(i) {

    u1 = par.matrix[1,i]
    u2 = par.matrix[2,i]
    t1 = par.matrix[3,i]
    t2 = par.matrix[4,i]
    r  = par.matrix[5,i]

    if (NA %in% par.matrix[,i]) {auc <- NA} else {

      auc.try <- try(sAUC(par.matrix[,i]), silent = TRUE)

      if(!class(auc.try)=="try-error") auc.try$value else NA

    }

  })

}

