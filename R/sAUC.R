#' Calculate Single AUC
#'
#' @description Calculate single AUC
#'
#' @param object c(u1, u2, t1, t2, r)
#'
#' @return integrate
#'
#' @importFrom grDevices gray.colors
#' @importFrom graphics curve lines points

#' @examples
#'
#' par.vec <- c(1,1, 0.5, 0.5, -0.7)
#' sAUC(par.vec)
#'
#' opt1 <- dtasens1(IVD, p = 0.7)
#' sAUC(opt1)

#' @export

sAUC <- function(object){

  if(inherits(object,"DTAsens")) par.vec <- object$par else {

    if (is.vector(object) & length(object) >= 5) {

      par.vec <- object} else stop("Please input either DTAsens object or a vector of c(u1, u2, t1, t2, r)")

  }

    u1  <- par.vec[1]
    u2  <- par.vec[2]
    t1  <- par.vec[3]
    t2  <- par.vec[4]
    r   <- par.vec[5]

  integrate(function(x) { plogis(u1 - (r*t1/t2) * (qlogis(x) + u2)) }, 0, 1)

}



#' Calculate multiple AUC
#'
#' @description Calculate multiple AUC
#' @param par.matrix cbind(u1, u2, t1, t2, r)
#'
#' @return auc
#'
#' @examples
#'
#' par.matrix <-matrix(c(1,1,0.5, 0.5, -0.6, 1,1,1, 2, -0.6), 5,2)
#' msAUC(par.matrix)
#'
#' @export

msAUC <- function(par.matrix){

  if(nrow(par.matrix) < 5) stop("Please check your parameter matrix")

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

