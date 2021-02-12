#' Calculate Single AUC
#'
#' @description Calculate single AUC
#'
#' @param par.vec c(u1, u2, t1, t2, r)
#'
#' @return integrate
#'
#' @importFrom grDevices gray.colors
#' @importFrom graphics curve lines points

#' @examples
#' par.vec <- c(1,1, 0.5, 0.5, -0.6)
#' sAUC(par.vec = par.vec)
#' opt1 <- dtasens1(IVD, p = 0.7)
#' sAUC(opt1)

#' @export

sAUC <- function(dtasens, par.vec){

  if(!missing(dtasens)) par.vec <- dtasens$par

    u1  <- par.vec[1]
    u2  <- par.vec[2]
    t1  <- par.vec[3]
    t2  <- par.vec[4]
    r   <- par.vec[5]

  integrate(function(x) { plogis(u1 - (r*t1/t2) * (qlogis(x) + u2)) }, 0, 1)

}


#' Calculate Single AUC with CI
#'
#' @description Calculate single AUC.CI
#'
#' @param par.vec c(u1, u2, t1, t2, r)
#' @param ci.level ci.level
#'
#' @return integrate
#'
#' @importFrom grDevices gray.colors
#' @importFrom graphics curve lines points

#' @examples
#' par.vec <- c(1,1, 0.5, 0.5, -0.6)
#' sAUC.ci(par.vec)
#'
#' @export

sAUC.ci <- function(par.vec, ci.level = 0.95){

  if (!is.null(par.vec)) {

    u1  <- par.vec[1]
    u2  <- par.vec[2]
    t1  <- par.vec[3]
    t2  <- par.vec[4]
    r   <- par.vec[5]

  }

  auc   <- integrate(function(x) { plogis(u1 - (r*t1/t2) * (qlogis(x) + u2)) }, 0, 1)$value

  auc.l <- integrate(function(x) { plogis(u1 - (r*t1/t2) * (qlogis(x) + u2) - qnorm((1-ci.level)/2, lower.tail = FALSE) * t1* sqrt(1-r^2)) }, 0, 1)$value

  auc.u <- integrate(function(x) { plogis(u1 - (r*t1/t2) * (qlogis(x) + u2) + qnorm((1-ci.level)/2, lower.tail = FALSE) * t1* sqrt(1-r^2)) }, 0, 1)$value

  auc.all <- c(auc.l, auc, auc.u)
  names(auc.all) <- c("auc.l", "auc", "auc.u")

  return(auc.all)

}


#' Calculate Single AUC with CL
#'
#' @description Calculate single AUC.CL
#'
#' @param par.ci.mat c(u1, u2, t1, t2, r)
#' @param ci.level ci.level
#'
#' @return integrate
#'
#' @importFrom grDevices gray.colors
#' @importFrom graphics curve lines points

#' @examples
#' opt <- dtasens1(IVD, p = 0.5, c1 = sqrt(0.5), ci.level = 0.95)
#' par.ci.mat <- opt$prof.llk.ci
#' sAUC.cl(par.ci.mat)
#'
#' @export

sAUC.cl <- function(par.ci.mat, ci.level = 0.95){

  ## auc
  auc.try <- try(sAUC(par.ci.mat[,2]), silent = TRUE)
  if(!inherits(auc.try, "try-error"))   auc <- auc.try$value   else auc <- NA

  ## auc.l
  auc.l.vec <- c(par.ci.mat[1:3,1], par.ci.mat[4:5,3])

  auc.l.try <- try(sAUC(auc.l.vec), silent = TRUE)
  if(!inherits(auc.l.try, "try-error"))   auc.l <- auc.l.try$value   else auc.l <- NA

  ## auc.u
  auc.u.try <- c(par.ci.mat[1:3,3], par.ci.mat[4:5,1])

  auc.u.try <- try(sAUC(auc.u.try), silent = TRUE)
  if(!inherits(auc.u.try, "try-error"))   auc.u <- auc.u.try$value   else auc.u <- NA

  auc.all <- c(auc.l, auc, auc.u)
  names(auc.all) <- c("auc.l", "auc", "auc.u")

  return(auc.all)

}


#' Calculate multiple AUC
#'
#' @description Calculate multiple AUC
#' @param par.matrix cbind(u1, u2, t1, t2, r)
#'
#' @return auc
#'
#' @examples
#' par.matrix <-matrix(c(1,1,0.5, 0.5, -0.6, 1,1,1, 2, -0.6), 5,2)
#' mAUC(par.matrix)
#'
#' @export

mAUC <- function(par.matrix){

  #par.matrix <- x


  sapply(1:ncol(par.matrix), function(i) {


    u1 = par.matrix[1,i]
    u2 = par.matrix[2,i]

    t1 = par.matrix[3,i]
    t2 = par.matrix[4,i]
    r  = par.matrix[5,i]


    if (NA %in% par.matrix[,i]) {auc <- NA} else {

      auc.try   <- try(sAUC(par.matrix[,i]), silent = TRUE)

      if(!class(auc.try)=="try-error")   auc.try$value   else NA

    }



  })

}


#' Calculate multiple AUC with CI
#'
#' @description Calculate multiple AUC.ci
#' @param par.matrix cbind(u1, u2, t1, t2, r)
#' @param ci.level ci.level
#'
#' @return auc
#'
#' @examples
#' par.matrix <-matrix(c(1,1,0.5, 0.5, -0.6, 1,1,1, 2, -0.6), 5,2)
#' mAUC.ci(par.matrix, ci.level = 0.9)
#'
#' @export

mAUC.ci <- function(par.matrix, ci.level = 0.95){

  #par.matrix <- x


  vapply(1:ncol(par.matrix), function(i) {


    u1 = par.matrix[1,i]
    u2 = par.matrix[2,i]

    t1 = par.matrix[3,i]
    t2 = par.matrix[4,i]
    r  = par.matrix[5,i]


    if (NA %in% par.matrix[,i]) {auc <- NA} else {

      sAUC.ci(par.matrix[,i], ci.level = ci.level)

    }

  }, c("auc.l" = 0, "auc" = 0, "auc.u" = 0))

}

