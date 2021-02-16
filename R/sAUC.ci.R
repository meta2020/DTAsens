#' sAUC.ci function
#'
#' @description Parametric Bootstrap CI of sAUC
#'
#' @param object data
#' @param B p
#' @param ncores ncores
#' @param type c1
#' @param ci.level par
#' @param hide.progress par
#'
#' @return convergence list
#'
#' @importFrom foreach foreach %dopar%
#' @importFrom doSNOW registerDoSNOW
#' @importFrom parallel makeCluster detectCores stopCluster
#' @importFrom stats quantile rnorm sd
#' @importFrom utils setTxtProgressBar txtProgressBar install.packages
#'
#' @export
#' @examples
#'
#' opt1 <- dtasens1(IVD, p = 0.7)
#' (ci <- sAUC.ci(opt1, B = 5))
#'
sAUC.ci <- function(object, B = 10, ncores, type = "SOCK", ci.level = 0.95,
                    hide.progress = FALSE)
{
  if(!requireNamespace("foreach")) install.packages("foreach")   else require(foreach)
  if(!requireNamespace("parallel")) install.packages("parallel") else require(parallel)
  if(!requireNamespace("doSNOW")) install.packages("doSNOW")     else require(doSNOW)

  if(!inherits(object, "DTAsens")) stop("Only valid for dtasens1 or dtasens2")

  data <- object$data
  S  <- nrow(data)
  y1 <- data$y1
  y2 <- data$y2
  v1 <- data$v1
  v2 <- data$v2

  if(missing(ncores)) ncores <- parallel::detectCores()

  cl <- parallel::makeCluster(ncores, type = type)
  doSNOW::registerDoSNOW(cl)

  opts <- NULL

  if (!hide.progress){

    pb <- txtProgressBar(max = B, style = 3, width = 40)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
  }

  if(object$func.name == "dtasens1"){

    sauc.t <- foreach(r=1:B, .combine=c, .packages="DTAsens", .options.snow = opts) %dopar% {

      y1.t <- sapply(1:S, function(i) rnorm(1,y1[i],v1[i]))
      y2.t <- sapply(1:S, function(i) rnorm(1,y2[i],v2[i]))

      data.t <- data.frame(y1 = y1.t, y2 = y2.t, v1 = v1, v2 = v2)

      args <- c(list(data = data.t), object$pars.info)
      opt1.t <- do.call("dtasens1", args)
      opt1.t$par[11]

    }
  }

  if(object$func.name == "dtasens2"){

      sauc.t <- foreach(r=1:B, .combine=c, .packages="DTAsens", .options.snow = opts) %dopar% {

        y1.t <- sapply(1:S, function(i) rnorm(1,y1[i],v1[i]))
        y2.t <- sapply(1:S, function(i) rnorm(1,y2[i],v2[i]))

        data.t <- data.frame(y1 = y1.t, y2 = y2.t, v1 = v1, v2 = v2)
        args <- c(list(data = data.t), object$pars.info)
        opt2.t <- do.call("dtasens2", args)
        opt2.t$par[11]

    }
  }

  if (!hide.progress) close(pb)
  parallel::stopCluster(cl)

  q1 <- quantile(scale(sauc.t), probs = (1-ci.level)/2)
  q2 <- quantile(scale(sauc.t), probs = 1-(1-ci.level)/2)
  se <- sd(sauc.t)
  sauc <- unlist(object$auc.all$value)

  list <- list(sAUC = sauc,
       CI.L = max(sauc+q1*se, 0),
       CI.U = min(sauc+q2*se, 1),
       cluster = cl)

  class(list) <- "sAUC.ci"

  list

}

#' Print sAUC CI
#'
#' @description Print sAUC CI
#'
#' @param x object
#' @param digits digits
#' @param ... graphical parameters to plot
#'
#' @rdname print.sAUC.ci
#' @export
print.sAUC.ci <- function(x, digits = 3, ...){

  if(!inherits(x, "sAUC.ci")) stop("Only valid for sAUC.ci")

  sAUC <- c(x$sAUC, x$CI.L, x$CI.U)
  names(sAUC) <-c("sAUC", "CI.L", "CI.L")

  print(sAUC, digits = digits, ...)

}
