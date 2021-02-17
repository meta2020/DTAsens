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
#' @param plot.ROC par
#' @param add.sum.point add.sum.point
#' @param roc.col roc.col
#' @param ... ...
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
sAUC.ci <- function(object, B = 10,
                    ncores, type = "SOCK", ci.level = 0.95,
                    hide.progress = FALSE,
                    plot.ROC = FALSE,
                    add.sum.point = FALSE,
                    roc.col = 1,
                    ...)
{
  if(!requireNamespace("foreach")) install.packages("foreach")   else requireNamespace("foreach")
  if(!requireNamespace("parallel")) install.packages("parallel") else requireNamespace("parallel")
  if(!requireNamespace("doSNOW")) install.packages("doSNOW")     else requireNamespace("doSNOW")

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

    par <- foreach(r=1:B, .combine=c, .packages="DTAsens", .options.snow = opts) %dopar% {

      y1.t <- sapply(1:S, function(i) rnorm(1,y1[i],v1[i]))
      y2.t <- sapply(1:S, function(i) rnorm(1,y2[i],v2[i]))

      data.t <- data.frame(y1 = y1.t, y2 = y2.t, v1 = v1, v2 = v2)

      args <- c(list(data = data.t), object$pars.info)
      opt1.t <- do.call("dtasens1", args)
      opt1.t$par[c(1:5, 11)]

    }
  }

  if(object$func.name == "dtasens2"){

      par <- foreach(r=1:B, .combine=c, .packages="DTAsens", .options.snow = opts) %dopar% {

        y1.t <- sapply(1:S, function(i) rnorm(1,y1[i],v1[i]))
        y2.t <- sapply(1:S, function(i) rnorm(1,y2[i],v2[i]))

        data.t <- data.frame(y1 = y1.t, y2 = y2.t, v1 = v1, v2 = v2)
        args <- c(list(data = data.t), object$pars.info)
        opt2.t <- do.call("dtasens2", args)
        opt2.t$par[c(1:5, 11)]

    }
  }

  if (!hide.progress) close(pb)
  parallel::stopCluster(cl)

  PAR <- matrix(par, nrow = 6)
  sauc.t   <- PAR[6,]

  se <- sd(sauc.t, na.rm = TRUE)
  sl.sauc.t <- (sauc.t - mean(sauc.t, na.rm = TRUE))/se

  s.sauc.r <- order(sl.sauc.t)
  PAR.r <- PAR[, s.sauc.r]

  s.sauc.t <- sort(sl.sauc.t)

  q1 <- quantile(s.sauc.t, probs = (1-ci.level)/2, na.rm = TRUE)
  q2 <- quantile(s.sauc.t, probs = 1-(1-ci.level)/2, na.rm = TRUE)

  sauc <- unlist(object$auc.all$value)

  list <- list(sAUC = sauc,
       CI.L = max(sauc+q1*se, 0),
       CI.U = min(sauc+q2*se, 1),
       cluster = cl)

  class(list) <- "sAUC.ci"

  if(plot.ROC){

    q1.pos <- which.min(abs(s.sauc.t - q1))
    q2.pos <- which.min(abs(s.sauc.t - q2))

    msROC(PAR.r[, q1.pos:q2.pos], add.sum.point = add.sum.point, ncols = rep("grey", B))
    sROC(object, add = TRUE, add.sum.point = add.sum.point, roc.col = roc.col,...)

  }

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
