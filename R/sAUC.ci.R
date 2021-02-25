#' @title Confidence interval of summary AUC
#'
#' @description Calculate the parametric bootstrap confidence interval (CI) for sROC and sAUC
#'
#' @param object object from function \code{dtasens1} or \code{dtasens2}.
#'
#' @param B The times for parametric bootstrapping.
#' Default is 1000.
#' It may be time consuming.
#'
#' @param ncores Set the an integer number of cores that will be used in parallel computing.
#' Default is 0, which detect the number of cores in the local device.
#'
#' @param type Computing types in the parallel computing.
#' See \code{type} augment in function \code{\link[parallel]{makeCluster}}.
#'
#' @param ci.level The significant value for confidence interval.
#' Default is 0.95, hence, a 2-tailed 95% CIs will be calculated by
#' profile likelihood.
#'
#' @param hide.progress Whether to hide the progress bar in the calculation.
#' Default is not to hide.
#'
#' @param plot.ROC.ci Whether to show the plot of sROC with the CIs lines.
#' Default is not to plot.
#'
#' @param add.plot.ROC.ci Whether to add the plot of sROC with the CIs lines
#' onto a new plot.
#' Default is not to add.
#'
#' @param add.sum.point Whether to add the summary point in the sROC plot.
#' Default it not the add.
#'
#' @param roc.ci.col The color of the CIs of sROC.
#' Default is grey.
#'
#' @param roc.ci.lty The line type of CIs.
#' Default is solid line.
#'
#' @param roc.ci.lwd The line width of CIs.
#' Defalt is 1.
#'
#' @param ... Other augments in function \code{\link{sROC}}.
#'
#' @return CI of sAUC; sROC plot with CIs
#'
#' @importFrom foreach foreach %dopar%
#' @importFrom doSNOW registerDoSNOW
#' @importFrom parallel makeCluster detectCores stopCluster
#' @importFrom stats quantile rnorm sd
#' @importFrom utils setTxtProgressBar txtProgressBar install.packages
#'
#' @seealso
#' \code{\link[parallel]{makeCluster}},
#' \code{\link{sROC}},
#' \code{\link[graphics]{matplot}},
#' \code{\link[base]{plot}}.
#'
#' @examples
#'
#' ## Here, we set B = 5 to save time in the calculation.
#' ## But, the results are not reliable.
#'
#' opt1 <- dtasens1(IVD, p = 0.7)
#' (ci <- sAUC.ci(opt1, B = 5, plot.ROC.ci = TRUE))
#'
#' @export

sAUC.ci <- function(object, B = 1000,
                    ncores = 0, type = "SOCK", ci.level = 0.95,
                    hide.progress = FALSE,
                    plot.ROC.ci = FALSE,
                    add.plot.ROC.ci = FALSE,
                    add.sum.point = FALSE,
                    roc.ci.col = "grey",
                    roc.ci.lty = 1,
                    roc.ci.lwd = 1,
                    ...)
{
  if(!requireNamespace("foreach"))  install.packages("foreach")   else requireNamespace("foreach")
  if(!requireNamespace("parallel")) install.packages("parallel")  else requireNamespace("parallel")
  if(!requireNamespace("doSNOW"))   install.packages("doSNOW")    else requireNamespace("doSNOW")

  if(!inherits(object, "dtasens")) stop("ONLY VALID FOR RESULTS OF dtasens1 OR dtasens2")

  data <- object$data
  S  <- nrow(data)
  y1 <- data$y1
  y2 <- data$y2
  v1 <- data$v1
  v2 <- data$v2

  if(ncores == 0) ncores <- parallel::detectCores() else ncores <- ceiling(ncores)

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

  n <- length(sauc.t)
  se <- (n-1)/n * sd(sauc.t, na.rm = TRUE)
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
       bs.par  = PAR,
       cluster = cl)


  if(plot.ROC.ci){

    fpr.t <- seq(0,1,0.001)
    se.t  <- sapply(1:B, function(i){

      u1 <- PAR[1,i]
      u2 <- PAR[2,i]
      t1 <- PAR[3,i]
      t2 <- PAR[4,i]
      r  <- PAR[5,i]
      plogis(u1 - (r*t1/t2) * (qlogis(fpr.t) + u2))

    })


    sROC(object, add.sum.point = add.sum.point, add = add.plot.ROC.ci,...)

    ci <- cbind(

      apply(se.t, 1, function(x) quantile(x, (1-ci.level)/2, na.rm = TRUE)),
      apply(se.t, 1, function(x) quantile(x, probs = 1-(1-ci.level)/2, na.rm = TRUE))

      )

    matplot(x = fpr.t, y = ci, type = "l",
            col = roc.ci.col, lty = roc.ci.lty, lwd = roc.ci.lwd,
            add = TRUE)

    list <- c(list, roc.ci <- list(x = fpr.t, y = ci))


  }

  class(list) <- "sAUC.ci"

  list

}

#' @title Print sAUC.ci
#'
#' @description Print results from function \code{\link{sAUC.ci}}
#'
#' @param x object from function \code{\link{sAUC.ci}}.
#'
#' @param digits digits of the results.
#'
#' @param ... other parameters in function \code{\link{print}}.
#'
#' @seealso
#' \code{\link{sAUC.ci}};
#' \code{\link[base]{print}}
#'
#' @rdname print.sAUC.ci
#'
#' @export
#'
print.sAUC.ci <- function(x, digits = 3, ...){

  if(!inherits(x, "sAUC.ci")) stop("ONLY VALID FOR RESULTS OF sAUC.ci")

  sAUC <- c(x$sAUC, x$CI.L, x$CI.U)
  names(sAUC) <-c("sAUC", "CI.L", "CI.L")

  print(sAUC, digits = digits, ...)

}
