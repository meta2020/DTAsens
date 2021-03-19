#' @title Parametric bootstrapped confidence interval of summary AUC
#'
#' @description Calculate the parametric bootstrap confidence interval (CI) for sAUC
#'
#' @param object object from function \code{dtametasa.fc} or \code{dtametasa.rc}.
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
#' @param set.seed Give a integer to set the seed.
#' Default is NULL, which does not set any seed to fix the bootstrap results.
#'
#' @param hide.progress Whether to hide the progress bar in the calculation.
#' Default is not to hide.
#'
#' @param plot.sroc.ci Whether to show the plot of sROC with the CIs lines.
#' Default is not to plot.
#'
#' @param add.plot.sroc Whether to add the sROC onto a new plot.
#' Default is not to add.
#'
#' @param add.spoint Whether to add the summary point in the sROC plot.
#' Default it not the add.
#'
#' @param sroc.ci.col The color of the CIs of sROC.
#' Default is grey.
#'
#' @param sroc.ci.lty The line type of CIs.
#' Default is solid line.
#'
#' @param sroc.ci.lwd The line width of CIs.
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
#' @importFrom doRNG %dorng%
#' @importFrom graphics matplot


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
#' sa1 <- dtametasa.fc(IVD, p = 0.7)
#' (ci <- sAUC.ci(sa1, B = 5, set.seed = 1, plot.ROC.ci = FALSE))
#'
#' @export

sAUC.ci <- function(object,
                    B = 1000,
                    ncores = 0, type = "SOCK",
                    ci.level = 0.95,
                    set.seed = NULL,
                    hide.progress = FALSE,
                    plot.sroc.ci = FALSE,
                    add.plot.sroc = FALSE,
                    add.spoint = FALSE,
                    sroc.ci.col = "grey",
                    sroc.ci.lty = 1,
                    sroc.ci.lwd = 1,
                    ...)
{
  if(!requireNamespace("foreach"))  install.packages("foreach")   else requireNamespace("foreach")
  if(!requireNamespace("parallel")) install.packages("parallel")  else requireNamespace("parallel")
  if(!requireNamespace("doSNOW"))   install.packages("doSNOW")    else requireNamespace("doSNOW")
  if(!requireNamespace("doRNG"))    install.packages("doRNG")     else requireNamespace("doRNG")

  if(!inherits(object, "dtametasa")) stop("ONLY VALID FOR RESULTS OF dtametasa.fc OR dtametasa.rc")

  data <- object$data

  S  <- nrow(data)
  y1 <- data$y1
  y2 <- data$y2
  v1 <- data$v1
  v2 <- data$v2

  if(ncores == 0) ncores <- detectCores() else ncores <- ceiling(ncores)

  cl <- makeCluster(ncores, type = type)
  registerDoSNOW(cl)

  opts <- NULL

  if (!hide.progress){

    pb <- txtProgressBar(max = B, style = 3, width = 40)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
  }


  if(object$func.name == "dtametasa.fc"){

    set.seed(set.seed)
    par <- foreach(r=1:B, .combine=rbind, .packages = "dtametasa", .options.snow = opts)  %dorng%  {

      y1.t <- sapply(1:S, function(i) rnorm(1,y1[i],sqrt(v1[i])))
      y2.t <- sapply(1:S, function(i) rnorm(1,y2[i],sqrt(v2[i])))

      data.t <- data.frame(y1 = y1.t, y2 = y2.t, v1 = v1, v2 = v2)

      args <- c(list(data = data.t), object$pars.info)
      sa1 <- do.call("dtametasa.fc", args)
      sa1$par[c(1,2,4,5, 10)]

    }
  }

  if(object$func.name == "dtametasa.rc"){

    set.seed(set.seed)

    par <- foreach(r=1:B, .combine=rbind, .packages = "dtametasa", .options.snow = opts)  %dorng%  {

      y1.t <- sapply(1:S, function(i) rnorm(1,y1[i], sqrt(v1[i])))
      y2.t <- sapply(1:S, function(i) rnorm(1,y2[i], sqrt(v2[i])))

      data.t <- data.frame(y1 = y1.t, y2 = y2.t, v1 = v1, v2 = v2)
      args <- c(list(data = data.t), object$pars.info)
      opt2.t <- do.call("dtametasa.rc", args)
      opt2.t$par[c(1,2,4,5, 10)]

  }
  }

  if (!hide.progress) close(pb)
  stopCluster(cl)

  PAR <- as.matrix(par)
  sauc.t   <- PAR[, 5]

  n <- length(sauc.t)
  se <- (n-1)/n * sd(sauc.t, na.rm = TRUE)
  sl.sauc.t <- (sauc.t - mean(sauc.t, na.rm = TRUE))/se

  s.sauc.r <- order(sl.sauc.t)
  #PAR.r <- PAR[, s.sauc.r]

  s.sauc.t <- sort(sl.sauc.t)

  q1 <- quantile(s.sauc.t, probs = (1-ci.level)/2, na.rm = TRUE)
  q2 <- quantile(s.sauc.t, probs = 1-(1-ci.level)/2, na.rm = TRUE)

  sauc <- object$par[10]

  list <- list(sauc = sauc,
       ci.l = max(sauc+q1*se, 0),
       ci.u = min(sauc+q2*se, 1),
       bootstrap.par  = PAR,
       cluster = cl)


  if(plot.sroc.ci){

    fpr.t <- seq(0,1,0.001)
    se.t  <- sapply(1:B, function(i){

      u1  <- PAR[i,1]
      u2  <- PAR[i,2]
      t22 <- PAR[i,3]
      t12 <- PAR[i,4]

      plogis(u1 - (t12/t22) * (qlogis(fpr.t) + u2))

    })


    sROC(object, add.spoint = add.spoint, add = add.plot.sroc, ...)

    ci <- cbind(

      apply(se.t, 1, function(x) quantile(x, (1-ci.level)/2, na.rm = TRUE)),
      apply(se.t, 1, function(x) quantile(x, probs = 1-(1-ci.level)/2, na.rm = TRUE))

      )

    matplot(x = fpr.t, y = ci, type = "l",
            col = sroc.ci.col, lty = sroc.ci.lty, lwd = sroc.ci.lwd,
            add = TRUE)

    list <- c(list, sroc.ci <- list(x = fpr.t, y = ci))


  }

  class(list) <- "sAUC.ci"

  list

}

#' @title Print sAUC.ci results
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

  sauc <- c(x$sauc, x$ci.l, x$ci.u)
  names(sauc) <- c("sauc", "ci.l", "ci.u")

  print(sauc, digits = digits, ...)

}

#' @title Plot sROC.ci results
#'
#' @description Plot the sROC from function \code{\link{sAUC.ci}}
#'
#' @param x object from function \code{\link{sAUC.ci}}.
#'
#' @param sroc.ci.col The color of the CIs of sROC.
#' Default is grey.
#'
#' @param sroc.ci.lty The line type of CIs.
#' Default is solid line.
#'
#' @param sroc.ci.lwd The line width of CIs.
#' Defalt is 1.
#'
#' @param add.sROC.ci Whether to add the plot of sROC with the CIs lines
#' onto a new plot.
#' Default is not to add.
#'
#' @param ci.level The significant value for confidence interval.
#' Default is 0.95, hence, a 2-tailed 95% CIs will be calculated by
#' profile likelihood.
#'
#' @param ... Other augments in function \code{\link{sROC}}
#'
#' @seealso
#' \code{\link{sAUC.ci}};
#' \code{\link[graphics]{matplot}},
#'
#' @rdname plot.sAUC.ci
#'
#' @examples
#'
#' sa1 <- dtametasa.fc(IVD, p = 0.7)
#'
#' sROC(sa1)
#' ci <- sAUC.ci(sa1, B = 5, set.seed = 1, plot.ROC.ci = FALSE)
#'
#' plot(ci)
#' p <- plot(ci)
#'
#' @export
#'

plot.sAUC.ci <- function(x,
                         sroc.ci.col = "grey",
                         sroc.ci.lty = 1,
                         sroc.ci.lwd = 1,
                         add.sROC.ci=TRUE,
                         ci.level = 0.95,
                         ...){

  if(!inherits(x, "sAUC.ci")) stop("ONLY VALID FOR RESULTS OF sAUC.ci")

  PAR <- x$bootstrap.par
  B <- dim(PAR)[1]


  fpr.t <- seq(0,1,0.001)
  se.t  <- sapply(1:B, function(i){

    u1  <- PAR[i,1]
    u2  <- PAR[i,2]
    t22 <- PAR[i,3]
    t12 <- PAR[i,4]

    plogis(u1 - (t12/t22) * (qlogis(fpr.t) + u2))

  })


  #sROC(object, add.spoint = add.spoint, add = add.plot.ROC.ci, ...)

  ci <- cbind(

    apply(se.t, 1, function(x) quantile(x, (1-ci.level)/2, na.rm = TRUE)),
    apply(se.t, 1, function(x) quantile(x, probs = 1-(1-ci.level)/2, na.rm = TRUE))

  )

  matplot(x = fpr.t, y = ci, type = "l",
          col = sroc.ci.col, lty = sroc.ci.lty, lwd = sroc.ci.lty,
          add = TRUE)

  list <- c(x, sroc.ci <- list(x = fpr.t, y = ci))


}
