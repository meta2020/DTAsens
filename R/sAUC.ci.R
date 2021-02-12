#' sAUC.ci function
#'
#' @description Parametric Bootstrap CI of sAUC
#'
#' @param object data
#' @param B p
#' @param ncores ncores
#' @param type c1
#' @param ci.level par
#'
#' @return convergence list
#'
#' @importFrom foreach foreach %dopar%
#' @importFrom doSNOW registerDoSNOW
#' @importFrom parallel makeCluster detectCores stopCluster
#' @importFrom stats quantile rnorm sd
#' @importFrom utils setTxtProgressBar txtProgressBar
#'
#' @export
#' @examples
#'
#' opt1 <- dtasens1(IVD, p = 0.7)
#' (ci <- sAUC.ci(opt1))
#'
sAUC.ci <- function(object, B = 10, ncores, type = "SOCK", ci.level = 0.95)
{
  requireNamespace("foreach")

  if(!inherits(object, "DTAsens")) stop("Only valid for dtasens1 or dtasens2")

  data <- object$data
  S  <- nrow(data)
  y1 <- data$y1
  y2 <- data$y2
  v1 <- data$v1
  v2 <- data$v2

  if(missing(ncores)) ncores <- parallel::detectCores()

  cl <- makeCluster(ncores, type = type)
  registerDoSNOW(cl)

  pb <- txtProgressBar(max = B, style = 3, width = 40)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)

  sauc.t <- foreach(r=1:B, .combine=c, .packages="DTAsens", .options.snow = opts) %dopar% {

    y1.t <- sapply(1:S, function(i) rnorm(1,y1[i],v1[i]))
    y2.t <- sapply(1:S, function(i) rnorm(1,y2[i],v2[i]))

    data.t <- data.frame(y1 = y1.t, y2 = y2.t, v1 = v1, v2 = v2)
    opt1.t <- dtasens1(data.t, p = round(object$p.hat,2))
    sAUC(opt1.t$par[1:5])$value
  }

  close(pb)
  stopCluster(cl)

  q1 <- quantile(scale(sauc.t), probs = (1-ci.level)/2)
  q2 <- quantile(scale(sauc.t), probs = 1-(1-ci.level)/2)
  se <- sd(sauc.t)
  sauc <- unlist(object$auc.all$value)

  list <- list(sAUC = sauc,
       CI.L = sauc+q1*se,
       CI.U = sauc+q2*se,
       cluster = cl)

  class(list) <- "sAUC.ci"

  list

}

#' @export

print.sAUC.ci <- function(object, digits = 3, ...){

  if(!inherits(object, "sAUC.ci")) stop("Only valid for sAUC.ci")

  print(list(sAUC = object$sAUC, CI.L = object$CI.L, CI.U= object$CI.U), digits = digits)

  #NextMethod("print")

}
