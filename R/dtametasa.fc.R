#' @title Sensitivity analysis of diagnostic meta-analysis with fixed c
#'
#' @description DTA sensitivity analysis with pre-specified (fixed) c1 and c2
#'
#' @param data Data with variable names either
#' \{TP, FN, TN, FP\} or \{y1, y2, v1, v2\}.
#' If not, please change the variable names.
#' Either data with the number of subjects or logit transformed data works.
#'
#' @param p Specified probability of selection (or publication); Pr(select) = p
#'
#' @param c1.sq Pre-specified \eqn{c_1^2}{c1-square}.
#' Default value is 0.5;
#' hence, \eqn{c_2^2 = 1 - c_1^2}{c2-square = 1-c1-square}
#'
#' @param correct.value Imputation value for ``continuity correction''.
#' Default is 0.5;
#' When zero cell exists, add some value.
#'
#' @param correct.type 2 ways for ``continuity correction''.
#' Default is \code{all}: add \code{correct.value} into all the cells of the row with zero value;
#' if \code{single}: add \code{correct.value} into only the cells with zero value.
#'
#' @param brem.init Initial values used for estimating the parameters in the bivariate random effects model.
#' Default is \code{NA}, which set the estimation without considering publication bias as initial values;
#' It should be changed by a vector of u1 u2 t1 t2 r, \code{c(u1, u2, t1, t2, r)}.
#' Bad initial values will cause non-convergence results.
#'
#' @param b.init An initial value of \eqn{\beta}{b}.
#' Default is 0.1.
#' Avoid to start from 0.
#' Bad initial value will cause non-convergence results.
#'
#' @param b.interval The constraint interval for \eqn{\beta}{b}.
#' Default is in the interval \code{c(0,2)}.
#' Positive side (>0) is recommended to be adopted.
#' The estimation of \eqn{\beta}{b} will be searched within the interval.
#'
#' @param a.interval The constraint interval for \eqn{\alpha}{a}.
#' Default is in the interval \code{c(-3,3)}.
#' then, the root of \eqn{\alpha}{a} will be searched within the interval.
#'
#' @param neg.r Whether to permit correlation parameter \eqn{\rho}{r} to be a negative value.
#' Default is \code{FALSE}, \eqn{\rho}{r} in [-1, 1].
#' If \code{TRUE}, \eqn{\rho}{r} is in [-1, 0)
#'
#' @param ci.level The significant value for confidence interval.
#' Default is 0.95, hence, a 2-tailed 95% confidence interval will be calculated by
#' profile likelihood.
#'
#' @param show.warn.message Whether to show the warning messages.
#' Default is to hide warning messages.
#'
#' @param a.root.extendInt See \code{extendInt} augment in function \code{\link[stats]{uniroot}}.
#'
#' @param ... See other augments in function \code{\link[stats]{uniroot}}.
#'
#' @return
#' confidence interval,
#' convergence list,
#' logit transformed data
#'
#' @importFrom stats integrate nlminb plogis pnorm qlogis uniroot qchisq
#' @importFrom mvmeta mvmeta
#'
#' @examples
#'
#' sa1 <- dtametasa.fc(IVD, p = 0.7)
#' sa1
#'
#' @details
#' \describe{
#' \item{Continuity correction}{\url{https://en.wikipedia.org/wiki/Continuity_correction}}
#' }
#'
#' @seealso \code{\link[stats]{uniroot}},
#' \code{\link{dtametasa.rc}}.
#'
#' @export

dtametasa.fc <- function(data,   ## 2 FORMAT: N OR Y, make data name as format
                     p,
                     c1.sq = 0.5, ##  square c1
                     correct.value = 0.5,
                     correct.type = "all",
                     brem.init = NULL,  ## u1, u2, t1, t2, r
                     b.init = 1,
                     b.interval = c(0, 2),
                     a.interval = c(-3, 3),
                     neg.r = FALSE,
                     ci.level = 0.95,
                     show.warn.message = FALSE,
                     a.root.extendInt = "downX",
                     ...
){

  ##
  ## INPUT: DATA PREPROCESS  ----------------------------------------------------------
  ##

  if (p <=0 || p>1) stop("PLEASE MAKE SET SELECTION PROB: P in (0, 1]",  call. = FALSE)

  if (!any(c("y1","y1", "v1", "v2", "TP", "FN", "TN", "FP") %in% names(data))) stop("DATA' COLNAMES MUST BE 'TP/FN/TN/FP' OR 'y1/y2/v1/v2'", call. = FALSE)

  n <- nrow(data)

  if ("TP" %in% names(data)){

    data <- correction(data, value = correct.value, type= correct.type)

    data <- logit.data(data)

  }

  y1 <- data$y1
  y2 <- data$y2
  v1 <- data$v1
  v2 <- data$v2


  c11 <- c1.sq
  c22 <- 1-c11
  c1  <- sqrt(c11)
  c2  <- sqrt(c22)

  ##
  ##  INPUT: OPTIMIZATION LOGLIKELIHOOD FUNCTION --------------------------------------
  ##

  ## AUTO-SET START POINTS

  #start6 <- c(0, 0, 0.1, 0.1, -0.1, b.init)

  if(is.null(brem.init)) {

    fit.m <- mvmeta::mvmeta(cbind(y1,y2),S=cbind(v1, rep(0, n), v2), method="ml")

    if(!inherits(fit.m, "try-error")) {

      if(fit.m$converged){

        p1 <- sqrt(fit.m$Psi[1])
        p2 <- sqrt(fit.m$Psi[4])
        p.r<- fit.m$Psi[3]/(p1*p2)

        start6 <- c(fit.m$coefficients, p1, p2, p.r, b.init)

      } else start6 <- c(0, 0, 0.1, 0.1, -0.1, b.init)

    } else start6 <- c(0, 0, 0.1, 0.1, -0.1, b.init)

  } else start6 <- c(brem.init, b.init)


  eps <- sqrt(.Machine$double.eps)

  if(neg.r) r.up <- eps else  neg.r <- 1

  fn <- function(par) llk.o(par = c(par[1:6], c1),
                            data = data, p = p,
                            a.root.extendInt = a.root.extendInt, a.interval = a.interval,
                            show.warn.message = show.warn.message, ...)

  opt <- try(nlminb(start6,
                   fn,
                   lower = c(-5, -5, eps, eps, -1, b.interval[1]),
                   upper = c( 5,  5, 3, 3, neg.r , b.interval[2])
  ), silent = TRUE)


  if(!inherits(opt,"try-error")) {

    ##
    ##  OUTPUT: ALL PARAMETERS -------------------------------------------------
    ##

    u1  <- opt$par[1]
    se  <- plogis(u1)
    u2  <- opt$par[2]
    sp  <- plogis(u2)
    t1  <- opt$par[3]
    t11 <- t1^2
    t2  <- opt$par[4]
    t22 <- t2^2
    r   <- opt$par[5]
    t12 <- t1*t2*r

    b  <- opt$par[6]

    t.ldor <- c11*t11 + c22*t22 + 2*c1*c2*t12
    u.ldor <- c1*u1 + c2*u2

    se.ldor2 <- c11*v1+c22*v2
    se.ldor  <- sqrt(se.ldor2)

    sq     <- sqrt(1 + b^2 * (1 + t.ldor/se.ldor2))

    ##
    ## ALPHA CALC --------------------------------------------------------
    ##

   a.p <- function(a) { sum(1/ pnorm( (a + b * u.ldor/se.ldor) / sq ), na.rm = TRUE) - n/p }

    if (!show.warn.message) a.opt.try <- suppressWarnings(try(
      uniroot(a.p, interval = a.interval, extendInt = a.root.extendInt,...),
      silent = TRUE)) else a.opt.try <- try(
        uniroot(a.p, interval = a.interval, extendInt=a.root.extendInt, ...), silent = TRUE)

    a.opt <- a.opt.try$root


    ##
    ##  HESSIANS -------------------------------------------------
    ##

    opt$num.hessian <- numDeriv::hessian(fn, opt$par)
    rownames(opt$num.hessian) <- c("u1", "u2", "t1", "t2", "r", "b")
    colnames(opt$num.hessian) <- c("u1", "u2", "t1", "t2", "r", "b")

    ##
    ## SAUC CI -------------------------------------------------
    ##

    hes <- opt$num.hessian

    if(p==1) inv.I.fun.m <- solve(hes[1:5,1:5]) else inv.I.fun.m <- solve(hes)

    opt$var.ml <- inv.I.fun.m

    f <- function(x) plogis(u1 - (t1*t2*r/(t2^2)) * (qlogis(x) + u2))


    sauc.try <- try(integrate(f, 0, 1))

    if(!inherits(sauc.try, "try-error")) sauc <- sauc.try$value else sauc <- NA

    sauc.lb2 <-  plogis(qlogis(sauc) + qnorm((1-ci.level)/2, lower.tail = TRUE) *
                          suppressWarnings(
                            sqrt(QIQ1(u1, u2, t1, t2, r, inv.I.fun.m[1:5,1:5]))/(sauc*(1-sauc))) )

    sauc.ub2 <-  plogis(qlogis(sauc) + qnorm((1-ci.level)/2, lower.tail = FALSE)*
                          suppressWarnings(
                            sqrt(QIQ1(u1, u2, t1, t2, r, inv.I.fun.m[1:5,1:5]))/(sauc*(1-sauc))) )

    opt$sauc.ci <- c(sauc, sauc.lb2, sauc.ub2)
    names(opt$sauc.ci) <- c("sauc", "sauc.lb", "sauc.ub")

    #
    # b CI -------------------------------------------------
    #


    if(p==1) opt$b.ci <- c(NA, NA, NA) else {

      b.se <- suppressWarnings(sqrt(inv.I.fun.m[6,6]))
      b.lb <- b + qnorm((1-ci.level)/2, lower.tail = TRUE)*b.se
      b.ub <- b + qnorm((1-ci.level)/2, lower.tail = FALSE)*b.se

      opt$b.ci <- c(b, b.lb, b.ub)

    }

    names(opt$b.ci) <- c("b", "b.lb", "b.ub")


    #s
    ## PAR AND ALL PAR ----------------------------------------
    ##

    opt$par.all <- c(u1, u2, t11, t22, t12, c1, c2,  b, a.opt, sauc, se, sp)

    names(opt$par.all) <- c("u1", "u2", "t11", "t22", "t12", "c1", "c2", "b", "a","sauc", "se", "sp")

    names(opt$par) <- c("u1", "u2", "t1", "t2", "r","b")

    ##
    ##  P.HAT CALC, FROM b FUNCTION ----------------------------------------
    ##

    bp <- pnorm( (a.opt + b * u.ldor/se.ldor) / sq )

    opt$p.hat <- n/sum(1/bp)


    opt$l.data <- data

    class(opt) <- "dtametasa"
}

  opt

}




