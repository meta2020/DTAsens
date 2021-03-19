#' @title Sensitivity analysis of diagnostic meta-analysis with released c
#'
#' @description DTA sensitivity analysis without pre-specified (with released) c1 and c2
#'
#' @param data Data with variable names either
#' \{TP, FN, TN, FP\} or \{y1, y2, v1, v2\}.
#' If not, please change the variable names.
#' Either data with the number of subjects or logit transformed data works.
#'
#' @param p Specified probability of selection (or publication); Pr(select) = p
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
#' @param c1.sq.init An initial value of \eqn{c_1^2}{c1-square}.
#' Default is 0.5.
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
#' @param positive.r Whether to permit correlation parameter \eqn{\rho}{r} to be a positive value,
#' though it is often a negative value
#' Default \code{TRUE}, to permit positive, in which \eqn{\rho}{r} is in [-1, 1].
#' If \code{FALSE}, \eqn{\rho}{r} is in [-1, 0)
#'
#' @param ci.level The significant value for confidence interval.
#' Default is 0.95, hence, a 2-tailed 95% confidence interval will be calculated by
#' profile likelihood.
#'
#' @param show.warn.message Whether to show the warning messages.
#' Default is to hide warning messages.
#'
#' @param a.root.extendInt See \code{extendInt} augments in function \code{\link[stats]{uniroot}}.
#'
#' @param ... See other augments in function \code{\link[stats]{uniroot}}.
#'
#' @return
#' confidence interval,
#' convergence list,
#' logit transformed data
#'
#' @examples
#'
#' sa2 <- dtametasa.rc(IVD, p = 0.7)
#' sa2
#'
#' @details
#' \describe{
#' \item{Continuity correction}{\url{https://en.wikipedia.org/wiki/Continuity_correction}}
#' }
#'
#' @seealso \code{\link[stats]{uniroot}},
#' \code{\link{dtametasa.fc}}.
#'
#' @export

dtametasa.rc <- function(data,
                  p,
                  correct.value = 0.5,
                  correct.type = "all",
                  brem.init = NULL,  ## u1, u2, t1, t2, r
                  b.init = 1,
                  c1.sq.init = 0.5,
                  b.interval = c(0, 2), ## SET A VALUE b.interval in [-5, 5]
                  a.interval = c(-5, 3),
                  positive.r = TRUE,
                  ci.level = 0.95,
                  show.warn.message = FALSE,
                  a.root.extendInt = "downX",
                  ...
                  ){

  ##
  ## INPUT: DATA PREPROCESS ----------------------------------------------------------
  ##

  if (p <=0 || p>1) stop("PLEASE MAKE SET SELECTION PROB: P in (0, 1]",  call. = FALSE)

  if (!any(c("y1","y1", "v1", "v2", "TP", "FN", "TN", "FP") %in% names(data))) stop("DATA' COLNAMES MUST BE 'TP/FN/TN/FP' OR 'y1/y2/v1/v2'", call. = FALSE)

  n <- nrow(data)

  if ("TP" %in% names(data)){

    data <- correction(data, value = correct.value, type=correct.type)

    data <- logit.data(data)

  }

    y1 <- data$y1
    y2 <- data$y2
    v1 <- data$v1
    v2 <- data$v2


  ##
  ##  INPUT: OPTIMIZATION LOGLIKELIHOOD FUNCTION --------------------------------------
  ##


    ## AUTO-SET START POINTS

    c1 <- sqrt(c1.sq.init)

    if(is.null(brem.init)) {

      fit.m <- mvmeta::mvmeta(cbind(y1,y2),S=cbind(v1, rep(0, n), v2), method="ml")

      if(!inherits(fit.m, "try-error")) {

        if(fit.m$converged){

          p1 <- sqrt(fit.m$Psi[1])
          p2 <- sqrt(fit.m$Psi[4])
          p.r<- fit.m$Psi[3]/(p1*p2)

          start7 <- c(fit.m$coefficients, p1, p2, p.r, b.init, c1)

        } else start7 <- c(0, 0, 0.1, 0.1, -0.1, b.init, c1)

      } else start7 <- c(0, 0, 0.1, 0.1, -0.1, b.init, c1)

    } else start7 <- c(brem.init, b.init, c1)


    eps <- sqrt(.Machine$double.eps)

    if(positive.r) r.up <- 1 else  r.up <- eps

    fn <- function(par) llk.o(par,
                              data = data,
                              p = p,
                              a.root.extendInt = a.root.extendInt, a.interval = a.interval,
                              show.warn.message = show.warn.message, ...)

    opt <- try(nlminb(start7,
                     fn,
                     lower = c(-5, -5, eps, eps,-1, b.interval[1], 0),
                     upper = c( 5,  5, 3, 3,  r.up, b.interval[2], 1)
    ),silent = TRUE)

  if(!inherits(opt,"try-error")) {

    # ##
    # ## CI ----------------------------------------------------
    # ##
    #
    # # u1
    # fx <- function(x) fn(par = c(x, opt$par[c(2:7)]))- opt$objective - qchisq(ci.level,1)/2
    # u1.l <- try(uniroot(fx, c(-5, opt$par[1])), silent = TRUE)
    # u1.u <- try(uniroot(fx, c(opt$par[1],  5)), silent = TRUE)
    # if (!inherits(u1.l, "try-error")) u1.l <- u1.l$root else u1.l <- NA
    # if (!inherits(u1.u, "try-error")) u1.u <- u1.u$root else u1.u <- NA
    #
    # # u2
    # fx <- function(x) fn(par = c(opt$par[1], x, opt$par[3:7]))- opt$objective - qchisq(ci.level,1)/2
    # u2.l <- try(uniroot(fx, c(-5, opt$par[2])), silent = TRUE)
    # u2.u <- try(uniroot(fx, c(opt$par[2],  5)), silent = TRUE)
    # if (!inherits(u2.l, "try-error")) u2.l <- u2.l$root else u2.l <- NA
    # if (!inherits(u2.u, "try-error")) u2.u <- u2.u$root else u2.u <- NA
    #
    # # t1
    # fx <- function(x) fn(par = c(opt$par[1:2], x, opt$par[4:7]))- opt$objective - qchisq(ci.level,1)/2
    # t1.l <- try(uniroot(fx, c(0, opt$par[3])), silent = TRUE)
    # t1.u <- try(uniroot(fx, c(opt$par[3], 3)), silent = TRUE)
    # if (!inherits(t1.l, "try-error")) t1.l <- t1.l$root else t1.l <- NA # 0
    # if (!inherits(t1.u, "try-error")) t1.u <- t1.u$root else t1.u <- NA #3
    #
    # # t2
    # fx <- function(x) fn(par = c(opt$par[1:3], x, opt$par[5:7]))- opt$objective - qchisq(ci.level,1)/2
    # t2.l <- try(uniroot(fx, c(0, opt$par[4])), silent = TRUE)
    # t2.u <- try(uniroot(fx, c(opt$par[4], 3)), silent = TRUE)
    # if (!inherits(t2.l, "try-error")) t2.l <- t2.l$root else t2.l <- NA #0
    # if (!inherits(t2.u, "try-error")) t2.u <- t2.u$root else t2.u <- NA #3
    #
    # # r
    # fx <- function(x) fn(par = c(opt$par[1:4], x, opt$par[6:7]))- opt$objective - qchisq(ci.level,1)/2
    # r.l <- try(uniroot(fx, c(-1,   opt$par[5])), silent = TRUE)
    # r.u <- try(uniroot(fx, c(opt$par[5], r.up)), silent = TRUE)
    # if (!inherits(r.l, "try-error")) r.l <- r.l$root else r.l <- NA #-1
    # if (!inherits(r.u, "try-error")) r.u <- r.u$root else r.u <- NA #r.up
    #
    # # b
    # fx <- function(x) fn(par = c(opt$par[1:5], x,  opt$par[7]))- opt$objective - qchisq(ci.level,1)/2
    # b.l <- try(uniroot(fx, c(b.interval[1], opt$par[6])), silent = TRUE)
    # b.u <- try(uniroot(fx, c(opt$par[6], b.interval[2])), silent = TRUE)
    # if (!inherits(b.l, "try-error")) b.l <- b.l$root else b.l <- NA #b.interval[1]
    # if (!inherits(b.u, "try-error")) b.u <- b.u$root else b.u <- NA #b.interval[2]
    #
    # # c1
    # fx <- function(x) fn(par = c(opt$par[1:6], x))- opt$objective - qchisq(ci.level,1)/2
    # c.l <- try(uniroot(fx, c(0, opt$par[7])), silent = TRUE)
    # c.u <- try(uniroot(fx, c(opt$par[7], 1)), silent = TRUE)
    # if (!inherits(c.l, "try-error")) c.l <- c.l$root else c.l <- NA #0
    # if (!inherits(c.u, "try-error")) c.u <- c.u$root else c.u <- NA # 1
    #
    #
    # opt$ci <- matrix(
    #   c(opt$par[1], u1.l, u1.u,
    #     opt$par[2], u2.l, u2.u,
    #     opt$par[3], t1.l, t1.u,
    #     opt$par[4], t2.l, t2.u,
    #     opt$par[5], r.l,  r.u,
    #     opt$par[6], b.l,  b.u,
    #     opt$par[7], c.l,  c.u), byrow = TRUE, ncol = 3,
    #   dimnames = list(c("u1","u2","t1", "t2", "r", "b", "c1"), c("estimate" ,"ci.low", "ci.up")))
    #

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

    b   <- opt$par[6]

    c1  <- opt$par[7]
    c11 <- c1^2
    c22 <- 1-c11
    c2  <- sqrt(c22)


    u.ldor   <- c1*u1 + c2*u2
    t.ldor   <- c11*t11 + c22*t22 + 2*c1*c2*t12

    se.ldor2 <- c11*v1+c22*v2
    se.ldor  <- sqrt(se.ldor2)

    sq     <- sqrt(1 + b^2 * (1 + t.ldor/se.ldor2))

    ##
    ## ALPHA CALC --------------------------------------------------------
    ##

    a.p <- function(a){ sum(1/ pnorm( (a + b * u.ldor/se.ldor) / sq ), na.rm = TRUE) - n/p }

    if (!show.warn.message) a.opt.try <- suppressWarnings(try(uniroot(a.p, a.interval, extendInt=a.root.extendInt,...), silent = TRUE)) else a.opt.try <- try(uniroot(a.p, a.interval, extendInt=a.root.extendInt,...), silent = FALSE)

    a.opt <- a.opt.try$root


    ##
    ## AUC CALC----------------------------------------
    ##

    auc <- sAUC(c(u1,u2,t22,t12))

    opt$par   <- c(u1, u2, t11, t22, t12, b, a.opt, c11, c22, auc, se, sp)
    names(opt$par) <- c("u1", "u2", "t11", "t22", "t12", "b", "a", "c11", "c22", "sauc", "se", "sp")

    ##
    ##  show.p.hat CALC, FROM b FUNCTION ----------------------------------------
    ##

    ##show.p.hat <- mean(pnorm(a.opt + opt$par[6]*t))

      bp <- pnorm( (a.opt + b * u.ldor/se.ldor) / sq )

      opt$p.hat <- n/sum(1/bp)


      opt$data <- data

      opt$func.name <- "dtametasa.rc"

      opt$pars.infor <- list(p = p,
                            correct.value = correct.value,
                            correct.type = correct.type,
                            brem.init = brem.init,  ## u1, u2, t1, t2, r, b
                            b.init = b.init,
                            c1.sq.init = c1.sq.init,
                            b.interval = b.interval,
                            a.interval = a.interval,
                            positive.r = positive.r,
                            ci.level = ci.level,
                            show.warn.message = show.warn.message,
                            a.root.extendInt = a.root.extendInt)

    class(opt) <- "dtametasa"

  }

  opt

}

