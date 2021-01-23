#' dtasens1 function
#'
#' @description dtasens1 function
#'
#' @param data data
#' @param p p
#' @param c1 c1
#' @param correct.value par
#' @param correct.type "single", "all"
#' @param start5 A vector of u1 u2 t1 t2 r
#' @param b0 A start value of b
#' @param b.interval par
#' @param a.interval par
#' @param optimize.type par
#' @param pos.r permit positive r
#' @param show.warn.message par
#' @param show.data par
#' @param show.p.hat par
#' @param show.auc.all par

#' @param a.root.extendInt par
#' @param ... par
#'
#' @return convergence list
#'
#' @importFrom stats integrate nlminb optim plogis pnorm qlogis uniroot
#' @importFrom mvmeta mvmeta

#' @examples
#' dtasens1(dta, p = 0.7)
#'
#' @export

dtasens1 <- function(data,   ## 2 FORMAT: N OR Y, make data name as format
                     p,

                     c1 = sqrt(0.5), ##  0<=c11<=1

                     correct.value = 0.5,
                     correct.type = "single",

                     start5 = NULL,  ## u1, u2, t1, t2, r, b
                     b0 = 0,
                     b.interval = c(0, 3),
                     a.interval = c(-5, 5),
                     optimize.type = c("optim", "nlminb"),  ## SAME
                     pos.r = FALSE,

                     show.warn.message = FALSE,
                     show.data = FALSE,
                     show.p.hat = FALSE,
                     show.auc.all = FALSE,

                     a.root.extendInt = "yes",
                     ...
){

  ##
  ## INPUT: DATA PREPROCESS  ----------------------------------------------------------
  ##

  if (!any(c("y1", "TP") %in% names(data))) stop("CHECK DATA NAMES")

  n <- nrow(data)


  if ("TP" %in% names(data)){

    data <- correction(data, value = correct.value, type= correct.type)

    data <- DOR.data(data)

  }

  y1 <- data$y1
  y2 <- data$y2
  v1 <- data$v1
  v2 <- data$v2

  c11 <- c1^2
  c22 <- 1-c11
  c2  <- sqrt(c22)

  ldor     <- c1*y1 + c2*y2
  se.ldor2 <- c11*v1+c22*v2
  se.ldor  <- sqrt(se.ldor2)

  t        <- ldor/se.ldor

  #if (is.null(b.interval))  b.interval <- c(0,10/diff(range(t)))

  ##
  ## LIKELIHOOD FUNCTION (6 PARS)-----------------------------------------------
  ## AVOID SQRT

  fn <- function(par) {


    u1  <- par[1]
    u2  <- par[2]

    t1  <- par[3]
    t11 <- t1^2
    t2  <- par[4]
    t22 <- t2^2

    r   <- par[5]
    t12 <- t1*t2*r

    b   <- par[6]


    u.ldor  <- c1*u1 + c2*u2
    t.ldor  <- c11*t11 + c22*t22 + 2*c1*c2*t12

    ##
    ## FUNCTOIN b(Sigma) -------------------------------------------------------
    ##

    f.b <- function(a){

      if (!show.warn.message) sq <- suppressWarnings(sqrt(1 + b^2 * (1 + t.ldor/se.ldor2))) else sq <- sqrt(1 + b^2 * (1 + t.ldor/se.ldor2))

      pnorm( (a + b * u.ldor/se.ldor) / sq )

    }


    ##
    ## FIND THE ROOT OF a = a.opt ----------------------------------------------
    ##

    a.p <- function(a) {sum(1/f.b(a), na.rm = TRUE) - n/p}

    if (!show.warn.message) a.opt.try <- suppressWarnings(try(

      uniroot(a.p,interval = a.interval, extendInt=c(a.root.extendInt),...), silent = TRUE

    )) else a.opt.try <- try(uniroot(a.p, interval=a.interval, extendInt=c(a.root.extendInt),...), silent = TRUE)

    a.opt <- a.opt.try$root


    ##
    ##  LOGLIKELIHOOD-1 OF y|Sigma ---------------------------------------------
    ##

    det.vec <- (v1+t11)*(v2+t22)-t12^2

    if (!show.warn.message) log.det.vec <- suppressWarnings(log(det.vec)) else log.det.vec <- log(det.vec)

    f.l1 <- ((y1-u1)^2*(v2+t22) - 2*(y2-u2)*(y1-u1)*t12 + (y2-u2)^2*(v1+t11)) / det.vec + log.det.vec

    s.l1 <- -0.5*sum(f.l1, na.rm = TRUE)


    ##
    ##  LOGLIKELIHOOD-2 OF a(a.opt) --------------------------------------------
    ##

    f.l2 <- pnorm(a.opt + b * t)

    s.l2 <- sum( log(f.l2), na.rm = TRUE )

    ##
    ##  LOGLIKELIHOOD-3 OF b(a.opt) --------------------------------------------
    ##

    f.l3 <- f.b(a.opt)

    s.l3 <- sum( log(f.l3), na.rm = TRUE )

    ##
    ##  FINAL LOGLIKELIHOOD ----------------------------------------------------
    ##

    return(-(s.l1 + s.l2 - s.l3))

  }

  ##
  ##  INPUT: OPTIMIZATION LOGLIKELIHOOD FUNCTION --------------------------------------
  ##

  optimize.type <- match.arg(optimize.type)

  ## AUTO-SET START POINTS

  if(is.null(start5)) {

    fit.m <- mvmeta(cbind(y1,y2),S=cbind(v1, rep(0, n), v2), method="ml")

    if(!inherits(fit.m, "try-error")) {

      p1 <- round(sqrt(fit.m$Psi[1]),1)
      p2 <- round(sqrt(fit.m$Psi[4]),1)
      p.r<- round(fit.m$Psi[3]/(p1*p2),1)
      start6 <- c(round(fit.m$coefficients,1), p1, p2, p.r, b0)

    } else start6 <- c(0, 0, 0.5, 0.5, -0.4, b0)

  } else start6 <- c(start5, b0)

  if(!pos.r) r.up <- 0 else  r.up <- 1

  if(optimize.type == "optim"){

    opt <- try(optim(start6,
                     fn,
                     method="L-BFGS-B",
                     lower = c(-5, -5, 0, 0, -1, b.interval[1]),
                     upper = c( 5,  5, 3, 3, r.up , b.interval[2])
    ), silent = TRUE)


  } else{

    opt <- try(nlminb(start6,
                      fn,
                      lower = c(-5, -5, 0, 0, -1, b.interval[1]), ## u1 u2 t1 t2 r b
                      upper = c( 5,  5, 3, 3, r.up, b.interval[2])
    ),silent = TRUE)

  }


  if(!inherits(opt,"try-error")) {

    #names(opt$convergence) <- c("conv")

    ##
    ##  OUTPUT: ALL PARAMETERS -------------------------------------------------
    ##

    u1  <- opt$par[1]
    u2  <- opt$par[2]
    t1  <- opt$par[3]
    t11 <- t1^2
    t2  <- opt$par[4]
    t22 <- t2^2
    r   <- opt$par[5]
    t12 <- t1*t2*r

    b  <- opt$par[6]

    t.ldor <- c11*t11 + c22*t22 + 2*c1*c2*t12
    u.ldor <- c1*u1 + c2*u2
    sq     <- sqrt(1 + b^2 * (1 + t.ldor/se.ldor2))

    ##
    ## ALPHA CALC --------------------------------------------------------
    ##

    a.p2 <- function(a){

      bp <- pnorm( (a + b * u.ldor/se.ldor) / sq )

      sum(1/bp, na.rm = TRUE) - n/p

    }

    if (!show.warn.message) a.opt.try <- suppressWarnings(try(

      uniroot(a.p2, interval = a.interval, extendInt = a.root.extendInt,...), silent = TRUE

    )) else a.opt.try <- try(uniroot(a.p2, interval = a.interval, extendInt=a.root.extendInt, ...), silent = TRUE)

    a.opt <- a.opt.try$root

    ##
    ## AUC CALC----------------------------------------
    ##

    auc.try <- try(sAUC(c(u1,u2, t1, t2, r)), silent = TRUE)

    if (show.auc.all) opt$auc <- auc.try

    if (!inherits(auc.try, "try-error")) auc <- auc.try$value else auc <- NA

    opt$par <- c(u1, u2, t1, t2, r, t12, auc, b, a.opt, c11, c22)

    names(opt$par) <- c("u1", "u2", "t1", "t2", "r", "t12", "auc", "b", "a", "c11", "c22")

    ##
    ##  P.HAT CALC, FROM b FUNCTION ----------------------------------------
    ##

    if (show.p.hat){

      bp <- pnorm( (a.opt + b * u.ldor/se.ldor) / sq )

      p.hat <- n/sum(1/bp)

      opt$par   <- c(u1, u2, t1, t2, r, t12, auc, b, a.opt, c11, c22, p.hat)

      names(opt$par) <- c("u1", "u2", "t1", "t2", "r", "t12", "auc", "b", "a", "c11", "c22", "p.hat")

    }


  }

  #class(opt) <- "DTAsens"

  if (!show.data) return(opt) else return(list(data=data, opt=opt))

}


