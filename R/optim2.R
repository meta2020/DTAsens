#' optim2 function
#'
#' @description optim2 function
#'
#' @param data data
#'
#' @return convergence list
#' @export
#'
#' @examples
#' optim2(dta, p = 0.7)
#'
optim2 <- function(data,
                  p,

                  ## STARTING EST POINT
                  ## STARTING EST POINTS
                  start7 = c(0, 0, 1, 1, -0.5, 0, 0.7),  ## u1, u2, t1, t2, r, b, c1
                  b.wid = c(0,1), ## SET A VALUE b.wid in [-5, 5]
                  ## CORRECTION
                  c.value = 0.5,
                  c.type = "single",

                  ## OTHERS
                  opt.type = c("optim", "nlminb"),
                  warn = FALSE,
                  show.data =FALSE,
                  p.hat=FALSE,
                  auc.all = FALSE,

                  ## UNIROOT
                  interval = c(-1e2, 1e2),
                  ...
                  ){

  ##
  ## INPUT: DATA PREPROCESS ----------------------------------------------------------
  ##

  if (!any(c("y1", "TP") %in% names(data))) stop("CHECK DATA NAMES")

  n    <- nrow(data)

  if ("TP" %in% names(data)){

    data <- correction(data, value =c.value, type=c.type)

    data <- DOR.data(data)

  }

    y1 <- data$y1
    y2 <- data$y2
    v1 <- data$v1
    v2 <- data$v2

  ##
  ## LIKELIHOOD FUNCTION (8 PARS)-----------------------------------------------
  ##

  fn <- function(par) {


    u1 <- par[1]
    u2 <- par[2]

    t1  <- par[3]
    t11 <- t1^2
    t2  <- par[4]
    t22 <- t2^2
    r   <- par[5]

    t12 <- t1*t2*r

    b  <- par[6]

    c1  <- par[7]
    c11 <- c1^2
    c22 <- 1-c11
    c2  <- sqrt(c22)

    ldor     <- c1*y1 + c2*y2

    se.ldor2 <- c11*v1+c22*v2
    se.ldor  <- sqrt(se.ldor2)

    u.ldor   <- c1*u1 + c2*u2
    t.ldor   <- c11*t11 + c22*t22 + 2*c1*c2*t12

    t        <- ldor/se.ldor

    ##
    ## FUNCTOIN b(Sigma) -------------------------------------------------------
    ##

    f.b <- function(a){

      if (!warn) sq <- suppressWarnings(sqrt(1 + b^2 * (1 + t.ldor/se.ldor2))) else sq <- sqrt(1 + b^2 * (1 + t.ldor/se.ldor2))

      pnorm( (a + b * u.ldor/se.ldor) / sq )

    }


    ##
    ## FIND THE ROOT OF a = a.opt ----------------------------------------------
    ##

    a.p <- function(a) {sum(1/f.b(a), na.rm = TRUE) - n/p}

    if (!warn) a.opt.try <- suppressWarnings(try(uniroot(a.p, extendInt="downX",interval,...), silent = TRUE)) else a.opt.try <- try(uniroot(a.p, extendInt="downX",interval,...), silent = TRUE)

    a.opt <- a.opt.try$root


    ##
    ##  LOGLIKELIHOOD-1 OF y|Sigma ---------------------------------------------
    ##

    det.vec <- (v1+t11)*(v2+t22)-t12^2

    if (!warn) log.det.vec <- suppressWarnings(log(det.vec)) else log.det.vec <- log(det.vec)

    f.l1  <- ((y1-u1)^2*(v2+t22) - 2*(y2-u2)*(y1-u1)*t12 + (y2-u2)^2*(v1+t11)) / det.vec + log.det.vec

    s.l1  <- -0.5*sum(f.l1, na.rm = TRUE)


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

    return(-(s.l1 + s.l2 - s.l3)) ## NEGATIVE

  }

  ##
  ##  INPUT: OPTIMIZATION LOGLIKELIHOOD FUNCTION --------------------------------------
  ##

    opt.type <- match.arg(opt.type)

    ## AUTO-SET START POINTS

    if(is.null(start7)) {

      fit.m <- mvmeta(cbind(y1,y2),S=cbind(v1, rep(0, n), v2), method="ml")

      if(!inherits(fit.m, "try-error")) {

        #v <-  unlist(fit.m$Psi)
        #start7 <- c(fit.m$coefficients, sqrt(v[1]), sqrt(v[4]), v[3]/sqrt(v[1]*v[4]), 0, 0.7)
        p1 <- sqrt(fit.m$Psi[1])
        p2 <- sqrt(fit.m$Psi[4])
        p12<- fit.m$Psi[3]
        start7 <- c(fit.m$coefficients, p1,p2, p12/(p1*p2), 0, 0.7)

      } else start7 <- c(0,0,1,1,-0.5,0,0.7)

    }

    if(opt.type == "optim"){

      opt <- try(optim(start7,
                       fn,
                       method="L-BFGS-B",
                       lower = c(-5, -5,   0,   0,-1, b.wid[1], 0),
                       upper = c( 5,  5, 2.5, 2.5, 0, b.wid[2], 1)
      ),silent = TRUE)

    } else {

      opt <- try(nlminb(start7,
                        fn,
                        lower = c(-5, -5,   0,   0,-1, b.wid[1], 0), ## u1 u2 t1 t2 r b c1
                        upper = c( 5,  5, 2.5, 2.5, 0, b.wid[2], 1)
      ), silent = TRUE)

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

    a.p2 <- function(a){

      bp <- pnorm( (a + b * u.ldor/se.ldor) / sq )

      sum(1/bp, na.rm = TRUE) - n/p

    }

    if (!warn) a.opt.try <- suppressWarnings(try(uniroot(a.p2, extendInt="downX",interval,...), silent = TRUE)) else a.opt.try <- try(uniroot(a.p2, extendInt="downX",interval,...), silent = TRUE)

    a.opt <- a.opt.try$root


    ##
    ## AUC CALC----------------------------------------
    ##

    auc.try <- try(integrate(function(x) {

      plogis(u1 + (t12/t22) * (-qlogis(x) - u2))

    }, 0, 1), silent = TRUE)

    if (auc.all) opt$auc <- auc.try

    if (!inherits(auc.try, "try-error")) auc <- auc.try$value else auc <- NA

    opt$par   <- c(u1, u2, t11, t22, t12, r, auc, b, a.opt, c11, c22)
    names(opt$par) <- c("u1", "u2", "t11", "t22", "t12", "r", "auc", "b", "a", "c11", "c22")

    ##
    ##  P.HAT CALC, FROM b FUNCTION ----------------------------------------
    ##

    ##p.hat <- mean(pnorm(a.opt + opt$par[6]*t))
    if (p.hat){

      bp <- pnorm( (a.opt + b * u.ldor/se.ldor) / sq )

      p.hat <- n/sum(1/bp)

      opt$par   <- c(u1, u2, t11, t22, t12, r, auc, b, a.opt, c11, c22, p.hat)
      names(opt$par) <- c("u1", "u2", "t11", "t22", "t12", "r", "auc", "b", "a", "c11", "c22", "p.hat")

    }

  }

  if (!show.data)  return(opt) else  return(list(data=data, opt=opt))

}


