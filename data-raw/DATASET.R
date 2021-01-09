## code to prepare `DATASET` dataset goes here

##
## SIMULATION DATA (POPULATION) FROM BI-NORMAL DISTRIBUTION
##
## CREATE: 2020-11-17 COPY FROM BN.DATA
## REVISE: 2021-01-04
##

bn.data <- function(S,          ## HOW MANY STUDIES
                    se, sp,       ## SENS AND SPEC
                    t11, t22, r   ## t1^2, t2^2, t12=r*t1*t2
){

  # v1 <- rchisq(S, v1.df)
  # v1[v1>2.5] <- 2.5
  # v1[v1<0.004] <- 0.004
  # v2 <- rchisq(S, v2.df)
  # v2[v2>2.5] <- 2.5
  # v2[v2<0.004] <- 0.004

  v1 <- rnorm(S, 0.5, 0.5)^2
  #v1[v1>2.5] <- 2.5
  #v1[v1<0.004] <- 0.004
  v2 <- rnorm(S, 0.5, 0.5)^2
  #v2[v2>2.5] <- 2.5
  #v2[v2<0.004] <- 0.004

  if (se<0 || se>1) stop("CHECK SENS IN [0,1]")
  if (sp<0 || sp>1) stop("CHECK SPEC IN [0,1]")

  u1 <- qlogis(se)
  u2 <- qlogis(sp)

  u  <- c(u1, u2)
  t12<- r*sqrt(t11*t22)

  ## Sigma+Omega

  SO <- lapply(1:S, function(s){

    matrix(c(v1[s]+t11, t12, t12, v2[s]+t22),2,2)

  })

  ## CHECK PD

  check.PD <- sapply(1:S, function(s){

    eigen(SO[[s]])$values

  })

  if (any(as.vector(check.PD)<= 0)) stop("VAR-COV MATRIX (S+O) is NOT PD")

  ## y FROM N(u, SO)

  Y <- t(sapply(1:S, function(s) mvtnorm::rmvnorm(1, u, SO[[s]])))

  ## SENS AND SPEC

  X <- plogis(Y)

  ## FINAL DATAFRMAE WITH NAME (y1, y2, v1, v2)

  ldor.t <- rowSums(Y)/sqrt(v1+v2)

  DT <- cbind.data.frame(X, Y, v1, v2, ldor.t)

  colnames(DT) <- c("se", "sp", "y1", "y2", "v1", "v2", "ldor.t")

  return(DT)
}

dta <- bn.data(20,0.8,0.8,0.5,0.5, -0.6)


usethis::use_data(dta, overwrite = TRUE)
