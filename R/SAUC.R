##
## SAUC BY SROC OR HSROC
## NOT OUTPUT
##

##
## DID for SROC
##

.DID.sroc <- function(u1, u2, t1, t2, r, var.matrix){

	Q1 <- function(x) {

		g <- plogis(u1 - (t1*t2*r/(t2^2)) * (qlogis(x) + u2))
		g*(1-g)
	}

	Q2 <- function(x) {

		g <- plogis(u1 - (t1*t2*r/(t2^2)) * (qlogis(x) + u2))
		p.u2 <- (-r*t1/t2)*g*(1-g)
	}

	Q3 <- function(x) {

		g <- plogis(u1 - (t1*t2*r/(t2^2)) * (qlogis(x) + u2))
		p.t1 <- (-r/t2*(qlogis(x)+u2))*g*(1-g)

	}

	Q4 <- function(x) {

		g <- plogis(u1 - (t1*t2*r/(t2^2)) * (qlogis(x) + u2))
		p.t2 <- r*t1/t2^2*( qlogis(x)+u2)*g*(1-g)

	}

	Q5 <- function(x) {

		g <- plogis(u1 - (t1*t2*r/(t2^2)) * (qlogis(x) + u2))
		p.r  <- (-t1)/t2*(qlogis(x) + u2)*g*(1-g)
	}

	fd <- c(integrate(Q1, 0, 1)$value,
									integrate(Q2, 0, 1)$value,
									integrate(Q3, 0, 1)$value,
									integrate(Q4, 0, 1)$value,
									integrate(Q5, 0, 1)$value
	)

	(fd %*% var.matrix %*% fd)

}



##
## DID for HSROC (not change yet)
##

.DID.hsroc <- function(u1, u2, t1, t2, r, var.matrix){

	Q1 <- function(x) {

		g <- plogis(u1 + u2*t1/t2 + t1/t2*qlogis(x))

		p.u1 <- g*(1-g) * 1
	}

	Q2 <- function(x) {

		g <- plogis(u1+u2*t1/t2 + t1/t2*qlogis(x))

		p.u2 <- g*(1-g) * (t1/t2)
	}

	Q3 <- function(x) {

		g <- plogis(u1+u2*t1/t2 + t1/t2*qlogis(x))
		p.t1 <- g*(1-g) * (u2 + qlogis(x))/t2

	}

	Q4 <- function(x) {

		g <- plogis(u1+u2*t1/t2 + t1/t2*qlogis(x))
		p.t2 <- g*(1-g) * (-1/t2^2)*(u2*t1+t1*qlogis(x))

	}



	fd <- c(integrate(Q1, 0, 1)$value,
									integrate(Q2, 0, 1)$value,
									integrate(Q3, 0, 1)$value,
									integrate(Q4, 0, 1)$value,
									0
	)

	(fd %*% var.matrix %*% fd)

}




##
## SAUC AND CI -----------------------------------------------------------------
##

SAUC.ci <- function(
	u1, u2, t1, t2, r,
	var.matrix,
	sauc.type = c("sroc", "hsroc"),
	ci.level){

  sauc.type <- match.arg(sauc.type)

		if (sauc.type=="sroc") {

		sroc <- function(x) plogis(u1 - (t1*t2*r/(t2^2)) * (qlogis(x) + u2))
		sauc.try <- try(integrate(sroc, 0, 1))
		if(!inherits(sauc.try, "try-error")) sauc <- sauc.try$value else sauc <- NA

		sauc.lb <-  plogis(qlogis(sauc) + qnorm((1-ci.level)/2, lower.tail = TRUE) *
																							suppressWarnings(
																								sqrt(.DID.sroc(u1, u2, t1, t2, r, var.matrix))/(sauc*(1-sauc))) )

		sauc.ub <-  plogis(qlogis(sauc) + qnorm((1-ci.level)/2, lower.tail = FALSE)*
																							suppressWarnings(
																								sqrt(.DID.sroc(u1, u2, t1, t2, r, var.matrix))/(sauc*(1-sauc))) )

	}  else {


	f <- function(x){

		b  <- sqrt(t2/t1)
		# Theta <- 0.5*(b*u1 - u2/b) ##coef[2] is the fpr, so need to change sign!
		# Lambda <- b*u1 + u2/b ##coef[2] is the fpr, so need to change sign!
		# sigma2theta <- 0.5*(t1*t2 - t12) ##coef[2] is the fpr, so need to change sign of Psi[1,2] as well!
		# sigma2alpha <- 2*(t1*t2 + t12)  ##coef[2] is the fpr, so need to change sign of Psi[1,2] as well!
		# beta <- log(t2/t1)

		# return(plogis(Lambda*exp(-beta/2) + exp(-beta)*qlogis(x)))

		return( plogis(u1+u2*t1/t2 + t1/t2*qlogis(x)) )

		# return(
		#   {plogis(exp(-beta/2)*(0.5*Lambda + Theta)+ (0.25*sigma2alpha-sigma2theta)/(0.25*sigma2alpha+sigma2theta)*exp(-beta)*(-qlogis(x) - exp(beta/2)*(0.5*Lambda - Theta)))}
		# )
	}

	sauc.try <- try(integrate(f, 0, 1))
	if(!inherits(sauc.try, "try-error")) sauc <- sauc.try$value else sauc <- NA

	sauc.lb <-  plogis(qlogis(sauc) + qnorm((1-ci.level)/2, lower.tail = TRUE) *
																					suppressWarnings(
																						sqrt(.DID.hsroc(u1, u2, t1, t2, r, var.matrix))/(sauc*(1-sauc))) )

	sauc.ub <-  plogis(qlogis(sauc) + qnorm((1-ci.level)/2, lower.tail = FALSE)*
																					suppressWarnings(
																						sqrt(.DID.hsroc(u1, u2, t1, t2, r, var.matrix))/(sauc*(1-sauc))) )

}

sauc.ci <- c(sauc, sauc.lb, sauc.ub)
names(sauc.ci) <- c("sauc", "sauc.lb", "sauc.ub")

sauc.ci
}


##
## FOR SROC CI
##

.QIQ.sroc <- function(x, u1, u2, t1, t2, r, inv.I.fun.m) {

  sapply(1: length(x), function(i){

    Q <- c(1, -r*t1/t2, -r/t2*(qlogis(x[i])+u2), r*t1/t2^2*(qlogis(x[i])+u2), -t1/t2*(qlogis(x[i])+u2))

    (Q %*% inv.I.fun.m %*% Q)

  })

}


