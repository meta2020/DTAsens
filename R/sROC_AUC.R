##
## sROC
##

sROC <- function(u1, u2, t12, t2, 
                 par = NULL,
                 add = FALSE,
                 s.point = TRUE,
                 ...) {
  
  if (!is.null(par)) {
    
    u1  <- par[1]
    u2  <- par[2]
    t12 <- par[5]
    t2  <- par[4]
    
  } 
  
  if (add){
    
    curve(plogis(u1 + (t12/t2) * (-qlogis(x) - u2)), xlab = "FPR", ylab = "TPR", add = TRUE, ...)
  
    if(s.point) points(plogis(-u2), plogis(u1),...)
  }
  
  else{
    
    curve(plogis(u1 + (t12/t2) * (-qlogis(x) - u2)), xlab = "FPR", ylab = "TPR", 
        xlim = c(0,1), ylim = c(0,1),...)
    
    if(s.point) points(plogis(-u2), plogis(u1),...)
  
  }
  
}

##
## sAUC
##
sAUC <- function(u1, u2, t12, t2, 
                 par = NULL){
  
  if (!is.null(par)) {
    
    u1  <- par[1]
    u2  <- par[2]
    t12 <- par[5]
    t2  <- par[4]
    
  } 
  
  integrate(function(x) {

    plogis(u1 + (t12/t2) * (-qlogis(x) - u2))

  }, 0, 1)$value
}


#sROC(par = Y$par)


##
## sROC for par matrix
##

sROC.bunch.add <- function(par.matrix, 
                           s.point=TRUE, s.line = FALSE, null.plot=TRUE,
                           ...) {
  
  if (null.plot) plot(NULL, xlim=c(0,1), ylim=c(0,1), xlab = "FPR", ylab = "TPR", ...)
  
  cols <- gray.colors(ncol(par.matrix), gamma = 1, start = 0, end = 0.8)

  for (i in 1:ncol(par.matrix)) {
    
    u1 = par.matrix[1,i]
    u2 = par.matrix[2,i]
    
    t12 = par.matrix[5,i]
    t2  = par.matrix[4,i]
    
    curve(plogis(u1 + (t12/t2) * (-qlogis(x) - u2)), 0, 1, col = cols[i], add = TRUE, #, 
          xlab = "FPR", ylab = "TPR", lwd=2)
  }
  
  sens <- plogis(par.matrix[1,])
  spec <- plogis(par.matrix[2,])
  if (s.point) points(1-spec, sens, col=cols,...)
  if (s.line)  lines(1-spec, sens)

}


##
## sAUC for par matrix
##
sAUC.bunch <- function(par.matrix){
  
  #par.matrix <- x
  
  sapply(1:ncol(par.matrix), function(i) {
    
    
    u1 = par.matrix[1,i]
    u2 = par.matrix[2,i]
    
    t12 = par.matrix[5,i]
    t22  = par.matrix[4,i]
    
    if (NA %in% par.matrix[c(1,2,5,4),i]) {auc <- NA} else{
      
      auc.try <- try(integrate(function(x) {
        
        plogis(u1 + (t12/t22) * (-qlogis(x) - u2))
        
      }, 0, 1), silent = TRUE)
      
      if(!class(auc.try)=="try-error") auc.try$value else NA
      
    }
    
  })

}

#sAUC.bunch(x)
  
##
## The second ttpe
##

sROC.bunch.add2 <- function(Lambda, Beta) {
  
    curve(plogis(Lambda*exp(-Beta/2) + exp(-Beta)*qlogis(x)), 0,1, col = 2, add = TRUE, lty = 6)
  
}
