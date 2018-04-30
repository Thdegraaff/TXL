print_fgs3sls <- function(x,digits= max(3, getOption("digits") - 2),width=getOption("width"),...) {
  
  cat("\nSpatial Simultaneous Equations Model:\n")
  cat("\nCall:\n")
  print(x$call)
  save.digits <- unlist(options(digits=digits))
  eq <- x$eq
  
  save.digits <- unlist(options(digits=digits))
  on.exit(options(digits=save.digits))
  numx<-vector("numeric",eq)
  
  for (i in 1:eq) {
    
    coeff <- x$coeff[[i]]
    se <- sqrt(diag(x$cov_matrix[[i]]))
    tr<-coeff/se
    pr<-pnorm(abs(as.matrix(tr)), lower.tail=FALSE)*2
    x_names <- x$x_names[[i]]
    y_names <- x$y_names[[i]]
    
    cat(" \n" )
    cat(paste('Equation', i, sep = " ", collapse = ""),"\n", sep = "")
    #cat("\n _______________________________________________________ \n")
    tables<- cbind(coeff, se, tr, pr )

    dimnames(tables)<-list(x_names ,c("Estimate", "Std.Error", "t value", "Pr(>|t|)"))
    
    if(i==eq) {
      legend=TRUE
    } else {
      legend=FALSE
    }
    
    printCoefmat(tables,digits=digits, signif.stars=TRUE,signif.legend=legend)
    cat(" \n" )   
    if(!is.null(x$rho[i])){
       cat(paste('Spatial autoregressive parameter:', round(x$rho[i],4), sep = " ", collapse = ""),"\n", sep = "")
    }
    cat("\n _______________________________________________________ \n")
  }
}