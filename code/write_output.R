write_output <- function(m){
  eq <- m$eq
  mlist <- vector("list",length=eq)
  for (i in 1:eq){
    c <- as.vector(m$coeff[[i]])
    se <- as.vector(sqrt(diag(m$cov_matrix[[i]])))
    tr<- c/se
    pr<-pnorm(abs(as.matrix(tr)), lower.tail=FALSE)*2
    pr <- as.numeric(pr)
    mlist[[i]] <- createTexreg(coef.names = m$x_names[[i]], coef = c, se = se, pvalues = pr)
  }
  mlist
}