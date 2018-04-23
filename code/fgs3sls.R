fgs3sls <- function(formula, data=list(), w, lags = NULL, errors= NULL){

  n <- dim(data)[[1]]
  eq <- length(formula)
  
  xlist<-vector("list",length=eq)
  ylist<-vector("list",length=eq)
  K<-vector("numeric",length=eq) # Number of coefficient in each specification
 
  for (i in 1:eq){
    xlist[[i]] <- model.matrix(formula[[i]], data = data)
    ylist[[i]] <- as.matrix(model.frame(formula[[i]], data = data)[1])
    K[i] <- dim(xlist[[i]])[[2]]
    
  } 
  
  allnames <- NULL
  x_names <- vector("list",length=eq)
  y_names <- NULL
  
  for (i in 1:eq) {
    x_names[[i]] <- colnames(xlist[[i]])
    y_names <- c(y_names, colnames(ylist[[i]]))
    allnames <- c(allnames,colnames(xlist[[i]]))
  }
  xall <- matrix(unlist(xlist),n,sum(K))
  colnames(xall) <- allnames

  allnames <- unique(allnames)
  sel <- vector("numeric",length=length(allnames))
  for (j in 1:length(allnames)) sel[j] <- unique(which(colnames(xall)==allnames[j]))[1]
  xall <- xall[,sel]
  wxall <- w %*% xall
  wwxall<- w %*% wxall
  
  h <- cbind(xall, wxall,wwxall)
  hhinv <- solve(t(h)%*%h)
  p_h <- h%*%(hhinv)
  p_h <- p_h %*% t(h)
  

    
}