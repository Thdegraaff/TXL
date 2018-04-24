fgs3sls <- function(formula, data=list(), w, lags = NULL, errors= NULL){
  
  n <- dim(data)[[1]]
  eq <- length(formula)
  
  xlist<-vector("list",length=eq)
  ylist<-vector("list",length=eq)
  K<-vector("numeric",length=eq) # Number of coefficients in each specification
  rho <- vector("numeric", length = eq)
  sigma <- vector("numeric", length = eq)
  
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
  
  # Create matrix of instruments
  h <- cbind(xall, wxall, wwxall)
  # Create projection matrix of matrix of instruments
  hhinv <- solve(crossprod(h))
  p_h <- h%*%(hhinv)
  p_h <- p_h %*% t(h)
  
  # Initialisation
  Z   <- vector("list",length=eq)
  PHZ  <- vector("list",length=eq)
  for (i in 1:eq) {
    temp = NULL
    temp_php = NULL
    temp_names = NULL
    for (j in 1:eq) {
      if (lags[[i]][j]) {
        temp <- cbind(temp,w %*% ylist[[j]])
        temp_php <- cbind(temp_php,p_h%*%(w %*% ylist[[j]]))
        temp_names <-c(temp_names, paste0("d_", y_names[j]))
      }
    }
    colnames(temp) <- temp_names
    colnames(temp_php) <- temp_names
    Z[[i]] <- cbind(temp, xlist[[i]])
    PHZ[[i]] <- cbind(temp_php, xlist[[i]])
  }
  print("Initialisation")
  
  # First step 2sls estimation
  
  delta <- vector("list",length=eq)
  PHu   <- vector("list",length=eq)
  
  for (i in 1:eq) {
    delta[[i]] <- (solve(t(PHZ[[i]]) %*% Z[[i]] ) %*% t(PHZ[[i]]) ) %*% ylist[[i]]
    PHu[[i]] <- ylist[[i]] - Z[[i]]%*%delta[[i]]
  }
  
  print("First step 2SLS estimation")
  
  # GM estimator
  
  trace_ww <- sum(diag(crossprod(w)))
  for (i in 1:eq) {
    w_PHu <- w %*% PHu[[i]]
    ww_PHu <- w %*% (w %*% PHu[[i]])
    # Create matrices
    g_1 <- crossprod(PHu[[i]])
    g_2 <- crossprod(w_PHu)
    g_3 <- crossprod(PHu[[i]], w_PHu)
    g <- (1/n)*matrix(c(g_1, g_2, g_3), nrow =3, ncol = 1)
    
    G_1 <- 2*g_3
    G_2 <- 2*crossprod(w_PHu, ww_PHu)
    G_3 <- crossprod(PHu[[i]], w_PHu) + g_2
    G_4 <- -1*g_2
    G_5 <- -1*crossprod(ww_PHu)
    G_6 <- -1*crossprod(w_PHu, ww_PHu)
    G_7 <- n
    G_8 <- trace_ww
    G_9 <- 0
    G <- (1/n)*matrix(c(G_1, G_2, G_3, G_4, G_5, G_6, G_7, G_8, G_9), nrow =3, ncol = 3)
    
    mm <- function(param) {
      par <- c(param[1], param[1]^2, sqrt(param[2]^2) )
      1*crossprod(g - G %*% par)
    }
    res <- optim(c(0.1,0.2), mm, method = "BFGS")
    
    rho[i] <- res$par[1]
    sigma[i] <- res$par[2]
    print(paste("rho is: ", rho[i]))
    print(paste("sigma is ", sigma)[i])
  }
  
  print("GMM estimation")
  
  # Spatial 2SLS
  
  
  # Spatial 3SLS 
}