fgs3sls <- function(formula, data=list(), w, lags = NULL, errors= NULL){
  
  type<-'fgs3sls'
  cl <- match.call()
  
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
  print(allnames)
  sel <- vector("numeric",length=length(allnames))
  for (j in 1:length(allnames)) sel[j] <- unique(which(colnames(xall)==allnames[j]))[1]
  xall <- xall[,sel]
  cor(xall)
  wxall <- w %*% xall
  wwxall<- w %*% wxall
  
  # Create matrix of instruments
  h <- cbind(xall, wxall)
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
        K[i] <- K[i] + 1
      }
    }
    colnames(temp) <- temp_names
    colnames(temp_php) <- temp_names
    x_names[[i]] <- c(temp_names, x_names[[i]])
    Z[[i]] <- cbind(temp, xlist[[i]])
    PHZ[[i]] <- cbind(temp_php, xlist[[i]])
  }
  print("Initialisation: done")
  
  # First step 2sls estimation
  
  delta <- vector("list",length=eq)
  PHu   <- vector("list",length=eq)
  
  for (i in 1:eq) {
    delta[[i]] <- (solve(t(PHZ[[i]]) %*% Z[[i]] ) %*% t(PHZ[[i]]) ) %*% ylist[[i]]
    PHu[[i]] <- ylist[[i]] - Z[[i]]%*%delta[[i]]
  }
  
  print("First step 2SLS estimation: done")
  
  # GM estimator
  
  # trace_ww <- sum(diag(crossprod(w)))   # the slow way
  trace_ww <- sum(w*w)  # this is the fast way (checked that they are similar)
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
  
  print("GMM estimation: done")
  
  # Spatial 2SLS
  Z_s      <- vector("list",length=eq)
  p_HZ_s    <- vector("list",length=eq)
  y_s      <- vector("list",length=eq)
  delta_s  <- vector("list",length=eq)  
  for (i in 1:eq) {
    Z_s[[i]]    <- Z[[i]] - rho[[i]] * (w %*% Z[[i]])
    p_HZ_s[[i]] <- p_h %*% Z_s[[i]]
    y_s[[i]]    <- ylist[[i]] -rho[[i]] * ( w %*% ylist[[i]])
    delta_s[[i]] <- (solve(t(p_HZ_s[[i]]) %*% Z_s[[i]]) %*% t(p_HZ_s[[i]]) ) %*% y_s[[i]]
  }
  
  print("Spatial 2SLS estimation: done")
  
  # Spatial 3SLS 
  
  begin <-vector("numeric",length=eq) 
  end   <-vector("numeric",length=eq) 
  
  total_var <- sum(K)
  res <-matrix(0, n, eq)
  Z_s_tot <- matrix(0, n*eq, total_var) 
  p_HZ_s_tot <- matrix(0, n*eq, total_var) 
  Z_tot <- matrix(0, n*eq, total_var) 
  y_s_tot <- NULL
  y_tot <- NULL
  Sigma <- matrix(0, eq, eq)
  p_HZ_cov <- matrix(0, total_var, eq*n)
  
  begin_temp  = 1
  end_temp = 0
  for (i in 1:eq) {
    begin[i] <- begin_temp
    end[i]   <- end_temp + K[i]
    begin_temp <- begin_temp + K[i]
    end_temp <- end[i]
  }
  
  for (i in 1:eq) {
    res[,i] <- y_s[[i]] - Z_s[[i]] %*% delta_s[[i]]
    y_s_tot <- rbind(y_s_tot, y_s[[i]])
    y_tot <- rbind(y_tot, ylist[[i]])
    Z_s_tot[ ((i-1)*n+1):(i*n) , begin[i]:end[i] ] <- Z_s[[i]]
    Z_tot[ ((i-1)*n+1):(i*n) , begin[i]:end[i] ] <- Z[[i]]
    p_HZ_s_tot[ ((i-1)*n+1):(i*n) , begin[i]:end[i] ] <- p_HZ_s[[i]]
  }
  
  for (i in 1:eq) {
    for (j in 1:eq) {
      Sigma[i,j] <- (1/n)*(crossprod(res[,j],res[,i]))
    }
  }
  
  inv_Sigma <- solve(Sigma)
  
  for (i in 1:eq) {
    for (j in 1:eq) {
      p_HZ_cov[ , ((i-1)*n+1):(i*n)] = 
        p_HZ_cov[ , ((i-1)*n+1):(i*n)] +
        t(p_HZ_s_tot[ ((i-1)*n+1):(i*n) , ]) * inv_Sigma[j,i]
    }
  }
  
  co <- solve(p_HZ_cov %*% Z_s_tot) %*% p_HZ_cov %*% y_s_tot
  res <- y_s_tot - Z_s_tot %*% co # Please check this one again!
  cov <- solve(p_HZ_cov%*%p_HZ_s_tot)
  
  coeff <- vector("list",length=eq)
  residuals <- vector("list",length=eq)
  cov_matrix <- vector("list",length=eq)
  
  for (i in 1:eq) {
    coeff[[i]] <- co[begin[i]:end[i]]  
    cov_matrix[[i]] <- cov[begin[i]:end[i], begin[i]:end[i]]
    residuals[[i]] <- res[((i-1)*n+1):(i*n)]
  }
  
  print("Spatial 3SLS estimation: done")
  
  fgs3sls <- return(list(coeff = coeff, 
                         cov_matrix = cov_matrix, 
                         residuals = residuals,
                         x_names = x_names,
                         y_names = y_names,
                         call = cl,
                         eq = eq,
                         rho = rho, 
                         sigma = sigma))
  class(fgs3sls) <- "fgs3sls"
  return(fgs3sls)
}