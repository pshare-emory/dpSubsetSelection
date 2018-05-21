
data.norm.based.E = function (dat.D, dat.E, num.ind, cat.ind) {
	    # normalization function
        # Args:
        #     dat.D: private dataset
        #     dat.E: public dataset
        #     num.ind: column index for numerical variables
        #     cat.ind: column index for categorical variables
        # Outputs;
        #     D: normalized private dataset
        #     E: normalized public dataset
        
		dat.norm.E = dat.E
	    dat.norm.D = dat.D
		for (j in num.ind) {
			Z.E = dat.E[,j]
			Z.D = dat.D[,j]
			a = min(Z.E)
			b = max(Z.E)
			dat.norm.E[,j] = (Z.E - a)/(b - a)
			dat.norm.D[,j] = (Z.D - a)/(b-a)
			dat.norm.D[,j] = pmax(dat.norm.D[,j], 0)
			dat.norm.D[,j] = pmin(dat.norm.D[,j], 1)
		}
		for (j in cat.ind) {
     	    dat.norm.E[,j] = dummy(dat.E[,j])/sqrt(2)
     	    dat.norm.D[,j] = dummy(dat.D[,j])/sqrt(2)
        }
		return(list(D = dat.norm.D, E = dat.norm.E))
}



dist_DE_all = function(setD, setE){
	  # pointwise distance function between public and private datasets
	  # Args:
	  #     setD: private dataset
	  #     setE: public dataset
	  # Outputs:
	  #     distAll: distance matrix between each data point in private dataset and each in public dataset
	  n_len = ncol(setD)
	  n_D = nrow(setD)
	  n_E = nrow(setE)
	  dist1 = (matrix(1,n_E,1))%*%t((setD*setD)%*%(matrix(1,n_len,1)))
	  dist2 = ((setE*setE)%*%(matrix(1,n_len,1)))%*%(matrix(1,1,n_D))
	  dist3 = 2 * setE%*%t(setD)
	  distAll = dist1+dist2-dist3
	  return(distAll)
}


weight_represent = function(dist_DE){
	# representation weight function
	# Args:
	#     dist_DE: pairwise distance function between public and private data points
	# Outputs:
	#     weight: representation weight in each public data point
	
    dum = apply(dist_DE, 2, which.min)
    weight = tabulate(dum, dim(dist_DE)[1])
    return(weight)
}


pred.error.fn = function(pred.error.prv, X, beta.hat) {
	# prediction error difference function between learning the model from private dataset and that from public (sub) dataset
	# Args:
	#     pred.error.prv; prediction error learning from private dataset
	#     X: private covariate
	#     beta.hat: logistic coefficient learning from public (sub) dataset
	# Outputs:
	#     error: l_2 norm of prediction error difference
	
    dum = X %*% beta.hat
    prob.hat = 1/(1 + exp(-dum)) #P(Y=1)
    error = norm(prob.hat - pred.error.prv, "2")
    return(error)
}


dp.weighted.logreg = function (k.start, K.n, setE_0, setD_0, dat.E, truncateWeight=TRUE, p, eps.w, eps.k, Y.D, X.D, w_0, dist_DE0, B=10, pred.error.prv) {
		# prediction error difference function from learning the model from the weighted public dataset adding Laplace noise on the weights 
		# Args:
		#     k.start: number of data points in the initial public sub dataset
		#     K.n: a sequence of numbers of data points in the sequential public sub dataset
		#     setE_0: rescaled public dataset
		#     setD_0: rescaled private dataset
		#     dat.E: original public dataset
		#     truncateWeight: boolean variable indicating whether truncating negative weights to zero
		#     p: number of covariates
		#     eps.w: privacy budget on the weights
		#     eps.k: privacy budget on the M-estimates in each sequential model
		#     Y.D: original response variable in the private dataset
		#     X.D: original predic variable in the private dataset
		#     B: replication number to repeat the precedure
		#     w_0: representation weights of the public data points for the private data points
		#     dist_DE0: pairwise distance between each public data point and each private data point
		#     pred.error.prv: prediction error in the private dataset
		# Outputs:
		#     pred.eror.seq: a sequence of the prediction error sequentially learing from public weighted sub data points under DP
		
		pred.error.y.B = rep(0, length(K.n))
		beta.w1 = rep(0, p)
		for (b in 1:B) {
			#if (b %% 10 == 0) 
			#print(paste("b = ", b))
			w_0.nois = w_0 + rexp(length(w_0), eps.w/2) - rexp(length(w_0), eps.w/2)
			if (truncateWeight) {
				w_0.nois[w_0.nois<0] = 0
			}
			#round(w_0.nois)
			ind.o = order(w_0.nois, decreasing=TRUE)
			setE_1 = setE_0[ind.o,]
			dat.E1 = dat.E[ind.o,]
			dist_DE1 = dist_DE0[ind.o,]
	        for (i in 1:length(K.n)) {
				#print(i)
				k = K.n[i]
				wt.E = weight_represent(dist_DE1[1:(k.start+k),])
		        wt1.E = wt.E + rexp(length(wt.E), eps.k/ 2) -rexp(length(wt.E), eps.k/ 2)
		        if (truncateWeight) {
					wt1.E[wt1.E < 0] = 0  
				}
				pub.dat=as.matrix(dat.E1)[1:(k.start+k),]
				beta.w1 = LogisticFit(Y=pub.dat[,1], X=pub.dat[,-1], w = wt1.E) 
			    pred.error.y.B[i] = pred.error.y.B[i] + pred.error.fn(pred.error.prv, X.D, beta.w1)
           }
		}
        pred.error.seq = pred.error.y.B / B
        return(pred.error.seq)
}


weighted.logreg.nonoise = function (k.start, K.n, setE_0, setD_0, p, Y.D, X.D, w_0, dist_DE0, pred.error.prv) {
		# prediction error difference function from learning the model from the weighted public dataset without adding noise on the weights 
		# Args:
		#     k.start: number of data points in the initial public sub dataset
		#     K.n: a sequence of numbers of data points in the sequential public sub dataset
		#     setE_0: rescaled public dataset
		#     setD_0: rescaled private dataset
		#     p: number of covariates
		#     Y.D: original response variable in the private dataset
		#     X.D: original predic variable in the private dataset
		#     w_0: representation weights of the public data points for the private data points
		#     dist_DE0: pairwise distance between each public data point and each private data point
		#     pred.error.prv: prediction error in the private dataset
		# Outputs:
		#     pred.eror.seq: a sequence of the prediction error sequentially learing from public weighted sub data points
		
		ind.o = order(w_0, decreasing=TRUE)
		setE_1 = setE_0[ind.o,]
		dat.E1 = dat.E[ind.o,]
		dist_DE1 = dist_DE0[ind.o,]
		beta.w1 = rep(0, p)
		pred.error.seq = rep(0, length(K.n))
        for (i in 1:length(K.n)) {
        	#print(i)
			k = K.n[i]
			wt.E = weight_represent(dist_DE1[1:(k.start+k),])
			pub.dat = as.matrix(dat.E1[1:(k.start+k),]) 
			beta.w1 = LogisticFit(Y=pub.dat[,1], X=pub.dat[,-1], w = wt.E) 
		    pred.error.seq[i] = pred.error.fn(pred.error.prv, X.D, beta.w1)
        }
        return(pred.error.seq)
}


pred.error.dp = function (pred.error, eps.pred, B) {
	# adding Laplace noise to the prediction error
	# Args:
	#     pred.error: prediction error 
	#     pres.pred: privacy budget on the prediction error
	#     B: replication number to repeat the precedure
	# Output:
	#    pred.error.noise: prediction error added Laplace noise
	
	m = length(pred.error)
	pred.error.noise.B = matrix(0, ncol=B, nrow=m)
	for (b in 1:B) {
	    pred.error.noise.b = pred.error + rexp(m, eps.pred*1) - rexp(m, eps.pred*1)
	    pred.error.noise.b[pred.error.noise.b < 0] = 0
	    pred.error.noise.B[,b] = pred.error.noise.b
	}
	pred.error.noise = rowMeans(pred.error.noise.B)
	return(pred.error.noise)
}


dp.selected.public.subset = function (pred.error.noise.hyb.w, K.n, setE_0, dat.E, truncateWeight=TRUE, eps.w, eps.k, w_0, dist_DE0) {
	# releasing the selected public subset and estimated logistic regression coeffient under DP  under which subset the pred.error.noise.hyb.w is minimized 
	# Args:
	#     pred.error.noise.hyb.w: prediction error in the sequential subets under DP
	#     K.n: a sequence of numbers of data points in the sequential public sub dataset
	#     setE_0: rescaled public dataset
	#     dat.E: original public dataset
	#     truncateWeight: boolean variable indicating whether truncating negative weights to zero
	#     eps.w: privacy budget on the weights
	#     eps.k: privacy budget on the M-estimates in each sequential model
	#     w_0: representation weights of the public data points for the private data points
	# Outputs:
	#     sel.pub.dat: the DP selected public sub dataset
	#     logreg.coef: the DP logistic regression coefficient under the selected subset
	

	w_0.nois = w_0 + rexp(length(w_0), eps.w/2) - rexp(length(w_0), eps.w/2)
	if (truncateWeight) {
		w_0.nois[w_0.nois<0] = 0
	}
	#round(w_0.nois)
	ind.o = order(w_0.nois, decreasing=TRUE)
	setE_1 = setE_0[ind.o,]
	dat.E1 = dat.E[ind.o,]
	dist_DE1 = dist_DE0[ind.o,]
	i.sel = which.min(pred.error.noise.hyb.w)

	k = K.n[i.sel]
	wt.E = weight_represent(dist_DE1[1:(k.start+k),])
    wt1.E = wt.E + rexp(length(wt.E), eps.k/ 2) -rexp(length(wt.E), eps.k/ 2)
    if (truncateWeight) {
        wt1.E[wt1.E < 0] = 0  
	}
	pub.dat=as.matrix(dat.E1)[1:(k.start+k),]
	beta.w1 = LogisticFit(Y=pub.dat[,1], X=pub.dat[,-1], w = wt1.E) 
	return(list(sel.pub.dat=pub.dat, logreg.coef=beta.w1) )
} 




########### below functions are referred to Jing Lei's paper #############
WeightedCov <- function(X, w) {
  # weighted covariance matrix
  # Args:
  #   X: n by p matrix
  #   w: n by 1 vector
  # Output
  #   P: p by p matrix = t(X) %*% diag(w) %*% X
  tX = t(X)
  n = dim(X)[1]
  for (i in 1:n) {
  	tX[,i] = tX[,i] * w[i]
  }
  P <- tX %*% X
  return(P)
}


WeightedOLS <- function(Y, X, wt) {
  # weighted OLS with possibly negative weights
  beta <- solve(WeightedCov(X, wt)) %*% (t(X) %*% (Y * wt))
  return(beta)
}


LogisticLogLikelihood <- function(Y, X, w, beta) {
  # log likelihood for Logistic model
  # Args:
  #   Y: n by 1 vector of binary response variable
  #   X: n by p matrix of predictors
  #   w: n by 1 vector of weights
  #   beta: p by 1 coefficients
  # Output:
  #   log.likelihood: log likelihood of beta, given data (Y, X)
  Xbeta = X %*% beta
  log.likelihood <- t(Y * w) %*% Xbeta - sum(log(1 + exp(Xbeta)) * w)
  return(log.likelihood)
}



LogisticFit <-
  function(Y, X, w = NULL, start = NULL, maxit = 15, eps.log = 1e-8, tol = 1e-3) {
  # Newton-Raphson algorithm to fit Logistic model
  # Args:
  #   Y: n by 1 vector of binary response variable
  #   X: n by p matrix of predictors
  #   w: n by 1 vector of weights
  #   start: the initial value of parameters
  #   maxit: the max number if iterations
  #   eps: the threshhold of likelihood improvment
  # Output:
  #   beta: the coefficients
  #   beta.all: all the beta values in the iterations
  #   log.likelihood: the log-likelihood value for each beta
  
  if (is.null(w)) {
    w <- matrix(1, nrow = nrow(Y), ncol = 1)
  }
  if (is.null(start)) {
    start <- ginv(WeightedCov(X, w), tol = tol) %*%
              (t(X) %*% (w * Y))
  }
  beta <- start
  log.likelihood <- LogisticLogLikelihood(Y, X, w, beta)

  for (i in 1:maxit) {
    predicted.prob <- 1 / (1 + exp(-X %*% beta))
    residuals <- Y - predicted.prob
    first.deriv <- t(X) %*% (w * residuals)
    hessian <- WeightedCov(X, w * predicted.prob * 
      (1 - predicted.prob))
    beta.new <- beta + ginv(hessian, tol = tol) %*% first.deriv
    log.likelihood.new <- LogisticLogLikelihood(Y, X, w, beta.new)
    if (is.nan(log.likelihood.new)) {
      break;
    } else if (log.likelihood.new - log.likelihood < eps.log) {
      break;
    } else {
      beta <- beta.new
      log.likelihood <- log.likelihood.new
    }
  }
  return(beta)
}

