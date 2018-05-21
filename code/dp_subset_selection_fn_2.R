
dp.public.subset.selection.in.logreg.fn = function (dat.D, dat.E, num.ind, cat.ind, eps, k.start, K.n) {
	# optimal public subset selection under DP
	# Args:
	#     dat.D: private dataset
	#     dat.E: public dataset
	#     num.ind: column index for numerical variables
	#     cate.ind: column index for categorical varialbes
	#     eps: privacy budget 
	#     k.start: initial number of public points to releast
    #     K.n: a sequence of numbers of public data points to release
	# Outputs:
	#     sel.pub.dat: the DP selected public sub dataset
	#     logreg.coef: the DP logistic regression coefficient under the selected subset
    #     pred.error.theo: prediction error learning from the weighted sequential public subsets without adding noise on the weights
    #     pred.error.noise.hyb.w: prediction error learning from the weighted sequential public subsets under DP
    #     pred.error.pub: prediction error naively learning from the whole public dataset
	
	
	eps.est = eps # privacy budget in estimation
    eps.pred = eps # privacy budget in prediction

	dat.D = data.frame(dat.D)
	dat.E = data.frame(dat.E)
	Y.D = dat.D[, 1]
	X.D = as.matrix(dat.D[, -1])
	Y.E = dat.E[, 1]
	X.E = as.matrix(dat.E[, -1])
	p = ncol(X.D)
	
	#### to get prediction error in the private dataset
	fit.D = glm(Y ~ . - 1, family = binomial, data = dat.D)
	beta.prv = matrix(fit.D$coefficients, ncol = 1, nrow = p)
	pred.error.prv = 1/(1 + exp(-X.D %*% beta.prv)) 
	
	#### to train the model in public dataset and get prediction error in private dataset
	fit.E = glm(Y ~ . - 1, family = binomial, data = dat.E)
	beta.pub = matrix(fit.E$coefficients, ncol = 1, nrow = p)
	pred.error.pub = norm(1/(1 + exp(-X.D %*% beta.pub)) - pred.error.prv, "2")
	
	
	#### to rescale the public and private datasets into the hypercube (Algorithm 1 step 1)
	dat.norm = data.norm.based.E(dat.D, dat.E, num.ind, cat.ind) 
	
	#### to get the representation weights (Algorithm 1 step 2)
	setD_0 = as.matrix(dat.norm[[1]])
	setE_0 = as.matrix(dat.norm[[2]])
	dist_DE0 = dist_DE_all(setD_0, setE_0)
	w_0 = weight_represent(dist_DE0)
	
	#### to get prediction error difference from learning the model from the weighted public dataset without adding noise on the weights compared to learning the model from the private dataset
	pred.error.theo = weighted.logreg.nonoise(k.start, K.n, setE_0, setD_0, p, Y.D, X.D, w_0, dist_DE0, pred.error.prv)
	
	#### to get prediction error difference from learning the model from the weighted public dataset adding Laplace noise on the weights compared to learning the model from the private dataset
	pred.error.hyb.w = dp.weighted.logreg(k.start, K.n, setE_0, setD_0, dat.E, truncateWeight=TRUE,  p, eps.w=eps.est*0.8, eps.k=eps.est*0.2/length(K.n), Y.D, X.D, w_0, dist_DE0, B=10, pred.error.prv) # truncated the weigths by zero
	
	#### further adding Laplace noise to pred.error.hyb.w to release the results
	pred.error.noise.hyb.w = pred.error.dp(pred.error.hyb.w, eps.pred, B=10)
	
	#### to release the selected public subset and estimated logistic regression coeffient under DP  under which subset the pred.error.noise.hyb.w is minimized
	sel.result = dp.selected.public.subset(pred.error.noise.hyb.w, K.n, setE_0, dat.E, truncateWeight=TRUE, eps.w=eps.est*0.8, eps.k=eps.est*0.2/length(K.n), w_0, dist_DE0)
	
	selected.pub.set = sel.result[[1]]
	logreg.coef.dp = sel.result[[2]]

   return(list(selected.pub.set=selected.pub.set, logreg.coef.dp=logreg.coef.dp, pred.error.theo=pred.error.theo, pred.error.noise.hyb.w=pred.error.noise.hyb.w, pred.error.pub=pred.error.pub ))
}


