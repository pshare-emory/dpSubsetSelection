####################################################
#  File name: dp_subset_selection.R
#  Author: Meng Wang
#  Contact: mengw1@stanford.edu
#  Date: May 2018
#  ----------------------------------------------------
#  Package required: "dummies", "MASS"
#  Source files: "dp_subset_selection_fn_1.R", "dp_subset_selection_fn_2.R"
#  ----------------------------------------------------
#  File description: to get optimal public subset under DP
#
#  Inputs:
#     dat.D: private dataset
#     dat.E: public dataset
#     num.ind: column index for numerical variables
#     cate.ind: column index for categorical varialbes
#     eps: privacy budget 
#     k.start: initial number of public points to release
#     K.n: a sequence of numbers of public data points to release
#
#  Outputs:
#     sel.pub.dat: the DP selected public sub dataset
#     logreg.coef: the DP logistic regression coefficient under the selected subset
#     pred.error.theo: prediction error learning from the weighted sequential public subsets without adding noise on the weights
#     pred.error.noise.hyb.w: prediction error learning from the weighted sequential public subsets under DP
#     pred.error.pub: prediction error naively learning from the whole public dataset
#
# Note for the input datasets:
#   dat.D and dat.E have the same prediction variables which are in the same column in each dataset
#   the first column is the response variable in each dataset
#  ---------------------------------------------------------


args = commandArgs(trailingOnly=TRUE)

source("./code/dp_subset_selection_fn_1.R")
source("./code/dp_subset_selection_fn_2.R")
library(dummies)
library(MASS)

dat.E.loc = args[1]
dat.D.loc = args[2]
num.ind.start = as.numeric(args[3])
num.ind.end = as.numeric(args[4])
cat.ind.start = as.numeric(args[5])
cat.ind.end = as.numeric(args[6])
eps = as.numeric(args[7])
k.start = as.numeric(args[8])
k.end = as.numeric(args[9])
k.by = as.numeric(args[10])

dat.E = read.csv(dat.E.loc)
dat.D = read.csv(dat.D.loc)
num.ind = num.ind.start:num.ind.end
cat.ind = cat.ind.start:cat.ind.end
K.n = c(0, seq(k.start, k.end, by=k.by))

result = dp.public.subset.selection.in.logreg.fn(dat.D, dat.E, num.ind, cat.ind, eps, k.start, K.n)
selected.pub.set = result[["selected.pub.set"]]
logreg.coef.dp = result[["logreg.coef.dp"]]
pred.error.theo = result[["pred.error.theo"]]
pred.error.noise.hyb.w = result[["pred.error.noise.hyb.w"]]
pred.error.pub = result[["pred.error.pub"]]

write.csv(selected.pub.set, file="./output/dp_selected_subset.csv")
write.csv(logreg.coef.dp, file="./output/dp_logreg_coef.csv")
save(K.n, pred.error.theo,pred.error.noise.hyb.w, pred.error.pub, file="./output/dp_prediction_error.RData")

png("./plot/plot_prediction_error_sim.png")
plot(K.n, pred.error.theo, type="b", pch=19, xlab="number of released points", ylab="predition error", main=substitute(paste(epsilon, " = ",a), list(a=eps)), cex.lab=1.5, cex.main=2)
points(K.n, pred.error.noise.hyb.w, type="b", col="red", pch=19)
abline(h = pred.error.pub, col="blue")
legend("topright", c("from hybrid datasets under DP", "from hybrid datasets w/o noise", "from public dataset"), col=c("red", "black", "blue"), lty=1, pch=c(19, 19, NA), cex=0.8)
dev.off()


