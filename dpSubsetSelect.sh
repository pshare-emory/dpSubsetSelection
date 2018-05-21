#!/bin/bash
#---------#---------#---------#---------#---------#---------#---------#---------
usage="
#---------#---------#---------#---------#---------#---------#---------#---------
# Introduction: 
  dpSubsetSelect is the software to implement the algorithm in the paper--
  Selecting Optimal Subset to Release Under Differentially Private M-Estimators from Hybrid Datasets. 
  Please contact <mengw1@stanford.edu> for bugs.
#---------#---------#---------#---------#---------#---------#---------#---------
Usage: 
./dpSubsetSelect.sh --prefixPrvData --prefixPubData --numIndStart --numIndEnd --catIndStart --catIndEnd --eps --kStart --kEnd --kBy
#---------#---------#---------#---------#---------#---------#---------#---------
Options:
  --prefixPrvData The file name and location for the private dataset.
  --prefixPubData The file name and location for the public dataset.
  --numIndStart	The starting column index for the numerical variables.
  --numIndEnd	The end column index for the numerical variables.
  --catIndStart	The starting column index for the categorical variables.
  --catIndEnd	The end column index for the categorical variables.
  --eps=privacy\_budget 	The privacy budget (recommend eps <=3).
  --kStart The initial number of public points to release.
  --kEnd The last number of public points to release.
  --kBy The segment of numbers of public data points to release.
For the example data: 
  ./dpSubsetSelect.sh --prefixPrvData=./raw_data/raw_private_dataset.csv --prefixPubData=./raw_data/raw_public_dataset.csv --numIndStart=2 --numIndEnd=3 --catIndStart=1 --catIndEnd=1 --eps=0.5 --kStart=10 --kEnd=1000 --kBy=100
#---------#---------#---------#---------#---------#---------#---------#---------
Output:
  The outputs are (1) the selected optimal public subset under DP, 
  (2) the estimated parameters in the logistic regression from the selected subset, (3) prediction errors.
#---------#---------#---------#---------#---------#---------#---------#---------   	 
"

# For R script
prefixPrvData=;
prefixPubData=;
numIndStart=;
numIndEnd=;
catIndStart=;
catIndEnd=;
eps=;
kStart=;
kEnd=;
kBy=;
#---------#---------#---------#---------#---------#---------#---------#---------
# Arguments loop
SED_=sed;
[ -x "$(command -v $SED_)" ] || { echo "SED is not working ($SED_)" && exit 1; }
while test -n "${1}"; do
	case ${1} in
		--prefixPrvData=*)
			prefixPrvData=`echo "${1}" | ${SED_} -e 's/[^=]*=//'`;;
		--prefixPubData=*)
			prefixPubData=`echo "${1}" | ${SED_} -e 's/[^=]*=//'`;;
		--numIndStart=*)
			numIndStart=`echo "${1}" | ${SED_} -e 's/[^=]*=//'`;;
		--numIndEnd=*)
			numIndEnd=`echo "${1}" | ${SED_} -e 's/[^=]*=//'`;;
		--catIndStart=*)
			catIndStart=`echo "${1}" | ${SED_} -e 's/[^=]*=//'`;;
		--catIndEnd=*)
			catIndEnd=`echo "${1}" | ${SED_} -e 's/[^=]*=//'`;;
		--eps=*)
			eps=`echo "${1}" | ${SED_} -e 's/[^=]*=//'`;;
		--kStart=*)
			kStart=`echo "${1}" | ${SED_} -e 's/[^=]*=//'`;;
		--kEnd=*)
			kEnd=`echo "${1}" | ${SED_} -e 's/[^=]*=//'`;;
		--kBy=*)
			kBy=`echo "${1}" | ${SED_} -e 's/[^=]*=//'`;;
		*)
			echo $"${1} is not available for $0!";
			exit 1;;	
	esac
	shift;
done
#---------#---------#---------#---------#---------#---------#---------#---------
echo "Start getting the output "
Rscript ./code/dp_subset_selection.R $prefixPrvData $prefixPubData $numIndStart $numIndEnd $catIndStart $catIndEnd $eps $kStart $kEnd $kBy
echo "Done and the result is in the output folder"


 