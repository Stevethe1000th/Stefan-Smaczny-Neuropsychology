# Translation of Chris Sperber's code (see DOI: 10.17632/yjkr647mzb.2) + 
# Bayesian analysis instead of frequentist permutation approach

# This code calculates correlations of parcel-to-parcel disconnection values 
# as provided by the Lesion Quantification Toolkit (Griffis et al., 2021) (_SDC.mat)
# with a continuous behavioural variable.
# Then it provides Bayes Factors as per Bayesian Hypothesis Testing using a
# Jeffreys-Zellner-Siow (JZS) prior (see Wetzels & Wagenmakers, 2012)

# Input:
# A .csv file with a Participant number column and at least one column of continuous data
# A folder with corresponding files ending with _SDC.mat as given out by the LQT.

# Output: a .csv file with a Matrix of parcel-to-parcel Bayes factors.

# Clear environment
rm(list=ls())

library('R.matlab')
library('tidyr')
library('dplyr')
library('rstatix')
library('ggplot2')

# set all directories that you need as input and output directories! They are marked with .../

# set random seed for reproducibility
set.seed(1)

# jzs function
jzs_corbf <- function(r,n){
  int <- function(r,n,g){
    (1+g)^((n-2)/2)*(1+(1-r^2)*g)^(-(n-1)/2)*g^(-3/2)*exp(-n/(2*g))
    }
  bf10 <- sqrt((n/2))/gamma(1/2)*integrate(int,lower=0,upper=Inf,r=r,n=n)$value
  return(bf10)	
}


# How many patients must have a disconnection for it to be included?
N <- 15

# read behav. data
behaviour <- read.csv('.../Behavioural.csv') # provide your behavioural data
dep.var <- behaviour$Multipl # provide the name of your relevant behavioural variable

# read imaging data
folder <- '.../Parcel Disconnection'
fileList <- list.files(folder)

temp <- readMat(paste(folder,'/', fileList[1],sep=""))$pct.sdc.matrix # read in the ...percent_parcel_SDC.mat files from LQT

# save everything in one large array
images_2d <- array(NA,c(nrow(temp),nrow(temp),length(fileList)))
for (i in 1:length(fileList)) {
  print(i)
  print(paste(folder,'/',fileList[i],sep=""))
  
    images_2d[,,i] <- readMat(paste(folder,'/',fileList[i],sep=""))$pct.sdc.matrix
}

# create 2d mask for imaging data
mask <- array(0,c(nrow(temp),nrow(temp)))

# remove all elements in diagonal and blow
for (i in 1:nrow(temp)) {
  for (j in 1:nrow(temp)) {
    if (i<j) {
      mask[i,j] <- 1
    }
  }
}

# then we need to remove rarely/never affected connections

#  binarise the image, every disconnection >0 is set to 1
images_2d_binary <- replace(images_2d,images_2d>0,1)

image_2d_sum <- array(0,c(nrow(temp),nrow(temp)))
for (i in 1:nrow(temp)) {
  for (j in 1:nrow(temp)) {
    image_2d_sum[i,j] <- sum(images_2d_binary[i,j,])
  }
}
image_2d_sum <- image_2d_sum*mask

# set connection N threshold (e.g., minimum 15 patients) 

mask <- replace(mask,image_2d_sum<N,0)
mask_vect <- as.vector(mask)


# vectorise the 2d images
images_vect <- array(0,c(length(fileList),sum(mask_vect)))
for (i in 1:length(fileList)) {
  temp <- images_2d[,,i]
  temp <- as.vector(temp)
  images_vect[i,] <- temp[mask_vect==1];
}

stat_vect_orig <- array(0,sum(mask_vect))



# do bayesian analysis
stat_vect_bf <- stat_vect_orig
cors <- c(NA)

for (i in 1:length(stat_vect_orig)) { 
  cors[i] <- cor(dep.var,images_vect[,i]) 
  images_vect[,i]
  my.bayescor <- jzs_corbf(
    cor(dep.var,
        images_vect[,i],
  ),
    length(dep.var)) #inputs are sample correlation r and number of observations n 
  cors[i] <- cor(dep.var, images_vect[,i]) 
  stat_vect_bf[i] <- my.bayescor
}

stat_vect_bf <- unlist(stat_vect_bf)

temp <- mask_vect
temp <- replace(temp,temp==1,stat_vect_bf)
results_2d <- matrix(temp,nrow(mask),nrow(mask))

write.csv(results_2d,paste('.../results_N',N,'.csv',sep=''))
