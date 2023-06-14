# With this script you can binarize continuous Nifti (.nii) files according to a (statistical) threshold such as
# Bayes Factors. This way you can visualize your statistical results in DSI Studio, which appears to have difficulties
# displaying continuous statistical maps.

library(RNifti)
library(BayesFactor)
library(tidyr)
library(dplyr)


folder <- "C:/Users/ssmaczny.NEUROLOGIE/Desktop/Chronisch/discmaps_results/Bayesian"  # Folder with your .nii files
filename <- "dif_add_mult.nii" # add the name of the file you want to binarize
file <- paste(folder, filename, sep = "/") # add the name of the file you want to binarize
threshold <- 1 # set the value of your threshold

results <- readNifti(file)
dimension_1st_lesion <- dim(results)
lesion_vect_length <- length(c(results))
v.results <- c(results)
v.results <- ifelse(v.results >= threshold,
                    v.results <- 1,
                    v.results <- 0)

bin.results <- array(v.results,dimension_1st_lesion)

bin.nifti  <- asNifti(bin.results, reference = results)

writeNifti(bin.nifti, paste(folder,'/binarized_',filename,sep=""))
