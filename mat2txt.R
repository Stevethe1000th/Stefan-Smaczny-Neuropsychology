# LQT-mat to txt

# a tool to rewrite .mat files (as derived from Lesion Quantification Toolkit output) to .txt files. 

# Clear environment
rm(list=ls())

library(R.matlab)
library(MASS)

# INPUT: The path in which your .mat files are in
path <- ... # Enter your path here

# -----------------

# grab all matfiles
matfiles <- list.files(path,full.names = TRUE, pattern='.mat')

# read in the matrices
lstmtrcs <- lapply(matfiles, function (x) {
  readMat(x)$pct.sdc.matrix
})

# rewrite them to .csv
rewrite <- function(matrx,name) {
  matrx <- as.data.frame(matrx)
  colnames(matrx) <- NULL
  write.csv(matrx,
               paste(
                 substr(name,1,nchar(name)-4),
                 ".csv",
                 sep=""),
            row.names = FALSE)
}

for(i in 1:length(matfiles)) {
  rewrite(lstmtrcs[i],matfiles[i])
}