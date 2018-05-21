# This is data science class example
# 
# Set variables with parameter values
file <- "basal_lumA.txt"   # this is the file we will be reading from

# these are first 25 genes we'll be using to filter (as an example -- no significance in the selection)
first25 <- c('ARHGEF10L','HIF3A','RNF17','RNF10','RNF11','RNF13','GTF2IP1','REM1','MTVR2','RTN4RL2','C16orf13','C16orf11','FGFR1OP2','TSKS','ATRX','PMM2','ASS1','NCBP1','ZNF709','ZNF708','RBM14','NCBP2','DISC1','CAMK1','RPL37')

# the number of group we are partitioning the dataset to for cross-validation
nfold <- 3

# read the first line of the file -- i.e. header
header <- scan(file, nlines = 1, what = character())

# read the entire file skipping first two lines (headers)
data <- read.table(file, skip = 2, header = FALSE, sep = "\t", quote = "", check.names=FALSE)

# set the column names to the header
names(data) <- header

# read the second header line -- it contains the type of cancer
header2 <- scan(file, skip = 1, nlines = 1, what = character())

# create two datasets -- containing only the first 25 genes and LumA type cancers  
LumA <- data[data$id %in% first25,header2=='LumA']
# second dataset will also have only the first 25 genes but Basal type cancer
Basal <- data[data$id %in% first25,header2=='Basal']

# partition all cancer samples (there is one column per sample into nfold groups)
# sample() function randomly assigns numbers from 1..ncol(LumA) into 1..nfold groups
#
# split() function takes column names (i.e. cancer sample names) and assigns
#         each into group based on the array returned from sample() function
LumA_groups <- split(colnames(LumA), sample(1:nfold, ncol(LumA), replace=T))
Basal_groups <- split(colnames(Basal), sample(1:nfold, ncol(Basal), replace=T))
  
# declare result as an array -- we'll save the misclassified rate per iteration
result <- array()
  
for (test_group in 1:nfold) {
    
    # create test set for each cancer type
    # -- choose only columns (cancer samples) assigned to a selected group (test_group) 
    testA <- LumA[,colnames(LumA) %in% unlist(LumA_groups[test_group])]
    testB <- Basal[,colnames(Basal) %in% unlist(Basal_groups[test_group])]
    
    # create training set for each cancer type
    # -- choose only columns (cancer samples) NOT assigned to a selected group (test_group) 
    trainingA <- LumA[,!(colnames(LumA) %in% unlist(LumA_groups[test_group]))]
    trainingB <- Basal[,!(colnames(Basal) %in% unlist(Basal_groups[test_group]))]
    
    
    # THIS IS THE CLASSIFIER
    
    # create a centroid for training set A and B
    # --- rowMeans computes average per row and returns a vector of averages
    centroidA <- rowMeans(trainingA)
    centroidB <- rowMeans(trainingB)
    
    # count the number of misclassified cancer samples
    # --- sapply() applies given function to each column (each test cancer sample)
    # --- sqrt(sum((x-centroidA)^2)) -- computes a distance from the centroidA i.e.
    # -----        (x-centroidA)^2 -- computes a square of distance for each gene
    # -----    sum(...) -- sums up all distances
    # ----- sqrt( ... ) -- takes a square root of distance
    misclassifiedA <- sum(sapply(testA, function(x) { sqrt(sum((x-centroidA)^2))-sqrt(sum((x-centroidB)^2))>0 }))
    misclassifiedB <- sum(sapply(testB, function(x) { sqrt(sum((x-centroidA)^2))-sqrt(sum((x-centroidB)^2))<0 }))
    
    # END OF THE CLASSIFIER
    
    # save to the result array so we can average it across all test groups
    result[test_group] <- (misclassifiedA+misclassifiedB)/(ncol(testA)+ncol(testB))
}
  
# calculate a mean and standard deviation
mean(result)
sd(result)

