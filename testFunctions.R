library(dplyr)

source("prepare4MFA.R")

# Toy dataset: 
# 3 groups:
# - Clin var; Categorical; 2 vars
# - Microbiome; frequencies; 3 vars
# - Metabolites; continuous; 3 vars

gender <- sample(c(1,2), rep=TRUE, size=10) 
atRisk <- sample(c(0,1), rep=TRUE, size=10, prob = c(0.8,0.2)) 
grup1<- round(t(rmultinom(10, size = 12, prob = c(0.1,0.2,0.8)))/10,3)
colnames(grup1) <- c("mic1", "mic2", "mic3")
grup2 <- round(matrix(rnorm(mean=0, sd=2, 30), nrow=10),2)
colnames(grup2) <- c("X1", "X2", "X3")
X <- data.frame(gender, atRisk, grup1, grup2)
rownames(X)<- paste("ind",1:nrow(X), sep="_")


# varsTable

varsTable(X)

# They are all numerical but, the groups are defined as follows

grups <- c(rep("clin",2), rep("microb",3), rep("metab", 3))

groupNames <- varsTable(X)
groupNames$Group <- grups
groupNames
