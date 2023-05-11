library(dplyr)

source("C:/Users/Usuario/Nuevo Equipo VHIR10 Dropbox/Alex al VHIR/SotaCV/MultiDataManager/prepare4MFA.R", echo=TRUE)

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


