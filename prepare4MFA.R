
#' Build a table with information about variables in a data frame
#' May be useful when, given a dataset, one wishes to create a table
#' that will be later used to define subroups
#'
#' The function returns a data frame with 
#' - the index, 
#' - name, 
#' - class and
#' -  type of each variable in a data frame.
#' The type of a variable is determined based on whether it is a factor/character or not.
#' 
#' @param x A data frame containing the variables to be analyzed.
#'
#' @return A data frame with the index, name, class and type of each variable in the data frame.
#' @export
#' 
#' @examples
#' varsTable(iris)
#' 
varsTable <- function(x){
  index <- 1:ncol(x)
  vars <- colnames(x)
  clase <- sapply(x, class)
  tipo <- sapply(x, function (column) 
    ifelse (is.factor(column) | is.character(column), "n", "c"))
  df <- data.frame(index=index, vars=vars, 
                   clase=clase, tipo=tipo, 
                   Group="pendingDefinition")
  df
}

#' Extract the subset of variables associated with a given label.
#'
#' This function returns the variable names associated with a given label in a data frame.
#' 
#' @param varsAndGroupsDF A data frame with the variables and their associated labels.
#' @param label The label for which we want to extract the variables.
#'
#' @return A character vector with the names of the variables associated with the given label.
#' @export
#' 
#' @examples
#' varsAndGroupsDF <- varsTable(iris) 
#' varsAndGroupsDF$Group <- c(rep("Sepal",2),rep("Petal",2), "Species")
#' extractVarNames(varsAndGroupsDF, "Sepal")
#' 
extractVarNames <- function(varsAndGroupsDF, label){
  return(with(varsAndGroupsDF, vars[Group==label]))
}


#' Create the groups of variables that may be used 
#' to run a multiple Factor Analysis on a dataset.
#'
#' This function creates groups of variables based on 
#' - their type (numeric or categorical) and 
#' - their associated labels.
#' The resulting groups are suitable for running a multiple Factor Analysis on the data.
#' 
#' @param myDf A data frame with the variables to be analyzed.
#' @param GroupsNames A data frame with the variables and their associated labels.
#' 
#' @return A list containing the data frames of each group of variables, the names and sizes of the groups, and the types of the groups.
#' @export
#' 
#' @examples
#' data(iris)
#' varsAndGroupsDF <- varsTable(iris) 
#' varsAndGroupsDF$Group <- c(rep("Sepal",2),rep("Petal",2), "Species")
#' GroupsNames <-varsAndGroupsDF[,c("vars", "Group")] 
#' groups <- creaGrups(iris, GroupsNames)
#'

creaGrups <- function(myDf, GroupsNames) {
  tipoDatos <- sapply(myDf, class)
  myVarsNames <- data.frame(vars = colnames(myDf), clase = tipoDatos)
  varsAndGroups <- left_join(myVarsNames, GroupsNames, by = "vars") %>%
    select(vars, clase, Group)
  groupsOfVars <- as.data.frame.matrix(table(varsAndGroups$Group, varsAndGroups$clase))
  groupTypes <- colnames(groupsOfVars)[apply(groupsOfVars, 1, function(r) which(r > 0))]
  numericCols <- which(colSums(groupsOfVars[, groupTypes, drop = FALSE]) > 0)
  numGroups <- rowSums(groupsOfVars[, numericCols, drop = FALSE])
  groupNames <- rownames(groupsOfVars)
  groupsOfVars <- groupsOfVars[, groupTypes, drop = FALSE]
  listOfVars <- list()
  for (i in 1:length(groupNames)) {
    listOfVars[[i]] <- list(vars = extractVarNames(varsAndGroups, groupNames[i]),
                            num = numGroups[i],
                            name = groupNames[i],
                            type = groupTypes[i])
  }
  names(listOfVars) <- groupNames
  listOfDatasets <- lapply(listOfVars, function(varsList) myDf[, varsList$vars, drop = FALSE])
  names(listOfDatasets) <- names(listOfVars)
  return(list(groupsData = listOfDatasets, groupsVars = listOfVars,
              groupsNames = groupNames, groupsSizes = numGroups, groupTypes = groupTypes))
}


### 
### checkFactorialStructure of the created groups object
###

showGroupsinList <- function(alistOfVars){
  for (i in 1:length(alistOfVars)){
    cat(names(alistOfVars[i]), "\n")
    cat("\t", "numVars = ", alistOfVars[[i]][[2]], "\n")
    cat("\t", "groupName = ", alistOfVars[[i]][[3]], "\n")
    cat("\t", "groupType = ", alistOfVars[[i]][[4]], "\n")
  }
}

###
### Create a unique data frame from all or some of the datasets on which the original data has been broken.
###

MultMerge2 <- function (lst, all.x = TRUE, all.y = TRUE, by = NULL) 
{
  # lst <- list(...) # The original version had "..." instead of "lñst" as argument
  if (length(lst) == 1) 
    return(lst[[1]])
  if (!is.null(by)) {
    for (i in seq_along(lst)) {
      rownames(lst[[i]]) <- lst[[i]][[by]]
      lst[[i]][by] <- NULL
    }
  }
  unames <- DescTools::SplitAt(make.unique(unlist(lapply(lst, colnames)), 
                                           sep = "."), cumsum(sapply(head(lst, -1), ncol)) + 1)
  for (i in seq_along(unames)) colnames(lst[[i]]) <- unames[[i]]
  res <- Reduce(function(y, z) merge(y, z, all.x = all.x, all.y = all.x), 
                lapply(lst, function(x) data.frame(x, rn = row.names(x))))
  rownames(res) <- res$rn
  res$rn <- NULL
  seq_ord <- function(xlst) {
    jj <- character(0)
    for (i in seq_along(xlst)) {
      jj <- c(jj, setdiff(xlst[[i]], jj))
    }
    return(jj)
  }
  ord <- seq_ord(lapply(lst, rownames))
  res[ord, ]
  if (!is.null(by)) {
    res <- data.frame(row.names(res), res)
    colnames(res)[1] <- by
    rownames(res) <- c()
  }
  return(res)
}

### test

# df1 <- data.frame(matrix(rnorm(20), nrow=10)); rownames(df1) <- paste("row",0:9, sep="")
# df2 <- data.frame(min=letters[1:10], may=LETTERS[1:10]); rownames(df2) <- paste("row",0:9, sep="")
# df3 <- data.frame(df1 < 0); rownames(df3) <- paste("row",0:9, sep="")
# dfList <- list(df1, df2, df3)
# library(DescTools)
# MultMerge2(list(df1, df2, df3))
# DescTools::MultMerge(df1, df2, df3)


showText <- function (aText){
  cat("\n",aText,"\n")
  cat(paste(rep("=", nchar(aText)), collapse=""),"\n")
}

checkFactorialStructure <- function (mylistOfGroups)
  #                          varsList, groupSizes, groupNames, groupTypes)
{
  mylistOfDataSets <- mylistOfGroups[1][[1]]
  uniqueDataSet <- MultMerge2 (mylistOfDataSets)
  varsList <- colnames(uniqueDataSet)
  actualTypes <- sapply(uniqueDataSet[,varsList], class)
  grupos <- character()
  tipos <- character()
  for (i in 1:length(mylistOfDataSets)){
    groupNames <-mylistOfGroups$groupsNames[i]
    groupSizes <- mylistOfGroups$groupsSizes[i]
    groupTypes <- mylistOfGroups$groupTypes[i]
    grupos <- c(grupos, rep(groupNames, groupSizes))
    assignedTypes<- tipos <- c(tipos, rep(groupTypes, groupSizes))
  }
  showText("Global dimensions")
  show(dim(uniqueDataSet))
  showText("Structure of each group")
  showGroupsinList(mylistOfGroups$groupsVars) 
  showText("Data type vs group label")
  show(table(actualTypes, assignedTypes))
  return(data.frame(Variable= varsList, TipoActual=actualTypes,
                    Grupo=grupos, TipoAsignado= tipos ))
}


#' Extracts a subgroup of columns from a data matrix
#'
#' This function extracts a subgroup of columns from a data matrix given the position of the subgroup to be extracted and a vector of sizes that defines the number of columns in each continuous subgroup in the data.
#'
#' @param x a data matrix
#' @param pos the position of the subgroup to be extracted
#' @param vecOfSizes a vector of sizes that defines the number of columns in each continuous subgroup in the data.
#'
#' @return a data frame containing the subgroup of columns
#'
#' @examples
#' X <- t(matrix(rep(1:10, 5), byrow = FALSE, ncol = 5))
#' sizes <- c(3, 5, 2)
#' extractGroup(X, 1, sizes)
#' extractGroup(X, 3, sizes)
#' extractGroup(X, 2, sizes)
extractGroup <- function (x, pos, vecOfSizes){
  if (pos==1){
    first<- 1
    last <-vecOfSizes[pos]
  }else{
    if (pos==length(vecOfSizes)){
      first<- sum(vecOfSizes[-pos])+1
      last <- sum(vecOfSizes)
    }else{
      first<- sum(vecOfSizes[1:(pos-1)])+1
      last <- sum(vecOfSizes[1:pos])
    }
  }
  groupData <- x |>
    as.data.frame.array() |>
    dplyr::select (first:last)
  return(groupData)
}
