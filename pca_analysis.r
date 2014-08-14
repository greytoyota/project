require(maps)
# load data
data <- read.table("binary-ling-data.data" , header = T)
clean.data <- data[complete.cases(data), 5:(length(data)-2)]
location.data <- data[, seq(length(data) - 1, length(data))]

# run principal component analysis
pca <- prcomp(clean.data, scale. = T)

# return list of questions and their effect on respective principal
# component
getPrincipalComponents <- function(pc){
	return(sort(abs(pca$rotation[,pc]), decreasing = T))
}

plotMap <- function(q.name) {
    q.index = which(colnames(clean.data) == q.name)
    q.responses = which(clean.data[, q.index] == 1)
    map('state')
    points(location.data[q.responses, 2:1], pch= ".", col="blue", main=q.name)
}

# get ten variables that cause high variation in first PC
PC1.high.variation <- getPrincipalComponents(1)[1:10]