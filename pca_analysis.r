data <- read.table("binary-ling-data.data" , header = T)
clean.data <- data[complete.cases(data), 5:(length(data)-2)]
pca <- prcomp(clean.data, scale. = T)
head(sort(abs(pca$rotation[,1]), decreasing=T))


getPrincipalCompenents <- function(question){
	indx <- which(rownames(pca$rotation) == question)
	return(sort(abs(pca$rotation[,indx])))
}