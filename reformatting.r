library(RUnit)
errMsg <- function(err) print(err)
load('reformatting-tests.rda')

# Implement the makeBinary function.
# args:
# <response.row>: a vector of integers giving the response values for each
#   question 
# <n.responses>: a vector of integers (same length as <response.row>)
#   indicating the number of possible responses for each question
#
# returns:
# a binary vector that reformats the responses of <response.row> as
# described in project1.pdf

makeBinary <- function(response.row, n.responses) {
    main.vec <- numeric(length = 0)
    for (i in 1:length(response.row)) {
        vec <- numeric(length = n.responses[i])
        vec[response.row[i]] <- 1
        main.vec <- c(main.vec, vec)
    }
    return(main.vec)
}


tryCatch(checkEquals(make.binary.test1, makeBinary(make.binary.rr1,
                                                   make.binary.nr)),
         error=function(err) errMsg(err))

tryCatch(checkEquals(make.binary.test2, makeBinary(make.binary.rr2,
                                                   make.binary.nr)),
         error=function(err) errMsg(err))

# use your "makeBinary" function to reformat your "ling-data-clean.data"
# dataset. Store this as a dataframe with variable names and order **as
# indicated in project1.pdf**. Save this dataframe as the file
# "binary-ling-data.data".

# making rows binary
data = read.table("lingData.txt", header=TRUE)
cut.data = data[, seq(5, length(data) - 2)]
n.responses = apply(cut.data, 2, max)
reformatted.data = as.data.frame(t(apply(cut.data, 1, makeBinary, n.responses)))

# setting col names
q.names = names(cut.data)
q.num = 1
main.name.vec = numeric(length = 0)
while (q.num <= length(q.names)) {
    q.name = q.names[q.num]
    response.names = character(n.responses[q.num])
    
    for (i in 1:length(response.names)) {
        response.name = paste(q.name, ".", toString(i), sep="")
        response.names[i] = response.name
    }
    main.name.vec = c(main.name.vec, response.names)
    q.num = q.num + 1
}

colnames(reformatted.data) = main.name.vec
full.data.frame = data.frame(data[, 1:4], reformatted.data,
    data[, (length(data) - 1):length(data)])

write.table(full.data.frame, file="binary-ling-data.data", row.names=FALSE)
