ling.data = read.table('lingData.txt', head=TRUE)
# create a subset of the data in lingData.txt where all observations that
# omitted every question have been removed. Store the **number** of
# observations that you omitted as the variable <n.no.response>

sums = rowSums(ling.data[, 5:71])
no.responses = (sums == 0)
n.no.response <- sum(no.responses)
nonempty.responses = ling.data[!no.responses, ]

# plot a histogram of the number of omitted responses for each observation
# after removing observations that omitted all questions

omitted = (nonempty.responses[, 5:71] == 0)
num.omitted = rowSums(omitted)
hist(num.omitted)

# using your subset (with observations that responded to no questions
# removed) find the 99th percentile cutoff for number of questions
# omitted. Remove all observations with more than this number of omitted
# questions.

cutoff = quantile(num.omitted, .99)
remove.obs = (num.omitted > cutoff)
valid.responses = nonempty.responses[!remove.obs, ]

# save the subset of remaining observations in a file named
# "ling-data-clean.data" 

write.table(valid.responses, file="ling-data-clean.data")
