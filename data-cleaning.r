ling.data = read.table('lingData.txt', head=TRUE)
# create a subset of the data in lingData.txt where all observations that
# omitted every question have been removed. Store the **number** of
# observations that you omitted as the variable <n.no.response>

# n.no.response <- your code here

# plot a histogram of the number of omitted responses for each observation
# after removing observations that omitted all questions



# using your subset (with observations that responded to no questions
# removed) find the 99th percentile cutoff for number of questions
# omitted. Remove all observations with more than this number of omitted
# questions.

# save the subset of remaining observations in a file named
# "ling-data-clean.data" 

## My Notes:
## Get number of possible responses by calling max on the column
## Get number of non-responded by calling sum(apply(data, 1, '>', 0)
## If == 0, can omit
