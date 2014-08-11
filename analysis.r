data = read.table("binary-ling-data.data", header=TRUE)

# Analyze the relationship between responses and location.
# <responses>: a dataframe of the responses to a certain question.
#              Just a subset of the full binary dataframe.
# <locations>: the longitude and latitude for all responders.
analyze <- function(responses, locations) {

}

dirty.data = read.table("ling-data-clean.data", header=TRUE)
cut.data = dirty.data[, seq(5, length(dirty.data) - 2)]
n.responses = apply(cut.data, 2, max)

response.data = data[, seq(5, length(data) - 2)]
location.data = data[, seq(length(data) - 1, length(data))]

response.index = 1

for (i in 1..length(n.responses)) {
    responses = response.data[, seq(response.index, response.index + n.responses[i] - 1)]
    analyze(responses, location.data)
    response.index = response.index + n.responses[i]
}
