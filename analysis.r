require("RColorBrewer")

binary.data = read.table("binary-ling-data.data", header=TRUE)
original.data = read.table("ling-data-clean.data", header=TRUE)

# Analyze the relationship between responses and location using binary data.
# <responses>: a dataframe of the responses to a certain question.
#              Just a subset of the full binary dataframe.
# <locations>: a numeric vector containing the longitude and latitude
#              of all responders.
analyzeBinary <- function(responses, locations) {
    
}

# Analyze the relationship between responses and location using original data.
# <responses>: a numeric vector giving the response of the responder.
# <locations>: a numeric vector containing the longitude and latitude
#              of all responders.
makeMap <- function(responses, locations, question.name) {
    n.choices = max(responses)
    cols = getColors(n.choices)
    plot(x=locations$long, y=locations$lat, xlab="Longitude", ylab="Latitude",
         main=question.name, col=cols[responses], pch=20)
    legend("topright", legend=1:n.choices, col=cols, pch=20)
}

makeMapsFor <- function(indices, responses, locations, question.names) {
    sapply(indices, function(i) {
        makeMap(responses[, i], locations, question.names[i])
    })
}

# Returns a vector of colors given a desired length.
# Only works for n.choices between 3 - 21, but this works fine for our purposes.
getColors <- function(n.choices) {
    if (n.choices > 9) {
        if (n.choices - 9 < 3) {
            cols = brewer.pal(n.choices, "Set3")
        } else {
            cols = c(brewer.pal(9, "Set1"), brewer.pal(n.choices - 9, "Set3"))
        }
    } else {
        cols = brewer.pal(n.choices, "Set1")
    }
    return(cols)
}
    

original.responses = original.data[, seq(5, length(original.data) - 2)]
binary.responses = binary.data[, seq(5, length(binary.data) - 2)]

n.responses = apply(original.responses, 2, max)
location.data = binary.data[, seq(length(binary.data) - 1, length(binary.data))]

question.names = names(original.responses)
response.index = 1

# Analyze binary data
for (i in 1:length(n.responses)) {
    responses = binary.responses[, seq(response.index, response.index + n.responses[i] - 1)]
    analyzeBinary(responses, location.data)
    response.index = response.index + n.responses[i]
}

# Analyze original data
makeMapsFor(1:length(n.responses), original.responses, location.data, question.names)
par(mfrow=c(3, 3))
makeMapsFor(which(n.responses == 3)[1:9], original.responses, location.data, question.names)

correlation.matrix <- cor(original.responses)
correlation.matrix[which(correlation.matrix > abs(.3))]
# Shows that correlation between responses very small
