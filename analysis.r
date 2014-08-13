require(maps)
require(RColorBrewer)

binary.data = read.table("binary-ling-data.data", header=TRUE)
original.data = read.table("ling-data-clean.data", header=TRUE)

# remove rows with NA for long or lat
location.data = binary.data[, seq(length(binary.data) - 1, length(binary.data))]
original.data = original.data[complete.cases(location.data), ]
binary.data = binary.data[complete.cases(location.data), ]
location.data = binary.data[, seq(length(binary.data) - 1, length(binary.data))]

original.responses = original.data[, seq(5, length(original.data) - 2)]
binary.responses = binary.data[, seq(5, length(binary.data) - 2)]

n.responses = apply(original.responses, 2, max)
question.names = names(original.responses)

xlim=c(-165, -65)
ylim=c(17, 72)

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

# Analyze the relationship between responses and location using binary data.
# <responses>: a dataframe of the responses to a certain question.
#              Just a subset of the full binary dataframe.
# <locations>: a numeric vector containing the longitude and latitude
#              of all responders.
# Plots the clusters of the most frequent response to the given question.
analyzeBinary <- function(responses, locations, k=8) {
    num.responses = apply(responses, 2, sum)
    # response is either most or least frequent response
    response = which(num.responses == max(num.responses))
    #response = which(num.responses == min(num.responses))
    answered = rownames(responses[which(responses[, response] == 1), ])
    question.name = colnames(responses)[response]
    plotClusters(answered, locations, k=k, question.name=question.name)
}

# Produces a plot of <k> clusters of the locations for <answered.indices>,
# taking a sample of size 3000 if more than 3000 indices given.
plotClusters <- function(answered.indices, location.data, k=8, question.name="Question Name") {
    if (length(answered.indices) > 3000) {
        answered.indices = sample(answered.indices, 3000)
    }
    answered.locations = location.data[answered.indices, ]
    answered.locations = answered.locations[, 2:1]
    distances = dist(answered.locations)
    hc = hclust(distances)
    tree = cutree(hc, k=k)
    cols = getColors(k)
    plot(answered.locations, pch=20, col=cols[tree], main=question.name,
         xlim=xlim, ylim=ylim)
    map('state', add=TRUE)
}

# Analyze binary data using analyzeBinary on every question.
# Will produce a plot of the most frequent response to each question,
# broken up into <k> clusters.
runBinaryAnalysis <- function(k=8) {
    response.index = 1
    for (i in 1:length(n.responses)) {
        responses = binary.responses[, seq(response.index, response.index + n.responses[i] - 1)]
        analyzeBinary(responses, location.data, k=k)
        response.index = response.index + n.responses[i]
    }
}

runBinaryAnalysis(k=4)

# Analyze the relationship between responses and location using original data.
# <responses>: a numeric vector giving the response of all responders to a certain question.
# <locations>: a dataframe containing the longitude and latitude of all responders.
makeMap <- function(responses, locations, question.name) {
    n.choices = max(responses)
    cols = getColors(n.choices)
    plot(x=locations$long, y=locations$lat, xlab="Longitude", ylab="Latitude",
         main=question.name, col=cols[responses], pch=20, xlim=xlim, ylim=ylim)
    legend("topright", legend=1:n.choices, col=cols, pch=20)
    map('state', add=TRUE)
}

makeMapsFor <- function(indices, responses, locations, question.names) {
    sapply(indices, function(i) {
        makeMap(responses[, i], locations, question.names[i])
    })
}

# Analyze original data
# makeMapsFor(1:length(n.responses), original.responses, location.data, question.names)
#par(mfrow=c(3, 3))
#makeMapsFor(which(n.responses == 3)[1:9], original.responses, location.data, question.names)

#correlation.matrix <- cor(original.responses)
#correlation.matrix[which(correlation.matrix > abs(.3))]
# Shows that correlation between responses very small
plotBinaryQuestion <- function(q.name) {
    q.index = which(colnames(binary.responses) == q.name)
    q.responses = which(binary.responses[, q.index] == 1)
    plot(location.data[q.responses, 2:1], pch=20, col="blue", main=q.name,
         xlim=xlim, ylim=ylim)
    map('state', add=TRUE)
}

plotOriginalQuestion <- function(q.name) {
    q.index = which(colnames(original.responses) == q.name)
    q.responses = original.responses[, q.index]
    n.choices = n.responses[q.index]
    cols = getColors(n.choices)
    plot(x=location.data$long, y=location.data$lat, xlab="Longitude", ylab="Latitude",
         main=q.name, col=cols[q.responses], pch=20)
    legend("topright", legend=1:n.choices, col=cols, pch=20)
    map('state', add=TRUE)
}

## par(mfrow=c(2, 2))
## plotBinaryQuestion("Q050.1")
## plotBinaryQuestion("Q073.1")
## plotBinaryQuestion("Q105.1")
## plotBinaryQuestion("Q080.1")
