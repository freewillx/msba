#' ---
#' title: "Twitter Assignment"
#' author: "Will Y. Zhang"
#' ---


library(igraph)
library(ggplot2)
library(psych)

# Network structure visualization
g = read.graph("data/graph_subset.txt", format = "ncol", directed = TRUE)

tkplot(g, vertex.size = 5,
       vertex.color="#5BA5F5", 
       vertex.label = NA, 
       vertex.frame.color= "black",
       vertex.label.color = "white",
       vertex.label.family = "sans",
       layout = layout.kamada.kawai, canvas.width = 610, canvas.height = 610)

# Plot with membership - walktrap.community
wc <- walktrap.community(g)
colors_wc <- rainbow(max(membership(wc)))
plot(g,
     vertex.size = 3,
     vertex.color=colors_wc[membership(wc)], 
     vertex.label = NA, 
     vertex.label.family = "sans",
     edge.arrow.size=0.2,
     layout=layout.kamada.kawai)

# Plot with membership - edge.betweenness.community
eb <- edge.betweenness.community(g)
colors_eb <- rainbow(max(membership(eb)))
plot(g,
     vertex.size = 3,
     vertex.color=colors_eb[membership(eb)], 
     vertex.label = NA, 
     vertex.label.family = "sans",
     edge.arrow.size=0.2,
     layout=layout.kamada.kawai)

# Data analysis
graph = read.graph("data/graph_complete.txt", format = "ncol", directed = TRUE)

# Count User with 0 followers
zeroFollowers = V(graph)[degree(graph, mode="in")==0]
zeroFDF = data.frame("user_id" = zeroFollowers$name, "follower_counts" = 0)

# Count User with followers and calculate the follower counts
haveFollowers = V(graph)[degree(graph, mode="in")>0]
haveFDF = data.frame("user_id" = haveFollowers$name, "follower_counts" = degree(graph, haveFollowers, mode="in"))
haveFDF = haveFDF[order(-haveFDF$follower_counts),] 

# Merge two dataframes
followerDist = rbind(haveFDF, zeroFDF)
followerDist = followerDist[order(-followerDist$follower_counts),]

# Plot Distribution - bar plot
conTableDf = as.data.frame(table(followerDist$follower_counts))
names(conTableDf) = c("follower_counts", "node_counts")
ggplot(conTableDf, aes(x=conTableDf$follower_counts, y = conTableDf$node_counts)) + geom_bar(stat="identity") +
  scale_y_continuous(name="Node counts", breaks=pretty(conTableDf$node_counts, n=10)) +
  scale_x_discrete(name="Number of followers", breaks=c(0, 10, 20, 30, 40, 50, 60, 80, 90, 100, max(followerDist$follower_counts)))

# Log transform follower counts
# For zero follower accounts, replace 0.1 to 0 for log tranformation
followerDist$log_follower_counts = ifelse(followerDist$follower_counts >0, followerDist$follower_counts, 0.1) 
followerDist$log_follower_counts = log(followerDist$log_follower_counts)

## Density plot of the logscale distribution
ggplot(followerDist, aes(x = log_follower_counts))+geom_density() + scale_x_continuous(name="Log number of followers", breaks = pretty(followerDist$log_follower_counts, n=10))

# Stastistical summaries
describe(followerDist$follower_counts)

# Calculate Top 10 users
ids_to_usernames <- read.csv("data/ids_to_usernames.csv")
# Merge data frame by user id - join
top10Users = merge(followerDist, ids_to_usernames, by.x = "user_id", by.y = "id", all = FALSE)
top10Users = top10Users[order(-top10Users$follower_counts),c(1,2,4)]
top10Users = head(top10Users, n=10)

print(top10Users)
