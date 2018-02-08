# Supreme Court Voting Patterns

# Author: Michael J. McFall
# Date: 2/18/2018
# Data can be found at http://scdb.wustl.edu/data.php under II. Justice Centerd Data 1. Cases Organized by Supreme Court Citation

# Importing Datasets
setwd("~/Tableau Makeover Monday/Supreme Court")
justice_centered = read.csv('SCDB_2017_01_justiceCentered_Citation.csv')

case_id = unique(justice_centered$caseId)
justices = unique(justice_centered$justiceName)
votes = data.frame(case_id)
for (justice in justices){
  votes[,justice] <- NA
}

for (row in 1:nrow(justice_centered)) {
  case = votes$case_id == justice_centered[row, "caseId"]
  justice = as.character(justice_centered$justiceName[row])
  votes[case, justice] = justice_centered[row, "vote"]
}

save(votes, file="votes.Rda")
write.csv(votes, file = "votes.csv")

same_vote = data.frame(justices)
voted_with = data.frame(justices)
for (justice in justices){
  same_vote[,justice] <- 0
  voted_with[,justice] <- 0
}
row.names(same_vote) = justices
row.names(voted_with) = justices
same_vote$justices = NULL
voted_with$justices = NULL

for (row in 1:nrow(votes)) {
  for (i in 1:8) {
    v = votes[row, -1] == i
    v[is.na(v)] = FALSE
    same_vote[v, v] = same_vote[v, v] + 1
  }
  w = !is.na(votes[row, -1])
  voted_with[w, w] = voted_with[w, w] +1
}

save(same_vote, file="same_vote_raw.Rda")
write.csv(same_vote, file = "same_vote_raw.csv")
save(voted_with, file="voted_with_raw.Rda")
write.csv(voted_with, file = "voted_with_raw.csv")

voted_with_percent = same_vote / voted_with

for (i in 1:length(justices)) {
  voted_with_percent[i, i] = NA
}

save(voted_with_percent, file="voted_with_percent.Rda")
write.csv(voted_with_percent, file = "voted_with_percent.csv")

current = c("AMKennedy", "CThomas", "EKagan", "JGRoberts", "NMGorsuch", "RBGinsburg", "SAAlito", "SGBreyer", "SSotomayor")

# HC current
hc_current = hclust(dist(voted_with_percent[current,], method = 'euclidean'), method = 'ward.D')
plot(as.dendrogram(hc_current),
     main = paste('Current Justices'),
     ylim = c(0,5))

# HC past
hc_start = hclust(dist(voted_with_percent[1:9,], method = 'euclidean'), method = 'ward.D')
plot(as.dendrogram(hc_start),
     main = paste('Justices in 1946'),
     ylim = c(0,5))
