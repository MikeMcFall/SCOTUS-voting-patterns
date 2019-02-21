# Supreme Court Voting Patterns

# Author: Michael J. McFall
# Date: 2/18/2018
# Data can be found at http://scdb.wustl.edu/data.php under II. Justice Centered Data 1. Cases Organized by Supreme Court Citation

# Importing Datasets
library(tidyverse)
setwd("~/GitHub/SCOTUS-voting-patterns")
justice_centered = read_csv('SCDB_2018_02_justiceCentered_Citation.csv')

justices = sort(unique(justice_centered$justiceName))

vote = justice_centered %>%
          select(caseId, justiceName, vote) %>%
          spread(justiceName, vote)

majority = justice_centered %>%
            select(caseId, justiceName, majority) %>%
            spread(justiceName, majority)

save(vote, file="vote.Rda")
write.csv(vote, file = "vote.csv")

voted_same = data.frame(justices)
voted_same_side = data.frame(justices)
cases_together = data.frame(justices)
for (justice in justices){
  voted_same[,justice] <- 0
  voted_same_side[,justice] <- 0
  cases_together[,justice] <- 0
}
row.names(voted_same) = justices
row.names(voted_same_side) = justices
row.names(cases_together) = justices
voted_same$justices = NULL
voted_same_side$justices = NULL
cases_together$justices = NULL

for (row in 1:nrow(vote)) {
  for (i in 1:8) {
    v = vote[row, -1]==i
    v[is.na(v)] = FALSE
    voted_same[v, v] = voted_same[v, v] + 1
  }
  for (i in 1:2) {
    w = majority[row, -1]==i
    w[is.na(w)] = FALSE
    voted_same_side[w, w] = voted_same_side[w, w] +1
  }
  x = !is.na(vote[row, -1])
  cases_together[x, x] = cases_together[x, x] +1
}

save(voted_same, file="voted_same_raw.Rda")
write.csv(voted_same, file = "voted_same_raw.csv")
save(voted_same, file="voted_same_side_raw.Rda")
write.csv(voted_same, file = "voted_same_side_raw.csv")
save(cases_together, file="cases_together_raw.Rda")
write.csv(cases_together, file = "cases_together_raw.csv")

voted_same_percent = voted_same / cases_together
voted_same_side_percent = voted_same_side / cases_together

for (i in 1:length(justices)) {
  voted_same_percent[i, i] = NA
  voted_same_side_percent[i, i] = NA
}

save(voted_same_percent, file="voted_same_percent.Rda")
write.csv(voted_same_percent, file = "voted_same_percent.csv")
save(voted_same_side_percent, file="voted_same_side_percent.Rda")
write.csv(voted_same_side_percent, file = "voted_same_side_percent.csv")

current = c("AMKennedy", "CThomas", "EKagan", "JGRoberts", "NMGorsuch", "RBGinsburg", "SAAlito", "SGBreyer", "SSotomayor")
j1946 = c("HHBurton", "RHJackson", "WODouglas", "FFrankfurter" ,"SFReed", "HLBlack", "WBRutledge", "FMurphy", "FMVinson")

# HC current
hc_current = hclust(dist(voted_same_percent[current,], method = 'euclidean'), method = 'ward.D')
plot(as.dendrogram(hc_current),
     main = paste('Current Justices'),
     ylim = c(0,5))

# HC past
hc_start = hclust(dist(voted_same_percent[j1946,], method = 'euclidean'), method = 'ward.D')
plot(as.dendrogram(hc_start),
     main = paste('Justices in 1946'),
     ylim = c(0,5))
