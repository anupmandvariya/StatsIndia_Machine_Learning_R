library(arules)
library(arulesViz)
library(iplots)

####################################
### Apriori Rules - Titanic Data ###
####################################

### Load Titanic Data
data("titanic.raw")

### Create Apriori Rules
(titanic.rules <- apriori(titanic.raw))

### Inspect Rules
titanic.rules <- sort(titanic.rules, by="lift")
inspect(titanic.rules)
plot(titanic.rules)
plot(titanic.rules, method = "grouped")
plot(titanic.rules, method = "graph")
plot(titanic.rules, method="graph", control=list(type="items"))
plot(titanic.rules, method = "paracoord")
plot(titanic.rules, method="paracoord", control=list(reorder=TRUE))
plot(titanic.rules, method = "iplots")

### "Survived" Rules
survived.rules <- apriori(titanic.raw, control = list(verbose=F),
                 parameter = list(minlen=2, supp=0.005, conf=0.8),
                 appearance = list(rhs=c("Survived=No", "Survived=Yes"),
                                   default="lhs"))
quality(survived.rules) <- round(quality(survived.rules), digits=3)
survived.rules <- sort(survived.rules, by="lift")
inspect(survived.rules)
plot(survived.rules)

### Redundant Rules
subset.matrix <- is.subset(survived.rules, survived.rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant.rules <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant.rules)

### Remove Redundant Rules
pruned.rules <- survived.rules[!redundant.rules]
inspect(pruned.rules)
plot(pruned.rules)

### "Survived=Yes" Rules
alive.rules <- apriori(titanic.raw, 
                 parameter = list(minlen=3, supp=0.002, conf=0.2),
                 appearance = list(rhs=c("Survived=Yes"),
                                   lhs=c("Class=1st", "Class=2nd", "Class=3rd",
                                         "Age=Child", "Age=Adult"),
                                   default="none"), 
                 control = list(verbose=F))
alive.rules <- sort(alive.rules, by="confidence")
inspect(alive.rules)
plot(alive.rules)

######################################
### Apriori Rules - Groceries Data ###
######################################

data(Groceries)
(rules <- apriori(Groceries, parameter=list(support=0.001, confidence=0.8)))
inspect(rules)

## Scatterplot
plot(rules)
sel <- plot(rules, interactive=TRUE)

## see all control options
plot(rules, control = list(verbose = TRUE))

## Two-key plot is a scatterplot with shading = "order"
plot(rules, shading="order", control = list(main = "Two-key plot", 
                                            col=rainbow(max(size(rules))-1L)))

## The following techniques work better with fewer rules
subrules <- subset(rules, lift>5)
subrules

## 2D matrix with shading
plot(subrules, method="matrix", measure="lift")
plot(subrules, method="matrix", measure="lift", control=list(reorder=TRUE))

## 3D matrix
plot(subrules, method="matrix3D", measure="lift")
plot(subrules, method="matrix3D", measure="lift", control=list(reorder=TRUE))

## matrix with two measures
plot(subrules, method="matrix", measure=c("lift", "confidence"))
plot(subrules, method="matrix", measure=c("lift", "confidence"), 
     control=list(reorder=TRUE))

## Interactive plot
plot(subrules, method="matrix", measure="lift", interactive=TRUE, control=list(reorder=TRUE))

## grouped matrix plot
plot(rules, method="grouped")
plot(rules, method="grouped", 
     control = list(col = grey.colors(10), 
                    gp_labels= gpar(col = "blue", cex=1, fontface="italic")))

sel <- plot(rules, method="grouped", interactive=TRUE)

## graphs only work well with very few rules
subrules2 <- sample(subrules, 25)
plot(subrules2, method="graph")
plot(subrules2, method="graph", 
     control = list(nodeCol = grey.colors(10), edgeCol = grey(.7), alpha = 1))

## igraph layout generators can be used (see ? igraph::layout_)
plot(subrules2, method="graph", control=list(layout=igraph::in_circle()))
plot(subrules2, method="graph", control=list(layout=igraph::with_graphopt(spring.const=5, mass=50)))
plot(subrules2, method="graph", control=list(type="itemsets"))
plot(subrules2, method="graph", interactive=TRUE)
plot(subrules2, method="graph", control=list(engine="graphviz"))

## parallel coordinates plot
plot(subrules2, method="paracoord")
plot(subrules2, method="paracoord", control=list(reorder=TRUE))

## Doubledecker plot only works for a single rule
oneRule <- sample(rules, 1)
inspect(oneRule)
plot(oneRule, method="doubledecker", data = Groceries)

## use iplots (experimental)
sel <- plot(rules, method="iplots", interactive=TRUE)


## for itemsets
itemsets <- eclat(Groceries, parameter = list(support = 0.02, minlen=2))
plot(itemsets)
plot(itemsets, method="graph")
plot(itemsets, method="paracoord", control=list(alpha=.5, reorder=TRUE))

## add more quality measures to use for the scatterplot
quality(itemsets) <- interestMeasure(itemsets, trans=Groceries)
head(quality(itemsets))
plot(itemsets, measure=c("support", "allConfidence"), shading="lift")

