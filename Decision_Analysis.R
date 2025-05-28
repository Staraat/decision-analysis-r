library(data.tree)
library(yaml)

setwd("C:/Users/Hames.c/Desktop")

fileName <- 'plantation.yaml'
l <- yaml.load_file(fileName)
jl <- as.Node(l)
print(jl, "type", "payoff", "p")

# payoff recursion
payoff <- function(node) {
  if (node$type == 'chance')
    node$payoff <- sum(sapply(node$children, function(child) child$payoff * child$p))
  else if (node$type == 'decision')
    node$payoff <- max(sapply(node$children, function(child) child$payoff))
}

jl$Do(payoff, traversal = "post-order", filterFun = isNotLeaf)

# optimal decision branch for each decision node
decision <- function(x) {
  po <- sapply(x$children, function(child) child$payoff)
  x$decision <- names(po[po == x$payoff])
}

jl$Do(decision, filterFun = function(x) x$type == 'decision')

# helpers for plotting / printing
GetNodeLabel <- function(node) switch(node$type,
                                      terminal = paste0('$ ', format(node$payoff, scientific = FALSE, big.mark = ",")),
                                      paste0('Ev\n', '$ ', format(node$payoff, scientific = FALSE, big.mark = ",")))

GetEdgeLabel <- function(node) {
  if (!node$isRoot && node$parent$type == 'chance') {
    label = paste0(node$name, " (", node$p, ")")
  } else {
    label = node$name
  }
  return (label)
}

GetNodeShape <- function(node) switch(node$type, decision = "box", chance = "circle", terminal = "none")

SetEdgeStyle(jl, fontname = 'helvetica', label = GetEdgeLabel)
SetNodeStyle(jl, fontname = 'helvetica', label = GetNodeLabel, shape = GetNodeShape)

jl$Do(function(x) SetEdgeStyle(x, color = "red", inherit = FALSE),
      filterFun = function(x) !x$isRoot && x$parent$type == "decision" && x$parent$decision == x$name)

SetGraphStyle(jl, rankdir = "LR")
print(jl, "type", "payoff", "p")
plot(jl)

