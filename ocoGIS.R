
rm(list=ls())

library(maptools)
library(colorRamps)
library(igraph)

source('createMapVector.R')
source('getNetwork.R')

I1 = read.table('data1.csv'); I1 = I1[,2:length(I1)]
I2 = read.table('data2.csv'); I2 = I2[,2:length(I2)]

state.map <- readShapeSpatial("Neighborhoods.shp")

zips = unique(I2[,3])
Dollars = 0*(1:length(zips))

Dollars = 0*(1:length(zips))

for(w in seq(1,length(zips))){
  cw = unique(I2[I2[,3]==zips[w],1])
  Dollars[w] = mean(I1[cw,2])
}

AZSc  = createMapVector(Dollars)

plot(state.map, col = AZSc, main='GIS data')

setEPS()
postscript("GIS.eps", width=4, height=8, fonts=c("serif", "Palatino"))
plot(state.map, col = AZSc, main='GIS data')
dev.off()


vin = I2[,1]
vout = I2[,2]
Tin = table(vin)
Tout = table(vout)
Ttot = c(Tin,Tout[1881:length(Tout)])

selectNode = unique(I2[,1])[1:80]

A = getNetwork(selectNode,I2)
nodes = A$nodes
links = A$links

net <- graph.data.frame(links, nodes, directed=T)
E(net)$arrow.mode <- 0
vsize = I1[nodes,2]-min(I1[nodes,2])
vsize = (vsize/max(vsize))*(12-2) + 2 # scale between 1 and 10
V(net)$size = vsize
E(net)$width = 3
plot(net, vertex.label=NA, main='Network hubs', sub='size = dollars')

setEPS()
postscript("Network.eps", width=20, height=16, fonts=c("serif", "Palatino"))
plot(net, vertex.label = NA)
dev.off()

