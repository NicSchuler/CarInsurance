# install.packages("rgdal")
# install.packages("plotrix")
# install.packages("classInt")

library('rgdal')      # Reading and projecting shapefiles
library('plotrix')    # Creating color scales
library('classInt')   # Assigning colors to data

# Reading departements
departements <- readOGR(dsn="../ShinyApp/Geofla/Useful/DEPARTEMENT.shp")

# Reading departements boundaries in order to plot France boundaries
bounderies <- readOGR(dsn="../ShinyApp/Geofla/Useful/LIMITE_DEPARTEMENT.shp")


bounderies <- bounderies[bounderies$NATURE %in% c('Fronti\xe8re internationale','Limite c\xf4ti\xe8re'),]


europe <- readOGR(dsn="../ShinyApp/Geofla/Useful/ne_110m_admin_0_countries.shp")


# Projection
europe <- spTransform(europe, CRS("+init=epsg:2154"))

# Plot
pdf('france.pdf',width=6,height=4.7)
par(mar=c(0,0,0,0))

plot(bounderies,  col="#FFFFFF")
plot(europe,      col="#E6E6E6", border="#AAAAAA",lwd=1, add=TRUE)
plot(bounderies,  col="#D8D6D4", lwd=6, add=TRUE)
plot(departements,col="#FFFFFF", border="#CCCCCC",lwd=.7, add=TRUE)
plot(bounderies,  col="#666666", lwd=1, add=TRUE)

dev.off()

departements@data$id = rownames(departements@data)
departements.points = fortify(departements)
departements.df = merge(departements.points, departements@data, by="id")

ggplot(departements.df) + 
  aes(long,lat,group=group) + 
  geom_polygon(fill="green4") +
  geom_path(color="black") +
  coord_equal() +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())

bounderies@data$id = rownames(bounderies@data)
bounderies.points = fortify(bounderies)
bounderies.df = merge(bounderies.points, bounderies@data, by="id")


