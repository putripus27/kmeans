data = read.csv("D:/UNY/SEMESTER 7/PKL/data/persentase rata rata pengguna dengan peserta aktif.csv")
str(data)
head(data)

#standarisasi skor baku, minmax
#tdk perlu karena var hampir sama
#cuttoff Sari (2020) tdk ada korelasi kuat antar variabelnya
#fenomena sosial knp kecamatan di kota cenderung klaster 1
#kemudahan, sosialisasi, preferensi

df = data[,-1]
round(var(df), 2)
round(cor(df), 2)

round(summary(df), 3)
round(0.2574, 4)
round(0.05408, 4)
round(0.00809, 4)
round(0.13655, 4)
round(0.08251, 4)
round(0.3653, 4)
round(0.09474, 4)

##### CLUSTERING #####
library(factoextra)
library(FactoMineR)
fit.pca <- PCA(df, scale.unit = TRUE, ncp = 6, graph = TRUE)
eig.val <- get_eigenvalue(fit.pca)
eig.val
round(eig.val, 2)

set.seed(2022)
clust.2means<-kmeans(x = df, centers = 3, nstart=50)
clust.2means

a = aggregate(df, by=list(cluster=clust.2means$cluster), mean)
round(a, 2)

dd <- cbind(data, cluster = clust.2means$cluster)
dd

fviz_cluster(clust.2means, data = df,
             palette = c("#FFB6C1", "#00AFBB", "#E7B800"),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal())

dd$kecamatan[dd$cluster == "1"]
dd$kecamatan[dd$cluster == "2"]
dd$kecamatan[dd$cluster == "3"]

sum(dd$cluster == "1")
sum(dd$cluster == "2")
sum(dd$cluster == "3")

write.csv(dd, "D:/UNY/PKL/data/hasil kluster.csv")

##### Peta Jogja #####
library(rgdal)
library(maptools)
library(ggplot2)
library(plyr)
library(dplyr)
library(ggmap)
library(mapproj)
library(ggsn)
library(ggspatial)
library(sp)

KEC <- readRDS("D:/UNY/SEMESTER 4/KOMPUTASI STATISTIKA/8. Membaca Data dari Berbagai Sumber dan Memvisualisasikan Data/gadm36_IDN_3_sp.rds")
DIY <- KEC[KEC$NAME_1 == "Yogyakarta",]
kec.diy <- DIY$NAME_3
kec.diy

KEC <- readRDS("D:/UNY/SEMESTER 4/KOMPUTASI STATISTIKA/8. Membaca Data dari Berbagai Sumber dan Memvisualisasikan Data/gadm36_IDN_3_sp.rds")
DIY <- KEC[KEC$NAME_1 == "Yogyakarta",]
kec.diy <- DIY$NAME_3
kec.diy


hasil <- read.csv(file="D:/UNY/PKL/data/hasil kluster.csv")
library(ggplot2)
library(raster) 
plot(DIY, col = c("skyblue","pink")[hasil$cluster], 
     axes = TRUE, cex = 0.25, border = "black") 
text(DIY, hasil$kecamatan, cex = 0.6)
legend("right", legend = c("Klaster 1", "Klaster 2"), 
       col = c("skyblue","pink"), 
       inset = 0.02, fill= c("skyblue","pink"), cex = 0.7, bty = "n") 
library(prettymapr)
addnortharrow(pos = "topright", scale = 0.5, padin = c(0.55, 0.15))
addscalebar()


library(maptools)
gpclibPermit()
d1 <- fortify(DIY, region = "NAME_3")
str(d1)
head(d1)
d1$kecamatan <- d1$id

library(dplyr)
d2 <- full_join(d1, dd, by = c("kecamatan"))
str(d2)

cnames <- aggregate(cbind(long, lat) ~ kecamatan, data=d2,
                    FUN=function(x)mean(range(x)))
ggplot() + geom_polygon(data = d2, 
                        aes(x = long, y = lat, group = group, fill = cluster)) +
  geom_text(data=cnames, aes(long, lat, label=kecamatan), size= 4)+
  coord_map() + labs(title="          Klaster Kecamatan berdasarkan Alat Kontrasepsi \n
        di Provinsi DI Yogyakarta Tahun 2021") +
  scale_fill_continuous(high = "dodgerblue4", low = "steelblue1", name = "Klaster") +
  theme_minimal() + blank() + theme(plot.title = element_text(hjust = 0.5)) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering)




##### DESKRIPTIF #####
KEC <- readRDS("D:/UNY/SEMESTER 4/KOMPUTASI STATISTIKA/8. Membaca Data dari Berbagai Sumber dan Memvisualisasikan Data/gadm36_IDN_3_sp.rds")
DIY <- KEC[KEC$NAME_1 == "Yogyakarta",]
kec.diy <- DIY$NAME_3
kec.diy

library(maptools)
gpclibPermit()
d1 <- fortify(DIY, region = "NAME_3")
str(d1)
head(d1)
d1$kecamatan <- d1$id

PA = read.csv("D:/UNY/PKL/data/PA kecamatan perbulan.csv")
str(PA)

dt = PA[,c(1,14)]

library(dplyr)
d3 <- full_join(d1, dt, by = c("kecamatan"))
str(d3)

cnames <- aggregate(cbind(long, lat) ~ kecamatan, data=d3,
                    FUN=function(x)mean(range(x)))
ggplot() + geom_polygon(data = d3, aes(x = long, y = lat, group = group, fill = PA)) +
  geom_text(data=cnames, aes(long, lat, label=kecamatan), size= 4)+
  coord_map() + labs(title="          Rata - rata Peserta Aktif menurut Kecamatan \n
        di Provinsi DI Yogyakarta Tahun 2021") +
  scale_fill_continuous(high = "lightskyblue4", low = "lightskyblue1", name = "Peserta Aktif") +
  theme_minimal() + blank() + theme(plot.title = element_text(hjust = 0.5)) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering)

summary(PA[,14])


PUS = read.csv("D:/UNY/PKL/data/PUS des 2021.csv")
str(PUS)

summary(PUS[,2])

PUS.PA = cbind(PUS, PA = round(PA[,14]))
head(PUS.PA)
