
mergedWINData = read.csv(file="~/Desktop/mergedWINData.csv")
mergedWINData$X = NULL

library(plyr)
summary(mergedWINData$density_baseline)
summary(mergedWINData$clustering_coeff_average.binary.)
summary(mergedWINData$clustering_coeff_average.binary._baseline)
summary(mergedWINData$transitivity.binary._baseline)
summary(mergedWINData$network_characteristic_path_length.binary._baseline)
summary(mergedWINData$small.worldness.binary._baseline)
summary(mergedWINData$global_efficiency.binary._baseline)
summary(mergedWINData$diameter_of_graph.binary._baseline)
summary(mergedWINData$radius_of_graph.binary._baseline)
summary(mergedWINData$local_efficiency.binary._baseline)
summary(mergedWINData$assortativity_coefficient.binary._baseline)
summary(mergedWINData$transitivity.weighted._baseline)
summary(mergedWINData$network_characteristic_path_length.weighted._baseline)
summary(mergedWINData$small.worldness.weighted._baseline)
summary(mergedWINData$global_efficiency.weighted._baseline)
summary(mergedWINData$diameter_of_graph.weighted._baseline)
summary(mergedWINData$radius_of_graph.weighted._baseline)
summary(mergedWINData$local_efficiency.weighted._baseline)
summary(mergedWINData$assortativity_coefficient.weighted._baseline)
summary(mergedWINData$baseline_p)
summary(mergedWINData$baseline_q)

library(outliers)
grubbs.test(mergedWINData$density_baseline)
grubbs.test(mergedWINData$clustering_coeff_average.binary.)
grubbs.test(mergedWINData$clustering_coeff_average.binary._baseline)
grubbs.test(mergedWINData$transitivity.binary._baseline)
grubbs.test(mergedWINData$network_characteristic_path_length.binary._baseline)
grubbs.test(mergedWINData$small.worldness.binary._baseline)
grubbs.test(mergedWINData$global_efficiency.binary._baseline)
grubbs.test(mergedWINData$diameter_of_graph.binary._baseline)
grubbs.test(mergedWINData$radius_of_graph.binary._baseline)
grubbs.test(mergedWINData$local_efficiency.binary._baseline)
grubbs.test(mergedWINData$assortativity_coefficient.binary._baseline)
grubbs.test(mergedWINData$transitivity.weighted._baseline)
grubbs.test(mergedWINData$network_characteristic_path_length.weighted._baseline)
grubbs.test(mergedWINData$small.worldness.weighted._baseline)
grubbs.test(mergedWINData$global_efficiency.weighted._baseline)
grubbs.test(mergedWINData$diameter_of_graph.weighted._baseline)
grubbs.test(mergedWINData$radius_of_graph.weighted._baseline)
grubbs.test(mergedWINData$local_efficiency.weighted._baseline)
grubbs.test(mergedWINData$assortativity_coefficient.weighted._baseline)
grubbs.test(mergedWINData$baseline_p)
grubbs.test(mergedWINData$baseline_q)

library(psych)
skew(mergedWINData$density_baseline)
skew(mergedWINData$clustering_coeff_average.binary._baseline)
skew(mergedWINData$transitivity.binary._baseline)
skew(mergedWINData$network_characteristic_path_length.binary._baseline)
skew(mergedWINData$small.worldness.binary._baseline)
skew(mergedWINData$global_efficiency.binary._baseline)
skew(mergedWINData$local_efficiency.binary._baseline)
skew(mergedWINData$assortativity_coefficient.binary._baseline)
skew(mergedWINData$transitivity.weighted._baseline)
skew(mergedWINData$network_characteristic_path_length.weighted._baseline)
skew(mergedWINData$small.worldness.weighted._baseline)
skew(mergedWINData$global_efficiency.weighted._baseline)
skew(mergedWINData$diameter_of_graph.weighted._baseline)
skew(mergedWINData$radius_of_graph.weighted._baseline)
skew(mergedWINData$local_efficiency.weighted._baseline)
skew(mergedWINData$assortativity_coefficient.weighted._baseline)
skew(mergedWINData$baseline_p)
skew(mergedWINData$baseline_q)

fit = glm(baseline_p~
           density_baseline+
           clustering_coeff_average.binary._baseline+
           transitivity.binary._baseline+
           network_characteristic_path_length.binary._baseline+
           small.worldness.binary._baseline+
           global_efficiency.binary._baseline+
           local_efficiency.binary._baseline+
           assortativity_coefficient.binary._baseline,
        family = gaussian(identity),
        data = mergedWINData)
summary(fit)

fit = glm(baseline_q~
           density_baseline+
           clustering_coeff_average.binary._baseline+
           transitivity.binary._baseline+
           network_characteristic_path_length.binary._baseline+
           small.worldness.binary._baseline+
           global_efficiency.binary._baseline+
           #diameter_of_graph.binary._baseline+
           #radius_of_graph.binary._baseline+
           local_efficiency.binary._baseline+
           assortativity_coefficient.binary._baseline,
        family = gaussian(identity),
        data = mergedWINData)
summary(fit)

fit = glm(baseline_p~
           density_baseline+
           transitivity.weighted._baseline+
           network_characteristic_path_length.weighted._baseline+
           small.worldness.weighted._baseline+
           global_efficiency.weighted._baseline+
           diameter_of_graph.weighted._baseline+
           radius_of_graph.weighted._baseline+
           local_efficiency.weighted._baseline+
           assortativity_coefficient.weighted._baseline,
        family = gaussian(identity),
        data = mergedWINData)
summary(fit)

fit = glm(baseline_q~
           density_baseline+
           transitivity.weighted._baseline+
           network_characteristic_path_length.weighted._baseline+
           small.worldness.weighted._baseline+
           global_efficiency.weighted._baseline+
           diameter_of_graph.weighted._baseline+
           radius_of_graph.weighted._baseline+
           local_efficiency.weighted._baseline+
           assortativity_coefficient.weighted._baseline,
        family = gaussian(identity),
        data = mergedWINData)
summary(fit)

myData <- mergedWINData[1:126, c(2,3,4,6,8,10,16,18)]
head(myData)

corrMatrix <- round(cor(myData, use="complete.obs"), 2)
head(corrMatrix)

library(reshape2)

getLowerTri <- function(corrMatrix){
  corrMatrix[upper.tri(corrMatrix)] <- NA
  return(corrMatrix)
}
lowerTri <- getLowerTri(corrMatrix)
meltedCorrMatrix <- melt(lowerTri, na.rm = TRUE )
head(meltedCorrMatrix)

library(ggplot2)

ggplot(data = meltedCorrMatrix, p.mat = p.mat, aes(x=Var1, y=Var2, fill=value)) + 
  
  geom_tile(color="white") +
  
  scale_x_discrete(labels = c("Density", "Clustering Coeff", "Transitivity", "Char Path Length", 
                            "Small Worldness", "Global Efficiency", "Local Efficiency", 
                            "Assortativity")) +
  
  scale_y_discrete(labels = c("Density", "Clustering Coeff", "Transitivity", "Char Path Length", 
                            "Small Worldness", "Global Efficiency", "Local Efficiency", 
                            "Assortativity")) +
  
  scale_fill_gradient(low = "gold", high = "red", 
                      limit = c(-1,1), space = "Lab",
                      name = "WIN Binary Variables\nCorrelation Matrix\n\n") + 
  theme_minimal() + 
  
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, size = 9, hjust = 1)) +
  
  coord_fixed() +
  
  geom_text(aes(Var1, Var2, label = value), color = "white", size = 4)

myData <- mergedWINData[1:126, c(2,5,7,9,11,17,19)]
head(myData)

corrMatrix <- round(cor(myData, use="complete.obs"), 2)
head(corrMatrix)

library(reshape2)

getLowerTri <- function(corrMatrix){
  corrMatrix[upper.tri(corrMatrix)] <- NA
  return(corrMatrix)
}
lowerTri <- getLowerTri(corrMatrix)
meltedCorrMatrix <- melt(lowerTri, na.rm = TRUE )
head(meltedCorrMatrix)

library(ggplot2)

ggplot(data = meltedCorrMatrix, p.mat = p.mat, aes(x=Var1, y=Var2, fill=value)) + 
  
  geom_tile(color="white") +

  scale_x_discrete(labels = c("Density", "Transitivity", "Char Path Length",
                              "Small Worldness", "Global Efficiency", "Local Efficiency",
                              "Assortativity")) +

  scale_y_discrete(labels = c("Density", "Transitivity", "Char Path Length",
                              "Small Worldness", "Global Efficiency", "Local Efficiency",
                              "Assortativity")) +
  
  scale_fill_gradient(low = "yellow", high = "red", 
                      limit = c(-1,1), space = "Lab",
                      name = "WIN Weighted Variables\nCorrelation Matrix") + 
  theme_minimal() + 
  
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, size = 9, hjust = 1)) +
  
  coord_fixed() +
  
  geom_text(aes(Var1, Var2, label = value), color = "white", size = 4, check_overlap = FALSE)

baseline_p_scaled <- scale(mergedWINData$baseline_p)
baseline_q_scaled <- scale(mergedWINData$baseline_q)

density_baseline_scaled <- scale(mergedWINData$density_baseline)
clustering_coeff_average.binary._baseline_scaled <- scale(mergedWINData$clustering_coeff_average.binary._baseline)
transitivity.binary._baseline_scaled <- scale(mergedWINData$transitivity.binary._baseline)
network_characteristic_path_length.binary._baseline_scaled <- scale(mergedWINData$network_characteristic_path_length.binary._baseline)
small.worldness.binary._baseline_scaled <- scale(mergedWINData$small.worldness.binary._baseline)
global_efficiency.binary._baseline_scaled <- scale(mergedWINData$global_efficiency.binary._baseline)
local_efficiency.binary._baseline_scaled <- scale(mergedWINData$local_efficiency.binary._baseline)
assortativity_coefficient.binary._baseline_scaled <- scale(mergedWINData$assortativity_coefficient.binary._baseline)

transitivity.weighted._baseline_scaled <- scale(mergedWINData$transitivity.weighted._baseline)
network_characteristic_path_length.weighted._baseline_scaled <- scale(mergedWINData$network_characteristic_path_length.weighted._baseline)
small.worldness.weighted._baseline_scaled <- scale(mergedWINData$small.worldness.weighted._baseline)
global_efficiency.weighted._baseline_scaled <- scale(mergedWINData$global_efficiency.weighted._baseline)
local_efficiency.weighted._baseline_scaled <- scale(mergedWINData$local_efficiency.weighted._baseline)
assortativity_coefficient.weighted._baseline_scaled <- scale(mergedWINData$assortativity_coefficient.weighted._baseline)

standardizedVariables <- data.frame(density=density_baseline_scaled,
                                    clust_binary=clustering_coeff_average.binary._baseline_scaled,
                                    trans_binary=transitivity.binary._baseline_scaled,
                                    net_binary=network_characteristic_path_length.binary._baseline_scaled,
                                    small_binary=small.worldness.binary._baseline_scaled,
                                    global_binary=global_efficiency.binary._baseline_scaled,
                                    local_binary=local_efficiency.binary._baseline_scaled,
                                    assort_binary=assortativity_coefficient.binary._baseline_scaled,
                                    
                                    trans_weighted=transitivity.weighted._baseline_scaled,
                                    net_weighted=network_characteristic_path_length.weighted._baseline_scaled,
                                    small_weighted=small.worldness.weighted._baseline_scaled,
                                    global_weighted=global_efficiency.weighted._baseline_scaled,
                                    local_weighted=local_efficiency.weighted._baseline_scaled,
                                    assort_weighted=assortativity_coefficient.weighted._baseline_scaled,
                                    
                                    Pscore=baseline_p_scaled,
                                    Qscore=baseline_q_scaled)

library(pls)

pcr.fit = pcr(baseline_p_scaled~
           density_baseline_scaled+
           clustering_coeff_average.binary._baseline_scaled+
           transitivity.binary._baseline_scaled+
           network_characteristic_path_length.binary._baseline_scaled+
           small.worldness.binary._baseline_scaled+
           global_efficiency.binary._baseline_scaled+
           local_efficiency.binary._baseline_scaled+
           assortativity_coefficient.binary._baseline_scaled,
          data = mergedWINData,
          scale = TRUE,
          validation = "CV"
           )

summary(pcr.fit)

validationplot(pcr.fit, val.type = "MSEP")
validationplot(pcr.fit, val.type = "R2")
predplot(pcr.fit)
coefplot(pcr.fit) # plot of regression coefficients

library(pls)

pcr.fit = pcr(baseline_q_scaled~
           density_baseline_scaled+
           clustering_coeff_average.binary._baseline_scaled+
           transitivity.binary._baseline_scaled+
           network_characteristic_path_length.binary._baseline_scaled+
           small.worldness.binary._baseline_scaled+
           global_efficiency.binary._baseline_scaled+
           local_efficiency.binary._baseline_scaled+
           assortativity_coefficient.binary._baseline_scaled,
          data = mergedWINData,
          scale = TRUE,
          validation = "CV"
           )

summary(pcr.fit)

validationplot(pcr.fit, val.type = "MSEP")
validationplot(pcr.fit, val.type = "R2")
predplot(pcr.fit)
coefplot(pcr.fit)

library(pls)

pcr.fit = pcr(baseline_p_scaled~
           transitivity.weighted._baseline_scaled+
           network_characteristic_path_length.weighted._baseline_scaled+
           small.worldness.weighted._baseline_scaled+
           global_efficiency.weighted._baseline_scaled+
           local_efficiency.weighted._baseline_scaled+
           assortativity_coefficient.weighted._baseline_scaled,
          data = mergedWINData,
          scale = TRUE,
          validation = "CV"
           )

summary(pcr.fit)

validationplot(pcr.fit, val.type = "MSEP")
validationplot(pcr.fit, val.type = "R2")
predplot(pcr.fit)
coefplot(pcr.fit)

library(pls)

pcr.fit = pcr(baseline_q_scaled~
           transitivity.weighted._baseline_scaled+
           network_characteristic_path_length.weighted._baseline_scaled+
           small.worldness.weighted._baseline_scaled+
           global_efficiency.weighted._baseline_scaled+
           local_efficiency.weighted._baseline_scaled+
           assortativity_coefficient.weighted._baseline_scaled,
          data = mergedWINData,
          scale = TRUE,
          validation = "CV"
           )

summary(pcr.fit)

validationplot(pcr.fit, val.type = "MSEP")
validationplot(pcr.fit, val.type = "R2")
predplot(pcr.fit)
coefplot(pcr.fit)

myData <- standardizedVariables[,c(1,2,3,4,5,6,7,8)]

myPCA = princomp(na.omit(myData),
               cor = TRUE,
               scores = TRUE)

summary(myPCA)

myPCA$loadings

plot(myPCA)


library("factoextra")
fviz_screeplot(myPCA, ncp=10)
fviz_pca_biplot(myPCA) + theme_minimal()

myData <- standardizedVariables[, c(9,10,11,12,13,14)]

myPCA = princomp(na.omit(myData),
               cor = TRUE,
               scores = TRUE)

summary(myPCA)
plot(myPCA)
myPCA$loadings

library("factoextra")
fviz_screeplot(myPCA, ncp=10)
fviz_pca_biplot(myPCA) + theme_minimal()

myData <- standardizedVariables[,c(1,2,3,4,5,6,7,8)]

svd(na.omit(myData), nu = min(124,5), nv = min(5,5)) #SVD 

myData <- standardizedVariables[, c(9,10,11,12,13,14)]

svd(na.omit(myData), nu = min(124,5), nv = min(5,5)) #SVD 


