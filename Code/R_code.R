
##----------------------------------------------------------------
##                       Loading Packages                       --
##----------------------------------------------------------------

library(tidyverse) # Easily Install and Load the 'Tidyverse', CRAN v1.3.0
library(GGally) # Extension to 'ggplot2', CRAN v2.0.0
library(knitr) # A General-Purpose Package for Dynamic Report Generation in R, CRAN v1.28
library(ggcorrplot) # Visualization of a Correlation Matrix using 'ggplot2', CRAN v0.1.3
library(psych) # Procedures for Psychological, Psychometric, and Personality Research, CRAN v1.9.12.31
library(factoextra) # Extract and Visualize the Results of Multivariate Data Analyses, CRAN v1.0.7
library(gridExtra) # Miscellaneous Functions for "Grid" Graphics, CRAN v2.3
library(ggfortify) # Data Visualization Tools for Statistical Analysis Results, CRAN v0.4.10
library(clustertend) # Check the Clustering Tendency, CRAN v1.4
library(clValid) # Validation of Clustering Results, CRAN v0.6-7
library(NbClust) # Determining the Best Number of Clusters in a Data Set, CRAN v3.0

##----------------------------------------------------------------
##                         Reading Data                         --
##----------------------------------------------------------------

breast_cancer <- read.csv('Data/datasets.csv', header = TRUE, row.names = 1, stringsAsFactors = FALSE)
breast_cancer <- breast_cancer[,1:11]
head(breast_cancer)


##---------------------------------------------------------------
##                  Exploratory Data Analysis                  --
##---------------------------------------------------------------

dim_desc(breast_cancer)
str(breast_cancer)
summary(breast_cancer)

ggcorr(data = breast_cancer[,-1], name = "corr", label = TRUE, method ='complete.obs' )+
  labs(title="Correlation Matrix of Numeric Variables")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))

ggpairs(breast_cancer, columns = 2:11, ggplot2::aes(colour=diagnosis), title = 'Correlogram of Variables')
ggplot(stack(breast_cancer[,-1]), aes(x = ind, y = values)) +
  geom_boxplot()+ coord_flip() + labs(title = 'Box Plot of Raw Variables', x = 'Variables', y = 'Values')



###### Scaling Data ######

breast_cancer_scaled <- scale(breast_cancer[,-1])
breast_cancer_scaled <- data.frame(diagnosis = breast_cancer$diagnosis, breast_cancer_scaled)
ggplot(stack(breast_cancer_scaled[,-1]), aes(x = ind, y = values)) +
  geom_boxplot()+ coord_flip() + labs(title = 'Box Plot of Scaled Variables', x = 'Variables', y = 'Values')




##---------------------------------------------------------------
##                 Data Pre-Processing for PCA                 --
##---------------------------------------------------------------

KMO(r = breast_cancer_scaled[,-1])
scaled_df_corr_matrix <- cor(breast_cancer_scaled[,-1], method = 'pearson', use = 'complete.obs')
scaled_df_eigen <- eigen(x = scaled_df_corr_matrix)
print(scaled_df_eigen[1])
scaled_df_var <- scaled_df_eigen$values/sum(scaled_df_eigen$values)
scaled_data_cumsum_var <- cumsum(scaled_df_var)
tibble(.rows = 1:10, eigenValue = scaled_df_eigen$values, Var = scaled_df_var, cumsumVar = scaled_data_cumsum_var) 


##----------------------------------------------------------------
##                         PCA Analysis                         --
##----------------------------------------------------------------

scaled_df_pca <- prcomp(x = breast_cancer_scaled[,-1])
scaled_df_pca$rotation
print(scaled_df_pca)
get_eig(scaled_df_pca)
fviz_screeplot(scaled_df_pca, ggtheme = theme_gray())
# Extract the results for variables
var <- get_pca_var(scaled_df_pca)
# Contributions of variables to PC1
pca_p1<- fviz_contrib(scaled_df_pca, choice = "var", axes = 1, top = 10, ggtheme = theme_gray())
# Contributions of variables to PC2
pca_p2<- fviz_contrib(scaled_df_pca, choice = "var", axes = 2, top = 10, ggtheme = theme_gray())
# Contributions of variables to PC3
pca_p3<- fviz_contrib(scaled_df_pca, choice = "var", axes = 3, top = 10, ggtheme = theme_gray())
pca_p4<- fviz_pca_var(X = scaled_df_pca, col.var = 'contrib', repel = TRUE,gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), ggtheme = theme_gray()) + ggtitle("Variables - PCA")
grid.arrange(pca_p1,pca_p2,pca_p3,pca_p4, nrow = 2)

autoplot(scaled_df_pca, data = breast_cancer_scaled, colour = 'diagnosis', loadings = TRUE, label = TRUE, label.size = 3,
         loadings.label = TRUE, loadings.label.size  = 4)


rotationed_df <- as.data.frame(predict(scaled_df_pca))
rotationed_df <- data.frame(diagnosis = breast_cancer$diagnosis, rotationed_df[,1:3])


##----------------------------------------------------------------
##              Data Pre-Processing for Clustering              --
##----------------------------------------------------------------


get_clust_tendency(data = rotationed_df[,-1], n = 5, seed =  123, graph = FALSE)


scaled_opt_algorithm_internal <- clValid(rotationed_df[,-1], nClust = 2:5, clMethods = c('kmeans','pam'), validation = "internal", verbose = TRUE, method = 'ward')
summary(scaled_opt_algorithm_internal)
# Choosing the right algorithm with stability measures
scaled_opt_algorithm_stability <- clValid(rotationed_df[,-1], nClust = 2:5, clMethods = c('kmeans','pam'), validation = "stability", verbose = TRUE, method = 'ward')
summary(scaled_opt_algorithm_stability)


set.seed(31)
# function to compute total within-cluster sum of squares
elbow <- fviz_nbclust(rotationed_df[,-1], kmeans, method = "wss", k.max = 24) + ggtitle("the Elbow Method") + theme_gray()
# Gap Statistics
gap <- fviz_nbclust(rotationed_df[,-1], kmeans, method = "gap_stat", k.max = 24) + ggtitle("Gap Statistics") + theme_gray()
# The Silhouette Method
silhouette1 <- fviz_nbclust(rotationed_df[,-1], kmeans, method = "silhouette", k.max = 24) + ggtitle("Silhouette Method") + theme_gray()
# Cluster method

scaled_nbclust <- NbClust(rotationed_df[,-1], distance = "manhattan", min.nc = 2, max.nc = 10, method = "ward.D2", index ="all")
nbclust1 <- fviz_nbclust(scaled_nbclust) + theme_gray() + ggtitle("NbClust's optimal number of clusters")
grid.arrange(elbow, gap, silhouette1, nbclust1)


##---------------------------------------------------------------
##                     K-Means Clustering                      --
##---------------------------------------------------------------

k2 <- kmeans(rotationed_df[,-1], centers = 2, nstart = 25)
k3 <- kmeans(rotationed_df[,-1], centers = 3, nstart = 25)
p2 <- fviz_cluster(k2, data = rotationed_df[,-1]) + ggtitle("k = 2")
p3 <- fviz_cluster(k3, data = rotationed_df[,-1]) + ggtitle("k = 2")
grid.arrange(p2, p3, nrow = 1, ncol = 2)


ssc <- data.frame(kmeans = c(2,3),
                  withinss = c(mean(k2$withinss), mean(k3$withinss)),
                  betweenss = c(k2$betweenss, k3$betweenss))

ssc %<>% gather(., key = "measurement", value = value, -kmeans)
ssc %>% ggplot(., aes(x=kmeans, y=log(value), fill = measurement)) + geom_bar(stat = "identity", position = "dodge") + 
  ggtitle("Cluster Model Comparison") + xlab("Number of Clusters") + ylab("Log10 Total Sum of Squares") + 
  scale_x_discrete(name = "Number of Clusters", limits = c('','2', '3',''))











































