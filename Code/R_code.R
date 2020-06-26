
##----------------------------------------------------------------
##                       Loading Packages                       --
##----------------------------------------------------------------

library(tidyverse) # Easily Install and Load the 'Tidyverse', CRAN v1.3.0
library(GGally) # Extension to 'ggplot2', CRAN v2.0.0
library(knitr) # A General-Purpose Package for Dynamic Report Generation in R, CRAN v1.28
library(ggcorrplot) # Visualization of a Correlation Matrix using 'ggplot2', CRAN v0.1.3
library(factoextra) # Extract and Visualize the Results of Multivariate Data Analyses, CRAN v1.0.7
library(gridExtra) # Miscellaneous Functions for "Grid<" Graphics, CRAN v2.3
library(ggfortify) # Data Visualization Tools for Statistical Analysis Results, CRAN v0.4.10
library(clustertend) # Check the Clustering Tendency, CRAN v1.4
library(magrittr) # A Forward-Pipe Operator for R, CRAN v1.5
library(cluster) # "Finding Groups in Data": Cluster Analysis Extended Rousseeuw et
library(clValid) # Validation of Clustering Results, CRAN v0.6-7
library(NbClust) # Determining the Best Number of Clusters in a Data Set, CRAN v3.0
library(scales) # Scale Functions for Visualization, CRAN v1.1.1
library(hrbrthemes) # Additional Themes, Theme Components and Utilities for 'ggplot2', CRAN v0.8.0
library(viridis) # Default Color Maps from 'matplotlib', CRAN v0.5.1
library(latex2exp) # Use LaTeX Expressions in Plots, CRAN v0.4.0
library(ggpubr) # 'ggplot2' Based Publication Ready Plots, CRAN v0.3.0
library(mclust) # Gaussian Mixture Modeling for Model-Based Clustering, Classification, and Density Estimation, CRAN v5.4.6
library(fpc) # Flexible Procedures for Clustering, CRAN v2.2-6
library(dbscan) # Density Based Clustering of Applications with Noise (DBSCAN) and Related Algorithms, CRAN v1.1-5
library(ClusterR) # Gaussian Mixture Models, K-Means, Mini-Batch-Kmeans, K-Medoids and Affinity Propagation Clustering, CRAN v1.2.2
library(psych) # Procedures for Psychological, Psychometric, and Personality Research, CRAN v1.9.12.31 
library(xtable) # Export Tables to LaTeX or HTML, CRAN v1.8-4
library(stargazer) # Well-Formatted Regression and Summary Statistics Tables, CRAN v5.2.2
library(reshape2)
##----------------------------------------------------------------
##                         Reading Data                         --
##----------------------------------------------------------------

breast_cancer <- read.csv('Data/datasets.csv', header = TRUE, row.names = 1, stringsAsFactors = FALSE)
breast_cancer <- breast_cancer[,1:11]
names(breast_cancer) <-  gsub(pattern = "_mean*", replacement = "", x = names(breast_cancer))
head(breast_cancer)
tablo1<- xtable::xtable(x = head(breast_cancer), caption = 'Veri setinin ilk altı gözlemi')
print(tablo1, scalebox = 0.7)


##---------------------------------------------------------------
##                  Exploratory Data Analysis                  --
##---------------------------------------------------------------

dim_desc(breast_cancer)
str(breast_cancer)
summary(breast_cancer)
stargazer(breast_cancer, summary = TRUE, title = 'Değişkenlere ait açıklayıcı istatistikler', median = TRUE, iqr = TRUE, min.max = TRUE, mean.sd = TRUE, font.size = 'small')
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
xtable::xtable(tibble(.rows = 1:10, eigenValue = scaled_df_eigen$values, Var = scaled_df_var, cumsumVar = scaled_data_cumsum_var),
               caption = 'Değişkenlere ait özdeğeğer ve Varyans değeleri') 


##----------------------------------------------------------------
##                         PCA Analysis                         --
##----------------------------------------------------------------

scaled_df_pca <- prcomp(x = breast_cancer_scaled[,-1])
scaled_df_pca$rotation
print(scaled_df_pca)
get_eig(scaled_df_pca)
screeplots <- fviz_screeplot(scaled_df_pca, ggtheme = theme_gray())
# Extract the results for variables
var <- get_pca_var(scaled_df_pca)
# Contributions of variables to PC1
pca_p1<- fviz_contrib(scaled_df_pca, choice = "var", axes = 1, top = 10, ggtheme = theme_gray())
# Contributions of variables to PC2
pca_p2<- fviz_contrib(scaled_df_pca, choice = "var", axes = 2, top = 10, ggtheme = theme_gray())
# Contributions of variables to PC3
pca_p3<- fviz_contrib(scaled_df_pca, choice = "var", axes = 3, top = 10, ggtheme = theme_gray())
pca_p4<- fviz_pca_var(X = scaled_df_pca, col.var = 'contrib', repel = TRUE,gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), ggtheme = theme_gray()) + ggtitle("Variables - PCA")
grid.arrange(pca_p1,pca_p2,screeplots,pca_p4, nrow = 2)

autoplot(scaled_df_pca, data = breast_cancer_scaled, colour = 'diagnosis', loadings = TRUE, label = TRUE, label.size = 3,
         loadings.label = TRUE, loadings.label.size  = 4)


rotationed_df <- as.data.frame(predict(scaled_df_pca))
rotationed_df <- data.frame(diagnosis = breast_cancer$diagnosis, rotationed_df[,1:2])


##----------------------------------------------------------------
##              Data Pre-Processing for Clustering              --
##----------------------------------------------------------------


get_clust_tendency(data = rotationed_df[,-1], n = 5, seed =  123, graph = FALSE)
clustertend:: hopkins(rotationed_df[,-1], nrow(rotationed_df[,-1])-1)

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


ssc <- data.frame(kmeans = c('k=2','k=3'),
                  withinss = c(mean(k2$withinss), mean(k3$withinss)),
                  betweenss = c(k2$betweenss, k3$betweenss))

ssc %<>% gather(., key = "measurement", value = value, -kmeans)
ggplot(ssc, aes(fill=measurement, y=log(value), x=kmeans)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("K-means Comparasion") +
  xlab("") + ylab(TeX("$\\ln{(value)}$"))




##----------------------------------------------------------------
##                     K-Medoids Clustering                     --
##----------------------------------------------------------------


# function to compute total within-cluster sum of squares
elbow <- fviz_nbclust(rotationed_df[,-1], pam, method = "wss", k.max = 24) + ggtitle("the Elbow Method") + theme_gray()
# Gap Statistics
gap <- fviz_nbclust(rotationed_df[,-1], pam, method = "gap_stat", k.max = 24) + ggtitle("Gap Statistics") + theme_gray()
# The Silhouette Method
silhouette1 <- fviz_nbclust(rotationed_df[,-1], pam, method = "silhouette", k.max = 24) + ggtitle("Silhouette Method") + theme_gray()
# Cluster method
scaled_nbclust <- NbClust(rotationed_df[,-1], distance = "manhattan", min.nc = 2, max.nc = 10, method = "ward.D2", index ="all",)
nbclust1 <- fviz_nbclust(scaled_nbclust) + theme_gray() + ggtitle("NbClust's optimal number of clusters")
grid.arrange(elbow, gap, silhouette1, nbclust1)


pam2 <- pam(rotationed_df[,-1],k = 2)
pam3 <- pam(rotationed_df[,-1],k = 3)
pamg2 <- fviz_cluster(pam2, data = rotationed_df[,-1]) + ggtitle("k = 2")
pamg3 <- fviz_cluster(pam3, data = rotationed_df[,-1]) + ggtitle("k = 2")
grid.arrange(pamg2, pamg3, nrow = 1, ncol = 2)




##----------------------------------------------------------------
##                    Hierarchical Clustering                   --
##----------------------------------------------------------------


# find optimal algorithm for hierarchical clustering ##

# methods to assess

methods <- c( "average", "single", "complete", "ward")
names(methods) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(rotationed_df[,-1], method = x)$ac
}

method_result <- map_dbl(methods, ac)
method_result
method_df <- data.frame(methods, method_result) # table yap

# Barplot
ggplot(method_df, aes(x=methods, y=method_result)) + 
  geom_bar(stat = "identity", fill = 'steelblue') + labs(title = 'Cluster Methods Comparation', x = 'models', y = 'Percentage of Ac')

## find optimal cluster ##

set.seed(31)
# function to compute total within-cluster sum of squares
elbow <- fviz_nbclust(rotationed_df[,-1], FUN = hcut, method = "wss", k.max = 24) + ggtitle("The Elbow Method") + theme_gray()
# Gap Statistics
gap <- fviz_nbclust(rotationed_df[,-1], FUN = hcut, method = "gap_stat", k.max = 24) + ggtitle("Gap Statistics") + theme_gray()
# The Silhouette Method
silhouette1<- fviz_nbclust(rotationed_df[,-1], FUN = hcut, method = "silhouette", k.max = 24) + ggtitle("Silhouette Method") + theme_gray()

grid.arrange(elbow, gap, silhouette1, nrow = 3)

# Dissimilarity matrix
distance <- dist(rotationed_df[,-1], method = "manhattan")
scaled_h_clust <- hclust(distance, method = "ward.D2")
sub_grp <- cutree(scaled_h_clust, k = 2)
table(sub_grp)
fviz_dend(scaled_h_clust, k = 2,
          cex = 0.5,
          k_colors = c("#2E9FDF", "#00AFBB", "#FC4E07"),
          color_labels_by_k = TRUE,
          rect = TRUE
)

table(sub_grp)
plot(scaled_h_clust, cex = 0.6)
abline(h = 5, lty = 2)
rect.hclust(scaled_h_clust, k = 5, border = 2:5)
cut_deng<- as.dendrogram(scaled_h_clust)
plot(cut(cut_deng, h = 28)$lower[[2]],
     main = "Second branch of lower tree with cut at h=28")



##----------------------------------------------------------------
##                   Model based Clustering                   --
##----------------------------------------------------------------

ggplot(rotationed_df, aes(x = PC1,y = PC2, colour = diagnosis))+
  geom_density2d()+ geom_point()

mc <- Mclust(rotationed_df[,-1]) # Model-based-clustering 
summary(mc)
mc$modelName
mc$G
head(mc$z, 30)
head(mc$classification, 30)


# BIC values used for choosing the number of clusters 
BIC1 <- fviz_mclust(mc, "BIC", palette = "viridis") 
# Classification: plot showing the clustering 
classification <- fviz_mclust(mc, "classification", geom = "point", pointsize = 1.5, palette = "viridis") + theme_gray()
# Classification uncertainty
uncertantiy <- fviz_mclust(mc, "uncertainty", palette = "viridis")+ theme_gray()
grid.arrange(BIC1, classification, uncertantiy, ncol = 2, nrow = 2)



##----------------------------------------------------------------
##                  Density Based Clustering                   --
##----------------------------------------------------------------

dbscan::kNNdistplot(rotationed_df[,-1], k = 10) 

db <- fpc::dbscan(rotationed_df[,-1], eps = 0.6, MinPts = 10)
fviz_cluster(db, data = rotationed_df[,-1], stand = TRUE,
             ellipse = TRUE, show.clust.cent = TRUE,
             geom = "point",palette = "jco", ggtheme = theme_gray())

db$cluster



##----------------------------------------------------------------
##                       Model Evaluation                       --
##----------------------------------------------------------------



K_Means <- external_validation(as.numeric(as.factor(rotationed_df$diagnosis)), k2$cluster, method = "adjusted_rand_index", summary_stats = T)
K_Medoids <- external_validation(as.numeric(as.factor(rotationed_df$diagnosis)), pam2$clustering, method = "adjusted_rand_index", summary_stats = T)
Hierarchical <- external_validation(as.numeric(as.factor(rotationed_df$diagnosis)), sub_grp, method = "adjusted_rand_index", summary_stats = T)
GMM_Model <- external_validation(as.numeric(as.factor(rotationed_df$diagnosis)), mc$classification, method = "adjusted_rand_index", summary_stats = T)
DBSCAN <- external_validation(as.numeric(as.factor(rotationed_df[-which(db$cluster == 0),1])), db$cluster[db$cluster>0], method = "adjusted_rand_index", summary_stats = T)



Result <- data.frame(models = c('K-Means', 'K-Medoids', 'Hierarchical', 'GMM', 'DBSCAN'),
                  values = c(K_Means, K_Medoids, Hierarchical, GMM_Model, DBSCAN))

Result  %<>% gather(., key = "measurement", value = value, -models)
ggplot(Result, aes(y= value, x= models)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("K-means Comparasion") +
  xlab("") + ylab(TeX("$\\ln{(value)}$"))



##----------------------------------------------------------------
##                    Descriptive Statistics                    --
##----------------------------------------------------------------


final_model <-cbind(breast_cancer[,-1], pam2$clustering)
colnames(final_model)[11]<-c("Group")
final_results <- describe.by(final_model, group='Group')
tablo1 <- xtable::xtable(final_results[[1]], caption = 'Birinci Grup')
print(tablo1, scalebox = 0.7)

tablo2 <- xtable::xtable(final_results[[2]], caption = 'İkinci Grup')
print(tablo2, scalebox = 0.7)

df.m <- melt(final_model, id.var = "Group")
df.m$Group <- as.character(df.m$Group)

ggplot(data = df.m, aes(x=variable, y=value)) +
  geom_boxplot(aes(fill = Group),outlier.size = 1) +
  facet_wrap( ~ variable, scales="free") +
  xlab(label = NULL) + ylab(label = NULL) + ggtitle("Boxplots for 2 Cell Type") +
  guides(fill=guide_legend(title="Groups"))

ggplot(data = df.m, aes(value, fill = Group)) +
  geom_density()+
  facet_wrap( ~ variable, scales="free") +
  xlab(label = NULL) + ylab(label = NULL) + ggtitle("Density for  for 2 Cell Type") +
  guides(fill=guide_legend(title="Groups"))















