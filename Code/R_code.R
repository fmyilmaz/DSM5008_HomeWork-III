
##----------------------------------------------------------------
##                       Loading Packages                       --
##----------------------------------------------------------------
library(tidyverse) # Easily Install and Load the 'Tidyverse', CRAN v1.3.0
library(GGally) # Extension to 'ggplot2', CRAN v2.0.0
library(knitr) # A General-Purpose Package for Dynamic Report Generation in R, CRAN v1.28
library(ggcorrplot) # Visualization of a Correlation Matrix using 'ggplot2', CRAN v0.1.3
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







breast_cancer_scaled <- scale(breast_cancer[,-1])
breast_cancer_scaled <- data.frame(diagnosis = breast_cancer$diagnosis, breast_cancer_scaled)












