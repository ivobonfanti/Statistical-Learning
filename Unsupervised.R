setwd("C:XXXXXX")

library(tidyverse)
library(recipes)
library(gridExtra)
library(tidytext)
library(plotly)
library(tclust)

#There are 9 features for each of the 167 countries. There are
#no missing values and a variable is renamed.
country_data<-read.csv("Unsupervised/Country-data.csv")
country_data %>% glimpse() 
colSums(is.na(country_data)) 
country_data1<-country_data %>% rename(gdp_gr.rate=inflation) 
summary(country_data1)

#1.EXPLORATIVE DATA ANALYSIS (box-plots)

p<-ggplot(country_data1, aes(x=child_mort)) + geom_boxplot(fill="grey")

p1<-ggplot(country_data1, aes(x=exports)) + geom_boxplot(fill="purple")

p2<-ggplot(country_data1, aes(x=health)) + geom_boxplot(fill="pink")

p3<-ggplot(country_data1, aes(x=imports)) + geom_boxplot(fill="brown")

p4<-ggplot(country_data1, aes(x=income)) + geom_boxplot(fill="green")

p5<-ggplot(country_data1, aes(x=gdp_gr.rate)) + geom_boxplot(fill="red")

p6<-ggplot(country_data1, aes(x=life_expec)) + geom_boxplot(fill="blue")

p7<-ggplot(country_data1, aes(x=total_fer)) + geom_boxplot(fill="turquoise")

p8<-ggplot(country_data1, aes(x=gdpp)) + geom_boxplot(fill="yellow")

grid.arrange(p, p1, p2, p3, p4, p5, p6, p7, p8, ncol=3) #boxplots variables

#2-PCA

#Within the recipe the features are scaled (standardized)
pca_rec <- recipe(~., data = country_data1) %>%
  update_role(country, new_role = "id") %>% 
  step_normalize(all_predictors()) %>% 
  step_pca(all_predictors())

pca_prep <- prep(pca_rec)

#loads and total energy
summary(pca_prep$steps[[2]]$res) 

tidied_pca <- tidy(pca_prep, 2)

#display first five components loads values
tidied_pca %>% 
  filter(component %in% paste0("PC", 1:5)) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL)

#display again loads highlighting thweir sign
tidied_pca %>%
  filter(component %in% paste0("PC", 1:4)) %>%
  group_by(component) %>%
  top_n(9, abs(value)) %>%
  ungroup() %>%
  mutate(terms = reorder_within(terms, abs(value), component)) %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  scale_y_reordered() +
  labs(
    x = "Absolute value of contribution",
    y = NULL, fill = "Positive?"
  )

#countries projections along the first two components
juice(pca_prep) %>%
  ggplot(aes(PC1, PC2, label = country)) +
  geom_point()+
  geom_text(check_overlap = TRUE) +
  labs(color = NULL)

#3-K-MEANS CLUSTERING

#Tidyverse recipes do not support yet k-means. 
#The following is a function to standardize values
stz.function<-function(vrb){ 
    vrb=(vrb-mean(vrb))/sd(vrb)
}

#new standardized data set
country_data2 <-country_data1 %>%  
                  mutate_if(is.numeric, ~ stz.function(.))

#parameter tuning
set.seed(888)
kclusts <-
  tibble(k = 1:9) %>%
  mutate(
    kclust = map(k, ~ kmeans(select(country_data2, -country), .x, iter.max=30)),
    glanced = map(kclust, glance),
  )

#4 clusters seem sufficient
kclusts %>% 
  unnest(cols = c(glanced)) %>%
  ggplot(aes(k, tot.withinss)) +
  geom_line(alpha = 0.5, size = 1.2, color = "midnightblue") +
  geom_point(size = 2, color = "midnightblue")

#apply k-means with 4 clusters
set.seed(888)
country_clust<-select(country_data2, -country) %>% 
              kmeans(centers=4, iter.max=30)

summary(country_clust)
tidy(country_clust)

#three-dimensional representation
plot_ly(x=country_data2$life_expec, y=country_data2$gdpp,
        z=country_data2$health, type="scatter3d", 
        text = ~paste('country:', country_data2$country), mode='markers', 
        color=factor(country_clust$cluster)) %>% 
layout(scene=list(xaxis=list(title='x-life expectancy'), 
                  yaxis=list(title='y-gdp per capita'), 
                  zaxis=list(title='z-health expenditure')))

#4-TRIMMED K-MEANS
country_data3<-as.matrix(country_data2[,c(4,10)])

#trimmed k-means. The centroids are initialized 20 times.
country_trimmed.clust<-tkmeans(country_data3, k=4, alpha=0.06,
                               nstart=20, iter.max=30)

summary(country_trimmed.clust)

#k-means two dimensional plot, health against gdpp, 4 clusters
plot.tkmeans(country_trimmed.clust, text=country_data2[,1])

#k-means two dimensional plot, health against gdpp, 4 clusters
augment(country_clust, country_data2) %>% 
  ggplot(aes(health, gdpp, color = .cluster, label=country)) +
  geom_point()+
  geom_text(check_overlap=TRUE)

