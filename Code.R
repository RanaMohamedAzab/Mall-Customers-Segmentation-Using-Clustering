#Read Data
Customers = read.csv("Mall_Customers.csv")
str(Customers)
summary(Customers)

#Remove Duplicates
Customers = unique(Customers)

#Data Preparation
Customers$Gender = ifelse(Customers$Gender == "Female", 0, 1)

normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}
Customers$Age = normalize(Customers$Age)
Customers$Annual.Income..k.. = normalize(Customers$Annual.Income..k..)
Customers$Spending.Score..1.100. = normalize(Customers$Spending.Score..1.100.)

#k-means clustering
set.seed(1)
KMC1 = kmeans(Customers[2:5], centers = 1, iter.max = 1000)
KMC2 = kmeans(Customers[2:5], centers = 2, iter.max = 1000)
KMC3 = kmeans(Customers[2:5], centers = 3, iter.max = 1000)
KMC4 = kmeans(Customers[2:5], centers = 4, iter.max = 1000)
KMC5 = kmeans(Customers[2:5], centers = 5, iter.max = 1000)
KMC6 = kmeans(Customers[2:5], centers = 6, iter.max = 1000)
KMC7 = kmeans(Customers[2:5], centers = 7, iter.max = 1000)
KMC8 = kmeans(Customers[2:5], centers = 8, iter.max = 1000)
KMC9 = kmeans(Customers[2:5], centers = 9, iter.max = 1000)
KMC10 = kmeans(Customers[2:5], centers = 10, iter.max = 1000)

# plot
Num_of_Clusters = seq(1,10,1)
Total_Withiniss = c(KMC1$tot.withinss, KMC2$tot.withinss, KMC3$tot.withinss, KMC4$tot.withinss, KMC5$tot.withinss, KMC6$tot.withinss, KMC7$tot.withinss, KMC8$tot.withinss, KMC9$tot.withinss, KMC10$tot.withinss)
plot(Num_of_Clusters, Total_Withiniss, type="b")

# From the graph, 6 clusters are suitable where adding more clusters will not significantly improve the total withiniss
str(KMC6)
Customers_Original = read.csv("Mall_Customers.csv")
Customers_Original$k_means_Cluster = KMC6$cluster
tapply(Customers$Gender, KMC6$cluster, mean )
tapply(Customers_Original$Age, KMC6$cluster, mean )
tapply(Customers_Original$Annual.Income..k.., KMC6$cluster, mean)
tapply(Customers_Original$Spending.Score..1.100., KMC6$cluster, mean)

# Hierarchical Clusturing
distances = dist(Customers[2:5], method = "euclidean")

Customers_Clustering = hclust(distances, method = "ward.D")
plot(Customers_Clustering)

rect.hclust(Customers_Clustering, k = 6, border = "red")
CustomerClusters = cutree(Customers_Clustering, k = 6)
Customers_Original$Hierarchical_Cluster = CustomerClusters
table(Customers_Original$Hierarchical_Cluster)
tapply(Customers$Gender, Customers_Original$Hierarchical_Cluster, mean )
tapply(Customers_Original$Age, Customers_Original$Hierarchical_Cluster, mean)
tapply(Customers_Original$Annual.Income..k.., Customers_Original$Hierarchical_Cluster, mean)
tapply(Customers_Original$Spending.Score..1.100., Customers_Original$Hierarchical_Cluster, mean)
