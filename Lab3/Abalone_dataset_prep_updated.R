################################################
#### Evaluating Classification & CLustering ####
################################################

library("caret")
library(GGally)
library(psych)



## read data
abalone <- read.csv("./Lab3/abalone/abalone.data", header=FALSE)

## rename columns
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight', 'rings' ) 

## derive age group based in number of rings
abalone$age.group <- cut(abalone$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))

## take copy removing sex and rings
abalone.sub <- abalone[,c(2:8,10)]

## convert class labels to strings
abalone.sub$age.group <- as.character(abalone.sub$age.group)

## convert back to factor
abalone.sub$age.group <- as.factor(abalone.sub$age.group)

## split train/test
train.indexes <- sample(4177,0.7*4177)

train <- abalone.sub[train.indexes,]
test <- abalone.sub[-train.indexes,]

## separate x (features) & y (class labels)
X <- train[,1:7] 
Y <- train[,8]

## features subset
# train <- train[,5:8]
# test <- test[,5:8]
t1 <- train[,5:7]
te1 <- test[,5:7]

t2 <- train[,1:4]
te2 <- test[,1:4]

## feature boxplots
boxplot(X, main="abalone features")

## class label distributions
plot(Y)


## feature-class plots
featurePlot(x=X, y=Y, plot="ellipse")

featurePlot(x=X, y=Y, plot="box")

scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=X, y=Y, plot="density", scales=scales)

## psych scatterplot matrix
pairs.panels(X,gap = 0,bg = c("pink", "green", "blue")[Y],pch=21)

## GGally 
ggpairs(train, ggplot2::aes(colour = Y))



## EOF ##
preProc1 <- preProcess(t1, method=c("center","scale"))
t1 <- predict(preProc1, t1)
te1 <- predict(preProc1, te1)

preProc2 <- preProcess(t2, method=c("center","scale"))
t2 <- predict(preProc2, t2)
te2 <- predict(preProc2, te2)

knn1 <- knn(
  t1,
  te1,
  Y,
  k = 17,
)

table(Predicted = knn1, Actual = test$age.group)

knn2 <- knn(
  t2,
  te2,
  Y,
  k = 10,
)

table(Predicted = knn2, Actual = test$age.group)

correct1 <- knn1 == test$age.group
correct2 <- knn2 == test$age.group

comparison_table <- table(correct1, correct2)
comparison_table

mcnemar.test(comparison_table)


# Optimal K
#----------------------------------------------------


train_features <- t1
test_features  <- te1


train_labels <- Y
test_labels  <- test$age.group


k_values <- seq(1, 100, by = 1)   # k from 1-100
accuracy <- numeric(length(k_values))


for (i in seq_along(k_values)) {
  k <- k_values[i]
  
  
  pred <- knn(train = train_features,
              test  = test_features,
              cl    = train_labels,
              k     = k)
  
  
  accuracy[i] <- mean(pred == test_labels)
}


best_index <- which.max(accuracy)
best_k <- k_values[best_index]
best_accuracy <- accuracy[best_index]

cat("Optimal k:", best_k, "\n")
cat("Accuracy at optimal k:", round(best_accuracy, 4), "\n")


plot(k_values, accuracy, type = "b", pch = 19, col = "blue",
     xlab = "k", ylab = "Accuracy",
     main = "kNN Accuracy vs k (Better Performing Model)")
abline(v = best_k, col = "red", lty = 2)


# Exercise 2
#---------------------------------------------------------------

features <- t1


features_scaled <- scale(features)


find_optimal_k <- function(data, method = c("kmeans","pam"), k_min=2, k_max=10) {
  method <- match.arg(method)
  sil_width <- numeric(k_max - k_min + 1)
  
  for (k in k_min:k_max) {
    if (method == "kmeans") {
      km <- kmeans(data, centers = k, nstart = 25)
      ss <- silhouette(km$cluster, dist(data))
    } else if (method == "pam") {
      pam_model <- pam(data, k = k)
      ss <- silhouette(pam_model$clustering, dist(data))
    }
    sil_width[k - k_min + 1] <- mean(ss[,3])  # Average silhouette width
  }
  
  optimal_k <- which.max(sil_width) + k_min - 1
  return(list(optimal_k = optimal_k, silhouette_widths = sil_width))
}

# -------------------- Find optimal k -----
kmeans_opt <- find_optimal_k(features_scaled, method="kmeans", k_min=2, k_max=10)
cat("Optimal k for k-means:", kmeans_opt$optimal_k, "\n")

pam_opt <- find_optimal_k(features_scaled, method="pam", k_min=2, k_max=10)
cat("Optimal k for PAM:", pam_opt$optimal_k, "\n")

km_final <- kmeans(features_scaled, centers = kmeans_opt$optimal_k, nstart = 25)
pam_final <- pam(features_scaled, k = pam_opt$optimal_k)


sil_km  <- silhouette(km_final$cluster, dist(features_scaled))
sil_pam <- silhouette(pam_final$clustering, dist(features_scaled))

plot_silhouette <- function(sil_obj, main_title = "Silhouette Plot") {
  n <- nrow(sil_obj)
  sil_height <- rep(1, n)
  sil_order <- order(sil_obj[,1], decreasing = FALSE)
  sil_sorted <- sil_obj[sil_order,]
  
  plot(0, type="n", xlim=c(-1,1), ylim=c(0, n),
       xlab="Silhouette Width", ylab="Observation", main=main_title)
  for (i in 1:n) {
    rect(0, i-1, sil_sorted[i,3], i, col=ifelse(sil_sorted[i,3] > 0, "skyblue", "pink"), border=NA)
  }
  abline(v=mean(sil_sorted[,3]), col="red", lty=2)  # average silhouette
}

plot_silhouette(sil_km, main_title = paste("k-means Silhouette, k =", kmeans_opt$optimal_k))
plot_silhouette(sil_pam, main_title = paste("PAM Silhouette, k =", pam_opt$optimal_k))

