data <- read.csv("data.csv")

data <- data[,c(1, 5, 7, 9)]
data <- data[data$indicator=="deaths",]
data <- data[data$country!="EU/EEA (total)",]

data[data$cumulative_count>=1 & !is.na(data$cumulative_count),]$cumulative_count = 
  log(data[data$cumulative_count>=1 & !is.na(data$cumulative_count),]$cumulative_count)

matrice_data <- matrix(data$cumulative_count, byrow=TRUE, ncol=176)

# On clean le data
matrice_data <- matrice_data[,-c(1:10)]
matrice_data <- matrice_data[,-c(122:166)]
matrice_data[14, 97] <- 3.7135721

# Potential issues 
# NA valeurs
# Tous les pays n'ont pas la même population on ne peut ainsi pas s'attendre à des valeurs du même ordre

data_wNA <- data[!is.na(data$cumulative_count),]
plot(data_wNA$year_week, data_wNA$cumulative_count,type="l", ylim=range(data_wNA$cumulative_count))

# Liste des pays que j'ai 
xeval <- seq(1, 121 ,by=1)
breaks <- seq(1, 121, by=5)
B <- bsplineS(xeval, norder=6, breaks) 
value <- B %*% data$cumulative_count


liste_pays <- unique(data$country)
max <- max(data$cumulative_count[!is.na(data$cumulative_count)])
data_country <- data[data$country=="Austria",]
plot(1:176, colMeans(matrice),type="l", ylim=c(0, max))
for (country in liste_pays){
  data_country <- data[data$country==country,]
  points(1:176, data_country$cumulative_count,type="l")
}

# Changer la forme des données en faisant une matrice avec comme rows les différents pays 
# et comme colonne les times 


for (country in 1:30){print(matrice[country,]==data[data$country==liste_pays[i],]$cumulative_count)}

# Faisons maintenant un PCA

perform_pca <- function(X){
  mu <- colMeans(X)
  X <- sweep(X,2,mu)
  
  SVD <- svd(X)
  Scores <- SVD$u %*% diag(SVD$d)
  Loadings <- SVD$v
  FVE <- SVD$d^2/sum(SVD$d^2)
  
  lam <- sqrt(length(xeval)) # measure change
  op <- par(mfrow=c(3,2),mar=rep(2,4))
  plot(xeval, X[1,]+mu,type="l", ylim=range(X+mu), main="Data and the mean")
  for(n in 1:dim(X)[1]) points(xeval, X[n,]+mu,type="l")
  points(xeval,mu,col=2,lwd=2,type="l")
  plot(Scores[1,]*sign(sum(Loadings[,1])), Scores[2,]*sign(sum(Loadings[,2])), main="1st vs 2nd PC scores")
  
  plot(xeval,Loadings[,1]*sign(sum(Loadings[,1])),type="l", main=paste0("1st PC (",round(100*FVE[1])," % of var)"))
  plot(xeval,Loadings[,2]*sign(sum(Loadings[,2])),type="l", main=paste0("2nd PC (",round(100*FVE[2])," % of var)"))
  plot(xeval,Loadings[,3]*sign(sum(Loadings[,3])),type="l", main=paste0("3rd PC (",round(100*FVE[3])," % of var)"))
  plot(xeval,Loadings[,4]*sign(sum(Loadings[,4])),type="l", main=paste0("4th PC (",round(100*FVE[4])," % of var)"))
}

# Régler le pb des na 
nbr_na <- c()
for (i in 1:166){
  nbr_na <- c(nbr_na, sum(is.na(matrice_data[,i])))
}

plot(1:166, nbr_na, type="l")

# On va commencer 10 semaines plus tard et finir 2 semaines plus tot 




