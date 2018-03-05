#######################
# Factorial Analysis
# NJ and UT
#######################

UserName = "Nico"
setwd(paste("/Users/", UserName, "/Dropbox/DataAnalysisProject/Output", sep = ""))

# Import data
data <- read.csv("data.csv")
drops <- c("X")
data <- data[, !(names(data) %in% drops)]
rownames(data) <- data$State
NameStates <- as.data.frame(data$State)
data$State <- NULL
data <- data[which(data$SwingState==TRUE),]

#Restrict to active/projected variables
data.active <- data[,c("X18.to.29.years",
                       "X30.to.44.years",
                       "X45.to.64.years",
                       "X65.years.and.over",
                       "White.alone",
                       "Black.or.African.American.alone",
                       "Hispanic.or.Latino",
                       "Bachelor.s.Degree.or.Higher",
                       "Below.Poverty.Level..Poverty.Rate.",
                       "Households.with.income..100.000.or.more",
                       "Adult.Obesity.Rate.2015",
                       "Gun.Ownership",
                       "Unemployment.rate")]

data.proj <- data[, c("Trump.pct.2016","Clinton.pct.2016",#"Others.pct.2016",
                      "Trump.error",
                      "Obama.pct.2012", "Romney.pct.2012"#,"Others.pct.2012"
                      )]

#Scale the matrix
data.scaled.indv <- scale(data.active, center = TRUE, scale = TRUE)
data.scaled.var <- 1/sqrt(length(data.active[,1]))*scale(data.active, center = TRUE, scale = TRUE)

#We create the correlation matrix
data.correl.indv <- (1/length(data.active[,1]))*t(data.scaled.indv)%*%data.scaled.indv
data.correl.var <- (1/length(data.active[1,]))*(data.scaled.var)%*%t(data.scaled.var)

#Eigen-values and vectors
data.eig.indv <- eigen(data.correl.indv)
eigenvalues.indv <- as.data.frame(data.eig.indv$values / sum(data.eig.indv$values))
plot(eigenvalues.indv)
eigenvectors.indv <- as.data.frame(data.eig.indv$vectors)

#NEW
numberDimension <- length(which(eigenvalues.indv>10^-10))
eigenvectors.indv <- eigenvectors.indv[,c(1:numberDimension)]

data.eig.var <- eigen(data.correl.var)
eigenvalues.var <- data.eig.var$values / sum(data.eig.var$values)
plot(eigenvalues.var)
eigenvectors.var <- as.data.frame(data.eig.var$vectors)


# New dataset:
c.alpha.indv <- as.matrix(data.scaled.indv) %*% as.matrix(eigenvectors.indv) #matrice Mnp-1

c.alpha.var <- as.matrix(t(data.scaled.var)) %*% as.matrix(eigenvectors.var) #matrice Mpn
#c.alpha.proj <- as.matrix(t(data.proj.scaled)) %*% as.matrix(eigenvectors.varReduite)

d.alpha <-matrix( nrow=length(data.active[1,]), ncol=numberDimension)
for (i in 1:length(data.active[1,])){
  for (j in 1:numberDimension){
    d.alpha[i,j] <- cor(data.active[,i],c.alpha.indv[,j])
  }
}

d.alpha.proj <- matrix(nrow=length(data.proj[1,]), ncol=numberDimension)
for (i in 1:length(data.proj[1,])){
  for (j in 1:numberDimension){
    d.alpha.proj[i,j] <- cor(data.proj[,i],c.alpha.indv[,j])
  }
}

covariance.c.alpha <- cor(c.alpha.indv)

a <- 4

rownames(d.alpha) <- colnames(data.active)
d.alpha <- a*d.alpha
rownames(d.alpha.proj) <- colnames(data.proj)
d.alpha.proj <- a*d.alpha.proj


#################
# Plot
##################

color <- as.data.frame(c(as.character(data$State.Winner), 
                         rep("Var", length(data.active[1,])),
                         rep("Proj", length(data.proj[1,]))
                         ))

for (i in 1:3){
  plot(rbind(c.alpha.indv[,c(2*i-1,2*i)],d.alpha[,c(2*i-1,2*i)],d.alpha.proj[,c(2*i-1,2*i)]), 
       col = ifelse(color == "T", "red", 
                    ifelse(color == "C", "blue", 
                           ifelse(color == "Var", "black", "green")
                    ))) 
  text(rbind(c.alpha.indv[,c(2*i-1,2*i)], 
             d.alpha[,c(2*i-1,2*i)],
             d.alpha.proj[,c(2*i-1,2*i)]), 
       labels=row.names(rbind(c.alpha.indv[,c(2*i-1,2*i)],
                              d.alpha[,c(2*i-1,2*i)],
                              d.alpha.proj[,c(2*i-1,2*i)])), 
       cex= 0.7, pos = 3,
       col = ifelse(color == "T", "red", 
                    ifelse(color == "C", "blue", 
                           ifelse(color == "Var", "black", "green")
                           
                    )))
  segments(0,0, d.alpha[,2*i-1], d.alpha[,2*i])
  segments(0,0, d.alpha.proj[,2*i-1], d.alpha.proj[,2*i],col="green") 
}

for (i in 1:2){
  plot(rbind(c.alpha.indv[,c(2*i,2*i+1)],d.alpha[,c(2*i,2*i+1)],d.alpha.proj[,c(2*i,2*i+1)]), 
       col = ifelse(color == "T", "red", 
                    ifelse(color == "C", "blue", 
                           ifelse(color == "Var", "black", "green")
                    ))) 
  text(rbind(c.alpha.indv[,c(2*i,2*i+1)], 
             d.alpha[,c(2*i,2*i+1)],
             d.alpha.proj[,c(2*i,2*i+1)]), 
       labels=row.names(rbind(c.alpha.indv[,c(2*i,2*i+1)],
                              d.alpha[,c(2*i,2*i+1)],
                              d.alpha.proj[,c(2*i,2*i+1)])), 
       cex= 0.7, pos = 3,
       col = ifelse(color == "T", "red", 
                    ifelse(color == "C", "blue", 
                           ifelse(color == "Var", "black", "green")
                           
                    )))
  segments(0,0, d.alpha[,2*i], d.alpha[,2*i+1])
  segments(0,0, d.alpha.proj[,2*i], d.alpha.proj[,2*i+1],col="green") 
}

#########################
# CO2 Matrix
#########################

CO2.indv <- matrix(nrow=length(data.active[,1]), 
                   ncol=numberDimension)
for (i in 1:length(data.active[,1])){
  for (j in 1:numberDimension){
    CO2.indv[i,j] <- (c.alpha.indv[i,j])^2/sum(data.scaled.indv[i,]^2)
  }
}
rownames(CO2.indv) <- NameStates$`data$State`


# CO2.var

CO2.var <- matrix(nrow=length(data.active[1,]), 
                   ncol=numberDimension)
for (i in 1:length(data.active[1,])){
  for (j in 1:numberDimension){
    CO2.var[i,j] <- (d.alpha[i,j]*d.alpha[i,j])/(a^2)
  }
}
rownames(CO2.var) <- colnames(data.active)


CO2.var.proj <- matrix(nrow=length(data.proj[1,]), 
                  ncol=numberDimension)
for (i in 1:length(data.proj[1,])){
  for (j in 1:numberDimension){
    CO2.var.proj[i,j] <- (d.alpha.proj[i,j]*d.alpha.proj[i,j])/(a^2)
  }
}
rownames(CO2.var.proj) <- colnames(data.proj)

TrumpError.explained.2 <- as.data.frame(t(d.alpha.proj[3,]/4))


##############################
#Correlation with Trump Error#
##############################
#Simple correlation
TrumpError.correl <- rep(0,length(data.active[1,]))
for (i in 1:length(data.active[1,])){
  TrumpError.correl[i] <- cor(data.active[,i],data.proj$Trump.pct.2016)^2
}
TrumpError.correl <- as.data.frame(t(TrumpError.correl))
colnames(TrumpError.correl) <- (colnames(data.active))

#Simple regression
TrumpError.explained <- rep(0,length(data.active[1,]))
for (i in 1:length(data.active[1,])){
  TrumpError.explained[i] <- cor(data.active[,i],data.proj$Trump.error)^2
}
TrumpError.explained <- as.data.frame(t(TrumpError.explained))
colnames(TrumpError.explained) <- (colnames(data.active))

# Multiple Linear Regression Example 
fit2 <- lm(data.proj[,1] ~ data.active[,1] + data.active[,2] + data.active[,3] + data.active[,4] 
           + data.active[,5] + data.active[,6] + data.active[,7] + data.active[,8]+
           data.active[,9] + data.active[,10] + data.active[,11] + data.active[,12] + data.active[,13] )
summary(fit2) # show results




#########################
#Contribution Matrix
#########################

contrib.var <- matrix(nrow=length(data.active[1,]), 
                           ncol=numberDimension)
for (i in 1:length(data.active[1,])){
  for (j in 1:numberDimension){
    contrib.var[i,j] <- (d.alpha[i,j]*d.alpha[i,j])/sum(d.alpha[,j]*d.alpha[,j])
  }
}

contrib.indv <- matrix(nrow=length(data.active[,1]),ncol=numberDimension)
for (i in 1:length(data.active[,1])){
  for (j in 1:numberDimension){
   contrib.indv[i,j] <- (c.alpha.indv[i,j]*c.alpha.indv[i,j])/sum(c.alpha.indv[,j]*c.alpha.indv[,j])
  }
}



#PCA graphs
#library(FactoMineR)
#res.pca <- PCA((data.scaled.indv), scale.unit=TRUE, ncp=5, graph=T)

#c.alpha.1.2 <- c.alpha.indv[,c(1,2)]
#d.alpha.1.2 <- d.alpha[,c(1,2)]

#for (i in 1:15){
#  for (j in 1:2){
#    d.alpha.1.2[i,j] <- d.alpha.1.2[i,j]/sqrt(sum(d.alpha.1.2[i,]*d.alpha.1.2[i,]))
#  }
#}
#plot(rbind(c.alpha.1.2, c.alpha.proj.1.2))
#text(rbind(c.alpha.1.2, c.alpha.proj.1.2)[,1], 
#     rbind(c.alpha.1.2, c.alpha.proj.1.2)[,2], 
#     labels=row.names(rbind(c.alpha.1.2, c.alpha.proj.1.2)), 
#     cex= 0.7, pos = 3)

######################################
######################################


# Matrix ofcontributions
#contrib.matrix <- sweep(as.matrix(c.alpha)*as.matrix(c.alpha), 
#                        2, 
#                        colSums(as.matrix(c.alpha)*as.matrix(c.alpha)))


# Analysis Axe 1
#coord.1 <- c.alpha[,1]
#contrib.1 <- coord.1*coord.1 / sum((t(coord.1)) %*% (coord.1))
#plot(contrib.1)





# PCA Graphs
#install.packages("FactoMineR")
#library(FactoMineR)
#pca <- PCA(t(data.scaled), scale.unit=TRUE, ncp=5, graph=T)



# Analysis of error function of results
#attach(data)
#plot(Trump.pct, Trump.error, main = "Title", xlab="pct ", ylab="error", pch=19)
#text(Trump.pct, Trump.error, labels=State, cex= 0.7)






