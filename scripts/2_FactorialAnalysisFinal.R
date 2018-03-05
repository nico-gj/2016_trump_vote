#######################
# Factorial Analysis
# NJ and UT
#######################

UserName = "nj995"
setwd(paste("/Users/", UserName, "/Dropbox/DataPiche/USElections2016/Output", sep = ""))

# Import data
data <- read.csv("data.csv")
data$X <- NULL
rownames(data) <- data$State
data$State <- NULL

#Exclude DC
data <- data[which(rownames(data) != "DistrictofColumbia"),]

#Restrict to Swing States
data <- data[which(data$SwingState==TRUE),]

#Restrict to active/projected variables
data.active <- data[,c("Age.18_29",
                       "Age.30_44",
                       "Age.45_64",
                       "Age.Over65",
                       "White",
                       "AfricanAmerican",
                       "Latino",
                       "CollegeEducation",
                       "PovertyRate",
                       "HouseholdIncomeOver100k",
                       "ObesityRate",
                       "GunOwnership",
                       "UnemploymentRate")]

data.proj <- data[, c("Trump.2016","Clinton.2016",
                      "Trump.PollError.2016",
                      "Obama.2012", "Romney.2012"
                      )]

scale_factor <- 4
out_factor <- 1000

##########################################################################
# All State Analysis 
##########################################################################

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

numberDimension <- length(which(eigenvalues.indv>10^-10))
axes <- matrix(nrow=numberDimension, ncol=1)
for (i in 1:numberDimension){
    axes[i] <- paste("PCA Axis", i)
}

origines <- matrix(nrow=length(data.proj), ncol=1)
for (i in 1:length(data.proj)){
  origines[i] <- paste("Origine", i)
}


eigenvectors.indv <- eigenvectors.indv[,c(1:numberDimension)]
data.eig.var <- eigen(data.correl.var)
eigenvalues.var <- data.eig.var$values / sum(data.eig.var$values)
plot(eigenvalues.var)
eigenvectors.var <- as.data.frame(data.eig.var$vectors)

# New dataset:
c.alpha.indv <- as.matrix(data.scaled.indv) %*% as.matrix(eigenvectors.indv) #matrice Mnp-1
colnames(c.alpha.indv) <- axes

d.alpha <-matrix( nrow=length(data.active[1,]), ncol=numberDimension)
for (i in 1:length(data.active[1,])){
  for (j in 1:numberDimension){
    d.alpha[i,j] <- cor(data.active[,i],c.alpha.indv[,j])
  }
}
rownames(d.alpha) <- colnames(data.active)
colnames(d.alpha) <- axes
d.alpha <- scale_factor*d.alpha

d.alpha.proj <- matrix(nrow=length(data.proj[1,]), ncol=numberDimension)
for (i in 1:length(data.proj[1,])){
  for (j in 1:numberDimension){
    d.alpha.proj[i,j] <- scale_factor*cor(data.proj[,i],c.alpha.indv[,j])
  }
}
rownames(d.alpha.proj) <- colnames(data.proj)
colnames(d.alpha.proj) <- axes


### Correlation lines ###
zeros <- matrix(nrow=length(data.proj[1,]), ncol=numberDimension)
for (i in 1:length(data.proj[1,])){
  for (j in 1:numberDimension){
    zeros[i,j] <- 0
  }
}
rownames(zeros) <- origines
colnames(zeros) <- axes

d.alpha.proj.out <- matrix(nrow=length(data.proj[1,]), ncol=numberDimension)
for (i in 1:length(data.proj[1,])){
  for (j in 1:numberDimension){
    d.alpha.proj.out[i,j] <- out_factor*cor(data.proj[,i],c.alpha.indv[,j])
  }
}
rownames(d.alpha.proj.out) <- paste(colnames(data.proj), "out", sep = ".")
colnames(d.alpha.proj.out) <- axes

correl.lines <- rbind(zeros, out_factor*d.alpha.proj.out)


##############################################################
# Contribution Matrix 
# ??? 
# How much did each State/Variable contribute to the axis?
##############################################################

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




####################################################
# CO2 Matrix
# ???
# How much does each axis explain a variable/State?
####################################################

CO2.indv <- matrix(nrow=length(data.active[,1]), 
                   ncol=numberDimension)
for (i in 1:length(data.active[,1])){
  for (j in 1:numberDimension){
    CO2.indv[i,j] <- (c.alpha.indv[i,j])^2/sum(data.scaled.indv[i,]^2)
  }
}
rownames(CO2.indv) <- rownames(data)

CO2.var <- matrix(nrow=length(data.active[1,]), 
                  ncol=numberDimension)
for (i in 1:length(data.active[1,])){
  for (j in 1:numberDimension){
    CO2.var[i,j] <- (d.alpha[i,j]*d.alpha[i,j])/(scale_factor^2)
  }
}
rownames(CO2.var) <- colnames(data.active)

CO2.var.proj <- matrix(nrow=length(data.proj[1,]), 
                       ncol=numberDimension)
for (i in 1:length(data.proj[1,])){
  for (j in 1:numberDimension){
    CO2.var.proj[i,j] <- (d.alpha.proj[i,j]*d.alpha.proj[i,j])/(scale_factor^2)
  }
}
rownames(CO2.var.proj) <- colnames(data.proj)

########################################
# Export
########################################

library(xlsx)
write.xlsx(c.alpha.indv, paste(getwd(), "/c_alpha_indv.xlsx", sep = ""))
write.xlsx(d.alpha, paste(getwd(), "/d_alpha.xlsx", sep = ""))
write.xlsx(d.alpha.proj, paste(getwd(), "/d_alpha_proj.xlsx", sep = ""))
write.xlsx(rbind(c.alpha.indv, d.alpha, d.alpha.proj, correl.lines), 
           paste(getwd(), "/all_ss.xlsx", sep = ""))









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
fit2 <- lm(data.proj[,1] ~ data.active[,1] + data.active[,2] + data.active[,3] 
           + data.active[,4] 
           + data.active[,5] + data.active[,6] + data.active[,7] + data.active[,8]+
           data.active[,9] + data.active[,10] + data.active[,11] + data.active[,12] 
           + data.active[,13] )
summary(fit2) # show results








#################
# Plot
##################

install.packages("devtools")  # so we can install from github
library("devtools")
install_github("ropensci/plotly")  # plotly is part of ropensci
library(plotly)
library(ggplot2)

states <- as.data.frame(c.alpha.indv[,c(1,2, 3)])
colnames(states) <- c('PCA_1', 'PCA_2', 'PCA_3')

vars <- as.data.frame(d.alpha[,c(1,2, 3)])/5
colnames(vars) <- c('PCA_1', 'PCA_2', 'PCA_3')

proj <- as.data.frame(d.alpha.proj[,c(1,2,3)])
colnames(proj) <- c('PCA_1', 'PCA_2', 'PCA_3')

test = rbind(states, vars, proj)

color <- as.data.frame(c(as.character(data$StateWinner.2016),
                         rep("Var", length(data.active[1,])),
                         rep("Proj", length(data.proj[1,]))
))

install.packages('ggrepel')
library(ggrepel)

ggplot(test, aes(PCA_1, PCA_2)) +
  geom_point(color=ifelse(color == "T", "red",
                          ifelse(color == "C", "blue",
                                 ifelse(color == "Var", "black", "green")
                          ))) +
  geom_text_repel(aes(PCA_1, PCA_2, label = row.names(test)))


p <- ggplot(test, aes(PCA_1, PCA_2))
p + geom_point(color=ifelse(color == "T", "red",
                           ifelse(color == "C", "blue",
                                  ifelse(color == "Var", "black", "green")
                                  )
                           )
              ) +
  geom_text(aes(label=row.names(test)),hjust=0, vjust=0)
  

  




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


