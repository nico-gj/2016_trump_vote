#######################
# Factorial Analysis
# NJ and UT
#######################

#install.packages('ggrepel')
#install.packages('devtools')  
#install_github("ropensci/plotly")
#install.packages('ggplot2')
#install.packages('gridExtra')
library(devtools)
library(plotly)
library(ggplot2)
library(ggrepel)
library(scales)
library(gridExtra)

UserName = "nj995"
setwd(paste("/Users/"
            , UserName
            , "/Dropbox/DataPiche/USElections2016/Output", sep = ""))

# Import data
data <- read.csv("data.csv")
data$X <- NULL
rownames(data) <- data$State
data$State <- NULL

#Exclude DC
data <- data[which(rownames(data) != "DistrictofColumbia"),]

#Restrict to Swing States
data.swing <- data[which(data$SwingState==TRUE),]


##########################################################################
# All State Analysis 
##########################################################################

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
colnames(data.active) <- c("Age.18_29"
                           ,"Age.30_44"
                           ,"Age.45_64"
                           ,"Age.65+"
                           ,"White"
                           ,"AfricanAmerican"
                           ,"Latino"
                           ,"CollegeEducation"
                           ,"Poverty"
                           ,"Income.100k+"
                           ,"Obesity"
                           ,"GunOwner"
                           ,"Unemployment")

data.proj <- data[, c("Trump.2016","Clinton.2016",
                      "Trump.PollError.2016",
                      "Obama.2012", "Romney.2012"
                      )]

scale_factor <- 2

#Scale the matrix
data.scaled.indv <- scale(data.active, center = TRUE, scale = TRUE)

#We create the correlation matrix
data.correl.indv <- (1/length(data.active[,1]))*t(data.scaled.indv)%*%data.scaled.indv

#Eigen-values and vectors
data.eig.indv <- eigen(data.correl.indv)
eigenvalues.indv <- as.data.frame(data.eig.indv$values / sum(data.eig.indv$values))
eigenvectors.indv <- as.data.frame(data.eig.indv$vectors)
#plot(eigenvalues.indv)

numberDimension <- length(which(eigenvalues.indv>10^-10))
axes <- matrix(nrow=numberDimension, ncol=1)
for (i in 1:numberDimension){
    axes[i] <- paste("PCA Axis", i)
}
eigenvectors.indv <- eigenvectors.indv[,c(1:numberDimension)]

# New dataset:
c.alpha.indv <- as.matrix(data.scaled.indv) %*% as.matrix(eigenvectors.indv)
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

##########################################################################
# Swing State Analysis 
##########################################################################

#Restrict to active/projected variables
data.active <- data.swing[,c("Age.18_29",
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
colnames(data.active) <- c("Age.18_29"
                           ,"Age.30_44"
                           ,"Age.45_64"
                           ,"Age.65+"
                           ,"White"
                           ,"AfricanAmerican"
                           ,"Latino"
                           ,"CollegeEducation"
                           ,"Poverty"
                           ,"Income.100k+"
                           ,"Obesity"
                           ,"GunOwner"
                           ,"Unemployment")


data.proj <- data.swing[, c("Trump.2016","Clinton.2016",
                      "Trump.PollError.2016",
                      "Obama.2012", "Romney.2012"
)]

scale_factor <- 2

#Scale the matrix
data.scaled.indv <- scale(data.active, center = TRUE, scale = TRUE)

#We create the correlation matrix
data.correl.indv <- (1/length(data.active[,1]))*t(data.scaled.indv)%*%data.scaled.indv

#Eigen-values and vectors
data.eig.indv <- eigen(data.correl.indv)
eigenvalues.indv <- as.data.frame(data.eig.indv$values / sum(data.eig.indv$values))
eigenvectors.indv <- as.data.frame(data.eig.indv$vectors)
#plot(eigenvalues.indv)

numberDimension <- length(which(eigenvalues.indv>10^-10))
axes <- matrix(nrow=numberDimension, ncol=1)
for (i in 1:numberDimension){
  axes[i] <- paste("PCA Axis", i)
}
eigenvectors.indv <- eigenvectors.indv[,c(1:numberDimension)]

# New dataset:
c.alpha.indv.swing <- as.matrix(data.scaled.indv) %*% as.matrix(eigenvectors.indv)
colnames(c.alpha.indv.swing) <- axes

d.alpha.swing <-matrix( nrow=length(data.active[1,]), ncol=numberDimension)
for (i in 1:length(data.active[1,])){
  for (j in 1:numberDimension){
    d.alpha.swing[i,j] <- cor(data.active[,i],c.alpha.indv.swing[,j])
  }
}
rownames(d.alpha.swing) <- colnames(data.active)
colnames(d.alpha.swing) <- axes
d.alpha.swing <- scale_factor*d.alpha.swing

d.alpha.proj.swing <- matrix(nrow=length(data.proj[1,]), ncol=numberDimension)
for (i in 1:length(data.proj[1,])){
  for (j in 1:numberDimension){
    d.alpha.proj.swing[i,j] <- scale_factor*cor(data.proj[,i],c.alpha.indv.swing[,j])
  }
}
rownames(d.alpha.proj.swing) <- colnames(data.proj)
colnames(d.alpha.proj.swing) <- axes




#################
# Plot
##################

states <- as.data.frame(c.alpha.indv[,c(1,2, 3)])
colnames(states) <- c('PCA_1', 'PCA_2', 'PCA_3')

vars <- as.data.frame(d.alpha[,c(1,2, 3)])*2
colnames(vars) <- c('PCA_1', 'PCA_2', 'PCA_3')

proj <- as.data.frame(d.alpha.proj[,c(1,2,3)])
colnames(proj) <- c('PCA_1', 'PCA_2', 'PCA_3')


p1 = ggplot() +
  geom_point(data=states
             , aes(x=PCA_1, y=PCA_2)
             , color=ifelse(data$StateWinner.2016 == "T", "red", "blue")
             , size = 2) +  
  geom_point(data=vars
             , aes(x=PCA_1, y=PCA_2)
             , color="black"
             , shape = 4) +  
  geom_segment(data = vars, x = 0, y = 0, aes(xend=PCA_1, yend=PCA_2)) +
  geom_text_repel(data=vars
                  , aes(x=PCA_1, y=PCA_2
                        , label = row.names(vars)
                        , fontface = "bold")
                  , size = 3)
p1


p2 = ggplot() +
  geom_point(data=vars
             , aes(x=PCA_1, y=PCA_2)
             , color="dark grey"
             , shape = 4) +  
  geom_segment(data = vars
               , x = 0, y = 0
               , aes(xend=PCA_1, yend=PCA_2)
               , color = "dark grey") +
  geom_text_repel(data=vars
                  , aes(x=PCA_1, y=PCA_2, label = row.names(vars))
                  , size = 3
                  , color = "dark grey") +
  geom_point(data=states
             , aes(x=PCA_1, y=PCA_2)
             , color=ifelse(data$StateWinner.2016 == "T", "red", "blue")
             , size = 2) +  
  geom_text_repel(data=states
                  , aes(x=PCA_1, y=PCA_2
                        , label = row.names(states)
                        #, fontface = "bold"
                        )
                  , color = ifelse(data$StateWinner.2016 == "T", "red", "blue")
                  , size = 3)
p2


states.swing <- as.data.frame(c.alpha.indv.swing[,c(1,2, 3)])
colnames(states.swing) <- c('PCA_1', 'PCA_2', 'PCA_3')

vars.swing <- as.data.frame(d.alpha.swing[,c(1,2, 3)])*1.5
colnames(vars.swing) <- c('PCA_1', 'PCA_2', 'PCA_3')

proj.swing <- as.data.frame(d.alpha.proj.swing[,c(1,2,3)])
colnames(proj.swing) <- c('PCA_1', 'PCA_2', 'PCA_3')


p3 = ggplot() +
  geom_point(data=states.swing
             , aes(x=PCA_1, y=PCA_2)
             , color=ifelse(data.swing$StateWinner.2016 == "T", "red", "blue")
             , size = 2) +  
  geom_point(data=vars.swing
             , aes(x=PCA_1, y=PCA_2)
             , color="black"
             , shape = 4) +  
  geom_segment(data = vars.swing, x = 0, y = 0, aes(xend=PCA_1, yend=PCA_2)) +
  geom_text_repel(data=vars.swing
                  , aes(x=PCA_1, y=PCA_2
                        , label = row.names(vars.swing)
                        , fontface = "bold")
                  , size = 3)
p3


p4 = ggplot() +
  geom_point(data=vars.swing
             , aes(x=PCA_1, y=PCA_2)
             , color="dark grey"
             , shape = 4) +  
  geom_segment(data = vars.swing
               , x = 0, y = 0
               , aes(xend=PCA_1, yend=PCA_2)
               , color = "dark grey") +
  geom_text_repel(data=vars.swing
                  , aes(x=PCA_1, y=PCA_2, label = row.names(vars.swing))
                  , size = 3
                  , color = "dark grey") +
  geom_point(data=states.swing
             , aes(x=PCA_1, y=PCA_2)
             , color=ifelse(data.swing$StateWinner.2016 == "T", "red", "blue")
             , size = 2) +  
  geom_text_repel(data=states.swing
                  , aes(x=PCA_1, y=PCA_2
                        , label = row.names(states.swing)
                        #, fontface = "bold"
                        )
                  , color = ifelse(data.swing$StateWinner.2016 == "T", "red", "blue")
                  , size = 3)
p4


states.swing = cbind(states.swing, data.swing)

p5 = ggplot() +
  geom_point(data=states.swing
             , aes(x=PCA_2, y=PCA_3, color = Trump.PollError.2016)
             , size = 2) +
  geom_point(data=vars.swing
             , aes(x=PCA_2, y=PCA_3)
             , color="black"
             , shape = 4) +  
  geom_segment(data = vars.swing, x = 0, y = 0, aes(xend=PCA_2, yend=PCA_3)) +
  geom_text_repel(data=vars.swing
                  , aes(x=PCA_2, y=PCA_3
                        , label = row.names(vars.swing)
                        , fontface = "bold")
                  , size = 3) +
  scale_colour_gradient(low = "pink", high = "purple", labels=percent) +
  labs(color = "Polling error in Trump's favor\n(Vote – Average.Polling)") +
  theme(legend.position="bottom")
p5

p6 = ggplot() +
  geom_point(data=vars.swing
             , aes(x=PCA_2, y=PCA_3)
             , color="dark grey"
             , shape = 4) +  
  geom_segment(data = vars.swing
               , x = 0, y = 0
               , aes(xend=PCA_2, yend=PCA_3)
               , color = "dark grey") +
  geom_text_repel(data=vars.swing
                  , aes(x=PCA_2, y=PCA_3, label = row.names(vars.swing))
                  , size = 3
                  , color = "dark grey") +
  geom_text_repel(data=states.swing
                  , aes(x=PCA_2, y=PCA_3
                        , label = row.names(states.swing)
                        , color = Trump.PollError.2016
                        #, fontface = "bold"
                        )
                  , size = 3) +
  geom_point(data=states.swing
             , aes(x=PCA_2, y=PCA_3, color = Trump.PollError.2016)
             , size = 2) +
  scale_colour_gradient(low = "pink", high = "purple", labels=percent) +
  labs(color = "Polling error in Trump's favor\n(Vote – Average.Polling)") +
  theme(legend.position="bottom")
p6

require(gridExtra)
grid1 <- grid.arrange(p1, p2, ncol=2)
grid2 <- grid.arrange(p3, p4, ncol=2)
grid3 <- grid.arrange(p5, p6, ncol=2)


ggsave('p1.png', plot = p1
       , scale = 1
)
ggsave('p2.png', plot = p2
       , scale = 1
)
ggsave('p3.png', plot = p3
       , scale = 1
)
ggsave('p4.png', plot = p4
       , scale = 1
)
ggsave('p5.png', plot = p5
       , scale = 1
)
ggsave('p6.png', plot = p6
       , scale = 1
)
ggsave('grid1.png', plot = grid1
       , scale = 1
)
ggsave('grid2.png', plot = grid2
       , scale = 1
)
ggsave('grid3.png', plot = grid3
       , scale = 1
)
