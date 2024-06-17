
#This code excludes all the clean-up and preparation.  Below is the loaded data for each biochemical variable,
#the PCA, and the models.  Clean example data is loaded into this repository.

#Beta-endorphins
#import as pca_data
pca_data1 <- read_excel("name_of_file.xlsx", 
                            col_types = c("skip", "numeric", "numeric", 
                                                    "text", "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric"))
pca_data1 <- as.data.frame(pca_data1)
pca_data <- pca_data1[, -c(1, 2, 37)]  # Exclude timepoint, Monkey, and beta_endorphins
pca_data <- pca_data1[, -c(1, 2, 3, 38:73)] #Exclude timepoint, Monkey, beta_endorphins, and principal components (if in saved file and reloading)
#The example data contains example principal componets, so you will need to include 38:73 if running the example data.

# Perform PCA
pca_result <- prcomp(pca_data, scale. = TRUE, center = TRUE)

# Variance explained by each principal component
prop_var <- pca_result$sdev^2 / sum(pca_result$sdev^2)
prop_var
plot(prop_var)

# Loadings of the principal components
loadings <- pca_result$rotation
loadings

#pca_data <- cbind(X_pca2_no_outliers1, pca_result$x)
pca_data <- cbind(pca_data1, pca_result$x)

#To build models, install package, open, then prepare data for input
#install.packages("MCMCglmm")
library(MCMCglmm)

#do PCA values
PC1 <- pca_result$x[,1]
PC2 <- pca_result$x[,2]
PC3 <- pca_result$x[,3]
PC4 <- pca_result$x[,4]
PC5 <- pca_result$x[,5]
PC6 <- pca_result$x[,6]
PC7 <- pca_result$x[,7]
PC8 <- pca_result$x[,8]
PC9 <- pca_result$x[,9]
PC10 <- pca_result$x[,10]
PC11 <- pca_result$x[,11]
PC12 <- pca_result$x[,12]
PC13 <- pca_result$x[,13]
PC14 <- pca_result$x[,14]
PC15 <- pca_result$x[,15]
PC16 <- pca_result$x[,16]
PC17 <- pca_result$x[,17]
PC18 <- pca_result$x[,18]
PC19 <- pca_result$x[,19]
PC20 <- pca_result$x[,20]
PC21 <- pca_result$x[,21]
PC22 <- pca_result$x[,22]
PC23 <- pca_result$x[,23]
PC24 <- pca_result$x[,24]
PC25 <- pca_result$x[,25]
PC26 <- pca_result$x[,26]
PC27 <- pca_result$x[,27]
PC28 <- pca_result$x[,28]
PC29 <- pca_result$x[,29]
PC30 <- pca_result$x[,30]
PC31 <- pca_result$x[,31]
PC32 <- pca_result$x[,32]

shapiro.test(pca_data1$beta_endorphins)  #normally distributed.  Can use gaussian distribution

#Save file before continuing if needed
#write.csv(pca_data, file = "name_of_file.csv", row.names = TRUE)

# Create the linear regression model
# Specify the number of iterations and burn-in length
iterations <- 1300000
burnin <- 300000  # Discard the first 300,000 iterations
thinning_interval <- 100

# Specify prior specification
prior <- list(
  R = list(V = 1, fix = 1),
  G = list(G1 = list(V = 1, nu = 0.002))
)

prior1 <- list(R = list(V = 1, nu = 1.002), G = list(G1 = list(V =1,
                                                               nu = 1.002,
                                                               alpha.mu = 0,
                                                               alpha.V = 1000)))

# Specify the number of iterations and burn-in length
iterations <- 1300000
burnin <- 300000  # Discard the first 300,000 iterations

#The above code will need to be modified according to your specific study
#These values were based upon the literature and performed well for our data.

# Create the linear regression model
# Below are several examples of models created but not the exhaustive list as backward regresison was completed and only variables retained that were significant.
# Timepoint and the treatment/control groups were always included in models.
set.seed(46)

null <- MCMCglmm(beta_endorphins ~ 1,  data = pca_data,  #Null model
                random= ~Monkey,
                family = "gaussian",
                verbose = FALSE,  # Turn off verbose output
                nitt = iterations,  # Number of iterations
                burnin = burnin,  # Burn-in length
                prior = prior)
summary(null)

model <- MCMCglmm(beta_endorphins ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + PC31 + PC32 + as.factor(Month) + as.factor(timepoint) + as.factor(Group),  data = pca_data, 
                      random= ~Monkey,
                      family = "gaussian",
                      verbose = FALSE,  # Turn off verbose output
                      nitt = 305000,  # Number of iterations
                      burnin = 50000,  # Burn-in length
                      prior = prior, #priors selected based upon These are based on examples in Houslay and Wilson 2017 and https://zenodo.org/records/6865934
                      thin = 3000,
                      pr = TRUE)

summary(model)  #DIC 1796.01
plot(model)


model2 <- MCMCglmm(beta_endorphins ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 +PC9 + PC10 + timepoint + as.factor(Group),  data = pca_data, 
                   random= ~Monkey,
                   family = "gaussian",
                   verbose = FALSE,  # Turn off verbose output
                   nitt = iterations,  # Number of iterations
                   burnin = burnin,  # Burn-in length
                   prior = prior,
                   thin = thinning_interval)
summary(model2)

#Final beta-endorphin model
model_reduced <- MCMCglmm(beta_endorphins ~ (PC4 + PC6 + PC11 + PC13 + PC26 + PC31) + as.factor(Month) + as.factor(timepoint) + as.factor(Group),  data = pca_data, 
                  random= ~Monkey,
                  family = "gaussian",
                  verbose = FALSE,  # Turn off verbose output
                  nitt = iterations,  # Number of iterations
                  burnin = burnin,  # Burn-in length
                  prior = prior1, 
                  thin = 500,
                  pr = TRUE)

summary(model_reduced)  #No interactions were significant when tested.
plot(model_reduced)


#This code was replicated with backward regression for each of the variables assessed.
#This notebook does not contain all possible models nor does it include the code used to organize, clean, or prepare the data for the analysis.

