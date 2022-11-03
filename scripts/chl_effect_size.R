
#code and annotations based on: Crystal-Ornelas, R. (2020). robcrystalornelas/meta-analysis_of_ecological_data: First release (Version v0.1.0). Zenodo. http://doi.org/10.5281/zenodo.4320107


#load packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(metafor)

#load data
df <- read.csv("data_raw/chl_data_formated.csv")

#remain ID column as articleID, because will be creating another ID column later on
df <- df %>%
  rename(articleID = ID)

#calculate effect sizes

##calculate Standard Mean Difference (AKA Hedges' g)
SMD_effect_sizes <-
  escalc( # This function in metafor calculates an effect size for each row in the dataframe
    "SMD",
    # Specify the effect size we want to calculate. In this case SMD for Standardized mean difference. Change this to "ROM" for Ratio of Means (AKA Response Ratio)
    m1i = mcon,
    # mean chlorophyll of connected sites
    n1i = ncon,
    # connected site sample size
    sd1i = sdcon,
    # connected site SD
    m2i = mdis,
    # mean richness at control sites
    n2i = ndis,
    # control site sample size
    sd2i = sddis,
    # control site SD
    data = df # This is where the escalc function can find all the data for our meta-analysis
  )

##calculate response ratio
RR_effect_sizes <- escalc( # Function for calculating effect sizes.
  "ROM",
  # Specify the effect size we want to calculate. In this case ROM for the Ratio of Means or Response Ratio
  m1i = mcon,
  # mean chlorophyll of connected sites
  n1i = ncon,
  # connected site sample size
  sd1i = sdcon,
  # connected site SD
  m2i = mdis,
  # mean richness at control sites
  n2i = ndis,
  # control site sample size
  sd2i = sddis,
  # control site SD
  data = df # This is where the escalc function can find all the data for our meta-analysis
)


#Random effects model with rma()
random_effect_model_results <- rma(yi, # effect size from each row in database
                                   vi, # measure of variance from each row in database
                                   method = "REML", # Using  a REML estimator which is common for random effect meta-analyses
                                   slab = articleID,
                                   data = RR_effect_sizes) # specify the dataframe to use for the model

random_effect_model_results

#Random effects model with rma.mv()
RR_effect_sizes$ID <- seq.int(nrow(RR_effect_sizes)) #assign a unique number to every row in the database
##run model
random_effect_model_results_again <- rma.mv(yi, vi, random = ~ 1 | ID, data = RR_effect_sizes) #should produce the same results as rma()
random_effect_model_results_again

#Random effects model with rma.mv() accounting for nonindependence
random_effects_model_results_with_author <-
  rma.mv(yi,
         vi,
         random = ~ 1 | articleID,
         slab = articleID, # This line is important for when we create our forest plot later.
         data = RR_effect_sizes)
random_effects_model_results_with_author

#Testing for heterogeneity in the data
##Q is a test for heterogeneity and is included in the model summary
##in addition, we can create a publication bias histogram...
pub_bias_histogram <-ggplot(RR_effect_sizes, aes(x=yi)) +
  geom_histogram(binwidth=0.05,color="black", fill="white") +
  xlab("ln(Response ratio)") +
  ylab ("Frequency of effect sizes") +
  theme_cowplot() +
  theme(plot.title = element_text(size = 25, hjust = -.06, face = "bold"),
        axis.title = element_text(size = 25, face = "bold"),
        axis.text = element_text(size = 15)) +

  scale_y_continuous(expand = c(0,0),limits=c(0,10))
pub_bias_histogram
##Relatively symmetrical histograms indicate a lack of publication bias in the dataset

#Forest plot of random effects model
forest(
  RR_effect_sizes$yi, # These are effect sizes from each row in database
  RR_effect_sizes$vi, # These are variances from each row in database
  annotate = TRUE, # Setting this to false prevents R from including CIs for each of the effect sizes in the forest plot. Setting it to TRUE is generally a good practice, but can make this plot cluttered.
  slab = random_effects_model_results_with_author$slab, # Along the left hand side, we want each individual effect size labeled with articleID. We specified this when we calculated the summary effect size above
  xlab = "ln(Response Ratio)", # Label for x-axis
  cex = .8, # Text side for study labels
  pch = 15, # shape of bars in forest plot
  cex.lab = 1, # Size of x-axis label
  rows = length(RR_effect_sizes$articleID) + 2 #create row space in plot for the summary effect size to be added
)

# This is code adds in the summary effect size for the random effects meta-analysis to the forest plot
addpoly(
  random_effects_model_results_with_author, # specify the model where your summary effect comes from.
  col = "orange",
  row = 1, #specify the row placement of the summary effect size
  cex = 1, # size for the text associates with summary effect
  annotate = TRUE, # Usually, we set this to true. It makes sure effect size and CI for summary effect size is printed on forest plot.
  mlab = "Summary" # Label for summary effect size
)

