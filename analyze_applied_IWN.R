# PRELIMINARIES ---------------------------------------------------------------

# This script uses renv to preserve the R environment specs (e.g., package versions.)
library(renv)
# run this if you want to reproduce results using the R environment we had:
# renv::restore()

to.load = c("dplyr",
            "data.table",
            "purrr",
            "tidyr",
            "stringr",
            "tibble",
            "ggplot2",
            "testthat",
            "plotly",
            "htmlwidgets", # for saving plotly
            "here",
            "haven",
            "mice",
            "gtsummary",
            "tableone",
            "corrr",
            "Amelia")

# load within installation if needed
for (pkg in to.load) {
  
  cat( paste("\nAbout to try loading package", pkg) )
  
  tryCatch({
    # eval below needed because library() will otherwise be confused
    # https://www.mitchelloharawild.com/blog/loading-r-packages-in-a-loop/
    eval( bquote( library( .(pkg) ) ) )
  }, error = function(err) {
    install.packages(pkg)
  })
  
}

# run this only if you want to update the R environment specs
# renv::snapshot()


# get helper fns
setwd( here() )
source("helper_applied_IWN.R")

# get fns from sim study
setwd("/Users/mmathur/Dropbox/Personal computer/Independent studies/2023/*IWN (Imputation without nightMARs)/Simulation code")
source("helper_IWN.R")

prepped.data.dir = "/Users/mmathur/Dropbox/Personal computer/Independent studies/2023/*IWN (Imputation without nightMARs)/Applied example/NADAC High School Longitudinal Study, 2009-2013 [United States] (ICPSR 36423)/Prepped data"

# no sci notation
options(scipen=999)


# Read in data  -------------------------------------------------

setwd(prepped.data.dir)
d2 = fread("prepped_data.csv")


# HIGH SCHOOL LONGITUDINAL STUDY ---------------------------------------------------------------

#analysis_vars = c("X2TXMTH", "X3TGPAACAD", "X1SES") # interaction is too weak to work well here
analysis_vars = c("female", "X2TXMTH", "males_better_math_cont", "public_school") #*kinda works - SAVE
#analysis_vars = c("female", "X3TGPAACAD", "males_better_math") # also works

cor(d2 %>% select(analysis_vars), use = "pairwise.complete.obs")

# could make composite of the male/female variables:
#S1ENGCOMP
#S1MTHCOMP
#S1SCICOMP

du = d2 %>% select(analysis_vars) %>% na.omit
dim(du)

names(du) = c("A1", "B1", "C1", "D1")
cor(du)

any(is.na(du$D1))

# straight from sim study, DAG 1K
du = du %>% rowwise() %>%
  mutate( 
    RA = rbinom( n = 1,
                 prob = 0.5, # this variable is MCAR
                 size = 1 ),
    RB = rbinom( n = 1,
                 prob = 0.5, # this variable is MCAR
                 size = 1 ),
    
    RC = rbinom( n = 1,
                 prob = expit(2*C1),
                 size = 1 ),
    
    RD = 1, # no missingness on this one
    
    A = ifelse(RA == 0, NA, A1),
    B = ifelse(RB == 0, NA, B1),
    C = ifelse(RC == 0, NA, C1),
    D = ifelse(RD == 0, NA, D1) )

cor(du)

# will only work for binary C
du %>% group_by(C1) %>%
  summarise(mean(RC))

di_std = du %>% select(A, B, C, D)

# # remove any completely missing observations
# di_std = di_std %>% filter( !(is.na(A) & is.na(B) & is.na(C)) )
# dim(di_std); dim(du)

colMeans(is.na(di_std))

# EEMM
#interaction is close to null in imputations
impute_compare(.dm = di_std,
               .du = du,
               .form.string.dm = "B ~ A*C",
               .coef.of.interest.dm = "A:C",
               .form.string.du = "B1 ~ A1*C1",
               .coef.of.interest.du = "A1:C1" )

# just for fun: also include public school
impute_compare(.dm = di_std,
               .du = du,
               .form.string.dm = "B ~ A*C + D",
               .coef.of.interest.dm = "A:C",
               .form.string.du = "B1 ~ A1*C1 + D",
               .coef.of.interest.du = "A1:C1" )

# still using A,B,C in imputation model here
impute_compare(.dm = di_std,
               .du = du,
               .form.string.dm = "B ~ A",
               .coef.of.interest.dm = "A",
               .form.string.du = "B1 ~ A1",
               .coef.of.interest.du = "A1" )

# impute without C in the model
# might become more efficient?
di_ours = di_std %>% select(A,B,D)
dim(di_ours)
impute_compare(.dm = di_ours,
               .du = du,
               .form.string.dm = "B ~ A",
               .coef.of.interest.dm = "A",
               .form.string.du = "B1 ~ A1",
               .coef.of.interest.du = "A1" )


#bm: should probably average over a large number of imputations


