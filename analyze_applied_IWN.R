
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

# seed for all stochastic things below
seed = 123


# Read in data  -------------------------------------------------

setwd(prepped.data.dir)
d2 = fread("prepped_data.csv")


# HIGH SCHOOL LONGITUDINAL STUDY: EEMM EXAMPLE ---------------------------------------------------------------

# comparing analysis laws for these two:
# math scores ~ female * males_better_math_cont
# math scores ~ female 

# could make composite of the male/female variables:
#S1ENGCOMP
#S1MTHCOMP
#S1SCICOMP

# analysis_vars = c("X2TXMTH", "X3TGPAACAD", "X1SES") # interaction is too weak to work well here
# analysis_vars = c("female", "X3TGPAACAD", "males_better_math") # also works
analysis_vars = c("female", "X2TXMTH", "males_better_math_cont", "public_school") #*kinda works - SAVE

# ~ Descriptives  -------------------------------------------------

CreateTableOne(data = d2 %>% select(analysis_vars))

cor(d2 %>% select(analysis_vars), use = "pairwise.complete.obs")

# missingness in underlying data on these vars
# up to 12% missing
d2 %>% select(analysis_vars) %>%
  summarise_all( function(x) mean(is.na(x)))


# ~ Impose missingness  -------------------------------------------------

du = d2 %>% select(analysis_vars) %>% na.omit
dim(du)

names(du) = c("A1", "B1", "C1", "D1")
cor(du)


# straight from sim study, DAG 1K
set.seed(seed)
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

# # will only work for binary C
# du %>% group_by(C1) %>%
#   summarise(mean(RC))


# ~ Make imputations and compare  -------------------------------------------------
di_std = du %>% select(A, B, C, D)

# # remove any completely missing observations
# di_std = di_std %>% filter( !(is.na(A) & is.na(B) & is.na(C)) )
# dim(di_std); dim(du)

colMeans(is.na(di_std))

# ~ Analysis law #1: EMM  -------------------------------------------------
# interaction is close to null in imputations
impute_compare(.dm = di_std,
               .du = du,
               
               .ords = "C",
               .noms = c("A", "D"),
               .imp.seed = seed,
               
               .form.string.dm = "B ~ A*C",
               .coef.of.interest.dm = "A:C",
               .form.string.du = "B1 ~ A1*C1",
               .coef.of.interest.du = "A1:C1" )


# ~ Analysis law #2: main effect only  -------------------------------------------------
# still using A,B,C in imputation model here
impute_compare(.dm = di_std,
               .du = du,
               
               .ords = "C",
               .noms = c("A", "D"),
               .imp.seed = seed,
               
               #.run.mice = FALSE,
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
               
               .noms = c("A", "D"),
               .imp.seed = seed,
               
               .form.string.dm = "B ~ A",
               .coef.of.interest.dm = "A",
               .form.string.du = "B1 ~ A1",
               .coef.of.interest.du = "A1" )


# bm: should probably average over a large number of imputations since they differ a bit

# # just for fun: also include public school
# impute_compare(.dm = di_std,
#                .du = du,
#                .form.string.dm = "B ~ A*C + D",
#                .coef.of.interest.dm = "A:C",
#                .form.string.du = "B1 ~ A1*C1 + D",
#                .coef.of.interest.du = "A1:C1" )



# CONFOUNDER FILE-MATCHING ---------------------------------------------------------------

# ### Test case 1: GPA ~ OVERALL GPA + MATH
# # works but not super interesting
# analysis_vars = c("X3TGPAACAD", "X2TXMTH", "public_school")
# 
# du = d2 %>% select(analysis_vars)
# du$noise = rnorm(n=nrow(du))  # temporary way to have a useless, but complete, aux variable to prevent warnings
# 
# cor(du, use = "pairwise.complete.obs")
# 
# dm = file_match(du,
#                 var1 = "X3TGPAACAD",
#                 var2 = "X2TXMTH" ) 
# 
# impute_compare(.dm = dm,
#                .du = du,
#                .form.string.dm = "public_school ~ X3TGPAACAD + X2TXMTH",
#                .coef.of.interest.dm = "X2TXMTH",
#                .form.string.du = "public_school ~ X3TGPAACAD + X2TXMTH",
#                .coef.of.interest.du = "X2TXMTH" )
# 
# # logistic regression - works pretty well; 50% bias
# impute_compare(.dm = dm,
#                .du = du,
#                .model = "logistic",
#                .form.string.dm = "public_school ~ X3TGPAACAD + X2TXMTH",
#                .coef.of.interest.dm = "X2TXMTH",
#                .form.string.du = "public_school ~ X3TGPAACAD + X2TXMTH",
#                .coef.of.interest.du = "X2TXMTH" )


### Test case 2

# some schools report SES while others only report the responding parent's education level
# building a predictive model for GPA that includes SES/parent education and other variables
# the coefficients for parental income and for SES will be biased.


# ~ Descriptives  -------------------------------------------------

analysis_vars = c("X3TGPAACAD", "par1_has_BA", "X1SES", "female", "public_school")

CreateTableOne(data = d2 %>% select(analysis_vars))

cor(d2 %>% select(analysis_vars), use = "pairwise.complete.obs")

# missingness in underlying data on these vars
# up to 28% missing (parent has BA)
d2 %>% select(analysis_vars) %>%
  summarise_all( function(x) mean(is.na(x)))


# ~ Impose missingness  -------------------------------------------------

du = d2 %>% select(analysis_vars) %>% na.omit
dim(du)

cor(du, use = "pairwise.complete.obs")

set.seed(seed)
dm = file_match(du,
                var1 = "par1_has_BA",
                var2 = "X1SES" ) 

colMeans(dm, na.rm = TRUE)
colMeans(is.na(dm))


# ~ Make imputations and compare  -------------------------------------------------

# ***THIS WORKS WELL!! FILE-MATCHING HALVES THE COEFFICIENT FOR PUBLIC SCHOOL
impute_compare(.dm = dm,
               .du = du,
               
               .noms = c("par1_has_BA", "female", "public_school"),
               .imp.seed = seed,
               #.run.mice = FALSE,
               
               .form.string.dm = "X3TGPAACAD ~ par1_has_BA + X1SES + female + public_school",
               .coef.of.interest.dm = "public_school",
               .form.string.du = "X3TGPAACAD ~ par1_has_BA + X1SES + female + public_school",
               .coef.of.interest.du = "public_school" )



