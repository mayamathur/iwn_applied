
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
setwd(here())
source("helper_applied_IWN.R")

# get fns from sim study
( sim.code.dir = str_replace_all( string = here(),
                                  pattern = "Applied example/Code",
                                  replacement = "Simulation study/Code" ) )
setwd(sim.code.dir)
source("helper_IWN.R")

# dataset cannot be released in our repo per terms of use
# see READ-ME for how to easily access it yourself (free)
prepped.data.dir = str_replace_all( string = here(),
                                    pattern = "Linked to OSF \\(IWN\\)/Applied example/Code",
                                    replacement = "Not linked to OSF/Private data for applied example/NADAC High School Longitudinal Study, 2009-2013 [United States] (ICPSR 36423)/Prepped data" )
# check it
setwd(prepped.data.dir)
# no sci notation
options(scipen=999)

# seed for all stochastic things below
seed = 123


# read in prepped data
setwd(prepped.data.dir)
d2 = fread("prepped_data.csv")



# CONFOUNDER FILE-MATCHING EXAMPLE ---------------------------------------------------------------

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

impute_compare(.dm = dm,
               .du = du,
               
               .noms = c("par1_has_BA", "female", "public_school"),
               .imp.seed = seed,
               #.run.mice = FALSE,
               
               .form.string.dm = "X3TGPAACAD ~ par1_has_BA + X1SES + female + public_school",
               .coef.of.interest.dm = "public_school",
               .form.string.du = "X3TGPAACAD ~ par1_has_BA + X1SES + female + public_school",
               .coef.of.interest.du = "public_school" )



