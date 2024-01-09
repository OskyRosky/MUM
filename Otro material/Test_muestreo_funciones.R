#################################
#       Test te muestreo        #
#################################

options(scipen=999)


#################
#   Libraries   #
#################

library(jfa)


#################
#   Dataframe   #
#################


set.seed(98745) # for reproducibility

df <- data.frame(
  ID = 1:10000,
  Value = rnorm(10000, mean = 500, sd = 50)  # Simulating normal data for demonstration
)


#################
#   AuditPlan   #
#################

auditPlan <- planning(materiality = 500, expectedError = 50, 
                      confidence = 0.95, N = nrow(df), method="random")

auditPlan <- planning(materiality = 0.05,
                      min.precision = NULL,
                      expected = 0.01,
                      likelihood = c("poisson", "binomial", "hypergeometric"),
                      conf.level = 0.95,
                      N.units = NULL,
                      by = 1,
                      max = 5000,
                      prior = FALSE
)

x <- planning(materiality = 0.001, likelihood = "poisson")

x$n
#################
#   Selection   #
#################


y <- selection(
  data = df, size = 100, units = "values",
  method = "random", values = "Value"
)

sample <- y$sample

data('BuildIt')



################################################################################################################################################
################################################################################################################################################
################                                              Get started with the jfa package                                  ################ 
################################################################################################################################################
################################################################################################################################################

#################
#   Libraries   #
#################

library(jfa)

###################################
#    Load the BuildIt population  #
###################################

data("BuildIt")
head(BuildIt)


###################################
# Stage 1: Planning               #
###################################

# Planning function
# https://koenderks.github.io/jfa/articles/audit-sampling.html

# planning(
#  materiality = NULL,
#  min.precision = NULL,
#  expected = 0,
#  likelihood = c("poisson", "binomial", "hypergeometric"),
#  conf.level = 0.95,
#  N.units = NULL,
#  by = 1,
#  max = 5000,
#  prior = FALSE
#  )

stage1 <- planning(materiality = 0.03, 
                   expected = 0.01,
                   likelihood = "poisson", 
                   conf.level = 0.95
)

summary(stage1)

sample_size <- stage1$n

###################################
#       Stage 2: Selection        #
###################################

# selection(
#   data,
#   size,
#   units = c("items", "values"),
#   method = c("interval", "cell", "random", "sieve"),
#   values = NULL,
#   order = NULL,
#   decreasing = FALSE,
#   randomize = FALSE,
#   replace = FALSE,
#   start = 1
# )

stage2 <- selection(
  data = BuildIt, 
  size = stage1,      #### Stage1 previous defined 
  units = "values", 
  values = "bookValue",   #### Column from  BuildIt
  method = "interval", start = 5
)
summary(stage2)

# sample <- stage2$sample


###################################
#       Stage 3: Execution        #
###################################

sample <- stage2[["sample"]]  # With the sample, it is request the documents review


#####################################
#       # Stage 4: Evaluation       #
#####################################

# evaluation(
#   materiality = NULL, 
#     method = c(
#    "poisson", "binomial", "hypergeometric",
#    "stringer", "stringer.meikle", "stringer.lta", "stringer.pvz",
#    "rohrbach", "moment", "coxsnell",
#    "direct", "difference", "quotient", "regression", "mpu"
#  ),
#  alternative = c("less", "two.sided", "greater"),
#  conf.level = 0.95,
#  data = NULL,
#  values = NULL,
#  values.audit = NULL,
#  strata = NULL,
#  times = NULL,
#  x = NULL,
#  n = NULL,
#  N.units = NULL,
#  N.items = NULL,
#  pooling = c("none", "complete", "partial"), 
#  prior = FALSE
#)

stage4 <- evaluation(
  materiality = 0.03, method = "stringer",
  conf.level = 0.95, data = sample,
  values = "bookValue", values.audit = "auditValue"
)

summary(stage4)

#####################################
#               Others              #
#####################################

#################
# Data auditing #
#################

# Digit distribution test
x <- digit_test(round(sinoForest$value), check = "first", reference = "benford")
print(x)


plot(x)

########################
# Repeated values test #
########################

x <- repeated_test(sanitizer$value, check = "lasttwo", samples = 5000)
print(x)

plot(x)


######################
######################


setwd("C:/Users/oscar.centeno/Desktop/CGR 2023/Muestreo Auditoría/App/data")

Audtit_1 <- BuildIt %>% dplyr::select("ID","bookValue")
Audtit_2 <- BuildIt %>% dplyr::select("ID","bookValue", "auditValue")


# Exportar datos

write.table(Audtit_1, file = "Audit1.csv", sep = ",")
write.table(Audtit_2, file = "Audit2.csv", sep = ",")




