######################################################################
#                                                                    #
#                                                                    #
#                     Muestreo MUS                                   #
#                                                                    #   
#                                                                    #
######################################################################

suppressMessages(library(readxl))
suppressMessages(library(dplyr))
suppressMessages(library(MUS))



##########################################
#                MUS-package             #   
##########################################


## Simple Example
library(MUS)
# Assume 500 invoices, each between 1 and 1000 monetary units
example.data.1 <- data.frame(book.value=round(runif(n=500, min=1,
                                                    max=1000)))

# Plan a sample and cache it
plan.results.simple <- MUS.planning(data=example.data.1,
                                    tolerable.error=100000, expected.error=20000)

# 100000
# 20000

plan.results.simple
# Extract a sample and cache it (no high values exist in this example)
extract.results.simple <- MUS.extraction(plan.results.simple)

# extract.results.simple$sample

# Copy book values into a new column audit values
audited.sample.simple <- extract.results.simple$sample
audited.sample.simple <- cbind(audited.sample.simple,
                               audit.value=audited.sample.simple$book.value)
# Edit manually (if any audit difference occur)
#audited.sample.simple <- edit(audited.sample.simple)
# Evaluate the sample, cache and print it
evaluation.results.simple <- MUS.evaluation(extract.results.simple,
                                            audited.sample.simple)
print(evaluation.results.simple)


##############################################################################################################################
#                MUS.binomial.bound Calculate a binomial bound for a Monetary Unit Sampling evaluation.                      #   
##############################################################################################################################


# Assume 500 invoices, each between 1 and 1000 monetary units
data <- data.frame(book.value=round(runif(n=500, min=1, max=1000)))
# Plan a sample and cache it
plan <- MUS.planning(data=data, tolerable.error=10000, expected.error=2000)
# Extract a sample and cache it (no high values exist in this example)
extract <- MUS.extraction(plan)
# Copy book value into a new column audit values, and inject some error
audited <- extract$sample$book.value*(1-rbinom(nrow(extract$sample), 1, 0.05))
audited <- cbind(extract$sample, audit.value=audited)
# Evaluate the sample, cache and print it
evaluation <- MUS.evaluation(extract, audited)
MUS.binomial.bound(evaluation)

#############################################################################
#   MUS.calc.n.conservative    Calculate a conservative sample size         #   
#############################################################################


sample1 <- MUS.calc.n.conservative(0.85, 10000, 5000, 100000)
sample1


#############################################################################
#   MUS.calc.n.conservative    Calculate a conservative sample size         #   
#############################################################################

## Simple Example
# Assume 500 invoices, each between 1 and 1000 monetary units
stratum.1 <- data.frame(book.value=round(runif(n=500, min=1, max=1000)))
plan.1 <- MUS.planning(data=stratum.1, tolerable.error=100000, expected.error=20000)
stratum.2 <- data.frame(book.value=round(runif(n=500, min=1, max=1000)))
plan.2 <- MUS.planning(data=stratum.2, tolerable.error=100000, expected.error=20000)
plan.combined <- MUS.combine(list(plan.1, plan.2))
print(plan.combined)



#############################################################################
#   MUS.combined.high.error.rate 
#   Calculate a high error rate bound for a combined Monetary Unit Sampling evaluation.
#############################################################################

# Assume 500 invoices, each between 1 and 1000 monetary units
data1 <- data.frame(book.value=round(runif(n=500, min=1, max=1000)))
# Plan a sample and cache it
plan1 <- MUS.planning(data=data1, tolerable.error=10000, expected.error=2000)
# Extract a sample and cache it (no high values exist in this example)
extract1 <- MUS.extraction(plan1)
# Copy book value into a new column audit values, and inject some error
audited1 <- extract1$sample$book.value*(1-rbinom(nrow(extract1$sample), 1, 0.05))
audited1 <- cbind(extract1$sample, audit.value=audited1)
# Evaluate the sample, cache and print it
evaluation1 <- MUS.evaluation(extract1, audited1)


# Assume 500 invoices, each between 1 and 1000 monetary units
data2 <- data.frame(book.value=round(runif(n=500, min=1, max=1000)))
# Plan a sample and cache it
plan2 <- MUS.planning(data=data2, tolerable.error=10000, expected.error=2000)
# Extract a sample and cache it (no high values exist in this example)
extract2 <- MUS.extraction(plan2)
# Copy book value into a new column audit values, and inject some error
audited2 <- extract2$sample$book.value*(1-rbinom(nrow(extract2$sample), 1, 0.05))
audited2 <- cbind(extract2$sample, audit.value=audited2)
# Evaluate the sample, cache and print it
evaluation2 <- MUS.evaluation(extract2, audited2)
combined <- MUS.combine(list(evaluation1, evaluation2))
MUS.combined.high.error.rate(combined)


#############################################################################
#   MUS.evaluation
#   Evaluate a sample using Monetary Unit Sampling.
#############################################################################


## Simple Example
# Assume 500 invoices, each between 1 and 1000 monetary units
example.data.1 <- data.frame(book.value=round(runif(n=500, min=1,
                                                    max=1000)))
# Plan a sample and cache it
plan.results.simple <- MUS.planning(data=example.data.1,
                                    tolerable.error=100000, expected.error=20000)
# Extract a sample and cache it (no high values exist in this example)
extract.results.simple <- MUS.extraction(plan.results.simple)
# Copy book value into a new column audit values


audited.sample.simple <- extract.results.simple$sample
audited.sample.simple <- cbind(audited.sample.simple,
                               audit.value=audited.sample.simple$book.value)
# Edit manually (if any audit difference occur)
#audited.sample.simple <- edit(audited.sample.simple)
# Evaluate the sample, cache and print it
evaluation.results.simple <- MUS.evaluation(extract.results.simple,
                                            audited.sample.simple)
print(evaluation.results.simple)


## Advanced Example
example.data.2 <- data.frame(own.name.of.book.values=round(runif(n=500,
                                                                 min=1, max=1000)))
plan.results.advanced <- MUS.planning(data=example.data.2,
                                      col.name.book.values="own.name.of.book.values", confidence.level=.70,
                                      tolerable.error=100000, expected.error=20000, n.min=3)
extract.results.advanced <- MUS.extraction(plan.results.advanced,
                                           start.point=5, seed=1, obey.n.as.min=TRUE)
extract.results.advanced <- MUS.extraction(plan.results.advanced)
audited.sample.advanced <- extract.results.advanced$sample
audited.sample.advanced <- cbind(audited.sample.advanced,
                                 own.name.of.audit.values=audited.sample.advanced$own.name.of.book.values)
#audited.sample.advanced <- edit(audited.sample.advanced)
evaluation.results.advanced <- MUS.evaluation(extract.results.advanced,
                                              audited.sample.advanced,
                                              col.name.audit.values="own.name.of.audit.values")
print(evaluation.results.advanced)


#############################################################################
#   MUS.extend
#   Extend a MUS sample
#############################################################################

## Simple Example
# Assume 500 invoices
mydata <- data.frame(book.value=
                       round(c(runif(n=480, min=10,max=20000),
                               runif(n=20, min=15000,max=50000)))
)
# Plan a sample and cache it
plan <- MUS.planning(data=mydata,
                     tolerable.error=50000, expected.error=3000)
plan
# Extract a sample and cache it
extract <- MUS.extraction(plan, obey.n.as.min=TRUE)
# Create a new plan
new_plan <- MUS.planning(data=mydata,
                         tolerable.error=50000, expected.error=5000)
# extends the sample using the new plan
extended <- MUS.extend(extract, new_plan)
# extends the sample by 20 itens using the original plan
extended20 <- MUS.extend(extract, additional.n=20)

#############################################################################
#   MUS.extraction
#   Extract a sample using Monetary Unit Sampling.
#############################################################################

## Simple Example
# Assume 500 invoices, each between 1 and 1000 monetary units
example.data.1 <- data.frame(book.value=round(runif(n=500, min=1,
                                                    max=1000)))
# Plan a sample and cache it
plan.results.simple <- MUS.planning(data=example.data.1,
                                    tolerable.error=100000, expected.error=20000)
plan.results.simple

# Extract a sample and cache it
extract.results.simple <- MUS.extraction(plan.results.simple)
extract.results.simple
## Advanced Example
example.data.2 <- data.frame(own.name.of.book.values=round(runif(n=500,
                                                                 min=1, max=1000)))
plan.results.advanced <- MUS.planning(data=example.data.2,
                                      col.name.book.values="own.name.of.book.values", confidence.level=.70,
                                      tolerable.error=100000, expected.error=20000, n.min=3)
extract.results.advanced <- MUS.extraction(plan.results.advanced,
                                           start.point=5, seed=0, obey.n.as.min=TRUE)

extract.results.advanced


#############################################################################
#   MUS.factor
#   Calculate MUS Factor
#############################################################################


MUS.factor(0.95, 0.5)


#############################################################################
#   MUS.moment.bound
#   Calculate the moment bound for a Monetary Unit Sampling evaluation.
#############################################################################

sample = c(rep(0, 96), -.16, .04, .18, .47)
MUS.moment.bound(sample)
# Assume 500 invoices, each between 1 and 1000 monetary units
data <- data.frame(book.value=round(runif(n=500, min=1, max=1000)))
# Plan a sample and cache it
plan <- MUS.planning(data=data, tolerable.error=10000, expected.error=2000)
# Extract a sample and cache it (no high values exist in this example)
extract <- MUS.extraction(plan)
# Copy book value into a new column audit values, and inject some error
audited <- extract$sample$book.value*(1-rbinom(nrow(extract$sample), 1, 0.05))
audited <- cbind(extract$sample, audit.value=audited)
# Evaluate the sample, cache and print it
evaluation <- MUS.evaluation(extract, audited)
MUS.moment.bound(evaluation)



#############################################################################
#   MUS.multinomial.bound
#   Calculate the moment bound for a Monetary Unit Sampling evaluation.
#############################################################################

# Assume 500 invoices, each between 1 and 1000 monetary units
data <- data.frame(book.value=round(runif(n=500, min=1, max=1000)))
# Plan a sample and cache it
plan <- MUS.planning(data=data, tolerable.error=10000, expected.error=2000)
# Extract a sample and cache it (no high values exist in this example)
extract <- MUS.extraction(plan)
# Copy book value into a new column audit values, and inject some error
audited <- extract$sample$book.value*(1-rbinom(nrow(extract$sample), 1, 0.05))
audited <- cbind(extract$sample, audit.value=audited)
# Evaluate the sample, cache and print it
evaluation <- MUS.evaluation(extract, audited)
MUS.multinomial.bound(evaluation)


#############################################################################
#   MUS.planning
#   Plan a sample using Monetary Unit Sampling.
#############################################################################

## Simple Example
# Assume 500 invoices, each between 1 and 1000 monetary units
example.data.1 <- data.frame(book.value=round(runif(n=500, min=1,
                                                    max=1000)))
# Plan a sample and cache it
plan.results.simple <- MUS.planning(data=example.data.1,
                                    tolerable.error=100000, expected.error=20000)

plan.results.simple


## Advanced Example
example.data.2 <- data.frame(own.name.of.book.values=round(runif(n=500,
                                                                 min=1, max=1000)))
plan.results.advanced <- MUS.planning(data=example.data.2,
                                      col.name.book.values="own.name.of.book.values", confidence.level=.70,
                                      tolerable.error=100000, expected.error=20000, n.min=3)

plan.results.advanced

















