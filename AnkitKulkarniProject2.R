#----------------------------------------------------------------------------

#Titanic Dataset

#download and install package "arules"
#load the libraries "Matrix", "arules"
library(Matrix)
library(arules)

#load data set from the working directory
titanic_data <- read.csv("titanic.csv")

#generate all rules
trules <- apriori(titanic_data)
inspect(trules)

#to get the rules with support = 0.01 and confidence = 0.9
tc_rules <- apriori(titanic_data, parameter = list(supp = 0.01, conf = 0.9), appearance = list(rhs=c("Survived=No", "Survived=Yes"), default = "lhs"), control = list(verbose = F))
inspect(tc_rules)

#sorting rules by lift
trules_sorted <- sort(tc_rules, by = "lift")

#to remove the redundant rules obtained from the above sorted list
subset.matrix <- is.subset(trules_sorted, trules_sorted)
subset.matrix[lower.tri(subset.matrix, diag = TRUE)] <- NA
red_trules <- colSums(subset.matrix, na.rm = TRUE) >= 1

#to display the non-redundant rules
pruned_trules <- trules_sorted[!red_trules]
inspect(pruned_trules)




#------------------------------------------------------------------------------

#Retail Dataset

#download and install package "arules"
#load the libraries "Matrix", "arules"
library(Matrix)
library(arules)

#load data set from the working directory
retail_data <- read.csv("retail.csv")

#as the columns 2:15 are logical, and column 1 can be ignored the following 2 lines are executed
rretail_data <- retail_data[, 2:15]
retail_data_s <- sapply(rretail_data, as.logical)

#the data is read as transactions using the line below
ret_data <- as(retail_data_s, "transactions")

#generate all rules
rrules <- apriori(ret_data)
inspect(rrules)

#to get the rules with support = 0.01 and confidence = 0.9
rcbev_rules <- apriori(ret_data, parameter = list(supp = 0.01, conf = 0.9), appearance = list(rhs=c("Beverage"), default = "lhs"))
inspect(rcbev_rules)
rcmeat_rules <- apriori(ret_data, parameter = list(supp = 0.01, conf = 0.9), appearance = list(rhs=c("Meat"), default = "lhs"))
inspect(rcmeat_rules)
rcpc_rules <- apriori(ret_data, parameter = list(supp = 0.01, conf = 0.9), appearance = list(rhs=c("PersonalCare"), default = "lhs"))
inspect(rcpc_rules)

#sorting rules by lift
rrules_sorted <- sort(rcpc_rules, by = "lift")

#to remove the redundant rules obtained from the above sorted list
subset.matrix <- is.subset(rrules_sorted, rrules_sorted)
subset.matrix[lower.tri(subset.matrix, diag = TRUE)] <- NA
red_rrules <- colSums(subset.matrix, na.rm = TRUE) >= 1

#to display the non-redundant rules
pruned_rrules <- rrules_sorted[!red_rrules]
inspect(pruned_rrules)




#------------------------------------------------------------------------------

#Game of Thrones Dataset

#download and install package "base" and "arules" if not present already
#load the libraries "Matrix", "arules"
library(Matrix)
library(arules)

#load data set from the working directory
GoT_data <- read.csv("game_of_thrones.csv", header = TRUE)

#as the columns 4:10 are logical, and column 1 can be ignored the following 2 lines are executed
GoT_data <- merge(GoT_data[, 2:3], as.data.frame(sapply(GoT_data[,4:10], as.logical)), by="row.names")
GoT_data <- GoT_data[, 2:10]

#generate all rules (defined support and confidence as setting it to 0 gives a memory error)
#using the default values for support and confidence gives 21 rules whereas specifying support = 0.01 and confidence = 0.8 gives 673 rules
#specifying the values for supp = 0.01 and conf = 0.8
gotrules <- apriori(GoT_data, parameter = list(supp = 0.01, conf = 0.8))
inspect(gotrules)
#using the default values for support and confidence
gotrules2 <- apriori(GoT_data)
inspect(gotrules2)

#to get the rules with support = 0.01 and confidence = 0.9
gotc_rules <- apriori(GoT_data, parameter = list(supp = 0.01, conf = 0.9), appearance = list(rhs = c("Survives"), default = "lhs"))
inspect(gotc_rules)

#sorting rules by lift
gotrules_sorted <- sort(gotc_rules, by = "lift")
inspect(gotrules_sorted)

#to remove the redundant rules obtained from the above sorted list
subset.matrix <- is.subset(gotrules_sorted, gotrules_sorted)
subset.matrix[lower.tri(subset.matrix, diag = TRUE)] <- NA
red_gotrules <- colSums(subset.matrix, na.rm = TRUE) >= 1

#to display the non-redundant rules
pruned_gotrules <- gotrules_sorted[!red_gotrules]
inspect(pruned_gotrules)

#to check if nobility plays any role in survival
nobilityrule <- subset(pruned_gotrules, rhs %in% c("Survives") & lhs %in% c("Nobility"))
inspect(nobilityrule)

#to check if rules exist for survival for male Game of Thrones characters
gendermrule <- subset(pruned_gotrules, rhs %in% c("Survives") & lhs %in% c("Gender=M"))
inspect(gendermrule)

#to check if rules exist for survival for female Game of Thrones characters
genderfrule <- subset(pruned_gotrules, rhs %in% c("Survives") & lhs %in% c("Gender=F"))
inspect(genderfrule)

#to check if a rule exists for Jon Snow in the pruned rules obtained above
jonrule <- subset(pruned_gotrules, rhs %in% c("Survives") & lhs %in% c("House=Night's Watch"))
inspect(jonrule)

#to check if a rule exists for any random character
aryarule <- subset(pruned_gotrules, rhs %in% c("Survives") & lhs %in% c("House=Stark", "Gender=F"))
inspect(aryarule)

#Resources cited:
#http://www.rdatamining.com/docs/association-rule-mining-with-r
#http://www.rdatamining.com/examples/association-rules
#http://www.r-bloggers.com/using-apply-sapply-lapply-in-r/
#Stackoverflow.com
#https://cran.r-project.org/web/packages/arules/arules.pdf

