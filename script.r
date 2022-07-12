#################
### Data Prep ###
#################
# Loading data
df = read.csv(file.choose())

# Converting target variables from original categories to 3-classes (Never/Occasional/Current use)
# tar1: Cigarette
df$tar1[df$SPUFF0A1 == 1] <- 1
df$tar1[df$SPUFF0A1 == 2] <- 0
df$tar1[df$SLST30A1 == 1] <- 1
df$tar1[df$SLST30A1 == 2] <- 2
df$tar1[df$SLST30A1 == 3] <- 2
df$tar1[df$SLST30A1 == 4] <- 2
df$tar1[df$SLST30A1 == 5] <- 2
df$tar1[df$SLST30A1 == 6] <- 2
df$tar1[df$SLST30A1 == 7] <- 2
df$tar1[df$SLST30A1 == 8] <- 2
df$tar1[df$SLST30A1 == 96] <- 0

# tar2: E-Cigarette
df$tar2[df$SUSECGA2 == 1] <- 1
df$tar2[df$SUSECGA2 == 2] <- 0
df$tar2[df$SECG30A1 == 1] <- 1
df$tar2[df$SECG30A1 == 2] <- 2
df$tar2[df$SECG30A1 == 3] <- 2
df$tar2[df$SECG30A1 == 4] <- 2
df$tar2[df$SECG30A1 == 5] <- 2
df$tar2[df$SECG30A1 == 6] <- 2
df$tar2[df$SECG30A1 == 7] <- 2
df$tar2[df$SECG30A1 == 8] <- 2

# tar3: Alcohol
df$tar3[df$ADRINKA1 == 1] <- 0
df$tar3[df$ADRINKA1 == 2] <- 1
df$tar3[df$ADRINKA1 == 3] <- 0
df$tar3[df$ADRINKA1 == 4] <- 1
df$tar3[df$ADRINKA1 == 5] <- 2
df$tar3[df$ADRINKA1 == 6] <- 2
df$tar3[df$ADRINKA1 == 7] <- 2
df$tar3[df$ADRINKA1 == 8] <- 2
df$tar3[df$ADRINKA1 == 9] <- 2
df$tar3[df$ADRINKA1 == 10] <- 2

# tar4: Marijuana
df$tar4[df$AOFTMJA2 == 1] <- 0
df$tar4[df$AOFTMJA2 == 2] <- 1
df$tar4[df$AOFTMJA2 == 3] <- 1
df$tar4[df$AOFTMJA2 == 4] <- 2
df$tar4[df$AOFTMJA2 == 5] <- 2
df$tar4[df$AOFTMJA2 == 6] <- 2
df$tar4[df$AOFTMJA2 == 7] <- 2
df$tar4[df$AOFTMJA2 == 8] <- 2
df$tar4[df$AOFTMJA2 == 9] <- 2

# Similar procedures for other covariates that need to combine original categories, e.g., weekly allowance

#############################
### Missing Data Analysis ###
#############################
# Loading required libraries
library(VIM)
library(FactoMineR)
library(missMDA)
library(naniar)

# Removing na rows for target variables
df4MI <- df[!is.na(df$tar1), ]
df4MI <- df4MI[!is.na(df4MI$tar2), ]
df4MI <- df4MI[!is.na(df4MI$tar3), ]
df4MI <- df4MI[!is.na(df4MI$tar4), ] 
dfNMV(na.omit(df4MI))

# EDA of missing data
res<-summary(aggr(dfNMV, sortVar=TRUE))$combinations # list variabless with missing values 
pct_miss(dfNMV) # percentage of missing values 
n_miss(dfNMV) # number of missing values 
n_var_miss(dfNMV) # number of variables that contain a missing value
n_case_miss(dfNMV) # number of cases that contain a missing value
n_complete(dfNMV) # without missing value
n_miss(dfNMV$BMI_Category) # number of missing value for BMI_Category
gg_miss_var(dfNMV) # plot of missing variables
gg_miss_case_cumsum(dfNMV) # cumulative sum of missing values for cases
gg_miss_upset(dfNMV, nsets = n_var_miss(dfNMV)) # number of missing values for each of the sets of data
matrixplot(dfNMV, sortby = 2) # matrix plot
vis_miss(dfNMV, sort_miss = TRUE, warn_large_data = FALSE) # ggplot of the missingness inside a dataframe

###########################
### Multiple Imputation ###
###########################
# Loading the library
library("mice")

# Coverting target variables to factors
df4MI$tar1      <- factor(df4MI$tar1)
df4MI$tar2      <- factor(df4MI$tar2)
df4MI$tar3      <- factor(df4MI$tar3)
df4MI$tar4      <- factor(df4MI$tar4)

# Defining imputation method for each covariate
imp <- mice(df4MI, m=1, maxit=0) # initiate MICE model
meth <- imp$method # initiate imputation method
meth[c("cov1")]="pmm" # for numeric variables
meth[c("cov2")]="logreg" # for factors (2 levels) 
meth[c("cov3")]="polyreg" # for factors (>2 levels) 
meth[c("cov4")]="polr" # for ordered variables (>2 levels) 
……

impMice <- mice(df4MI, m=5, maxit=50, method = meth, seed = 246, printFlag = TRUE, nnet.MaxNWts = 8000) # generate 5 imputed datasets, with 50 iterations for each imputed dataset
impdat <- complete(impMice,action="long",include = FALSE) # stack imputed datasets in long format, exclude the original data
pool_mean <- with(impdat, by(impdat, .imp, function(x) c(mean(x$cov1),sd(x$cov1)))) # compute mean and standard deviation in each imputed dataset
fit.t.test <- with(data=impMice, exp=lm(tar1+tar2+tar3+tar4 ~ cov1+cov2+cov3+...)) # conduct an independent t-test via lm in each imputed dataset
t.test.estimates <- pool(fit.t.test)

########################
### LASSO Regression ###
########################
# Loading required libraries
library(data.table)
library(ggplot2)       # plotting
library(glmnet)        # ridge, elastic net, and lasso 
library(glmnetcr)     # Fit a Penalized Constrained Continuation Ratio Model Using Lasso
or Elasticnet Regularization Via ’glmnet’

# Defining lambda_seq
lambda_seq <- 10^seq(1.2, -1.2, by = -.1)

# glmnet requires x matrix (of predictors) and vector (values for y)
x = model.matrix(tar1 + tar2 + tar3 + tar4 ~ ., data = dfLasso) # matrix of predictors      
y = dfLasso$tar1 + dfLasso$tar2 + dfLasso$tar3 + dfLasso$tar4 # vector y values 

# Splitting data into train and test
set.seed(168) # replicate results
train = sample(1:nrow(x), nrow(x)/2)
x_test = (-train)
y_test = y[x_test]
glmnet.fit <- glmnetcr(x_test, y_test) 
AIC <- select.glmnetcr(glmnet.fit, which="AIC") 
fitted(glmnet.fit, s=AIC) 
# Determine optimal value of lambda using 10-fold cross-validation
lasso_model <- cv.glmnet(x[train,], y[train],
                       alpha = 1, lambda = lambda_seq,
                       nfolds = 10) # alpha=1 is lasso

# Identifying best lambda
best_lambda_lasso <- lasso_model$lambda.1se # largest lambda in 1 SE
 
# Rebuilding the model with best lamda value identified
lasso_best <- glmnet(x_vars[train,], y_var[train], alpha = 1, lambda = best_lam)
pred <- predict(lasso_best, s = best_lam, newx = x_vars[x_test,])

final <- cbind(y_var[x_test], pred)
# Checking the first six obs
head(final)

# Inspecting beta coefficients
coef(lasso_best)
lasso_coef <- lasso_model$glmnet.fit$beta[,lasso_model$glmnet.fit$lambda  == best_lambda_lasso] # retrieve coefficients at lambda.1se

# Building coefficients table 
coef_l = data.table(lasso = lasso_coef)      # build table
coef_l[, feature := names(lasso_coef)]       # add feature names
to_plot_r = melt(coef_l                      # label table
               , id.vars='feature'
               , variable.name = 'model'
               , value.name = 'coefficient')
ggplot(data=to_plot_r,                       # plot coefficients
       aes(x=feature, y=coefficient, fill=model)) +
       coord_flip() +         
       geom_bar(stat='identity', fill='purple', color='blue') +
       facet_wrap(~ model) + guides(fill=FALSE)


#########################
### Dynamic Modelling ###
#########################
# Converting df from wide to long format
df_long <- reshape(df_wide, idvar = "ID", varying = list(2:4, 5:7, 8:10, 11:13, 14:16, 17:19, 20:22, 23:25, 26:28, 29:31, 32:34, 35:37, 38:40, 41:43, 44:46, 47:49, 50:52, 53:55, 56:58, 59:61, 62:64, 65:67, 68:70, 71:73, 74:76, 77:79, 80:82, 83:85, 86:88, 89:91, 92:94, 95:97, 98:100, 101:103, 104:106, 107:109, 110:112, 113:115), timevar="Time", v.names = c("Cigarette", "eCigarette", "Alcohol", "Marijuana", "HouseholdIncome", "Urbanity", "TotalPointsInterest", "DrinkingPlaces", "DrugStores", "LiquorStores", "TobaccoStores", "Province", "Grade", "Sex", "Race", "GetMoney", "TransportationToSchool", "PAfriends", "Breakfast", "SmokingFriends", "SupportQuitTobacco", "SupportQuitDrugAlcohol", "MathMarks", "EnglishMarks", "LikedEdu", "WillingEdu", "SkipClass", "BMI_Category", "SchoolConnectedness", "TotalDailySedentary", "SedentaryTime", "PAtime", "PA_level", "FLOURISH", "GAD7", "CESD", "DERS", "GambleOnline"), direction = "long")
df_long <- df_long[order(df_long$ID, df_long$Time), ]

# Moving responses to the end (per the LMest package, re-arrange cols to new seq: Time, ID, covariates…, responses)
df_long <- df_long[,c(2,1,7:40,3:6)] 

# Plotting longitudinal categorical data using longCatPlot pkg 
library("longCatEDA")
W=df[,-c(1:100,104:118)] # only keep PSU16/17/18 three cols
times <- c(1,2,3,4) 
fig <- longCat(W, times)
summary(fig)
par(mfrow=c(1,2), bg='cornsilk3')
longContPlot(W, times,
             ylim=c(1,4),
              main='Transition Curves', ylab='Use Pattern',
              xlab='Years')
longCatPlot(fig, lwd=2, xlab = "Years",
             main='Transition Patterns',
             colScheme='heat')
par(mfrow=c(1,1), bg='transparent')

# Loading required library for LMM
library("lmest")

# Building LMM with covariates from LASSO regression 
LMM_lasso20cov <- lmest(responsesFormula = tar1 + tar2 + tar3 + tar4 ~ NULL,
latentFormula =  ~Urbanity + DrugStores + GRADE + RACE + GetMoney + PAfriends + Breakfast + SmokingFriends + SupportQuitDrugAlcohol + EnglishMarks + WillingEdu + SkipClass + BMI_ACAT + SchoolConnectedness + SedentaryTime + PA_LEVEL + GAD7 + CESD + DERS + GambleOnline | Urbanity + DrugStores + GRADE + RACE + GetMoney + PAfriends + Breakfast + SmokingFriends + SupportQuitDrugAlcohol + EnglishMarks + WillingEdu + SkipClass + BMI_ACAT + SchoolConnectedness + SedentaryTime + PA_LEVEL + GAD7 + CESD + DERS + GambleOnline,
index = c("ID","Time"),
modBasic = 0,
data = df_long,
k = 2:7,
seed = 123,
paramLatent = "multilogit",
out_se=TRUE)

# Final LMM
LMM_final <- lmest(responsesFormula = tar1 + tar2 + tar3 + tar4 ~ NULL, 
latentFormula = ~Urbanity + GRADE + RACE + GetMoney + PAfriends + Breakfast + SmokingFriends + SupportQuitDrugAlcohol + SEX + SkipClass + BMI_ACAT + SchoolConnectedness + SedentaryTime + GambleOnline | Urbanity + GRADE + RACE + GetMoney + PAfriends + Breakfast + SmokingFriends + SupportQuitDrugAlcohol + SkipClass + BMI_ACAT + SchoolConnectedness + SEX + SedentaryTime + GambleOnline, 
data = df_long, 
index = c("ID", "Time"), 
k = 4, 
start = 1, 
modBasic = 1, 
seed = 123,
paramLatent = "multilogit", 
out_se = TRUE)

# Obtaining lk, BIC, and AIC values 
LMM_final$Lk
LMM_final$Bic
LMM_final$Aic

# Decoding 
deco1 <- lmestDecoding(LMM_final)
df_deco1 <- as.data.frame(deco1)
W <- df_deco1[,c(1:3)]


