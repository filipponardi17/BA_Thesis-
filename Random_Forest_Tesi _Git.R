setwd('DIRECTORY')

load("DIRECTORY/WVSurvey_reduced_with_ps.rda")

W7_no = W7R[which(W7R$cntry_name == "Norway"), ]
W7_no$cntry_name = NULL

table(W7_no$E181_EVS5)

# Replace NA with average

# for (i in 1:dim(W7_no)[2]){
mn = colMeans(W7_no[,], na.rm = T) # la prima colonna e' il partito politico
for (i in 2:dim(W7_no)[2]){
  i0 = which(is.na(W7_no[,i]))
  W7_no[i0,i] = mn[i]
}

# summary(W7_no[[2]])  # controllo il rimpiazzo NA



#57801 NO: Labour Party			LIB
#57802 NO: Democrats in Norway		NA
#57803 NO: Progress Party			POP
#57804 NO: Conservative Party		LIB
#57805 NO: Christian Democratic Party	LIB
#57806 NO: Coastal Party			NA	
#57807 NO: Green Party			LIB
#57808 NO: The Christians			NA
#57809 NO: Pensioners' Party			NA
#57810 NO: Red Party				POP 
#57811 NO: Centre Party			POP
#57812 NO: Socialist Le􏰀 Party		LIB
#57813 NO: Liberal Party			LIB
#57814 NO: Other, please specify (WRITE IN)
#57866 NO: No party appeals to me (spontanious)





n = dim(W7_no)[1]
W7_no$pop_vote = rep("lib", n)

i1_0 = which(is.na(W7_no$E181_EVS5))
i1_1 = which(W7_no$E181_EVS5 == 57866)
i1_2 = which(W7_no$E181_EVS5 == 57814)
i1_3 = which(W7_no$E181_EVS5 == 57809)
i1_4 = which(W7_no$E181_EVS5 == 57808)
i1_5 = which(W7_no$E181_EVS5 == 57806)
i1_6 = which(W7_no$E181_EVS5 == 57802)
W7_no$pop_vote[i1_0] = NA
W7_no$pop_vote[i1_1] = NA
W7_no$pop_vote[i1_2] = NA
W7_no$pop_vote[i1_3] = NA
W7_no$pop_vote[i1_4] = NA
W7_no$pop_vote[i1_5] = NA
W7_no$pop_vote[i1_6] = NA

ip_1 =which(W7_no$E181_EVS5 == 57811)
ip_2 =which(W7_no$E181_EVS5 == 57810)
ip_3 =which(W7_no$E181_EVS5 == 57803)
W7_no$pop_vote[ip_1] = "pop"
W7_no$pop_vote[ip_2] = "pop"
W7_no$pop_vote[ip_3] = "pop"

W7_no$pop_vote = as.factor(W7_no$pop_vote)

table(W7_no$E181_EVS5)
table(W7_no$pop_vote, useNA = "always")



# Random Forest

i0 = which(is.na(W7_no$pop_vote))
WnoR = W7_no[-i0, ]
WnoR$E181_EVS5 <- NULL
names(WnoR)
dim(WnoR)

# Calculate the size of each of the data sets:
nrow(WnoR)


#import the package
library(randomForest)
library(randomForestExplainer)
# Perform training:
set.seed(17)
norf_classifier = randomForest(pop_vote ~ ., data=WnoR, # na.action = na.roughfix,
                             proximity = T,
                             ntree=500, mtry=4, importance=TRUE)

norf_classifier
plot(norf_classifier, main="Training Norway")
importance_tr_nor = importance(norf_classifier)
importance_tr_nor
varImpPlot(norf_classifier, n.var=15, main="Training Norway: 15 variables")

min_depth_frame_tr_nor <- min_depth_distribution(norf_classifier)
plot_min_depth_distribution(min_depth_frame_tr_nor, mean_sample = "relevant_trees", k = 5, main="Training Norway: min and mean depth")

importance_frame_training_tr_nor <- measure_importance(norf_classifier)

set.seed(18)
vnor = pop_vote ~ c_eu1 + city_siz + dmc_cnt + c_gvr + job_ntn + pol_rad + imp_imm + 
  cq_obd + js_div + incm_lvl
norf_classifier_1 = randomForest(vnor, data = WnoR,
                               proximity = T,
                               ntree=500, mtry=2, localImp=TRUE)


norf_classifier_1
plot(norf_classifier_1)
importance_nor = importance(norf_classifier_1)
importance_nor 

varImpPlot(norf_classifier_1, main = "Norway: Important predictors")
mrg = margin(norf_classifier_1, sort = T)
mrg
plot(margin(norf_classifier_1), main = "Classified vs Misclassified", xlab = "Voters")
abline(a = 0, b = 0)

importance_frame_nor <- measure_importance(norf_classifier_1) # questo calcola i p-value
save(importance_frame_nor, file = "DIRECTORY/norway_importance_frame.rda")
load("DIRECTORY/norway_importance_frame_nor.rda")
importance_frame_nor


min_depth_frame_nor <- min_depth_distribution(norf_classifier_1)
save(min_depth_frame_nor, file = "DIRECTORY/norway_min_depth_frame.rda")
load("DIRECTORY/norway_min_depth_frame.rda")
head(min_depth_frame_nor, n = 100)
str(min_depth_frame_nor)

# plot_min_depth_distribution(forest) # gives the same result as below but takes longer
plot_min_depth_distribution(min_depth_frame_nor)
plot_min_depth_distribution(min_depth_frame_nor, mean_sample = "relevant_trees")
plot_min_depth_distribution(min_depth_frame_nor, mean_sample = "relevant_trees", k = 5)

tapply(WnoR$c_eu1, WnoR$pop_vote, FUN = mean, na.rm = T)
tapply(WnoR$city_siz, WnoR$pop_vote, FUN = mean, na.rm = T)
tapply(WnoR$dmc_cnt, WnoR$pop_vote, FUN = mean, na.rm = T)
tapply(WnoR$c_gvr, WnoR$pop_vote, FUN = mean, na.rm = T)
tapply(WnoR$job_ntn, WnoR$pop_vote, FUN = mean, na.rm = T)
tapply(WnoR$pol_rad, WnoR$pop_vote, FUN = mean, na.rm = T)
tapply(WnoR$imp_imm, WnoR$pop_vote, FUN = mean, na.rm = T)
tapply(WnoR$cq_obd, WnoR$pop_vote, FUN = mean, na.rm = T)
tapply(WnoR$js_div, WnoR$pop_vote, FUN = mean, na.rm = T)
tapply(WnoR$incm_lvl, WnoR$pop_vote, FUN = mean, na.rm = T)

## Now let's create an MDS-plot to show how the samples are related to each other

distance.matrix <- as.dist(1-norf_classifier_1$proximity)

mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)

mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2],
                       Status=WnoR$pop_vote)
library(ggplot)
library(ggplot2)

ggplot(data=mds.data, aes(x=X, y=Y, label=Sample)) + 
  geom_text(aes(color=Status)) +
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle("MDS plot using (1 - Random Forest Proximities Norway)")

# ggsave(file="random_forest_mds_plot.pdf")


######################
#    HUNGARY        #
#####################


W7_hu = W7R[which(W7R$cntry_name == "Hungary"), ]
W7_hu$cntry_name = NULL
sum(is.na(W7_hu$E181_EVS5)) #486 NA ancora prima di far nulla su 1514 osservazioni
nrow(W7_hu) #1514 osservazioni
table(W7_hu$E181_EVS5) # 34802 --> 536 & 34804 --> 128 unici due sopra i 100

# Replace NA with average

# for (i in 1:dim(W7_no)[2]){
mn = colMeans(W7_hu[,], na.rm = T) # la prima colonna e' il partito politico
for (i in 2:dim(W7_hu)[2]){
  i0 = which(is.na(W7_hu[,i]))
  W7_hu[i0,i] = mn[i]
}

# summary(W7_no[[2]])  # controllo il rimpiazzo NA


#34801 HU: Hungarian Socialist Party (MSZP)			LIB
#34802 HU: Fidesz							POP
#34803 HU: Christian Democratic People's Party (KDNP)		POP
#34804 HU: Movement for a Better Hungary (Jobbik)		POP
#34805 HU: Politics Can Be Different (LMP)				LIB
#34806 HU: Democratic Coalition (DK)				LIB
#34807 HU: Together – Party for a New Era (Együtt)			NA	
#34808 HU: Dialogue for Hungary (PM)				LIB
#34809 HU: Momentum Movement (Momentum)			LIB
#34810 HU: Hungarian Liberal Party (Liberálisok)			NA
#34811 HU: Hungarian Two-tailed Dog Party (MKKP)		NA
#34812 HU: Hungarian Workers' Party (Munkáspárt)		NA
#34826 HU: Other, please specify (WRITE IN)				NA
#34866 HU: No [no other] party appeals to me (spontaneous)	NA





n = dim(W7_hu)[1]
W7_hu$pop_vote = rep("lib", n)

i1_0 = which(is.na(W7_hu$E181_EVS5))
i1_1 = which(W7_hu$E181_EVS5 == 34807)
i1_2 = which(W7_hu$E181_EVS5 == 34810)
i1_3 = which(W7_hu$E181_EVS5 == 34811)
i1_4 = which(W7_hu$E181_EVS5 == 34812)
i1_5 = which(W7_hu$E181_EVS5 == 34826)
i1_6 = which(W7_hu$E181_EVS5 == 34866)
W7_hu$pop_vote[i1_0] = NA
W7_hu$pop_vote[i1_1] = NA
W7_hu$pop_vote[i1_2] = NA
W7_hu$pop_vote[i1_3] = NA
W7_hu$pop_vote[i1_4] = NA
W7_hu$pop_vote[i1_5] = NA
W7_hu$pop_vote[i1_6] = NA

ip_1 =which(W7_hu$E181_EVS5 == 34802)
ip_2 =which(W7_hu$E181_EVS5 == 34803)
ip_3 =which(W7_hu$E181_EVS5 == 34804)
W7_hu$pop_vote[ip_1] = "pop"
W7_hu$pop_vote[ip_2] = "pop"
W7_hu$pop_vote[ip_3] = "pop"

W7_hu$pop_vote = as.factor(W7_hu$pop_vote)

table(W7_hu$E181_EVS5)
table(W7_hu$pop_vote, useNA = "always")



# Random Forest

i0 = which(is.na(W7_hu$pop_vote))
WhuR = W7_hu[-i0, ]
WhuR$E181_EVS5 <- NULL
names(WhuR)
dim(WhuR)

# Calculate the size of each of the data sets:
nrow(WhuR)

#import the package
library(randomForest)
library(randomForestExplainer)
# Perform training:
set.seed(20)
hurf_classifier = randomForest(pop_vote ~ ., data=WhuR, # na.action = na.roughfix,
                               proximity = T,
                               ntree=500, mtry=4, importance=TRUE)


hurf_classifier
plot(hurf_classifier, main="Training Hungary")
varImpPlot(hurf_classifier, n.var=15, main="Training Hungary: 15 variables")
importance_tr_hur = importance(hurf_classifier)
importance_tr_hur


min_depth_frame_tr_hur <- min_depth_distribution(hurf_classifier)
plot_min_depth_distribution(min_depth_frame_tr_hur, mean_sample = "relevant_trees", k = 5, main="Training Hungary: min and mean depth")

importance_frame_tr_hur <- measure_importance(hurf_classifier)
importance_frame_tr_hur

set.seed(21)
vhur = pop_vote ~ pol_sp + ps_sat + dmc_cnt + c_gvr + c_eu1 + c_onu + imp_imm +
  post_mat + c_prl + incm_lvl
hurf_classifier_1 = randomForest(vhur, data = WhuR,
                                 proximity = T,
                                 ntree=500, mtry=2, localImp=TRUE)

hurf_classifier_1
plot(hurf_classifier_1)
imp.hungary = importance(hurf_classifier_1)

varImpPlot(hurf_classifier_1, main = "Hungary: Important predictors")
mrg = margin(hurf_classifier_1, sort = T)
mrg
plot(margin(hurf_classifier_1), main = "Classified vs Misclassified", xlab = "Voters")
abline(a = 0, b = 0)

importance_frame_hur <- measure_importance(hurf_classifier_1) # questo calcola i p-value
save(importance_frame_hur, file = "DIRECTORY/hungary_importance_frame.rda")
load("DIRECTORY/hungary_importance_frame.rda")
importance_frame_hur


min_depth_frame_hur <- min_depth_distribution(hurf_classifier_1)
save(min_depth_frame_hur, file = "DIRECTORY/hungary_min_depth_frame.rda")
load("DIRECTORY/hungary_min_depth_frame.rda")
head(min_depth_frame_hur, n = 100)
str(min_depth_frame_hur)

# plot_min_depth_distribution(forest) # gives the same result as below but takes longer
plot_min_depth_distribution(min_depth_frame_hur)
plot_min_depth_distribution(min_depth_frame_hur, mean_sample = "relevant_trees")
plot_min_depth_distribution(min_depth_frame_hur, mean_sample = "relevant_trees", k = 5)

tapply(WhuR$pol_sp, WhuR$pop_vote, FUN = mean, na.rm = T)
tapply(WhuR$ps_sat, WhuR$pop_vote, FUN = mean, na.rm = T)
tapply(WhuR$dmc_cnt, WhuR$pop_vote, FUN = mean, na.rm = T)
tapply(WhuR$c_gvr, WhuR$pop_vote, FUN = mean, na.rm = T)
tapply(WhuR$c_eu1, WhuR$pop_vote, FUN = mean, na.rm = T)
tapply(WhuR$c_onu, WhuR$pop_vote, FUN = mean, na.rm = T)
tapply(WhuR$imp_imm, WhuR$pop_vote, FUN = mean, na.rm = T)
tapply(WhuR$post_mat, WhuR$pop_vote, FUN = mean, na.rm = T)
tapply(WhuR$c_prl, WhuR$pop_vote, FUN = mean, na.rm = T)
tapply(WhuR$incm_lvl, WhuR$pop_vote, FUN = mean, na.rm = T)

## Now let's create an MDS-plot to show how the samples are related to each other.
distance.matrix <- as.dist(1-hurf_classifier_1$proximity)

mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)

mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2],
                       Status=WhuR$pop_vote)
library(ggplot)
library(ggplot2)

ggplot(data=mds.data, aes(x=X, y=Y, label=Sample)) + 
  geom_text(aes(color=Status)) +
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle("MDS plot using (1 - Random Forest Proximities Hungary)")

# ggsave(file="random_forest_mds_plot.pdf")

######################
#    GERMANY         #
#####################

W7_ge = W7R[which(W7R$cntry_name == "Germany"), ]
W7_ge$cntry_name = NULL

table(W7_ge$E181_EVS5)

# Replace NA with average

# for (i in 1:dim(W7_no)[2]){
mn = colMeans(W7_ge[,], na.rm = T) # la prima colonna e' il partito politico
for (i in 2:dim(W7_ge)[2]){
  i0 = which(is.na(W7_ge[,i]))
  W7_ge[i0,i] = mn[i]
}




#27601 DE: Christian Democratic Party/Christian Social Union		LIB
#27602 DE: German Social-Democratic Party					LIB
#27603 DE: German Liberal Party						LIB
#27604 DE: The Green Party							LIB
#27605 DE: The Left								POP
#27606 DE: Alternative for Germany						POP
#27607 DE: Other, please specify (WRITE IN)					NA
#27666 DE: No [no other] party appeals to me (spontaneous)		NA


n = dim(W7_ge)[1]
W7_ge$pop_vote = rep("lib", n)

i1_0 = which(is.na(W7_ge$E181_EVS5))
i1_1 = which(W7_ge$E181_EVS5 == 27666)
i1_2 = which(W7_ge$E181_EVS5 == 27607)
W7_ge$pop_vote[i1_0] = NA
W7_ge$pop_vote[i1_1] = NA
W7_ge$pop_vote[i1_2] = NA

ip_1 =which(W7_ge$E181_EVS5 == 27605)
ip_2 =which(W7_ge$E181_EVS5 == 27606)
W7_ge$pop_vote[ip_1] = "pop"
W7_ge$pop_vote[ip_2] = "pop"

W7_ge$pop_vote = as.factor(W7_ge$pop_vote)

table(W7_ge$E181_EVS5)
table(W7_ge$pop_vote, useNA = "always")



# Random Forest

i0 = which(is.na(W7_ge$pop_vote))
WgeR = W7_ge[-i0, ]
WgeR$E181_EVS5 <- NULL
names(WgeR)
dim(WgeR)

# Calculate the size of each of the data sets:
nrow(WgeR)
#data_set_size <- floor(0.9*nrow(WnoR))
#data_set_size <- floor(nrow(WnoR))

# Generate a random sample of "data_set_size" indexes
#indexes <- sample(1:nrow(WnoR), size = data_set_size)

# Assign the data to the correct sets
#training <- WnoR[indexes,]
# validation1 <- WnoR[-indexes,]

#import the package
library(randomForest)
library(randomForestExplainer)
# Perform training:
set.seed(23)
gerf_classifier = randomForest(pop_vote ~ ., data=WgeR, # na.action = na.roughfix,
                               proximity = T,
                               ntree=500, mtry=4, importance=TRUE)
# vedi anche parametro: localImp = TRUE

gerf_classifier
plot(gerf_classifier, main="Training Germany")
importance_tr_ger = importance(gerf_classifier) 
importance_tr_ger

min_depth_frame_tr_ger <- min_depth_distribution(gerf_classifier)
plot_min_depth_distribution(min_depth_frame_tr_ger, mean_sample = "relevant_trees", k = 5, main="Training Germany: min and mean depth")
varImpPlot(gerf_classifier, n.var=15, main = "Training Germany: 15 variables")
importance_frame_tr_ger <- measure_importance(gerf_classifier)
importance_frame_tr_ger





set.seed(24)
vger = pop_vote ~ pol_rad + c_gvr + imp_imm + dmc_cnt + pol_sp + c_eu1 + c_prl + job_ntn +
  ps_sat + incm_lvl

gerf_classifier_1 = randomForest(vger, data = WgeR,
                                 proximity = T,
                                 ntree=500, mtry=2, localImp=TRUE)


gerf_classifier_1
plot(gerf_classifier_1)
importance_ger = importance(gerf_classifier_1)
importance_ger


varImpPlot(gerf_classifier_1, main = "Germany: Important predictors")
mrg = margin(gerf_classifier_1, sort = T)
mrg
plot(margin(gerf_classifier_1), main = "Classified vs Misclassified", xlab = "Voters")
abline(a = 0, b = 0)

importance_frame_ger <- measure_importance(gerf_classifier_1) # questo calcola i p-value
save(importance_frame_ger, file = "DIRECTORY/importance_frame_ger.rda")
load("DIRECTORY/importance_frame_ger.rda")
importance_frame_ger


min_depth_frame_ger <- min_depth_distribution(gerf_classifier_1)
save(min_depth_frame_ger, file = "DIRECTORY/germany_min_depth_frame.rda")
load("DIRECTORY/germany_min_depth_frame.rda")
head(min_depth_frame_ger, n = 100)
str(min_depth_frame_ger)

# plot_min_depth_distribution(forest) # gives the same result as below but takes longer
plot_min_depth_distribution(min_depth_frame_ger)
plot_min_depth_distribution(min_depth_frame_ger, mean_sample = "relevant_trees")
plot_min_depth_distribution(min_depth_frame_ger, mean_sample = "relevant_trees", k = 5)

tapply(WgeR$pol_rad, WgeR$pop_vote, FUN = mean, na.rm = T)
tapply(WgeR$c_gvr, WgeR$pop_vote, FUN = mean, na.rm = T)
tapply(WgeR$imp_imm, WgeR$pop_vote, FUN = mean, na.rm = T)
tapply(WgeR$dmc_cnt, WgeR$pop_vote, FUN = mean, na.rm = T)
tapply(WgeR$pol_sp, WgeR$pop_vote, FUN = mean, na.rm = T)
tapply(WgeR$c_eu1, WgeR$pop_vote, FUN = mean, na.rm = T)
tapply(WgeR$c_prl, WgeR$pop_vote, FUN = mean, na.rm = T)
tapply(WgeR$job_ntn, WgeR$pop_vote, FUN = mean, na.rm = T)
tapply(WgeR$ps_sat, WgeR$pop_vote, FUN = mean, na.rm = T)
tapply(WgeR$incm_lvl, WgeR$pop_vote, FUN = mean, na.rm = T)


## Now let's create an MDS-plot to show how the samples are related to each other.
distance.matrix <- as.dist(1-gerf_classifier_1$proximity)

mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)


mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2],
                       Status=WgeR$pop_vote)
library(ggplot)
library(ggplot2)

ggplot(data=mds.data, aes(x=X, y=Y, label=Sample)) + 
  geom_text(aes(color=Status)) +
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle("MDS plot using (1 - Random Forest Proximities Germany)")


#THE END