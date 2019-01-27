args <- commandArgs(T)
print("need to confirm the name of pro same")

if(length(args) != 3){
        stop("Rscript xgboost.tune.R [pro1] [pro2] [prefix]
                pro1: train
                pro2: lable
                prefix: out")

}

# load libray
library(xgboost)
library(caret)

# load data
pro1 <- read.table(args[1], header=T, row.names=1, check.names=F, sep="\t")
pro2 <- read.table(args[2], header=T, row.names=1, check.names=F, sep="\t")
prefix <- args[3]

#
if(all(rownames(pro1)==rownames(pro2))){
        print("sample is OK!")
}else{
        print("match the sample ID!")
}

input_x <- pro1
input_y <- pro2[,1]


# function

tuneplot <- function(x, probs = .90) {
  ggplot(x) +
    coord_cartesian(ylim = c(quantile(x$results$RMSE, probs = probs), min(x$results$RMSE))) +
    theme_bw()
}



# step 1 number of lterations and the learning rate
set.seed(57)
nrounds <- 1000

tune_grid <- expand.grid(
  nrounds = seq(from = 100, to = nrounds, by = 50),
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(2, 3, 4,5, 6),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)
tune_control <- caret::trainControl(
  method = "cv", # cross-validation
  number = 5, # with n folds
  #index = createFolds(tr_treated$Id_clean), # fix the folds
  verboseIter = FALSE, # no training log
  allowParallel = TRUE # FALSE for reproducible results
)
xgb_tune <- caret::train(
  x = input_x,
  y = input_y,
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "xgbTree",
  verbose = TRUE
)

pdf(paste0(prefix, "step1.tune.pdf"), width = 7, height = 7)
tuneplot(xgb_tune)
dev.off()


print("step1 is done\n")
print(paste0("max_depth: ",xgb_tune$bestTune[, "max_depth"], "\n"))
print(paste0("eta: ",xgb_tune$bestTune[, "eta"], "\n"))

# step 2  Maximum Depth and Minimum Child Weight
tune_grid2 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = xgb_tune$bestTune$eta,
  max_depth = ifelse(xgb_tune$bestTune$max_depth == 2,
    c(xgb_tune$bestTune$max_depth:4),
    xgb_tune$bestTune$max_depth - 1:xgb_tune$bestTune$max_depth + 1),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = c(1, 2, 3),
  subsample = 1
)

xgb_tune2 <- caret::train(
  x = input_x,
  y = input_y,
  trControl = tune_control,
  tuneGrid = tune_grid2,
  method = "xgbTree",
  verbose = TRUE
)

pdf(paste0(prefix, "step2.tune.pdf"), width = 7, height = 7)
tuneplot(xgb_tune2)
dev.off()

print("step2 is done\n")
print(paste0("max_depth: ",xgb_tune2$bestTune[, "max_depth"], "\n"))
print(paste0("min_child_weight: ",xgb_tune2$bestTune[, "min_child_weight"], "\n"))

# step 3: Column and Row Sampling
tune_grid3 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = xgb_tune$bestTune$eta,
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = 0,
  colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = c(0.5, 0.75, 1.0)
)

xgb_tune3 <- caret::train(
  x = input_x,
  y = input_y,
  trControl = tune_control,
  tuneGrid = tune_grid3,
  method = "xgbTree",
  verbose = TRUE
)
pdf(paste0(prefix, "step3.tune.pdf"), width = 7, height = 7)
tuneplot(xgb_tune3, probs = .95)
dev.off()

print("step3 is done\n")
print(paste0("colsample: ",xgb_tune3$bestTune[, "colsample_bytree"], "\n"))
print(paste0("subsample: ",xgb_tune3$bestTune[, "subsample"], "\n"))


# step 4: gamma

tune_grid4 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = xgb_tune$bestTune$eta,
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
  colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = xgb_tune3$bestTune$subsample
)

xgb_tune4 <- caret::train(
  x = input_x,
  y = input_y,
  trControl = tune_control,
  tuneGrid = tune_grid4,
  method = "xgbTree",
  verbose = TRUE
)

pdf(paste0(prefix, "step4.tune.pdf"), width = 7, height = 7)
tuneplot(xgb_tune4)
dev.off()

print("step4 is done\n")
print(paste0("gamma: ",xgb_tune4$bestTune[, "gamma"], "\n"))

# step 5: Reducing the Learning Rate

tune_grid5 <- expand.grid(
  nrounds = seq(from = 100, to = 10000, by = 100),
  eta = c(0.01, 0.015, 0.025, 0.05, 0.1),
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = xgb_tune4$bestTune$gamma,
  colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = xgb_tune3$bestTune$subsample
)

xgb_tune5 <- caret::train(
  x = input_x,
  y = input_y,
  trControl = tune_control,
  tuneGrid = tune_grid5,
  method = "xgbTree",
  verbose = TRUE
)

pdf(paste0(prefix, "step5.tune.pdf"), width = 7, height = 7)
tuneplot(xgb_tune5)
dev.off()

print("step5 is done\n")
print(paste0("eta: ",xgb_tune4$bestTune[, "eta"], "\n"))


final_grid <- expand.grid(
  nrounds = xgb_tune5$bestTune$nrounds,
  eta = xgb_tune5$bestTune$eta,
  max_depth = xgb_tune5$bestTune$max_depth,
  gamma = xgb_tune5$bestTune$gamma,
  colsample_bytree = xgb_tune5$bestTune$colsample_bytree,
  min_child_weight = xgb_tune5$bestTune$min_child_weight,
  subsample = xgb_tune5$bestTune$subsample
)

xgb_model <- caret::train(
  x = input_x,
  y = input_y,
  trControl = tune_control,
  tuneGrid = final_grid,
  method = "xgbTree",
  verbose = TRUE
)

print("all done\n")
print(xgb_model$result)

# save the final model
save(xgb_model, file = paste0(prefix, ".xgboost.RData"))
