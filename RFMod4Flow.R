RFMod4Flow <- function(flowpcs, sstpcs)
{
  # Fit a Random Forest Model
  rf_mod <- randomForest(x = sstpcs, y = flowpcs, ntree = 1000, mtry = ncol(sstpcs),
                                 importance = TRUE)
  
  # Predict off the Random Forest Model
  pred_vals <- predict(rf_mod, newdata = sstpcs)
  
  return(list(mod = rf_mod, preds = pred_vals))
}