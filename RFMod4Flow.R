RFMod4Flow <- function(flowpcs, sstpcs)
{
  # Fit a Random Forest Model
  rf_mod <- randomForest(flowpcs ~., data = sstpcs, ntree = 1000, mtry = 5,
                                 importance = TRUE)
  
  # Predict off the Random Forest Model
  pred_vals <- predict(rf_mod, newdata = sstpcs)
  
  return(list(mod = rf_mod, preds = pred_vals))
}