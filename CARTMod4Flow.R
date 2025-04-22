CARTMod4Flow <- function(flowdata_gage, sst_pcs, gagename)
{
  cart <- rpart(flowdata_gage ~., data = sst_pcs, method = "anova")
  
  cartplot <- rpart.plot(cart, type = 3, extra = 101, under = TRUE, 
                         tweak = 1.2, 
                         main = paste("CART on Annual Spring Streamflow for", 
                                      gagename, 
                                      "\nUsing Winter SST PCs (4) as Covariates"))
  return(list(
    mod = cart,
    plot = cartplot))
}
