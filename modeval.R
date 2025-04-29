modeval <- function(actvals, predvals){
# Calculate Correlation and RMSE
n <- length(actvals)
corrskill <- numeric(n)
rmseskill <- numeric(n)
  
for (i in 1:n){
  corrskill[i] <- cor(actvals[i], predvals[i])
  rmseskill[i] <- sqrt(mean((actvals[i] - predvals[i])^2))
}

# Create data frames for plotting
corr_df <- data.frame(metric = "Correlation", value = corrskill)
rmse_df <- data.frame(metric = "RMSE", value = rmseskill)

  
# Boxplot the Correlation and RMSE
corrplot <- ggplot(corr_df, aes(x = metric, y = value)) +
  geom_boxplot() +
  labs(title = "Corr.", x = "", y = "")+
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5))+
  coord_cartesian(ylim = c(-0.5,2))

rmseplot <- ggplot(rmse_df, aes(x = metric, y = value)) +
  geom_boxplot() +
  labs(title = "RMSE", x = "", y = "")+
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5))+
  coord_cartesian(ylim = c(0,5000))

return(list(corrplot = corrplot, rmseplot = rmseplot))
}