modeval <- function(actvals, predvals){
# Calculate Correlation and RMSE
corrskill = 1:length(actvals)
rmseskill = 1:length(actvals )
  
for (i in 1:length(actvals)){
    mean = mean(actvals)
    sd = sd(actvals)
    corrskill[i] = cor(actvals[i],predvals[i])
    rmseskill[i] = sqrt(mean((actvals[i] - predvals[i])^2))
}

# Boxplot the Correlation and RMSE
corrplot <- ggplot()+
  geom_boxplot(mapping = aes(x = "Correlation", corrskill))+
  labs(title = "Corr.", x = "", y = "")+
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5))+
  coord_cartesian(ylim = c(-0.5,2))

rmseplot <- ggplot()+
  geom_boxplot(mapping = aes(x = "RMSE", rmseskill))+
  labs(title = "RMSE", x = "", y = "")+
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5))+
  coord_cartesian(ylim = c(0,5000))

return(list(corrplot = corrplot, rmseplot = rmseplot))
}