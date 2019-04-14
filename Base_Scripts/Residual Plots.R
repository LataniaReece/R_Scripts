theme_update(plot.title = element_text(hjust = 0.5)) 

res_full <- ggplot(data = full_model, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")

his_full <-ggplot(data = full_model, aes(x = .resid)) +
  geom_histogram(binwidth = 0.05) +
  xlab("Residuals")+
  ggtitle("Full Model")

qqnorm(full_model$residuals)
qqline(full_model$residuals)