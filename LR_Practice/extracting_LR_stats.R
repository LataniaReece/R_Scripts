models <- list(full_model, m1, m2, m3, m4, m5, m6)

rsquare_values <- map(models, function(x) {
  rsquares <- cbind(base::summary(x)$r.squared)
})

adj_rsquare_values <- map(models, function(x) {
  adj.rsquares <- cbind(base::summary(x)$adj.r.squared)
})

p_values <- map(models, function(x) {
  pvalue <- cbind(broom::glance(x)$p.value)
})

f_stat <- map(models, function(x) {
  fstat <- cbind(broom::glance(x)$statistic)
})

f_stat

glance(m1)$statistic


models_sum <- data.frame(model = c("Full Model", "M1", "M2", "M3", "M4", "M5", "M6"), 
                         r.squares = matrix(unlist(rsquare_values)),
                         adj.r.squares =matrix(unlist(adj_rsquare_values,)),
                         p.values = matrix(unlist(p_values)),
                         f.stat = matrix(unlist(f_stat)))

ggplot(data = models_sum, aes(x = model, y = r.squares))+
  geom_bar(stat = "identity")

ggplot(data = models_sum, aes(x = model, y = adj.r.squares))+
  geom_bar(stat = "identity")

ggplot(data = models_sum, aes(x = model, y = p.values))+
  geom_bar(stat = "identity")

ggplot(data = models_sum, aes(x = model, y = f.stat))+
  geom_bar(stat = "identity")
