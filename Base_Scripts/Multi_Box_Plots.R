multBoxPlots <- function(mydata, x_var, y_var) {
  bp <- ggplot(mydata, aes_(as.name(x_var), as.name(y_var))) +
    geom_boxplot()
  print(bp)
}
x_vars <- names(cat_vars[,1:4])
x_vars

mybplots <- list()
for(i in seq_along(x_vars)){
  mybplots[[i]] <- multBoxPlots(mydata = cat_vars, x_var = x_vars[i], y_var = "chanceofadmit")$plot
}
