#creates multiple frequency bar charts

library(ggplot2)
multbarcharts <- function(mydata, x_var) {
  bp <- ggplot(mydata, aes_(as.name(x_var))) +
    geom_bar(stat = 'count')
  print(bp)
}
x_vars <- names(ints)
x_vars

mybarcharts <- list()
for(i in seq_along(x_vars)){
  mybarcharts[[i]] <- multbarcharts(mydata = ints, x_var = x_vars[i])$plot
}