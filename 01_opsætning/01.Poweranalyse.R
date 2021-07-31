library(devtools)
library(cjpowR)
library(plotly)

df = cjpowr_amce(amce = 0.03, power = 0.8, levels = 5)

df$n/(2*8)

 cjpowr_amce_vec <- Vectorize(cjpowr_amce)
 d <- expand.grid(amce = c(0.02, 0.03, 0.05, 0.1),n = seq(from = 100, to = 20000, length.out = 1000))
 df <- t(cjpowr_amce_vec(amce = d$amce, n = d$n, sims = 100000, levels = 5, alpha = 0.05, delta0 = 0.5))
 df <- data.frame(df)
 df[] <- lapply(df, unlist)
 
 ggplot(df, aes(x = n, y = exp_typeM, color = amce)) +
   geom_point()

 plot_ly(df, x = ~n, y = ~exp_typeM, type = 'scatter', mode = 'lines', linetype = ~amce, color = I('black')) %>%
  layout(
    xaxis = list(title = "Effektiv stikprøve størelse",
                 zeroline = F),
    yaxis = list(title = "Exaggeration ratio (Overdrivelsesratio)",
                 range = c(0,10),
                 zeroline = F),
    legend=list(title=list(text='<b> AMCE </b>'))
  ) 
 
 sf = cjpowr_amce(amce = 0.03, power = 0.8, levels = 5)
 
 sf$n/(2*8)
 