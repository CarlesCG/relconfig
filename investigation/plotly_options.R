df <- tibble(x = runif(50), y = x ^ 2)
plot_ly(data = df, x=~x, y=~y, type = "scatter") %>% 
        config(displayModeBar=FALSE) 
