opt_des <- doptAntoine(A = 8.07131, B = 1730.63, C = 233.426, 1, 100)

sens_opt <- dsensAntoine( 8.07131, 1730.63, 233.426, dmatrixAntoine(8.07131, 1730.63, 233.426, opt_des))
sens_min <- findminval(sens_opt, 1, 100, 1000)

delta <- 1/3
bounds <- delta_bound(delta, 3, sens_min)

deff <- 0.85
  # Puntos de corte y valor del corte
  cross <- sort(crosspoints(deff, delta, 3, sens_opt, 1000, 10^(-3), 1, 100))
  val <- sens_val_to_add(deff, delta, 3)
  # Obtener start y par para tener las regiones
  start <- getStart(cross, 1, 100, val, sens_opt)
  par <- getPar(cross)
  fac_reg_opt <- getCross2(cross, 1, 100, start, par)
  
  sens_new <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + theme_ipsum() + xlim(1, 100) + labs(x = "Temperature (ºC)", y = "Pressure")
  
  # Loop para pintar las regiones factibles
  for(i in 1:(length(fac_reg_opt)/2)){
    loop_input = paste("geom_segment(aes(x=",fac_reg_opt[2*i-1],",xend=",fac_reg_opt[2*i],",y=0,yend=0), color = 'green3')", sep="")
    sens_new <- sens_new + eval(parse(text=loop_input))  
  }
  opt_des_data <- data.frame("Point" = opt_des[["Point"]], "Value" = rep(0, length(opt_des[["Point"]])))
  sens_new <- sens_new + geom_point(data = opt_des_data, aes(x = Point, y = Value), shape = 16, size = 4, color = "steelblue3")
  sens_res <- sens_new
  
  x_val <- seq(1, 100, length.out = 10000)
  eff <- function(x){
    return((1-delta)*(1+delta*sens_opt(x)/(1-delta))^(1/3))
  }
  y_val <- purrr::map_dbl(x_val, eff)
  
  
  p <- ggplot() +  geom_line(mapping = aes(x = x_val, y = y_val), color = "steelblue3") + 
    geom_hline(yintercept =  eff(cross[1]), color = "goldenrod3") + 
    xlim(1, 100) + 
    labs(x = "Temperature", y = "Efficiency")+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  p
  
  for(i in 1:(length(fac_reg_opt)/2)){
    loop_input = paste("geom_segment(aes(x=",fac_reg_opt[2*i-1],",xend=",fac_reg_opt[2*i],",y=eff(cross[1]),yend=eff(cross[1])), size = 1.5, color = 'green3')", sep="")
    p <- p + eval(parse(text=loop_input))  
  }
  
  p
  
  # falta bucle para hacer los vectores
  values <- map_dbl(cross, eff)
  opt_des_data <- data.frame("Point" = cross, "Values" = values)
  ggplot() +
    geom_point(data = opt_des_data, aes(x = Point, y = Values), shape = 16, size = 3, color = "darkred")
  
  p + geom_point(data = opt_des_data, mapping = aes(x = Point, y = Values), shape = 16, size = 3, color = "darkred")
  
  p <- p + geom_point(data = opt_des_data, aes(x = Point, y = Values), shape = 16, size = 3, color = "firebrick3")
  p
  
  p+geom_segment(aes(x = 1, xend = 1, y = bounds[1], yend = bounds[2]), col = "mediumpurple2", size = 1.5)
  
  eff <- (1-delta)*(1+delta*sens(x)/(1-delta))^(1/3)
  
  colors()
  
  
  