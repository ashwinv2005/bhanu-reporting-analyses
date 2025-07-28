animate_rainfall  = function(datapath, season, col1 = "#0072B2", col2 = "#D55E00", 
                             background = "white")
{
  # create animated graphs
  
  library(tidyverse)
  library(gganimate)
  library(av)
  
  bng.rain = read.csv(datapath)
  bng.rain$Summer = bng.rain$March + bng.rain$April + bng.rain$May
  bng.rain$Monsoon = bng.rain$June + bng.rain$July + bng.rain$August
  bng.rain$Autumn = bng.rain$September + bng.rain$October + bng.rain$November
  bng.rain$Winter = bng.rain$December + bng.rain$January + bng.rain$February
  
  bng.rain = bng.rain %>%
    pivot_longer(cols = -c("year","elnino","lanina"), names_to = "month", values_to = "rain")
  nin = bng.rain %>% distinct(year,elnino,lanina)
  
  years = sort(unique(bng.rain$year))
  moving_window = list()
  
  len = 10
  
  for (i in len:length(years))
  {
    window = years[(i-len+1):i]
    moving_window[[i-len+1]] = window
  }
  
  for (i in 1:length(moving_window))
  {
    sm = bng.rain %>%
      filter(year %in% unlist(moving_window[i])) %>%
      group_by(month) %>%
      reframe(mean = mean(na.omit(rain)),
              sd = sd(na.omit(rain))) %>%
      mutate(year = max(unlist(moving_window[i])))
    
    if (i == 1)
      df = sm
    if (i > 1)
      df = df %>% bind_rows(sm)
  }
  
  df = df %>%
    pivot_longer(cols = -c("year","month"), names_to = "measure", values_to = "value") %>%
    dplyr::select(year,month,measure,value) %>%
    mutate(month = factor(month, levels = c("January","February","March","April","May","June",
                                            "July","August","September","October","November","December",
                                            "Summer","Monsoon","Autumn","Winter",
                                            "Total")))
  
  library(extrafont)
  

  
  # animated graph - specify season = "Total","October",etc.
  
  
  df_filtered = df %>% filter(month == season, measure == "mean")
  
  # Create a cumulative dataset
  years <- unique(df_filtered$year)
  
  # Create a new data frame to store cumulative values
  df_cumulative <- data.frame(year = integer(), value = numeric())
  
  for (year in years) {
    current_values <- df_filtered$value[df_filtered$year <= year]
    # Add the cumulative values for the current year
    df_cumulative <- rbind(df_cumulative, data.frame(year = rep(year, length(current_values)), value = current_values))
  }
  
  # Create the horizontal lines data
  hline_data_1 = data.frame(
    year = c(min(df$year)),
    value = c(
      df$value[df$year == min(df$year) & df$measure == "mean" & df$month == season]
    )
  )
  
  hline_data_2 = data.frame(
    year = c(min(df$year)),
    value = c(
      df$value[df$year == max(df$year) & df$measure == "mean" & df$month == season]
    )
  )
  
  loess_model = loess(value ~ year, data = df_filtered,span = 0.2)
  smooth_data = data.frame(year = df_filtered$year)
  smooth_data$value = predict(loess_model, newdata = smooth_data)
  
  ggp = ggplot() +
    geom_point(data = df_cumulative, size = 2,  aes(x = year, y = value, group = year)) +
    geom_line(data = smooth_data, aes(x = year, y = value, group = 1), linetype = "solid", linewidth = 1,
              stat = 'summary') +
    #facet_wrap(.~month, nrow = 3, scales = 'free_y') +
    geom_hline(data = hline_data_1, aes(yintercept = value), linetype = "dotted", linewidth = 1) +
    geom_hline(data = hline_data_2, aes(yintercept = value), linetype = "dotted", linewidth = 1) +
    theme_bw() +
    xlab("Year") +
    ylab("Average precipitation in the 10 preceding years (mm)") +
    theme(strip.text.x = element_text(size = 15))
  
  ggpx = ggp +
    theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 12),
          axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
    theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
    theme(text=element_text(family="Gill Sans MT")) +
    scale_x_continuous(expand = c(0,0), breaks = seq(1910,2020,10)) +
    #scale_y_continuous(breaks = seq()) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          #panel.border = element_blank(),
          plot.margin=unit(c(1,1,0.5,0.5), "cm"),
          legend.title = element_blank(),
          #axis.line.x = element_blank(),
          #axis.text.x = element_blank(),
          #axis.ticks.x = element_blank(),
          #plot.background = element_rect(fill = "transparent",colour = NA),
          #panel.background = element_rect(fill = "transparent",colour = NA),
          #strip.background = element_blank(),
          legend.position = "none") +
    transition_reveal(year)
  
  name = paste("mean and points/animated_rainfall_",season,".mp4",sep="")
  
  anim_save(name, animation = ggpx, renderer = av_renderer(), res = 144, fps = 10,
            width = 11, height = 7.5, units = "in")
  
  
  
  
  
  # animated mean and sd graph - specify season = "total","October",etc.
  
  
  df_filtered_1 = df %>% filter(month == season, measure == "mean")
  df_filtered_2 = df %>% filter(month == season, measure == "sd")
  
  
  years <- unique(df$year)
  
  
  loess_model_1 = loess(value ~ year, data = df_filtered_1,span = 0.2)
  smooth_data_1 = data.frame(year = df_filtered_1$year)
  smooth_data_1$value = predict(loess_model_1, newdata = smooth_data_1)
  
  loess_model_2 = loess(value ~ year, data = df_filtered_2,span = 0.2)
  smooth_data_2 = data.frame(year = df_filtered_2$year)
  smooth_data_2$value = predict(loess_model_2, newdata = smooth_data_2)
  
  ggp = ggplot() +
    geom_line(data = smooth_data_1, aes(x = year, y = value, group = 1), linetype = "solid", linewidth = 1,
              stat = 'summary', col = col1) +
    geom_line(data = smooth_data_2, aes(x = year, y = value, group = 1), linetype = "solid", linewidth = 1,
              stat = 'summary', col = col2) +
    theme_bw() +
    xlab("Year") +
    ylab("Precipitation in the 10 preceding years (mm)") +
    theme(strip.text.x = element_text(size = 15))
  
  ggpx = ggp +
    theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 12),
          axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
    theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
    theme(text=element_text(family="Gill Sans MT")) +
    scale_x_continuous(expand = c(0,0), breaks = seq(1910,2020,10)) +
    #scale_y_continuous(breaks = seq()) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          #panel.border = element_blank(),
          plot.margin=unit(c(1,1,0.5,0.5), "cm"),
          legend.title = element_blank(),
          #axis.line.x = element_blank(),
          #axis.text.x = element_blank(),
          #axis.ticks.x = element_blank(),
          #plot.background = element_rect(fill = "transparent",colour = NA),
          #panel.background = element_rect(fill = "transparent",colour = NA),
          #strip.background = element_blank(),
          legend.position = "none") +
    transition_reveal(year)
  
  name = paste("mean and sd/animated_meansd_rainfall_",season,".mp4",sep="")
  
  anim_save(name, animation = ggpx, renderer = av_renderer(), res = 144, fps = 10,
            width = 11, height = 7.5, units = "in")
  
}