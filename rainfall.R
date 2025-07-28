library(tidyverse)

bng.rain = read.csv("Bangalore rainfall data 1900-2023 Aug.csv")
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

ggp = ggplot(data = df %>% filter(!month %in% c("Total","Summer","Monsoon","Autumn","Winter")), 
             aes(x = year, y = value, col = measure)) +
  #geom_point() +
  facet_wrap(.~month, nrow = 3, scales = 'free_y') +
  geom_smooth(se = F,span = 0.2) +
  theme_bw() +
  xlab("Year") +
  ylab("Precipitation in the 10 preceding years (mm)") +
  theme(strip.text.x = element_text(size = 15))

ggpx = ggp +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_color_manual(breaks = c("mean","sd"), labels = c("Average (mean)","Variation (sd)"), 
                     values = c("#0072B2","#D55E00")) +
  scale_x_continuous(expand = c(0,0), breaks = seq(1920,2020,20)) +
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
        legend.position = "bottom")

ggsave(filename = "1_monthly_change.png", plot = ggpx, dpi = 500, bg = "transparent",
       width = 11, height = 7.5, units = "in")

ggp = ggplot(data = df %>% filter(month %in% c("Summer","Monsoon","Autumn","Winter")), 
             aes(x = year, y = value, col = measure)) +
  #geom_point() +
  facet_wrap(.~month, nrow = 2, scales = 'free_y') +
  geom_smooth(se = F,span = 0.2) +
  theme_bw() +
  xlab("Year") +
  ylab("Precipitation in the 10 preceding years (mm)") +
  theme(strip.text.x = element_text(size = 15))

ggpx = ggp +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_color_manual(breaks = c("mean","sd"), labels = c("Average (mean)","Variation (sd)"), 
                     values = c("#0072B2","#D55E00")) +
  scale_x_continuous(expand = c(0,0), breaks = seq(1920,2020,20)) +
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
        legend.position = "bottom")

ggsave(filename = "2_seasonal_change.png", plot = ggpx, dpi = 500, bg = "transparent",
       width = 11, height = 7.5, units = "in")

ggp = ggplot(data = df %>% filter(month == "Total", measure == "mean"), 
             aes(x = year, y = value, col = measure)) +
  geom_point(size = 2) +
  #facet_wrap(.~month, nrow = 3, scales = 'free_y') +
  geom_smooth(se = F, linewidth = 2,span = 0.2) +
  geom_hline(yintercept = df$value[df$year == min(df$year) & df$measure == "mean" & df$month == "Total"], 
             linetype = "dotted", linewidth = 1) +
  geom_hline(yintercept = df$value[df$year == max(df$year) & df$measure == "mean" & df$month == "Total"], 
             linetype = "dotted", linewidth = 1) +
  theme_bw() +
  xlab("Year") +
  ylab("Average precipitation in the 10 preceding years (mm)") +
  theme(strip.text.x = element_text(size = 15))

ggpx = ggp +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_color_manual(breaks = c("mean","sd"), labels = c("Average (mean)","Variation (sd)"), 
                     values = c("#0072B2","#D55E00")) +
  scale_x_continuous(expand = c(0,0), breaks = seq(1910,2020,10)) +
  scale_y_continuous(breaks = seq(200,1200,100)) +
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
        legend.position = "none")

ggsave(filename = "3_annual_change.png", plot = ggpx, dpi = 500, bg = "transparent",
       width = 11, height = 7.5, units = "in")



# el nino and la nina

custom_palette = c("#66c2a5", "#fc8d62", "#8da0cb")

df.mean = df %>% filter(measure == "mean") %>% dplyr::select(-measure)
clim.events = df.mean %>% left_join(bng.rain)
clim.events$event = clim.events$elnino
clim.events$event[clim.events$event == "Y"] = "El Nino"
clim.events$event[clim.events$event == "N"] = "Normal"
clim.events$event[clim.events$lanina == "Y"] = "La Nina"
clim.events$diff = round(100*(clim.events$rain - clim.events$value)/clim.events$value,1)

clim.events.tot = clim.events %>% filter(month == "Total")

bootst = function(vec) {
  mn = numeric(1000)
  
  for (i in 1:1000)
  {
    temp = sample(vec, replace = T)
    mn[i] = mean(temp)
  }
  
  lci = quantile(mn,0.025)
  med = median(mn)
  rci = quantile(mn,0.975)
  
  res = c(lci,med,rci)
  
  return(res)
}

event.sum = clim.events %>%
  filter(!is.na(diff),!month %in% c("Total","Summer","Monsoon","Autumn","Winter")) %>%
  group_by(event,month) %>%
  reframe(lci.boot = bootst(diff)[1],
          Mean.boot = bootst(diff)[2],
          rci.boot = bootst(diff)[3]) %>%
  mutate(month = factor(month, levels = c("January","February","March","April","May","June",
                                          "July","August","September","October","November","December")),
         event = factor(event, levels = c("Normal","El Nino","La Nina")))

event.sum.seas = clim.events %>%
  filter(!is.na(diff),month %in% c("Summer","Monsoon","Autumn","Winter")) %>%
  group_by(event,month) %>%
  reframe(lci.boot = bootst(diff)[1],
          Mean.boot = bootst(diff)[2],
          rci.boot = bootst(diff)[3]) %>%
  mutate(month = factor(month, levels = c("Summer","Monsoon","Autumn","Winter")),
         event = factor(event, levels = c("Normal","El Nino","La Nina")))

event.sum.tot = clim.events.tot %>%
  filter(!is.na(diff)) %>%
  group_by(event) %>%
  reframe(lci.boot = bootst(diff)[1],
          Mean.boot = bootst(diff)[2],
          rci.boot = bootst(diff)[3],
          n = n_distinct(year)) %>%
  mutate(event = factor(event, levels = c("Normal","El Nino","La Nina")))


ggp = ggplot(data = event.sum, aes(x = event, y = Mean.boot, col = event)) +
  facet_wrap(.~month, nrow = 3, scales = 'free_y') +
  geom_point() +
  geom_errorbar(aes(ymin = lci.boot, ymax = rci.boot), width = 0, linewidth = 5) +
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1) +
  theme_bw() +
  xlab("Climatic event") +
  ylab("Deviation from average precipitation in the 10 preceding years (%)") +
  theme(strip.text.x = element_text(size = 15))

ggpx = ggp +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_colour_manual(breaks = c("Normal","El Nino","La Nina"), 
                      labels = c("Neutral","El Nino","La Nina"),
                      values = custom_palette) +
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
        legend.position = "none")

ggsave(filename = "4_monthly_clim_event.png", plot = ggpx, dpi = 500, bg = "transparent",
       width = 11, height = 7.5, units = "in")

ggp = ggplot(data = event.sum.seas, aes(x = event, y = Mean.boot, col = event)) +
  facet_wrap(.~month, nrow = 2, scales = 'free_y') +
  geom_point() +
  geom_errorbar(aes(ymin = lci.boot, ymax = rci.boot), width = 0, linewidth = 5) +
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1) +
  theme_bw() +
  xlab("Climatic event") +
  ylab("Deviation from average precipitation in the 10 preceding years (%)") +
  theme(strip.text.x = element_text(size = 15))

ggpx = ggp +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_colour_manual(breaks = c("Normal","El Nino","La Nina"), 
                      labels = c("Neutral","El Nino","La Nina"),
                      values = custom_palette) +
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
        legend.position = "none")

ggsave(filename = "5_seasonal_clim_event.png", plot = ggpx, dpi = 500, bg = "transparent",
       width = 11, height = 7.5, units = "in")


ggp = ggplot(data = event.sum.tot, aes(x = event, y = Mean.boot, col = event)) +
  #facet_wrap(.~month, nrow = 3, scales = 'free_y') +
  #geom_point() +
  geom_errorbar(aes(ymin = lci.boot, ymax = rci.boot), width = 0, linewidth = 7) +
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1) +
  theme_bw() +
  xlab("Climatic event") +
  ylab("Deviation from average precipitation in the 10 preceding years (%)") +
  theme(strip.text.x = element_text(size = 15))

ggpx = ggp +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_colour_manual(breaks = c("Normal","El Nino","La Nina"), 
                      labels = c("Neutral","El Nino","La Nina"),
                      values = custom_palette) +
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
        legend.position = "none")

ggsave(filename = "6_total_clim_event.png", plot = ggpx, dpi = 500, bg = "transparent",
       width = 11, height = 7.5, units = "in")



# August

august = clim.events %>%
  filter(month == "August") %>%
  mutate(event = factor(event, levels = c("Normal","El Nino","La Nina")))

ggp = ggplot(data = august, aes(x = year, y = diff, fill = event)) +
  geom_bar(stat = "identity", position = "identity") +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  theme_bw() +
  ylab("Deviation from average precipitation in August (%)") +
  theme(strip.text.x = element_text(size = 15))

ggpx = ggp +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_x_continuous(breaks = seq(1909,2023,6)) +
  scale_fill_manual(breaks = c("Normal","El Nino","La Nina"), 
                      labels = c("Neutral","El Nino","La Nina"),
                      values = custom_palette) +
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
        legend.position = "bottom")

ggsave(filename = "7_august_clim_event.png", plot = ggpx, dpi = 500, bg = "transparent",
       width = 11, height = 7.5, units = "in")



# October

october = clim.events %>%
  filter(month == "October") %>%
  mutate(event = factor(event, levels = c("Normal","El Nino","La Nina")))

ggp = ggplot(data = october, aes(x = year, y = diff, fill = event)) +
  geom_bar(stat = "identity", position = "identity") +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  theme_bw() +
  ylab("Deviation from average precipitation in October (%)") +
  theme(strip.text.x = element_text(size = 15))

ggpx = ggp +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_x_continuous(breaks = seq(1909,2023,6)) +
  scale_fill_manual(breaks = c("Normal","El Nino","La Nina"), 
                      labels = c("Neutral","El Nino","La Nina"),
                      values = custom_palette) +
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
        legend.position = "bottom")

ggsave(filename = "8_october_clim_event.png", plot = ggpx, dpi = 500, bg = "transparent",
       width = 11, height = 7.5, units = "in")