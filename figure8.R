pacman::p_load(dplyr, tidyr, ggplot2, stringr)
getwd()
# import and summarize data
r <- read.csv("results2.csv") %>% select(-c(X, per_ch61, budget)) 
r$type <- factor(r$type, levels = c("Opt.", "Largest", "Least"),
                         labels = c("Optimal", "Largest", "Least Expensive"))


r <- r %>% pivot_longer(cols = -c("scenario", "type")) 
r$name <- factor(r$name, levels = c("n","area_acre", "metric_area_acre", 
                                    "efficiency", "diff_budget", "per_bound_prot"))
summary <- r %>% 
  group_by(name, type) %>% summarise(mean = mean(value), sd = sd(value))

#plot
ggplot()+
  geom_errorbar(data = summary, mapping = aes(x = type, ymin = mean, ymax = mean, color = type),size = 1.5)+
  geom_jitter(data = r, mapping = aes(x = type, y = value), width = 0.25, alpha = 0.75 )+
  ylim(0, NA)+
  geom_hline(yintercept = 0, size = 0.5, color = "#666666")+
  facet_wrap(facets = vars(name), nrow = 2, scales = "free_y")+
  facet_wrap(~name, 
           nrow = 2, 
           scales = "free_y",
           strip.position = "left", 
           labeller = as_labeller(c(n = "Number of Selected Parcels", 
                                    area_acre = "Total Area (acres)",
                                    metric_area_acre = "Area of Target Land (acres)",
                                    per_bound_prot = "% Area Borders Protected Land",
                                    efficiency = "Efficiency (%)",
                                    diff_budget = "Difference from Budget ($)"))) + 
  xlab("")+ylab("")+ labs(title = "Mean Summaries of Parcel Selection Methods",
                          color = "Selection Method")+
  theme_minimal()+  theme(strip.background = element_blank(),
                          strip.placement = "outside",
                          legend.position = "bottom",
                          axis.text.x = element_text(angle=-70,hjust = 0, vjust = 1))+
  theme(text=element_text(size=20), #change font size of all text
        axis.text=element_text(size=15), #change font size of axis text
        axis.title=element_text(size=15), #change font size of axis titles
        plot.title=element_text(size=15), #change font size of plot title
        legend.text=element_text(size=15), #change font size of legend text
        legend.title=element_text(size=15)) #change font size of legend title  

ggsave("figures/summary.png", width = 15, height = 15/1.618)

#-------------------------------------------------------------------------------#
r$lc <- str_extract(r$scenario, "([A-Z])\\w+$")
r$lc <- factor(recode(r$lc,
               "Habitat" = "Priority Habitat",
               "Forest" = "Forest",
               "Wetlands" = "Wetlands", 
               "Wet" = "Wetlands",
               "Dev" = "Non-Developed"), levels = c("Priority Habitat", "Forest", "Wetlands", "Non-Developed"))



summary <- r %>% 
  group_by(name, lc) %>% summarise(mean = mean(value), sd = sd(value))


ggplot()+
  geom_errorbar(data = summary, mapping = aes(x = lc, ymin = mean, ymax = mean, color = lc),size = 1.5)+
  scale_color_manual(values = c("#F37748", "#73956F", "#336699", "#52154E"))+
  geom_jitter(data = r, mapping = aes(x = lc, y = value), width = 0.25, alpha = 0.75 )+
  geom_hline(yintercept = 0, size = 0.5, color = "#666666")+
  ylim(0, NA)+
  facet_wrap(facets = vars(name), nrow = 2, scales = "free_y")+
  facet_wrap(~name, 
             nrow = 2, 
             scales = "free_y",
             strip.position = "left", 
             labeller = as_labeller(c(n = "Number of Selected Parcels", 
                                      area_acre = "Total Area (acres)",
                                      metric_area_acre = "Area of Target Land (acres)",
                                      per_bound_prot = "% Area Borders Protected Land",
                                      efficiency = "Efficiency (%)",
                                      diff_budget = "Difference from Budget ($)"))) + 
  xlab("")+ylab("")+ labs(title = "Mean Summaries of Land Covers",
                          color = "Land Cover")+
  theme_minimal()+  theme(strip.background = element_blank(),
                          strip.placement = "outside",
                          legend.position = "bottom",
                          axis.text.x = element_text(angle=-70,hjust = 0, vjust = 1))+
  theme(text=element_text(size=20), #change font size of all text
        axis.text=element_text(size=15), #change font size of axis text
        axis.title=element_text(size=15), #change font size of axis titles
        plot.title=element_text(size=15), #change font size of plot title
        legend.text=element_text(size=15), #change font size of legend text
        legend.title=element_text(size=15)) #change font size of legend title  

ggsave("figures/summary_lc.png", width = 15, height = 15/1.618)

