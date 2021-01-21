################
# Plot FIGURE 3
################
fittedCasesPlot <- ggplot(fittedCasesPlot_data,
                          mapping=aes(x=as.numeric(log10(corona_cases)),
                                      y=as.numeric(log10(corona_deaths*100)),
                                      color=Label1)) +
  geom_point() + 
  geom_smooth(method="lm",aes(weight = population)) +
  facet_geo(~state, scale="free_y") +
  scale_x_continuous() +
  scale_y_continuous() +
  scale_color_discrete(name  ="Date",
                       labels=c("April 17", "July 1")) +
  labs(title="", y=expression(log(I[t])), x=expression(log(C[t])),
       color="Date", caption="") +
  theme(plot.title = element_text(face="bold",size=24),
        legend.position = "bottom",
        legend.title = element_text(face="bold",size=44),
        legend.text=element_text(face="bold",size=40),
        strip.text.x =element_text(color="black",size=17,face="bold"),
        axis.title.x = element_text(color="black",size=52,face="bold"),
        axis.title.y = element_text(color="black",size=48,face="bold"),
        axis.text.x = element_text(color="black",size=24, angle=0),
        axis.text.y = element_text(color="black",size=24, angle=0))

### Save FIGURE 2
ggsave("plots/fittedCasesPlot.pdf", plot = fittedCasesPlot, scale = 1,width = 32,height = 20,
       dpi = 1600,limitsize = FALSE)



