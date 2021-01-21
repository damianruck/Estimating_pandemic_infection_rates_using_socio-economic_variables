################
# Plot FIGURE 2
################
### Left
my.formula <- y ~ x
caseDeathRatePlot1 <- ggplot(covid_deathRate_long2,
                             mapping=aes(x=log10(as.numeric(covidCount)),
                                         y=log10(as.numeric(deathCount)),
                             )) +
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_wrap(~ dayDiff,nrow=7) +
  stat_poly_eq(aes(label = paste(..rr.label..)), 
               label.x.npc = "left", label.y.npc = 0.99,
               formula = my.formula, parse = TRUE, size = 6,color="red") + 
  scale_x_continuous() +
  scale_y_continuous() +
  coord_cartesian(ylim=c(0.2,2.5),xlim=c(1,3)) + 
  scale_fill_brewer(palette = "Paired") +
  labs(title=paste("Daily case-death delay (",
                   min(covid_deathRate_long2$covidDate),")",sep=""), 
       y="log(death count)", x="log(corona count)", caption="") +
  theme_classic2() +
  theme(plot.title = element_text(face="bold",size=24),
        legend.position = "none",
        legend.title = element_text(face="bold",size=16),
        legend.text=element_text(face="bold",size=14),
        axis.title.x = element_text(color="black",size=20,face="bold"),
        axis.title.y = element_text(color="black",size=20,face="bold"),
        axis.text.x = element_text(color="black",size=16, angle=0),
        axis.text.y = element_text(color="black",size=16, angle=0),
        strip.text.x = element_text(size = 18, color="black",face="bold"))

### Right
dailyCaseDeath1 <- ggplot(data=dailyCasesDeaths_clean) +
  geom_line(mapping=aes(x=as.Date(Date),y=log10(cases),color="log(cases)"),size=2) + 
  geom_line(mapping=aes(x=as.Date(Date),y=log10(deaths),color="log(deaths)"),size=2) +   
  scale_x_date(breaks = "1 month", minor_breaks = "1 week", date_labels = "%B") +
  scale_y_continuous() +
  coord_cartesian(ylim=c(0,5)) + 
  labs(x="Date", y="", caption="",color="") +
  theme_classic2() + 
  theme(plot.title = element_text(face="bold",size=24),
        panel.grid.minor.x = element_line(colour="grey", size=0.2),
        legend.position = c(0.05, 0.95),
        legend.justification = c("left", "top"),
        legend.background = element_rect(fill = "white",linetype="solid",color="black "),
        legend.title = element_blank(),
        legend.text=element_text(face="bold",size=24),
        axis.title.x = element_text(color="black",size=20,face="bold"),
        axis.title.y = element_text(color="black",size=20,face="bold"),
        axis.text.x = element_text(color="black",size=16, angle=0),
        axis.text.y = element_text(color="black",size=16, angle=0))

### Combine
delay_plot <- (caseDeathRatePlot1 | dailyCaseDeath1 )
ggexport(plotlist = list(delay_plot), filename = 
           paste("plots/","delay_","combined",".png",sep=""),
         width = 1200, height = 1000)