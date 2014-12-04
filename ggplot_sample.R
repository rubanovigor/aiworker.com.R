
result.long = melt(df, id=c('Date'))

my.colour = c("actual" = "grey60", "elman" = "orange")
my.size = c("actual" = 2, "elman" = 1)

p = list()

p[[1]] = ggplot(data = result.long, aes(x=Date, y = OutputPar, group=type,colour=type)) +
         geom_vline(aes(xintercept=as.numeric(fcStartPoint)), colour="white",linetype="solid",size=4) +
         geom_line(aes(colour=type,size=type)) +
  
         scale_clour_manual(values = my_colour)+
         scale_size_manual(values = my.size) +
         geom_point() + 
  
        # -- title
  ggtitle("...") + 
  theme(plot.title = element_text(lineheight=1,size=16,face="bold"))+
        # -- Y
  scale_y_continuous(name="Y",labels=comma)+
  theme(axis.title.y = element_text(face="bold",colour="black",size=18),
        axis.text.y = element_text(colour="darkblue",face="bold",vjust=0.5,size=16)) +
        # -- X
  scale_x_date(labels=date_format("%b-%Y"), breaks="2 month") +
  theme(axis.title.x = element_blanck(),
        axis.text.x = element_text(colour="darkblue",face="bold",angle=90,vjust=0.5,size=16)) +
  
  
png("")  
grid.arrange(p)
dev.off()