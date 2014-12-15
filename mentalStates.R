# -- prelimenary analyses of EEG (Neurosky) Att/Med at different mental states
rm(list=ls());
function_dir = "~/aiworker.com.R/function"
# -- loading data
load("~/aiworker.com.R/data/mental_states.RData")

library(cluster); library(ggplot2); library(gridExtra); library(scatterplot3d);
library(reshape2)

# df_hist_att = hist(eegData$high_alpha, 
#                    breaks = 70, 
#                    col="blue", xlab="wave", ylab="count", main="Distribution of delta")
 

df = eegData
wave_name = colnames(df[,c(6:13)])
# vectorOfPlots<-list()
# for(i in 1:length(wave_name)) {
for(i in seq(6,13,1)) {
#      i=1
#   df=dt[, c("att", wave_name[i]) ]
#   df=dt
  j = i-5
  p05 = quantile(df[,wave_name[j]], 0.00, na.rm = T )
  p95 = quantile(df[,wave_name[j]], 0.80, na.rm = T )
  pat = df[,wave_name[j]] > p95 | df[,wave_name[j]] < p05
  df[ pat ,wave_name[j]] = NA
}  
#   
#     vectorOfPlots[[length( vectorOfPlots) + 1]] =
# #   i=1
# #   vectorOfPlots[[i]] <- 
#       ggplot(df, aes(x = df$att, y = df[,wave_name[i]] )  ) +
#           geom_point(size=1, colour="blue") +
#           #           geom_line(size=1, colour="blue") +
#           ggtitle(wave_name[i]) #+ xlab("iterations") + ylab("waves")
# #           ggtitle(i) #+ xlab("iterations") + ylab("waves")

# }

# df_waves_long = melt(dt[,c(6:13)]); 
df_waves_long = melt(df[,c(4,5,6:13,14)], id.vars = c("att", "med", "CurrentActivity"),
                                    variable.name = "waves", 
                                    value.name = "value"); 

df_waves_long = melt(df[,c(4,5,8,14)], id.vars = c("att", "med", "CurrentActivity"),
                     variable.name = "waves", 
                     value.name = "value"); 


# df_waves_long = dt[,c(4, 6)]; 
# df_waves_long = head(df_waves_long)
# names(df_waves_long) = c("waves","value")
# tail(df_waves_long)
df_waves_long = na.omit(df_waves_long)
wavesHIST <- 
#     ggplot(df_waves_long, aes(x=df_waves_long$value, fill=df_waves_long$waves)) +
#     ggplot(df_waves_long, aes(x=df_waves_long$att, y=df_waves_long$delta)) +
    ggplot(df_waves_long, aes(x=value)) + geom_histogram(colour="white") +
 
#     ggplot(df_waves_long, aes(x=med, y=value)) +
#         geom_line(aes(colour=waves,size=waves)) +
#         geom_point(aes(colour=waves), size=1)+
#           geom_line(size=1)
        
#         geom_histogram(colour="black", binwidth=5) +
#         facet_grid(waves ~ .) +
        facet_wrap(CurrentActivity~waves) +
#         facet_wrap(CurrentActivity~waves, scales="free") +
        ggtitle("EEG waves distribution") +
        theme(legend.position="none")  

wavesHIST

# p1 = vectorOfPlots[[1]]; p2 = vectorOfPlots[[2]]; p3 = vectorOfPlots[[3]]; p4 = vectorOfPlots[[4]];
# p5 = vectorOfPlots[[5]]; p6 = vectorOfPlots[[6]]; p7 = vectorOfPlots[[7]]; p8 = vectorOfPlots[[8]];
# 
# pdf("~/aiworker.com.R/output/Att_vs_Waves.pdf")
# grid.arrange(wavesHIST, ncol=1, nrow=1)
# dev.off()
# 
# # getwd()
# setwd(function_dir)
# source("multiplot.R")
# multiplot(wavesHIST)
# multiplot(vectorOfPlots[[1]],vectorOfPlots[[2]], vectorOfPlots[[3]], vectorOfPlots[[4]], cols=2)
# 
# 
# # grid.arrange(vectorOfPlots1, vectorOfPlots2, vectorOfPlots3, vectorOfPlots4, ncol=2, nrow=2)   
# grid.arrange(vectorOfPlots[[1]], vectorOfPlots[[2]], vectorOfPlots[[3]], vectorOfPlots[[4]], ncol=2, nrow=2)
# 
# # pdf("~/aiworker.com.R/output/Att_vs_Waves.pdf")
# # grid.arrange(vectorOfPlots[[1]], vectorOfPlots[[2]], vectorOfPlots[[3]], vectorOfPlots[[4]],
# #              vectorOfPlots[[5]], vectorOfPlots[[6]], vectorOfPlots[[7]], vectorOfPlots[[8]])
# # dev.off()
# 
# # remove(vectorOfPlots)
