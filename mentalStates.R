# -- prelimenary analyses of EEG (Neurosky) Att/Med at different mental states
rm(list=ls());
# --===========================================================================
library(cluster); library(ggplot2); library(gridExtra); library(scatterplot3d);
library(reshape2)
# --===========================================================================
csv_dir = "~/aiworker.com.R/data/csv"
function_dir = "~/aiworker.com.R/function"
# -- 
    setwd(function_dir); source("csvDataProcessing.R")
eegData = csvDataProcessing(csv_dir)

# -- parsing and formatting time and location
# data.frame(do.call('rbind', strsplit(as.character(eegData$time[1]),'T',fixed=TRUE)))
# substr(as.character(eegData$time[1]),1,13)

# --
df = eegData;  wave_name = colnames(df[,c(6:13)])
# -- clean up rawData
for(i in seq(6,13,1)) {
  j = i-5
  p05 = quantile(df[,wave_name[j]], 0.00, na.rm = T )
  p95 = quantile(df[,wave_name[j]], 0.80, na.rm = T )
  pat = df[,wave_name[j]] > p95 | df[,wave_name[j]] < p05
  df[ pat ,wave_name[j]] = NA
}  
df$s = df$att-df$med
# # df_waves_long = melt(dt[,c(6:13)]); 
# df_waves_long = melt(df[,c(4,5,6:13,14)], id.vars = c("att", "med", "CurrentActivity"),
#                                           variable.name = "waves", value.name = "value"); 

# df_waves_long = melt(df[,c(4,5,8,14)], id.vars = c("att", "med", "CurrentActivity"),
#                                        variable.name = "waves", value.name = "value"); 

# --=============================
# -- plotting distribution of Att
df_waves_long = melt(df[,c(4,14)], id.vars = c("CurrentActivity"),
                     variable.name = "index", value.name = "Att"); 
    # -- remove Att=0
df_waves_long[df_waves_long==0] = NA;  df_waves_long = na.omit(df_waves_long)
yScales = "free"; 
gTitle = "EEG Att distribution";
p1 = ggplot(df_waves_long, aes(x=Att)) +
  geom_histogram(colour="white") +  facet_wrap(~CurrentActivity, scales = yScales, nrow=1) +
  ggtitle(gTitle) +  theme(legend.position="none") 

# --=============================
# -- plotting distribution of Med
df_waves_long = melt(df[,c(5,14)], id.vars = c("CurrentActivity"),
                     variable.name = "index", value.name = "Med"); 
# -- remove Att=0
df_waves_long[df_waves_long==0] = NA;  df_waves_long = na.omit(df_waves_long)
yScales = "free"; 
gTitle = "EEG Med distribution";
p2 = ggplot(df_waves_long, aes(x=Med)) +
  geom_histogram(colour="white") +  facet_wrap(~CurrentActivity, scales = yScales, nrow=1) +
  ggtitle(gTitle) +  theme(legend.position="none") 

# --=============================
# -- plotting distribution of S 
df_waves_long = melt(df[,c(14,15)], id.vars = c("CurrentActivity"),
                     variable.name = "index", value.name = "S");

    # -- remove S=0
df_waves_long[df_waves_long==0] = NA;  df_waves_long = na.omit(df_waves_long)

yScales = "free"; 
gTitle = "EEG Att-Med distribution";
p3 = ggplot(df_waves_long, aes(x=S)) +
  geom_histogram(colour="white") +  facet_wrap(~CurrentActivity, scales = yScales, nrow=1) +
  geom_vline(aes(xintercept=-30), colour="blue",linetype="solid",size=0.5) +
  geom_vline(aes(xintercept=0), colour="red",linetype="solid",size=1) +
  geom_vline(aes(xintercept=30), colour="blue",linetype="solid",size=0.5) +
  ggtitle(gTitle) +  theme(legend.position="none") 


grid.arrange(p1,p2,p3, ncol=1, nrow=3)

# df_waves_long = na.omit(df_waves_long)
# wavesHIST <- 
# #     ggplot(df_waves_long, aes(x=df_waves_long$value, fill=df_waves_long$waves)) +
# #     ggplot(df_waves_long, aes(x=df_waves_long$att, y=df_waves_long$delta)) +
#     ggplot(df_waves_long, aes(x=value)) + geom_histogram(colour="white") +
#  
# #     ggplot(df_waves_long, aes(x=med, y=value)) +
# #         geom_line(aes(colour=waves,size=waves)) +
# #         geom_point(aes(colour=waves), size=1)+
# #           geom_line(size=1)
#         
# #         geom_histogram(colour="black", binwidth=5) +
# #         facet_grid(waves ~ .) +
#         facet_wrap(CurrentActivity~waves) +
# #         facet_wrap(CurrentActivity~waves, scales="free") +
#         ggtitle("EEG waves distribution") +
#         theme(legend.position="none")  
# 
# wavesHIST

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





# -- loading data
# load("~/aiworker.com.R/data/mental_states.RData")


# df_hist_att = hist(eegData$high_alpha, 
#                    breaks = 70, 
#                    col="blue", xlab="wave", ylab="count", main="Distribution of delta")


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
