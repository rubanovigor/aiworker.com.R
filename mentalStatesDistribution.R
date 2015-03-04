# EEG (Att/Med/S) distribution by activity and username
#=============================================================================
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rm(list=ls());
# --===========================================================================
library(cluster); library(ggplot2); library(gridExtra); library(scatterplot3d);
library(reshape2); require(plyr)
# --===========================================================================
# # csv_dir = "~/aiworker.com.R/data/csv"
# # csv_dir = "~/aiworker.com.R/data/csv/alex"
# csv_dir = "~/aiworker.com.R/data/csv/exosphere"
# function_dir = "~/aiworker.com.R/function"
# # -- 
# setwd(function_dir); source("csvDataProcessing.R")
# eegData = csvDataProcessing(csv_dir)
# save(eegData, file = "~/aiworker.com.R/data/exosphere.RData")
load("~/aiworker.com.R/data/exosphere.RData")

# --formatting and fixing initial data
df = eegData; 
levels(df$CurrentActivity) <- c(levels(df$CurrentActivity), "football") 
df$CurrentActivity[df$CurrentActivity=="basketball"] = "football";
levels(df$username) <- c(levels(df$username), c("Gerson","Moritz")) 
df$username[df$username=="AntonioManno"] = "Gerson";
df$username[df$username=="SkinnerLayne"] = "Moritz";
    # -- adding empty records
temp = df[1:50,]; temp[,] = NA
temp$username= "EzequielDjeredjian"; temp$CurrentActivity = "football" 
temp$att = 50; temp$med = 50;
df = rbind(df, temp)
remove(temp)
    # -- parsing date
# df$time = substr(toString(df$time), 1,8) 
df$time = substr(df$time, 1,8) 
    # -- getting waves name from initial data
wave_name = colnames(df[,c(6:13)])
# -- clean up rawData
# for(i in seq(6,13,1)) {
#   j = i-5
#   p05 = quantile(df[,wave_name[j]], 0.05, na.rm = T )
#   p95 = quantile(df[,wave_name[j]], 1.00, na.rm = T )
#   pat = df[,wave_name[j]] > p95 | df[,wave_name[j]] < p05
#   df[ pat ,wave_name[j]] = NA
# }  
    # -- creating S index
df$s = df$att-df$med
    # -- remove ZERO values
df <- df[ which(df$med!=0 & df$att!=0), ]
    # -- remove unused Acticity, e.g. 'default'
# df <- df[ which(df$CurrentActivity!='default'), ]

# -- plot only aggregate Distribution
TotalFlag = F; AggregationFlag = F;
# =============================
# =============================
# -- plotting distribution of Att
gTitle = "EEG: Att Distribution"; yScales = "free_y";
if(AggregationFlag){
  meltVar = c("CurrentActivity","att"); 
  GraphRow = 4; GraphCol = 1; TitletxtSize = 16;  GraphWidth = 800;
  xVar = c("CurrentActivity"); xFacetWrap = ~CurrentActivity;
}else{
  meltVar = c("username","CurrentActivity","att"); 
  GraphRow = 4; GraphCol = 3; TitletxtSize = 9;  GraphWidth = 1200;
  xVar = c("username", "CurrentActivity"); xFacetWrap = username~CurrentActivity;
}

    # -- transforming df to plot using face_wrap
df_waves_long = melt(df[,meltVar], id.vars = xVar, variable.name = "index", value.name = "att");

if(AggregationFlag){
  df_waves_long_Total = df[,meltVar]; df_waves_long_Total$index = 'att'; df_waves_long_Total$CurrentActivity = 'aggregate';
  df_waves_long_Total = df_waves_long_Total[c(1,3,2)]
  df_waves_long = rbind(df_waves_long, df_waves_long_Total) 
}
if(TotalFlag){df_waves_long = df_waves_long_Total}

    # -- remove Att=0
# df_waves_long$att[df_waves_long$att==0] = NA;  df_waves_long$att = na.omit(df_waves_long$att)

    # -- crete Att distribution graph
p1 = ggplot(df_waves_long, aes(x=att)) + geom_histogram(colour="white") +
  facet_wrap(xFacetWrap, scales = yScales, nrow = GraphRow, ncol = GraphCol) +
  geom_vline(aes(xintercept=30), colour="blue",linetype="solid",size=0.5) +
  geom_vline(aes(xintercept=50), colour="red", linetype="solid",size=1) +
  geom_vline(aes(xintercept=70), colour="blue",linetype="solid",size=0.5) +
  ggtitle(gTitle) + 
  theme(legend.position="none", 
        plot.title = element_text(lineheight=1,size=18, face="bold"),
        strip.text.x = element_text(size=TitletxtSize, face="bold"))

    # -- saving graph to png file
png("~/aiworker.com.R/output/att_distribution.png", width=GraphWidth, height=800, res=120)
p1
dev.off()

# =============================
# =============================
# -- plotting distribution of Med
gTitle = "EEG: Med Distribution"; yScales = "free_y"; 
if(AggregationFlag){
  meltVar = c("CurrentActivity","med");
  GraphRow = 4; GraphCol = 1; TitletxtSize = 16; GraphWidth = 800;
  xVar = c("CurrentActivity"); xFacetWrap = ~CurrentActivity;
}else{
  meltVar = c("username","CurrentActivity","med"); 
  GraphRow = 4; GraphCol = 3; TitletxtSize = 9; GraphWidth = 1200;
  xVar = c("username", "CurrentActivity"); xFacetWrap = username~CurrentActivity;
}

# -- transforming df to plot using face_wrap
df_waves_long = melt(df[,meltVar], id.vars = xVar, variable.name = "index", value.name = "med");

if(AggregationFlag){
  df_waves_long_Total = df[,meltVar]; df_waves_long_Total$index = 'med'; df_waves_long_Total$CurrentActivity = 'aggregate';
  df_waves_long_Total = df_waves_long_Total[c(1,3,2)]
  df_waves_long = rbind(df_waves_long, df_waves_long_Total)
}
if(TotalFlag){df_waves_long = df_waves_long_Total}

# -- remove med=0
# df_waves_long$med[df_waves_long$med==0] = NA;  df_waves_long$med = na.omit(df_waves_long$med)

# -- crete med distribution graph
p2 = ggplot(df_waves_long, aes(x=med)) + geom_histogram(colour="white") +
  facet_wrap(xFacetWrap, scales = yScales, nrow = GraphRow, ncol = GraphCol) +
  geom_vline(aes(xintercept=30), colour="blue",linetype="solid",size=0.5) +
  geom_vline(aes(xintercept=50), colour="red", linetype="solid",size=1) +
  geom_vline(aes(xintercept=70), colour="blue",linetype="solid",size=0.5) +
  ggtitle(gTitle) + 
  theme(legend.position="none", 
        plot.title = element_text(lineheight=1,size=18, face="bold"),
        strip.text.x = element_text(size=TitletxtSize, face="bold"))

# -- saving graph to png file
png("~/aiworker.com.R/output/med_distribution.png", width=GraphWidth, height=800, res=120)
p2
dev.off()


# =============================
# =============================
# -- plotting distribution of S
gTitle = "EEG: S Distribution"; yScales = "free_y";
if(AggregationFlag){
  meltVar = c("CurrentActivity","s"); 
  GraphRow = 4; GraphCol = 1; TitletxtSize = 16;  GraphWidth = 800;
  xVar = c("CurrentActivity"); xFacetWrap = ~CurrentActivity;
}else{
  meltVar = c("username","CurrentActivity","s"); 
  GraphRow = 4; GraphCol = 3; TitletxtSize = 9;  GraphWidth = 1200;
  xVar = c("username", "CurrentActivity"); xFacetWrap = username~CurrentActivity;
}

# -- transforming df to plot using face_wrap
df_waves_long = melt(df[,meltVar], id.vars = xVar, variable.name = "index", value.name = "s");

if(AggregationFlag){
  df_waves_long_Total = df[,meltVar]; df_waves_long_Total$index = 's'; df_waves_long_Total$CurrentActivity = 'aggregate';
  df_waves_long_Total = df_waves_long_Total[c(1,3,2)]
  df_waves_long = rbind(df_waves_long, df_waves_long_Total)
}
if(TotalFlag){df_waves_long = df_waves_long_Total}

# -- remove s=0
# df_waves_long$s[df_waves_long$s==0] = NA;  df_waves_long$s = na.omit(df_waves_long$s)

# -- crete s distribution graph
p3 = ggplot(df_waves_long, aes(x=s)) + geom_histogram(colour="white") +
  facet_wrap(xFacetWrap, scales = yScales, nrow = GraphRow, ncol = GraphCol) +
  geom_vline(aes(xintercept=-30), colour="blue",linetype="solid",size=0.5) +
  geom_vline(aes(xintercept=0), colour="red", linetype="solid",size=1) +
  geom_vline(aes(xintercept=30), colour="blue",linetype="solid",size=0.5) +
  ggtitle(gTitle) + 
  theme(legend.position="none", 
        plot.title = element_text(lineheight=1,size=18, face="bold"),
        strip.text.x = element_text(size=TitletxtSize, face="bold"))

# -- saving graph to png file
png("~/aiworker.com.R/output/s_distribution.png", width=GraphWidth, height=800, res=120)
p3
dev.off()


# -- saving graph to png file
if(TotalFlag)
{
  png("~/aiworker.com.R/output/total_a_m_s_distribution.png", width=800, height=800, res=120)
  grid.arrange(p1,p2,p3, ncol=1, nrow=3)
  dev.off()
}

# =============================
# =============================
# -- calculate daily average
# gTitle = "EEG: day by day Att";
# dfAVG = ddply(df, .(username, CurrentActivity, time), summarize, mean = round(mean(att), 2),  sd = round(sd(att), 2))

# gTitle = "EEG: day by day Med";
# dfAVG = ddply(df, .(username, CurrentActivity, time), summarize, mean = round(mean(med), 2),  sd = round(sd(med), 2))

gTitle = "EEG: day by day S";
dfAVG = ddply(df, .(username, CurrentActivity, time), summarize, mean = round(mean(s), 2),  sd = round(sd(s), 2))
    #-- temp fix
dfAVG$time[11] = '20150202'

yScales = 'fixed'
GraphRow = 3; GraphCol = 4; TitletxtSize = 9;  GraphWidth = 1200;
xFacetWrap = CurrentActivity~username;

p4 = ggplot(dfAVG, aes(x=time, y=mean)) + 
  geom_line(aes(group=username), colour="red", size = 2) +
  geom_point(colour="blue", size=4) +
  facet_wrap(xFacetWrap, scales = yScales, nrow = GraphRow, ncol = GraphCol) +
  ggtitle(gTitle) + 
  theme(legend.position="none", 
        plot.title = element_text(lineheight=1,size=18, face="bold"),
        strip.text.x = element_text(size=TitletxtSize, face="bold"),
        axis.text = element_text(colour = "black", size = 6, face="bold"))

# -- saving graph to png file
# png("~/aiworker.com.R/output/att_mean_day2day.png", width=GraphWidth, height=800, res=120)
# png("~/aiworker.com.R/output/med_mean_day2day.png", width=GraphWidth, height=800, res=120)
png("~/aiworker.com.R/output/s_mean_day2day.png", width=GraphWidth, height=800, res=120)
p4
dev.off()


