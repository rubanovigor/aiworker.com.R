# -- loading data
load("~/R/data/so_v2_2014.03.21.RData")
 
library(cluster); library(ggplot2); library(gridExtra); library(scatterplot3d)

data$s = data$att-data$med
data$s_shift = data$s+100
data$art_cl[data$s < -30] = 3
data$art_cl[data$s >= -30 & data$s<=30] = 1
data$art_cl[data$s > 30] = 2
data$iter = seq(1,length(data[,c(1)]),1)
df = data[,c("att","med","s_shift","art_cl")]


df_hist_att = hist(df$att, breaks = seq(0,100, by=1), 
                   col="blue", xlab="att", ylab="count", main="Distribution of att")
df_hist_med = hist(df$med, breaks = seq(0,100, by=1), 
                   col="blue", xlab="med", ylab="count", main="Distribution of med")
df_hist_s = hist(df$s_shift, breaks = seq(0,200, by=1), 
                 col="blue", xlab="s", ylab="count", main="Distribution of s")

# tx = data.frame(df_hist_s$breaks)
# ty = data.frame(df_hist_s$counts)
# t =cbind(tx[2:length(tx[,c(1)]),],ty)
# names(t) = c("x","y")
# ggplot(t, aes(x = t$x, y = t$y)  )  + geom_bar()
# 
# pdf("~/R/out/distr_att_med_s.pdf")
# grid.arrange(df_hist_att, df_hist_med )#, p_vector[[3]], p_vector[[4]], ncol=2, nrow=2)
# dev.off()
# N_clusters = 3
# c_means = fanny(data$s,N_clusters)
# c_means_all = fanny(data[,c("att","med","s")],N_clusters)
# 
# res = data.frame(cbind(data,c_means$membership,c_means$clustering, c_means_all$clustering) )
# plot(res$data.att...data.med, col = res$c_means.clustering)


# i=4
pdf("~/R/out/waves.pdf")

df = data
wave_name = colnames(df[,c(7:14)])
p_vector<-list()
for(i in 1:length(wave_name)) {
  #   i=1
  p05 = quantile(df[,wave_name[i]], 0.15, na.rm = T )
  p95 = quantile(df[,wave_name[i]], 0.85, na.rm = T )
  pat = df[,wave_name[i]] > p95 | df[,wave_name[i]] < p05
  df[ pat ,wave_name[i]] = NA
  
  
  #   p_vector[[length( p_vector) + 1]] =
  i=1
  p_vector[[i]] = 
    ggplot(df, aes(x = df$att, y = df[,wave_name[i]] )  ) +
    geom_point(size=1, colour="blue") +
    #           geom_line(size=1, colour="blue") +
    ggtitle(wave_name[i]) #+ xlab("iterations") + ylab("waves")
  i=2
  p_vector[[i]] = 
    ggplot(df, aes(x = df$att, y = df[,wave_name[i]] )  ) +
    geom_point(size=1, colour="blue") +
    #           geom_line(size=1, colour="blue") +
    ggtitle(wave_name[i]) #+ xlab("iterations") + ylab("waves")
  
}

# grid.arrange(p_vector1, p_vector2, p_vector3, p_vector4, ncol=2, nrow=2)   
grid.arrange(p_vector[[1]], p_vector[[2]])#, p_vector[[3]], p_vector[[4]], ncol=2, nrow=2)


dev.off()

remove(p_vector)






# p1 = ggplot(res, aes(x =res$s, y = res$art_cl)) +
#         geom_point(size=1, colour="blue") +
#         geom_point(aes(x =res$s, y = res$c_means.clustering+0.1), size=1, colour="red") +
#         geom_point(aes(x =res$s, y = res$c_means_all.clustering+0.2), size=1, colour="green") +
# #      scale_x_continuous(limits=c(0,8)) +
# #      scale_y_continuous(limits=c(0,8)) +
#      ggtitle("All in One comparison")+ xlab("S = Att-Med, [-100:100]") + ylab("Clusters")
# 
# p2 = ggplot(res, aes(x =res$s, y = res$art_cl)) + geom_point(size=1, colour="blue") +
#         ggtitle("Manual Clustering") + xlab("S = Att-Med, [-100:100]") + ylab("Clusters")
# p3 = ggplot(res, aes(x =res$s, y = res$c_means.clustering+0.1)) + geom_point(size=1, colour="red") +
#         ggtitle("c-means based on S=att-med Clustering")+ xlab("S = Att-Med, [-100:100]") + ylab("Clusters")
# p4 = ggplot(res, aes(x =res$s, y = res$c_means_all.clustering-0.1)) + geom_point(size=1, colour="green") +
#         ggtitle("c-means based on att/med/S Clustering")+ xlab("S = Att-Med, [-100:100]") + ylab("Clusters")
# 
# pdf("~/R/out/clusters.pdf")
# grid.arrange( p2, p3, p4, p1, ncol=2, nrow=2)
# dev.off()


# # 3D Scatterplot
# pdf("~/R/out/fn1.pdf")
# scatterplot3d(res$att,res$s,res$med, color = res$art_cl, main="artificial clustering")
# scatterplot3d(res$att,res$s,res$med, color = res$c_means.clustering, main="c_means clustering")
# dev.off()