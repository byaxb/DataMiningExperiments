


###########################################
##
##  实验2_藻类数量预测v6.0.R
##
##  byaxb (axb@bupt.edu.cn)
##
##  2018-4-2
##
###########################################




###########################################
#数据描述及其预处理
rm(list = ls())
algae_url <- "https://github.com/byaxb/DataMiningExperiments/raw/master/data/algae.txt"
library(tidyverse)
#algae_url <- "data/algae.txt"
algae <- read.table(algae_url,
                    header=F,
                    col.names=c('season','size','speed','mxPH','mnO2','Cl',
                                'NO3','NH4','oPO4','PO4','Chla','a1','a2','a3','a4',
                                'a5','a6','a7'),
                    na.strings=c('XXXXXXX'))
str(algae)

#查看数据结构
head(algae)
View(head(algae))



###########################################
#探索性数据分析

#进行简单的统计汇总
summary(algae)

#采用Hmisc包来describe()
library(Hmisc)
describe(algae)

hist(algae$mxPH, prob=T)
lines(density(algae$mxPH, na.rm = TRUE), col = "red")
abline(v = mean(algae$mxPH, na.rm = T), col = "blue", lwd = 4)
abline(v = median(algae$mxPH, na.rm = T), col = "green", lwd = 2)

hist(algae$mxPH, prob=T, xlab='',
     main='Histogram of maximum pH value',ylim=0:1)
#绘制概率密度曲线
lines(density(algae$mxPH,na.rm=T), col = "red")

#其实自己也可以绘制
density_results <- density(algae$mxPH,na.rm=T)
str(density_results)
density_x <- density_results$x
density_y <- density_results$y
points(density_x, density_y, col = "blue")

#添加坐标轴须，为避免重叠进行抖动
rug(jitter(algae$mxPH))
#查看jitter前后的结果
before_after_jitter <- data.frame(before_jitter = algae$mxPH,
           after_jitter = jitter(algae$mxPH))
length(unique(before_after_jitter$before_jitter))
length(unique(before_after_jitter$after_jitter))

#由此可见，jitter之后
#取值数量从73变为200
before_after_jitter %>%
  gather(key = type, value = value) %>%
  group_by(type) %>%
  summarise(unique_value_count = n_distinct(value)) %>%
  arrange(desc(type)) %>%
  View()
  
#查看数据的分布形态
car::qqPlot(algae$mxPH,
            main='Normal QQ plot of maximum pH')

#感兴趣的同学再看看ggplot里边实现类似的效果
y <- quantile(algae$mxPH, c(0.25, 0.75), na.rm = TRUE)
x <- qnorm(c(0.25, 0.75))
slope <- diff(y)/diff(x)
int <- y[1L] - slope * x[1L]
d <- data.frame(resids = algae$mxPH)
library(ggplot2)
ggplot(algae, aes(sample = mxPH)) +
  stat_qq() 

ggplot(d, aes(sample = resids)) +
  stat_qq() + 
  geom_abline(slope = slope, 
              intercept = int,
              colour = "red",
              size = 1.5,
              alpha = 0.4)



#箱线图
boxplot(algae$oPO4,ylab='Orthophosphate (oPO4)')
rug(jitter(algae$oPO4),side=2)
abline(h = mean(algae$oPO4,na.rm=T),
       lty=2, 
       lwd = 2, 
       col = "red")

#交互式boxplot
#调用boxplot和identify函数实现
iboxplot <- function(a_vector, ...) {
  if(!is.vector(a_vector)) {
    stop("Currently only vector is acceptable")
    #  stopifnot(is.vector(a_vector))
  }
  boxplot(a_vector, ...)
  idx <- identify(x = rep(1, length(a_vector)), 
                  y = a_vector, 
                  labels = paste0(1:length(a_vector), "  [", a_vector, "]"), 
                  ...)
  return(idx)
}

iboxplot(algae$mxPH)
#not run
#iboxplot(algae$mxPH~algae$size)

#当然，绘制成散点图，也不是不可以
plot(algae$mxPH, xlab = "")
lower_whisker <- max(min(algae$mxPH, na.rm = TRUE),
                     fivenum(algae$mxPH)[2] - 1.5 * IQR(algae$mxPH,
                                                        na.rm = TRUE))
upper_whisker <- min(max(algae$mxPH, na.rm = TRUE),
                     fivenum(algae$mxPH)[4] + 1.5 * IQR(algae$mxPH, na.rm = TRUE))
abline(h = lower_whisker,
       lty = 1, col = "red")
abline(h = upper_whisker,
       lty = 2, col = "red")
abline(h = median(algae$mxPH, na.rm = T), 
       lwd = 2)
clicked.rows <- identify(algae$mxPH)
View(algae[clicked.rows, ])


library(ggplot2)
ggplot(algae, aes(x = size, y = a1, fill = size )) +
  geom_boxplot(outlier.colour = "red", 
               outlier.shape = 3, outlier.size = 2)
#参数定制
ggplot(algae, aes(x = fct_inorder(size), 
                  y = a1, 
                  fill = size )) +
  geom_boxplot(outlier.colour = "red", 
               outlier.shape = 3, 
               outlier.size = 2,
               notch = TRUE) +
  xlab("River Size") +
  ylab("Algae 1") +
  theme(legend.position = "none")

#自定义二元操作符
'%+%' <- function(a, b) {
  a^2 + b^2
}
x <- 10
y <- 20
x %+% y

View(cor(na.omit(algae[, 4:11])))

algae_complete <- na.omit(algae)
nrow(algae_complete)

library(GGally)
ggpairs(algae[, c(1:3, 12)],
        axisLabels = "none")
ggpairs(algae[, paste0("a", 1:7)])
ggpairs(algae[, c(4:11)])
ggpairs(algae[, c(4:12)])





###########################################
##缺失值处理
###########################################

#查看总共有多少缺失值
sum(is.na(algae))

#查看缺失值的分布
library(mice)
edit(md.pattern(algae))

#完整记录数
complete.cases(algae)
which(complete.cases(algae))
sum(complete.cases(algae))
sum(complete.cases(algae))
which(!complete.cases(algae))

View(algae[!complete.cases(algae), ])
View(algae[which(!complete.cases(algae)), ])
View(algae[-which(complete.cases(algae)), ])

algae_complete <- algae[complete.cases(algae), ]
sum(!complete.cases(algae_complete))

#一律补充为0
#algae[is.na(algae)] <- 0
#注意上述两条语句中下标的用法

na.count <- function(x) {
  sum(is.na(x))
}
#(idx <- which(apply(algae, 1, na.count) == 6))
(idx <- which(apply(algae, 1, na.count) >= ncol(algae) * 0.2))
#View(algae[idx, ])

#缺失值过多，直接删除
algae <- algae[-idx, ]


#重新读取数据
algae <- read.table(algae_url,
                    header=F,
                    col.names=c('season','size','speed','mxPH','mnO2','Cl',
                                'NO3','NH4','oPO4','PO4','Chla','a1','a2','a3','a4',
                                'a5','a6','a7'),
                    na.strings=c('XXXXXXX'))
#以下方式也可以
too_many_na_idx <- which(apply(algae, 1, function(x) {
  sum(is.na(x))
}) > ncol(algae) * 0.2)
algae <- algae[-too_many_na_idx, ]


na_idx_PO4 <- which(is.na(algae$PO4))
ilm <- lm(PO4~oPO4, data = algae[-which(!complete.cases(algae[, c("PO4", "oPO4")])), ])

predicted_PO4 <- predict(ilm, newdata = algae)
# View(data.frame(predicted_values = predicted_PO4,
#                 real_values = algae$PO4,
#                 baseline = algae$oPO4))

algae$PO4[which(is.na(algae$PO4))] <- predicted_PO4[which(is.na(algae$PO4))]
algae$PO4[28]

#edit(md.pattern(algae))
set.seed(2012)
ri  <- sample(1:nrow(algae), 5)
#ri <- c(63, 150, 119, 12, 83)
# [1]  63 150 119  12  83
#欲使结果和讲义完全一致，可以直接将上述结果赋给ri

rj <- sample(4:11, 4)
#rj <- c(4, 11, 6, 10)
# [1]  4 11  6 10
part_algae_real <- algae[ri, rj]
algae[ri, rj] <- NA
cc_idxs <- which(complete.cases(algae))
icc_idxs <- which(!complete.cases(algae))
length(cc_idxs) + length(icc_idxs) == nrow(algae)

selected_cols <- c("mxPH", "mnO2", "Cl", "NO3", 
              "NH4", "oPO4", "PO4", "Chla")
beta0 <- 2
k <- 3
for(cur_ic_row_idx in icc_idxs) {
  ic_col_idxs <- which(is.na(algae[cur_ic_row_idx, ]))
  for(cur_ic_col_idx in ic_col_idxs) {
    cur_sub_cols <- setdiff(selected_cols, colnames(algae)[cur_ic_col_idx])
    cur_sub_cols <- setdiff(cur_sub_cols, colnames(algae)[which(is.na(algae[cur_ic_row_idx, ]))])
    
    cc_idx <- which(complete.cases(algae[, cur_sub_cols]))
    cc_idx <- setdiff(cc_idx, which(is.na(algae[, cur_ic_col_idx])))
    baseline_idx <- setdiff(cc_idx, cur_ic_row_idx)
    cur_dist <- sapply(baseline_idx, function(x) {
      sqrt(sum((algae[x, cur_sub_cols] - algae[cur_ic_row_idx, cur_sub_cols])^2))
    })
    names(cur_dist) <- baseline_idx
    nn_dist <- sort(cur_dist)[1:k]
    nn_idx <- as.integer(names(sort(cur_dist)[1:k]))
    algae[cur_ic_row_idx, cur_ic_col_idx] <- median(algae[nn_idx, cur_ic_col_idx])
    algae[cur_ic_row_idx, cur_ic_col_idx] <- mean(algae[nn_idx, cur_ic_col_idx])
    algae[cur_ic_row_idx, cur_ic_col_idx] <- 
      sum(exp(-nn_dist*beta0) * algae[nn_idx, cur_ic_col_idx]) /
      sum(exp(-nn_dist*beta0))
  }
}

View(part_algae_real)

View(algae[ri, rj])



algae[which(algae$mxPH > 8.0), ]
library(tidyverse)
algae %>%
  #filter(mxPH > 8.0) %>%
  #select(mxPH:PO4, size) %>%
  group_by(size) %>%
  summarise(mean = mean(mxPH),
            max = max(mxPH)) %>%
  View()

library(forcats)
algae %>%
  ggplot(aes(x = fct_inorder(size), y = mxPH, fill = size)) +
  geom_boxplot() +
  xlab("河流大小") +
  ylab("最大PH值")

library(caret)
library(tidyverse)
modelLookup() %>%
  select(-(2:3)) %>%
  distinct() %>%
  View()
?models


###########################################
#线性模型
load(url("https://github.com/byaxb/DataMiningExperiments/raw/master/data/algae.cc.rda"))

str(algae.cc)
?lm
lm.a1 <- lm(a1 ~ .-a2-a3-a4-a5-a6-a7, algae.cc)
lm.a1 <- lm(a1 ~ .,data=algae.cc[,1:12])

class(lm.a1)
str(lm.a1)
lm.a1$coefficients


fivenum(as.numeric(lm.a1$residuals)) / sd(algae$a1)

#模型诊断
summary(lm.a1)
x <- seq(0.001, 5, len = 100)
tmp <- df(x^2, 5, 30)
plot(tmp, type = "l", xlim = c(0, 60))
polygon(c(30:100, 30), c(tmp[30:100], 0), col = "grey")
tmp <- dt(x^2, 5)
plot(tmp, type = "l")

plot(function(x) dt(x, df = 3), -6, 6, 
     main = "Non-central t - Density", yaxs = "i")
x <- seq(-6, 6, length = 100)
plot(dt(x, 30), type = "l")
polygon(c(1:30, 30), c(dt(x, 30)[1:30], 0), col = "grey")
polygon(c(70:100, 70), c(dt(x, 30)[70:100], 0), col = "grey")

nrow(algae.cc)

lm.a1.final <- step(lm.a1)
summary(lm.a1.final)
op <- par(mfrow = c(2, 2))
plot(lm.a1.final)
par(op)

a1.predicted <- predict(lm.a1.final, newdata = algae.cc)
View(data.frame(real = algae.cc$a1, predicted = a1.predicted))
#k-折交叉检验
k.fold <- 5
k.fold.idx <- list()
idxs <- 1:nrow(algae.cc)
for(i in 1:(k.fold - 1)) {
  k.fold.idx[[i]] <- sample(idxs, 
                            round(nrow(algae.cc)/k.fold))
  idxs <- setdiff(idxs, k.fold.idx[[i]])
}
k.fold.idx[[k.fold]] <- idxs
k.fold.idx
#标准化均方误差
nmse <- function(y, yhat) {
  return(sum((yhat - y)^2) / 
           sum((mean(y) - y)^2))
}

train.nmse <- NULL
test.nmse <- NULL
for(i in 1:k.fold) {
  cat("\n\nprocessing", i)
  lm.a1 <- lm(a1~., algae.cc[-k.fold.idx[[i]], 1:12])
  lm.a1.final <- step(lm.a1)
  train.nmse <- c(train.nmse, nmse(algae.cc[-k.fold.idx[[i]], "a1"], 
                                   predict(lm.a1.final, newdata = algae.cc[-k.fold.idx[[i]],])))
  test.nmse <- c(test.nmse, nmse(algae.cc[k.fold.idx[[i]], "a1"], 
                                 predict(lm.a1.final, newdata = algae.cc[k.fold.idx[[i]],])))
}
mean(train.nmse)
mean(test.nmse)

qqnorm(lm.a1.final$residuals)

#梯度下降法
x <- algae.cc$oPO4
y <- algae.cc$PO4
theta0 <- 0
theta1 <- 1
alpha <- 0.003
cost_pre <- 1/length(x)*sum((theta0 + theta1 * x- y)^2)
repeat {
  cat("\n", cost_pre)
  theta0 <- theta0 - alpha *(1/length(x)) * sum(((theta0 + theta1 * x- y)*x))
  theta1 <- theta1 - alpha * (1/length(x)) * sum((theta0 + theta1 * x- y))
  cost <- 1/length(x)*sum((theta0 + theta1 * x- y)^2)
  if(cost_pre - cost > 0.00001) {
    cost_pre <- cost
  } else {
    break
  }
}
c(`Intercept 1` = theta0, x = theta1)

algae.cc$`another` <- paste0(algae.cc$season, algae.cc$size)
algae.cc$another
algae.cc$`season and size` <- paste0(algae.cc$season, algae.cc$size)
View(algae.cc)
c(`(Intercept)` = theta0, x = theta1)
#summary(lm(y~x))
lm(y~x)$coefficients 




#绘制等高线
theta0 <- seq(-40, 120, len = 100)
theta1 <- seq(0.2, 2.2, len = 100)
cf <- outer(theta0, theta1, function(theta0, theta1) {
  theta <- data.frame(theta0, theta1)
  apply(theta, 1, function(x){
    1/(2*nrow(algae.cc))*sum(((x[1] + x[2] * algae.cc$oPO4) - algae.cc$PO4)^2)
  } )
})
View(cf)
contour(theta0, theta1, cf, nlevels = 30)

require(rgl)
jet.colors <-   colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

filled.contour(theta0, theta1, cf,
               color.palette = jet.colors,
               nlevels = 30,
               plot.axes = {
                 axis(1)
                 axis(2)
                 contour(theta0, theta1, cf, nlevels=30,add=T,lwd=1)             
               }
)


points = seq(-2, 0, length=20)
#create a grid
XY = expand.grid(X=points,Y=-points)
# A z-function 
Zf <- function(X,Y){
  2./exp((X-.5)^2+Y^2)-2./exp((X+.5)^2+Y^2);
}
# populate a surface
Z <- Zf(XY$X, XY$Y)
zlim <- range(Z)
zlen <- zlim[2] - zlim[1] + 1

jet.colors <-   colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

colorzjet <- jet.colors(100)  # 100 separate color 
require(rgl)
open3d()
rgl.surface(x=points, y=matrix(Z,20), 
            coords=c(1,3,2),z=-points, 
            color=colorzjet[ findInterval(Z, seq(min(Z), max(Z), length=100))] )
axes3d()


lm.a1 <- lm(a1~., algae.cc[, 4:12])
lm.a1.final <- step(lm.a1)
predicted.a1.lm <- predict(lm.a1.final, newdata = algae.cc)
nmse(algae.cc$a1, predicted.a1.lm)


#k-折交叉检验
k.fold <- 5
k.fold.idx <- list()
idxs <- 1:nrow(algae.cc)
for(i in 1:(k.fold - 1)) {
  k.fold.idx[[i]] <- sample(idxs, 
                            round(nrow(algae.cc)/k.fold))
  idxs <- setdiff(idxs, k.fold.idx[[i]])
}
k.fold.idx[[k.fold]] <- idxs
k.fold.idx
#标准化均方误差
nmse <- function(y, yhat) {
  return(sum((yhat - y)^2) / 
           sum((mean(y) - y)^2))
}

train.nmse <- NULL
test.nmse <- NULL
for(i in 1:k.fold) {
  cat("\n\nprocessing", i)
  lm.a1 <- lm(a1~., algae.cc[-k.fold.idx[[i]], 4:12])
  lm.a1.final <- step(lm.a1)
  train.nmse <- c(train.nmse, nmse(algae.cc[-k.fold.idx[[i]], "a1"], 
                                   predict(lm.a1.final, newdata = algae.cc[-k.fold.idx[[i]],])))
  test.nmse <- c(test.nmse, nmse(algae.cc[k.fold.idx[[i]], "a1"], 
                                 predict(lm.a1.final, newdata = algae.cc[k.fold.idx[[i]],])))
}
mean(train.nmse)
mean(test.nmse)


###########################################
#神经网络
library(nnet)
library(caret)

algae.cc.scaled <- scale(algae.cc[, 4:12])
a1.center <- attr(algae.cc.scaled, "scaled:center")["a1"]
a1.scale <- attr(algae.cc.scaled, "scaled:scale")["a1"]
ann.a1 <- nnet(a1~.,
               algae.cc.scaled,
               linout = TRUE,
               decay = 0.3,
               size = 1)
library(NeuralNetTools)
plotnet(ann.a1)

prediced.a1.scaled <- predict(ann.a1, newdata = algae.cc.scaled)
predicted.a1.unscaled <- scale(prediced.a1.scaled,
                               center = -a1.center/a1.scale, 
                               scale = 1/a1.scale)
nmse(algae.cc$a1, predicted.a1.unscaled)


#挤压到0~1区间之中
algae.cc.scaled <- scale(algae.cc[, 4:12], 
                         center = apply(algae.cc[, 4:12], 2, min),
                         scale = apply(algae.cc[, 4:12], 2, max) - apply(algae.cc[, 4:12], 2, min))
a1.center <- attr(algae.cc.scaled, "scaled:center")["a1"]
a1.scale <- attr(algae.cc.scaled, "scaled:scale")["a1"]
ann.a1 <- nnet(a1~.,
               algae.cc.scaled,
               linout = TRUE,
               decay = 0.3,
               size = 1)
library(NeuralNetTools)
plotnet(ann.a1)


prediced.a1.scaled <- predict(ann.a1, newdata = algae.cc.scaled)
predicted.a1.unscaled <- scale(prediced.a1.scaled,
                               center = -a1.center/a1.scale, 
                               scale = 1/a1.scale)
nmse(algae.cc$a1, predicted.a1.unscaled)



mygrid <- expand.grid(.decay = 10^c(-1, -2, -3, -4, -5), .size = 1:9)
nnetfit <- train(a1~., 
                 data = algae.cc.scaled, 
                 linout = TRUE,
                 method = "nnet",
                 maxit = 1000,
                 tuneGrid = mygrid)
print(nnetfit)
library(NeuralNetTools)
plotnet(nnetfit)
prediced.a1.scaled <- predict(nnetfit, newdata = algae.cc.scaled)
predicted.a1.unscaled <- scale(prediced.a1.scaled,
                               center = -a1.center/a1.scale, 
                               scale = 1/a1.scale)
nmse(algae.cc$a1, predicted.a1.unscaled)

library(caret)
mygrid <-  expand.grid(.layer1 = 5, .layer2 = 0, .layer3 = 0)
neuralnet.a1 <- train(a1~.,
                      data = algae.cc.scaled,
                      method = "neuralnet",
                      algorithm = "backprop",
                      act.fct = "logistic",
                      err.fct = "sse",
                      learningrate = 0.01,
                      stepmax = 100000,
                      rep = 3,
                      linear.output = FALSE,
                      tuneGrid = mygrid)
prediced.a1.scaled <- predict(neuralnet.a1, algae.cc.scaled)
predicted.a1.unscaled <- scale(prediced.a1.scaled,
                               center = -a1.center/a1.scale, 
                               scale = 1/a1.scale)
nmse(algae.cc$a1, predicted.a1.unscaled)
library(neuralnet)
neuralnet.a1 <- neuralnet(a1~mxPH+mnO2+Cl+NO3+NH4+oPO4+PO4+Chla, 
                          data = algae.cc.scaled, 
                          algorithm = "backprop",
                          learningrate = 0.01,
                          act.fct = "logistic",
                          err.fct = "sse",
                          hidden = c(10, 10, 10),
                          stepmax = 40000,
                          rep = 4,
                          linear.output = FALSE)
plot(neuralnet.a1, 
     rep="best",
     radius = 0.1,
     show.weights = F)
prediced.a1.scaled <- compute(neuralnet.a1, 
                              algae.cc.scaled[, 1:8])$net.result
predicted.a1.unscaled <- scale(prediced.a1.scaled,
                               center = -a1.center/a1.scale, 
                               scale = 1/a1.scale)
nmse(algae.cc$a1, predicted.a1.unscaled)
#	learningrate = 0.01, hidden = c(5, 5, 5), 0.6422152274
#	learningrate = 0.01, hidden = c(5, 0, 0), 0.5499479129



#k-折交叉检验
k.fold <- 5
k.fold.idx <- list()
idxs <- 1:nrow(algae.cc)
for(i in 1:(k.fold - 1)) {
  k.fold.idx[[i]] <- sample(idxs, 
                            round(nrow(algae.cc)/k.fold))
  idxs <- setdiff(idxs, k.fold.idx[[i]])
}
k.fold.idx[[k.fold]] <- idxs
k.fold.idx
#标准化均方误差
nmse <- function(y, yhat) {
  return(sum((yhat - y)^2) / 
           sum((mean(y) - y)^2))
}

train.nmse <- NULL
test.nmse <- NULL
a1.center <- attr(algae.cc.scaled, "scaled:center")["a1"]
a1.scale <- attr(algae.cc.scaled, "scaled:scale")["a1"]
for(i in 1:k.fold) {
  cat("\n\nprocessing", i)	
  ann.a1 <- nnet(a1~.,
                 algae.cc.scaled[-k.fold.idx[[i]], ],
                 linout = TRUE,
                 decay = 0.1,
                 size = 1)
  train.prediced.a1.scaled <- predict(ann.a1, algae.cc.scaled[-k.fold.idx[[i]], ])
  train.predicted.a1.unscaled <- scale(train.prediced.a1.scaled,
                                       center = -a1.center/a1.scale, 
                                       scale = 1/a1.scale)
  test.prediced.a1.scaled <- predict(ann.a1, algae.cc.scaled[k.fold.idx[[i]], ])
  test.predicted.a1.unscaled <- scale(test.prediced.a1.scaled,
                                      center = -a1.center/a1.scale, 
                                      scale = 1/a1.scale)	
  train.nmse <- c(train.nmse, nmse(algae.cc[-k.fold.idx[[i]], "a1"], 
                                   train.predicted.a1.unscaled))
  test.nmse <- c(test.nmse, nmse(algae.cc[k.fold.idx[[i]], "a1"], 
                                 test.predicted.a1.unscaled))
}
mean(train.nmse)
mean(test.nmse)




#k-折交叉检验
k.fold <- 5
k.fold.idx <- list()
idxs <- 1:nrow(algae.cc)
for(i in 1:(k.fold - 1)) {
  k.fold.idx[[i]] <- sample(idxs, 
                            round(nrow(algae.cc)/k.fold))
  idxs <- setdiff(idxs, k.fold.idx[[i]])
}
k.fold.idx[[k.fold]] <- idxs
k.fold.idx
#标准化均方误差
nmse <- function(y, yhat) {
  return(sum((yhat - y)^2) / 
           sum((mean(y) - y)^2))
}

train.nmse <- NULL
test.nmse <- NULL
a1.center <- attr(algae.cc.scaled, "scaled:center")["a1"]
a1.scale <- attr(algae.cc.scaled, "scaled:scale")["a1"]
for(i in 1:k.fold) {
  cat("\n\nprocessing", i)	
  neuralnet.a1 <- neuralnet(a1~mxPH+mnO2+Cl+NO3+NH4+oPO4+PO4+Chla, 
                            data =algae.cc.scaled[-k.fold.idx[[i]], ], 
                            algorithm = "backprop",
                            learningrate = 0.01,
                            act.fct = "logistic",
                            err.fct = "sse",
                            hidden = 5,
                            stepmax = 100000,
                            rep = 3,
                            linear.output = FALSE)
  train.prediced.a1.scaled <- compute(neuralnet.a1, 
                                      algae.cc.scaled[-k.fold.idx[[i]], 1:8])$net.result
  train.predicted.a1.unscaled <- scale(train.prediced.a1.scaled,
                                       center = -a1.center/a1.scale, 
                                       scale = 1/a1.scale)
  test.prediced.a1.scaled <- compute(neuralnet.a1, 
                                     algae.cc.scaled[k.fold.idx[[i]], 1:8])$net.result
  test.predicted.a1.unscaled <- scale(test.prediced.a1.scaled,
                                      center = -a1.center/a1.scale, 
                                      scale = 1/a1.scale)	
  train.nmse <- c(train.nmse, nmse(algae.cc[-k.fold.idx[[i]], "a1"], 
                                   train.predicted.a1.unscaled))
  test.nmse <- c(test.nmse, nmse(algae.cc[k.fold.idx[[i]], "a1"], 
                                 test.predicted.a1.unscaled))
}
mean(train.nmse)
mean(test.nmse)

###########################################
#回归树
library(rpart)
rp.a1 <- rpart(a1~., data = algae.cc[, 4:12])
library(rattle)
fancyRpartPlot(rp.a1)


library(rpart.plot)
rpart.plot(rp.a1,
           type=4, 
           cex = 0.75,
           under.cex=0.75,
           fallen=T, 
           branch=.5, 
           round=0, 
           leaf.round=6,
           #right = FALSE,
           clip.right.labs=F,
           box.palette="GnYlRd",
           prefix="Algae1\n", 
           branch.col="gray", 
           branch.lwd=2,
           extra=101, 
           under=T, 
           split.yshift = 1,
           split.cex=0.85)


#剪枝
opt <- which.min(rp.a1$cptable[,"xerror"])
icp <- rp.a1$cptable[opt, "CP"]
rp.a1.pruned <- prune(rp.a1,cp= icp)
plotcp(rp.a1.pruned)
fancyRpartPlot(rp.a1.pruned)

rpart.plot(rp.a1.pruned,
           type=4, 
           cex = 0.75,
           under.cex=1,
           fallen=T, 
           branch=.5, 
           round=0, 
           leaf.round=6,
           #right = FALSE,
           clip.right.labs=F,
           box.palette="GnYlRd",
           prefix="Algae1\n", 
           branch.col="gray", 
           branch.lwd=2,
           extra=101, 
           under=T, 
           split.yshift = 1,
           split.cex=0.85)


k.fold <- 5
k.fold.idx <- list()
idxs <- 1:nrow(algae.cc)
for(i in 1:(k.fold - 1)) {
  k.fold.idx[[i]] <- sample(idxs, 
                            round(nrow(algae.cc)/k.fold))
  idxs <- setdiff(idxs, k.fold.idx[[i]])
}
k.fold.idx[[k.fold]] <- idxs
nmse <- function(y, yhat) {
  return(sum((yhat - y)^2) / 
           sum((mean(y) - y)^2))
}
train.nmse <- NULL
test.nmse <- NULL
for(i in 1:k.fold) {
  cat("\n\nprocessing", i)	
  rp.a1 <- rpart(a1~., data = algae.cc[-k.fold.idx[[i]], 4:12])
  opt <- which.min(rp.a1$cptable[,"xerror"])
  icp <- rp.a1$cptable[opt, "CP"]
  rp.a1.pruned <- prune(rp.a1,cp= icp)
  train.prediced.a1 <- predict(rp.a1.pruned, algae.cc[-k.fold.idx[[i]], 4:12]) 
  test.prediced.a1 <- predict(rp.a1.pruned, algae.cc[k.fold.idx[[i]], 4:12])
  train.nmse <- c(train.nmse, nmse(algae.cc[-k.fold.idx[[i]], "a1"], 
                                   train.prediced.a1))
  test.nmse <- c(test.nmse, nmse(algae.cc[k.fold.idx[[i]], "a1"], 
                                 test.prediced.a1))
}
mean(train.nmse)
mean(test.nmse)



library(rpart)
k.fold <- 5
k.fold.idx <- list()
idxs <- 1:nrow(algae.cc)
for(i in 1:(k.fold - 1)) {
  k.fold.idx[[i]] <- sample(idxs, 
                            round(nrow(algae.cc)/k.fold))
  idxs <- setdiff(idxs, k.fold.idx[[i]])
}
k.fold.idx[[k.fold]] <- idxs
nmse <- function(y, yhat) {
  return(sum((yhat - y)^2) / 
           sum((mean(y) - y)^2))
}
train.nmse <- NULL
test.nmse <- NULL
for(i in 1:k.fold) {
  cat("\n\nprocessing", i)	
  rp.a1 <- rpart(a1~., data = algae.cc[-k.fold.idx[[i]], 4:12])
  opt <- which.min(rp.a1$cptable[,"xerror"])
  icp <- rp.a1$cptable[opt, "CP"]
  rp.a1.pruned <- rp.a1
  train.prediced.a1 <- predict(rp.a1.pruned, algae.cc[-k.fold.idx[[i]], 4:12]) 
  test.prediced.a1 <- predict(rp.a1.pruned, algae.cc[k.fold.idx[[i]], 4:12])
  train.nmse <- c(train.nmse, nmse(algae.cc[-k.fold.idx[[i]], "a1"], 
                                   train.prediced.a1))
  test.nmse <- c(test.nmse, nmse(algae.cc[k.fold.idx[[i]], "a1"], 
                                 test.prediced.a1))
}
mean(train.nmse)
mean(test.nmse)


###########################################
#随机森林
library(randomForest)
rf.a1 <- randomForest(a1~., data = algae.cc[, 4:12],
                      ntree=500, importance = TRUE)
View(importance(rf.a1))
varImpPlot(rf.a1,
           col = rainbow(ncol( algae.cc[, 4:12])), 
           pch = 1:ncol( algae.cc[, 4:12]))


k.fold <- 5
k.fold.idx <- list()
idxs <- 1:nrow(algae.cc)
for(i in 1:(k.fold - 1)) {
  k.fold.idx[[i]] <- sample(idxs, 
                            round(nrow(algae.cc)/k.fold))
  idxs <- setdiff(idxs, k.fold.idx[[i]])
}
k.fold.idx[[k.fold]] <- idxs
nmse <- function(y, yhat) {
  return(sum((yhat - y)^2) / 
           sum((mean(y) - y)^2))
}
train.nmse <- NULL
test.nmse <- NULL
for(i in 1:k.fold) {
  cat("\n\nprocessing", i)	
  rf.a1 <- randomForest(a1~., 
                        data = algae.cc[-k.fold.idx[[i]], 4:12],
                        ntree = 1000)
  train.prediced.a1 <- predict(rf.a1, algae.cc[-k.fold.idx[[i]], 4:12]) 
  test.prediced.a1 <- predict(rf.a1, algae.cc[k.fold.idx[[i]], 4:12])
  train.nmse <- c(train.nmse, nmse(algae.cc[-k.fold.idx[[i]], "a1"], 
                                   train.prediced.a1))
  test.nmse <- c(test.nmse, nmse(algae.cc[k.fold.idx[[i]], "a1"], 
                                 test.prediced.a1))
}
mean(train.nmse)
mean(test.nmse)
