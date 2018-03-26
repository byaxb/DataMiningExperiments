


###########################################
##
##  实验1：学生成绩分析v6.0.R
##
##  updated at 20180312
##
##  byaxb (axb@bupt.edu.cn)
##
##  (c)2012~2018
##
###########################################


###########################################
##  数据对象及其预处理
###########################################
#R里边常用的数据对象有六种
#向量/因子
#矩阵/数组
#列表/数据框

xm <- c("周黎", "汤海明", "舒江辉", "翁柯", "祁强", "湛容")
xm[3] #子集只包含第三个元素
xm[c(3, 5)] #子集包含第三个和第五个元素
ywcj <- c(94, 87, 92, 91, 85, 92)
(ywcj <- c(94, 87, 92, 91, 85, 92))
ywcj
ywcj <- ywcj + 10
ywcj <- ywcj + 10 #结果依旧是一个长度为6向量，每个同学的语文加了10分
ywcj > 100

xm[ywcj > 100]

ywcj[which(ywcj > 100)] <- 100

ywcj[ywcj > 100] <- 100

idx <- which(ywcj > 100)

ywcj[idx] <- 100
cjb <- cbind(xm, ywcj)

View(cjb)

table(cj$性别, cj$文理分科)
tmp <- table(cj$性别, cj$文理分科)
tmp <- as.data.frame(tmp)
View(tmp)

tmp %>%
  group_by(Var2, Var1)

xb <- factor(c("女", "男", "男", "女", "男", "女"))

xb[1] > xb[2]

dj <- factor(c("良", "中", "差", "优","中", "中"), 
             ordered = TRUE, 
             levels = c("差", "中", "良", "优"))

dj[1] > dj[2]
dj[3] > dj[4]

cjjz <- matrix(c(
  94, 82, 96, 97, 97, 98, 95, 94, 88,
  87, 94, 89, 95, 94, 94, 90, 90, 89,
  92, 79, 86, 98, 95, 96, 89, 94, 87,
  91, 84, 96, 93, 97, 94, 82, 90, 83,
  85, 92, 82, 93, 87, 88, 95, 94, 93,
  92, 82, 85, 91, 90, 92, 82, 98, 90),
  byrow = TRUE, ncol = 9)

View(cjjz)

rowSums(cjjz)

colMeans(cjjz)

apply(cjjz, 1, sum)
apply(cjjz, 2, mean)

wjf <- c(89, 76, 90, 88)

order(wjf)
sort(wjf)
sort(wjf, decreasing = TRUE)
?sort
wjf[order(wjf)]

?order

cj <- read.csv("data/cj.csv", 
               header = TRUE,
               stringsAsFactors = FALSE)

cj <- read.csv(file.choose(), 
               header = TRUE,
               stringsAsFactors = FALSE)
cj <- read.csv("clipboard", 
               header = TRUE,
               sep = "\t",
               stringsAsFactors = FALSE)
#当然也可以自定义一个函数
read_clipboard <- function(sep = "\t", ...) {
  read.csv("clipboard", sep = sep, ...)
}
read_clipboard <- function(...) {
  read.csv("clipboard", sep = "\t", ...)
}
read_clipboard <- function(...) {
  mcall = as.list(match.call())[-1L]
  if (!"sep" %in% names(mcall)){
    read.csv("clipboard", sep = "\t", ...)
  } else {
    read.csv("clipboard", ...)
  }
}

read_clipboard()
read_clipboard(sep = "\t")
cj <- read_clipboard()

library(tidyverse)
file_name <- "D://wd/R/data/cj.csv"
cj <- read.csv(file_name)
View(cj)
cj <- cj %>%
  select(-1)
View(cj)


read.csv(file_name) %>%
  select(-1) %>%
  View()

#当然也可以从互联网读取
file_name <- 
  "https://raw.githubusercontent.com/byaxb/DataMiningExperiments/master/data/cj.csv"
#在读取csv文件时，经常碰到的一个棘手问题是乱码
#采用readr::read_csv是一个比较保险的办法
cj <- readr::read_csv(file_name)
cj <- as.data.frame(cj)
cj <- read.csv(file_name, fileEncoding = "UTF-8")
View(cj)
str(cj)
head(cj)
View(tail(cj, n = 10))

cj$班级 <- factor(cj$班级)
cj$性别 <- factor(cj$性别)
cj$文理分科 <- factor(cj$文理分科)
# #当然也可用下边的方式代替
# for(i in c("班级", "性别", "文理分科")) {
#   cat("processing ", i)
#   cj[, i] <- factor(cj[, i])
# }

str(cj)
cj$班级
str(cj)
#增加一列总成绩
cj$总成绩 <- apply(cj[, 4:12], 1, sum)

View(cj)

#对数据进行分箱，离散化处理
cj$等级水平 <- cut(cj$总成绩, 
               breaks = fivenum(cj$总成绩),
               labels = c("差", "中", "良", "优"),
               ordered_result = TRUE)
#当然也可以对某一门课进行五分制成绩
cj$数学等级 <- cut(cj$数学, 
               breaks = c(0, 60, 70, 80, 90, 100),
               labels = c("不及格", "及格", "中", "良", "优"),
               include.lowest = TRUE,
               ordered_result = TRUE)
#一般来讲，我们需要对数据进行了解
#看看大致的分布
table(cj$等级水平, cj$班级)
table(cj$等级水平, cj$性别)
#由此可以看出，男女有别~


table(cj$数学等级)
hist(cj$数学)
table(cj$数学等级,cj$性别)
#更详细的数据探索在下一个小节展开
str(cj)
library(tidyverse)
cj %>%
  as.data.frame() %>%
  select(3:12)%>%
  gather(key = 科目, value = 成绩, -性别) %>%
  group_by(科目, 性别) %>%
  summarise(平均分 = mean(成绩),
              中位数 = median(成绩),
               最高分 = max(成绩),
               最低分 = min(成绩),
              记录数 = n()) %>%
  View()

file_name <- "D://wd/R/data/cj.csv"
cj <- read.csv(file_name)
cj <- cj %>%
  select(-1) %>%
  filter(语文 == 0) %>%
  View()

cj <- as.data.frame(cj)
cj <- select(cj, 3:12)
View(cj)
cj_bak <- cj
cj <- gather(cj, key = x1, value = x2, 数学, 外语)


View(cj)

View(cj)
View(cj_bak)
cj <- group_by(cj, 科目, 性别)
cj <- group_by(cj, 性别, 科目)
cj <- summarise(cj, 平均分 = mean(成绩),
               最高分 = max(成绩),
               最低分 = min(成绩))
View(cj)




cj %>%
  as.data.frame() %>%
  select(3:12)%>%
  gather(科目,成绩,-性别) %>%
  group_by(科目, 性别) %>%
  summarise(平均分 = median(成绩)) %>%
  View()

sort(cj$总成绩)


View(cj)

library(tidyverse)
cj <- cj %>%
  select(-1) %>%
  filter(总成绩 != 0) %>%
  arrange(desc(总成绩))
# 
# View(cj)
# 
# cj <- arrange(cj, desc(总成绩))
# View(cj)
# 
# cj <- cj[-which(cj$总成绩 == 0), ]
# names(cj)
View(cj)

cn <- names(cj)
str(cj)
colnames(cj) <-  c("xm", "bj", "xb", "yw", "sx", "wy",
                   "zz", "ls", "dl", "wl", "hx", "sw", "wlfk", "zcj")
dzb <- data.frame(cn = cn, py = colnames(cj))
View(dzb)
attr(cj, "variable.labels") <- cn#在RStudio中能显示中文的标注
save(cj, dzb, file = "data/cj.rda")
load("data/cj.rda")

load("data/cj.rda", verbose = TRUE)


###########################################
#数据的高矮胖瘦
load("data/cj.rda", verbose = TRUE)
mean(cj$yw)
mean(cj$yw, trim = 0.1) #
median(cj$yw)
sd(cj$yw)
length(which(cj$yw > mean(cj$yw) - sd(cj$yw) & cj$yw < mean(cj$yw) + sd(cj$yw))) / nrow(cj)

var(cj$yw)

min(cj$yw)
sum(cj$yw < 60)
sum(cj$yw >= 60)
hist(cj$yw)
hist(cj$yw[cj$yw >= 60])

tmp <- cj %>%
  filter(yw >= 60) %>%
  select(yw)
hist(tmp[, 1])

max(cj$yw)
range(cj$yw)
diff(range(cj$yw))

x <- c(1, 3, 9, 4)
diff(diff(x))

range(cj$yw)[2] - range(cj$yw)[1]

quantile(cj$yw)
fivenum(cj$yw)
quantile(cj$yw, seq(0, 1, by = 0.1))
stem(cj$yw)

hist(cj$yw, freq = FALSE)
lines(density(cj$yw), col = "red", lwd = 2)

boxplot(cj$yw)
boxplot(zcj~wlfk, data = cj)
names(cj)
library(reshape2)
cj.melt <- melt(cj[, 4:12])
boxplot(value~variable, 
        data = cj.melt)

cor(cj[, 4:12])

library(corrplot)
corrplot(cor(cj[, 4:12]), 
         method="color",
         diag = FALSE)

plot(cj[, 4:12])
panel.hist <- function(x, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
panel.cor <- function(x, y, 
                      digits = 2, 
                      prefix = "", 
                      cex.cor, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(cj[, 4:12], 
      diag.panel = panel.hist,
      lower.panel = panel.smooth, 
      upper.panel = panel.cor)

corrplot(cor(iris[, 1:4]) * 10, 
         method="color",
         is.corr = FALSE, 
         diag = FALSE,
         tl.cex = 0.75,
         cl.cex = 0.75,
         tl.col = rgb(50, 50, 50, maxColorValue = 255))

str(cj)
round(cor(cj[, 4:12]), digits = 2)
symnum(cor(cj[, 4:12]))


###########################################
#映射关系
rm(list = ls())
getwd()
load("data/cj.rda", verbose = TRUE)
View(cj)
str(cj)
tmp <- library(rpart)
if(!require("rpart")) {
  install.packages("rpart")
  library(rpart)
}
cj <- as.data.frame(cj)
colnames(cj)
tree_model <- rpart(wlfk~., data = cj[, 4:13])

tree_model
str(tree_model)


#tree_model <- rpart(wlfk~.-xm, data = cj)
#最简单的树模型绘制
plot(tree_model,
     uniform = TRUE, 
     branch = 0.8, 
     margin = 0.1)
text(tree_model, 
     all = TRUE, 
     use.n = TRUE, cex = 0.7)
tree_model

#稍微复杂一点的版本
#但是好看了许多
library(rattle)
fancyRpartPlot(tree_model)
#添加一些参数
fancyRpartPlot(tree_model,
               main = "文理分科决策树",
               sub = paste0("数据挖掘实验一 ", Sys.time()),
               palettes=c("Greys", "Oranges"))
fancyRpartPlot(tree_model,
               main = "文理分科决策树",
               sub = paste0("数据挖掘实验一 ", Sys.time()),
               palettes=c("Greens", "YlOrRd"))


system.time(for(i in 1:10000) x <- 1)
sp <- Sys.time()
for(i in 1:10000) x <- 1
ed <- Sys.time()
difftime(ed, sp, units = "sec")
cat(round(difftime(ed, sp, units = "sec"), digits = 2), " seconds")


split.fun <- function(x, labs, digits, varlen, faclen) {
  # labs <- gsub(",", " ", labs)
  # for(i in 1:length(labs)) {
  #   # split labs[i] into multiple lines
  #   labs[i] <- paste(strwrap(labs[i], width=20), collapse="\n")
  # }
  labs <- paste0(substring(labs, 1, 20), "...等")
  labs
}
library(rpart.plot)
rpart.plot(tree_model,
           type=4, 
           fallen=T, 
           branch=.5, 
           round=0, 
           leaf.round=6,
           clip.right.labs=T,
           cex = 0.75,
           under.cex=0.75,
           box.palette="GnYlRd",
           prefix="当前类别\n", 
           branch.col="gray", 
           branch.lwd=2,
           extra=101, 
           under=T, 
           lt=" < ", 
           ge=" >= ",
           split.cex=0.85)
library(rpart.plot)
rpart.plot(tree_model,
           type=4, 
           fallen=T, 
           branch=.5, 
           round=0, 
           leaf.round=6,
           #right = FALSE,
           clip.right.labs=F,
           cex = 0.75,
           under.cex=0.75,
           box.palette="GnYlRd",
           #prefix="文理分科\n", 
           branch.col="gray", 
           branch.lwd=2,
           extra=101, 
           under=T, 
           split.yshift = 1,
          # lt=" < ", 
           #ge=" >= ",
           split.cex=0.85)

wlfk_predicted <- predict(tree_model, cj, type = "class")
confusion_table <- table(wlfk_predicted, cj$wlfk)
sum(diag(confusion_table)) / sum(confusion_table)

class(confusion_table)
View(as.data.frame(confusion_table))

as.data.frame.matrix(confusion_table)
confusion_table %>%
  as.data.frame.matrix() %>%
  View()

set.seed(2012)
train_set_idx <- sample(1:nrow(cj), round(nrow(cj)*0.7))
tree_model <- rpart(wlfk~., data = cj[train_set_idx, 4:13])
train_pre <- predict(tree_model, cj[train_set_idx, ], type = "class")
train_conf_matri <- table(cj[train_set_idx, "wlfk"], train_pre)
test_pre <- predict(tree_model, cj[-train_set_idx, ], type = "class")
test_conf_matri <- table(cj[-train_set_idx, "wlfk"], test_pre)
sum(diag(train_conf_matri)) / sum(train_conf_matri)
sum(diag(test_conf_matri)) / sum(test_conf_matri)


#输出规则
library(rattle)
asRules(tree_model)

rattle("D://desktop/cj.csv")
rattle("D://desktop/cj_utf8.csv")

###########################################
#距离关系
kcjl <- hclust(dist(t(scale(cj[, 4:12])))) 
plot(kcjl, h = -1)
rect.hclust(kcjl, 3)

random_count <- 20
random_idx <- c(sample(which(cj$wlfk == "文科"), random_count),
                sample(which(cj$wlfk == "理科"), random_count))
dist_matri <- dist(cj[random_idx, 4:12])
names(dist_matri) <- cj$xm[random_idx]
stu_hclust <- hclust(dist_matri, method = "ward.D") 
str(stu_hclust)
stu_hclust$labels <- cj$xm[random_idx]
#学术论文中的各种配色
library(ggsci)
library(scales)
mypal = pal_npg("nrc", alpha = 0.7)(9)
show_col(pal_uchicago()(9))
show_col(pal_jco("default")(10))
mypal[3:4]


k <- 2
factoextra::fviz_dend(
  stu_hclust,
  k = k,
  k_colors = mypal,
  type = "phylogenic",
  repel = TRUE)

my_plt <- factoextra::fviz_dend(
  stu_hclust,
  k = k,
  k_colors = mypal[1:k],
  type = "phylogenic",
  repel = TRUE)
inside_my_plt <- ggplot_build(my_plt)
inside_my_plt


#加载字体
library(extrafont)
loadfonts(device="win")
windowsFonts()

#深入my_plt内部
#修改参数之后进行设置
inside_my_plt$data[[3]][["fontface"]] <- "bold"
my_plt_revised <- ggplot_gtable(inside_my_plt)
grid.draw(my_plt_revised)

#另存为图片文件
ppi <- 300
png(filename = "D://desktop/Fig.11.png", width = 7*ppi, height = 7*ppi, 
    res = ppi, type = "cairo-png")  # units are pixels
grid.draw(my_plt_revised)
dev.off()

factoextra::fviz_dend(
  stu_hclust,
  k = 2,
  k_colors = mypal,
  type = "circular",
  repel = TRUE)

factoextra::fviz_dend(
  stu_hclust,
  k = 2,
  k_colors = mypal,
  type = "rectangle",
  repel = TRUE)

cj %>%
  filter(row_number() %in% random_idx) %>%
  arrange(wlfk) %>%
  View()

cj[random_idx, ] %>%
  arrange(wlfk) %>%
  View()

confusion_table <- table(cutree(stu_hclust, 2), cj$wlfk[random_idx])
sum(diag(confusion_table)) / sum(confusion_table)


###########################################
#伴随关系
rm(list = ls())
load("data/cj.rda", verbose = TRUE)
cj_bak <- cj
str(cj)
library(arules)
apriori(cj)
cjbq <- c("差", "中", "良", "优")
cjdj <- list()
for(i in 4:12) {
  cjdj[[names(cj)[i]]] <- cut(cj[, i], fivenum(cj[, i]), cjbq, include.lowest = TRUE)
} 
View(cj_bak)
for(i in 4:12) {
  cj[, i] <- cut(cj[, i],
                 breaks = c(0, 60, 70, 80, 90, 100),
                 label = c("不及格", "及格", "中", "良", "优"),
                 include.lowest = TRUE)
}

cj_sub <- cj[, 3:13]
View(cj_sub)

library(arules)

library(arulesViz)
str(cj_sub)
cj_sub$xb <- factor(cj_sub$xb)
cj_sub$wlfk <- factor(cj_sub$wlfk)
my_rules <- apriori(cj_sub,
                    parameter = list(supp = 50/nrow(cj_sub), conf = 0.8, target = "rules"),
                    appearance = list(rhs = c("wlfk=理科", "wlfk=文科"),
                                      default="lhs"))

my_rules
inspect(my_rules)
inspectDT(my_rules)
plot(my_rules[1:15],  
     method = "graph",
     engine = "htmlwidget")

View(cj)
cjdj <- as.data.frame(cjdj)
cjdj$xb <- cj$xb
cjdj$wlfk <- cj$wlfk
View(cjdj)

library(arules)
rules <- apriori(cjdj, 
                 parameter = list(supp = 0.05, conf = 0.8, target = "rules"),
                 appearance = list(rhs = c("wlfk=理科", "wlfk=文科"),
                                   default="lhs"))
rules
inspect(rules)

quality(rules) <- round(quality(rules), digits = 3)
subset.matrix <- is.subset(rules, rules)
View(as.matrix(subset.matrix))
subset.matrix <- as.matrix(subset.matrix)
subset.matrix[lower.tri(subset.matrix, diag = TRUE)] <- NA
redundant <- colSums(subset.matrix, na.rm = TRUE) >= 1



which(redundant)
rules.pruned <- rules[!redundant]
inspect(rules.pruned)
library(arulesViz)
plot(rules.pruned, method="graph")
#交互式可视化
inspectDT(rules.pruned)
plot(rules.pruned,  engine = "htmlwidget")
plot(rules.pruned, method = "graph", engine = "htmlwidget")

###########################################
#相关关系
icor <- function(x, y) {
  x.increment <- x[-1] - x[-length(x)]
  x.up.down <- ifelse(x.increment > 0, 1, -1)
  y.increment <- y[-1] - y[-length(y)]
  y.up.down <- ifelse(y.increment > 0, 1, -1)
  sum(x.up.down == y.up.down) / length(x.up.down)
}
icor(cj$yw, cj$sx)
icor(cj$hx, cj$sw)
icor_results <- NULL
for(i in 4:12) {
  for(j in 4:12) {
    icor_results <- c(icor_results, icor(cj[, i], cj[, j]))
  }
}
icor_results <- matrix(icor_results, 
                       byrow = TRUE, ncol = 12 - 4 + 1)
dimnames(icor_results) <- list(names(cj)[4:12], names(cj)[4:12])
View(icor_results)

cj.xy <- cj[sample(nrow(cj)), 4:13]
names(cj.xy) <- c(paste0("X", 1:9), "Y")
View(cj.xy)
names(cj.xy) <- names(cj)[4:13]

rm(list = ls())
load("data/cj.rda")
View(cj)
icor_score <- cor(cj[, 4:12])
library(igraph)
getAdj <- function(idat, threshold = 0.5) {
  apply(idat, 1, function(x) {
    x[x < threshold | x == 1] <- 0
    return(x)
  })
}
adj <- getAdj(icor_score, 0.4)
ig <- graph_from_adjacency_matrix(adj*10, "undirected")
member<-walktrap.community(ig)#类似层次聚类中的凝聚法
V(ig)$member<-member$membership#为每个点添加模块号属性
member.color<-rainbow(max(V(ig)$member),star=0,end=1)#每个模块统一设置颜色
for(i in V(ig)$member){
  V(ig)[member==i]$color<-member.color[i]
}
ig <- simplify(ig)
plot(ig,
     #layout = ilay,
     #vertex.label = NA,
     vertex.label.cex = 2,
     vertex.shape = "sphere", 
     #layout = layout.circle,
     vertex.size =6,
     #vertex.label.dist = c(1.5, 1.5, -1.5, 1.5, -1.5, -1.5, -1.5, -1.5),
     #edge.label = NA,
    # edge.width = 1,
     edge.arrow.width = 1,
     edge.arrow.size = 0.1)

library(intergraph)
library(sna)
V(ig)$id <- names(V(ig))
inet <- asNetwork(ig)
inet$name <- V(ig)$name
as.character(dzb$cn)[V(ig)$name %in% as.character(dzb$py)]
inet
gplot3d(inet,
        label = as.character(dzb$py)[4:12])

###########################################
#文本分析
library(showtext)
font.add("fzqt", regular = "D://tools/常用字体/简启体.TTF")
font.add("lihei", regular = "D://tools/常用字体/LiHeiPro.otf")
font.add("lihei", regular = "D://tools/常用字体/MSYHBD.TTC") #微软雅黑
font.families()

showtext.begin()
op <- par(family = "fzqt", mar = c(0.1, 0.1, 3.1, 1.1))
mingzi <- unlist(strsplit(cj$xm, split = ""))
xing <- unlist(lapply(strsplit(cj$xm, split = ""), function(x) x[1]))
ming <- unlist(lapply(strsplit(cj$xm, split = ""), function(x) x[-1]))
sort(table(ming), decreasing = TRUE)
freq <- as.integer(table(ming))
words <- names(table(ming))
library(wordcloud)
wordcloud(words, freq,
          random.order = FALSE,
          rot.per = 0,
          scale = c(8, 0.6),
          fixed.asp = TRUE,
          col = rainbow(length(freq)))
#以下代码请拷贝至R Console之后运行
#wordcloud2只有在浏览器中才有效果
# library(wordcloud2)
# wordcloud2(data.frame(words = words, freq = freq), 
#            color = "random-light", 
#            backgroundColor = "grey",
#            fontFamily = "微软雅黑", 
#            size = 1,
#            shape = 'star')
par(op)
showtext.end()

showtext.begin()
op <- par(family = "fzqt", mar = c(0.1, 0.1, 3.1, 1.1))
nan.ming <- unlist(lapply(strsplit(cj$xm[cj$xb == "男"], split = ""), 
                          function(x) x[-1]))
freq <- as.integer(sort(table(nan.ming), decreasing = TRUE))
word <- names(sort(table(nan.ming), decreasing = TRUE))
nan <- data.frame(ming = word, nan = freq)
nv.ming <- unlist(lapply(strsplit(cj$xm[cj$xb == "女"], split = ""), 
                         function(x) x[-1]))
freq <- as.integer(sort(table(nv.ming), decreasing = TRUE))
word <- names(sort(table(nv.ming), decreasing = TRUE))
nv <- data.frame(ming = word, nv = freq)
nannv <- merge(nan, nv,
               by.x = "ming", by.y = "ming",
               all.x = TRUE, all.y =TRUE)
row.names(nannv) <- nannv[, 1]
nannv <- nannv[, -1]
nannv[is.na(nannv$nan), 1] <- 0
nannv[is.na(nannv$nv), 2] <- 0
View(nannv)
names(nannv) <- c("NAN SHENG", "NV SHENG")
library(wordcloud)
comparison.cloud(nannv,
                 scale = c(7, 0.4),
                 random.order=FALSE,
                 rot.per=0,
                 colors=brewer.pal(ncol(nannv),"Dark2"),
                 title.size=2)
par(op)
showtext.end()
