


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

file_name <- "D://wd/R/data/cj.csv"
file_name <- 
  "https://raw.githubusercontent.com/byaxb/DataMiningExperiments/master/data/cj.csv"
cj <- readr::read_csv(file_name)
cj <- as.data.frame(cj)

View(cj)
str(cj)
head(cj)
View(tail(cj, n = 10))

cj$班级 <- factor(cj$班级)
cj$性别 <- factor(cj$性别)
cj$文理分科 <- factor(cj$文理分科)
str(cj)

for(i in c("班级", "性别", "文理分科")) {
  cj[, i] <- factor(cj[, i])
}
str(cj)
cj$总成绩 <- apply(cj[, 4:12], 1, sum)
View(cj)
cj <- cj[, -1]
cj$X <- NULL
cj$等级水平 <- cut(cj$总成绩, 
               breaks = fivenum(cj$总成绩),
               labels = c("差", "中", "良", "优"),
               ordered_result = TRUE)
str(cj)
View(cj)
table(cj$等级水平)
table(cj$等级水平, cj$班级)
table(cj$等级水平, cj$性别)

sort(cj$总成绩)


View(cj)

cj %>%
  arrange(desc(总成绩)) %>%
  View()
cj <- arrange(cj, desc(总成绩))
View(cj)

cj <- cj[-which(cj$总成绩 == 0), ]
names(cj)
View(cj)
cn <- names(cj)
str(cj)
colnames(cj) <-  c("xm", "bj", "xb", "yw", "sx", "wy",
                   "zz", "ls", "dl", "wl", "hx", "sw", "wlfk", "zcj", "djsp")
dzb <- data.frame(cn = cn, py = colnames(cj))

attr(cj, "variable.labels") <- cn#在RStudio中能显示中文的标注
save(cj, dzb, file = "cj.rda")

load("cj.rda", verbose = TRUE)


###########################################
#数据的高矮胖瘦
load("cj.rda")
mean(cj$yw)
mean(cj$yw, trim = 0.1) #
median(cj$yw)
sd(cj$yw)
length(which(cj$yw > mean(cj$yw) - sd(cj$yw) & cj$yw < mean(cj$yw) + sd(cj$yw))) / nrow(cj)
var(cj$yw)
min(cj$yw)
max(cj$yw)
range(cj$yw)
diff(range(cj$yw))
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
load(".cj.rda")
str(cj)
library(rpart)
tree.model <- rpart(wlfk~., data = cj[, 4:13])
plot(tree.model,
     uniform = TRUE, 
     branch = 0.8, 
     margin = 0.1)
text(tree.model, 
     all = TRUE, 
     use.n = TRUE, cex = 0.7)
tree.model


library(rattle)
fancyRpartPlot(tree.model)


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
X11()
x11()
rpart.plot(tree.model,
           type=4, 
           fallen=T, 
           branch=.5, 
           round=0, 
           leaf.round=6,
           clip.right.labs=F,
           cex = 0.75,
           under.cex=0.75,
           box.palette="GnYlRd",
           prefix="文理分科\n", 
           branch.col="gray", 
           branch.lwd=2,
           extra=101, 
           under=T, 
           lt=" < ", 
           ge=" >= ",
           split.cex=0.85,
           #split.yspace = 2,
           split.fun=split.fun)
library(rpart.plot)
x11()
rpart.plot(tree.model,
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
bringToTop(which = dev.cur(), stay = TRUE)


wlfk.predicted <- predict(tree.model, cj, type = "class")
confusion.table <- table(wlfk.predicted, cj$wlfk)
sum(diag(confusion.table)) / sum(confusion.table)

xlj <- sample(1:nrow(cj), round(nrow(cj)*0.7))
tree.model <- rpart(wlfk~., data = cj[xlj, 4:13])
xlj.pre <- predict(tree.model, cj[xlj, ], type = "class")
xlj.confusion.matrix <- table(cj[xlj, "wlfk"], xlj.pre)
csj.pre <- predict(tree.model, cj[-xlj, ], type = "class")
csj.confusion.matrix <- table(cj[-xlj, "wlfk"], csj.pre)
sum(diag(xlj.confusion.matrix)) / sum(xlj.confusion.matrix)
sum(diag(csj.confusion.matrix)) / sum(csj.confusion.matrix)


#输出规则
library(rattle)
asRules(tree.model)
View(asRules(tree.model))




###########################################
#距离关系
kcjl <- hclust(dist(t(scale(cj[, 4:12])))) 
plot(kcjl, h = -1)
rect.hclust(kcjl, 3)

cjjl <- hclust(dist(cj[, 4:12]), method = "ward.D") 
confusion.table <- table(cutree(cjjl, 2), cj$wlfk)
sum(diag(confusion.table)) / sum(confusion.table)


###########################################
#伴随关系
rm(list = ls())
load("cj.rda", verbose = TRUE)
str(cj)
library(arules)
apriori(cj)
cjbq <- c("差", "中", "良", "优")
cjdj <- list()
for(i in 4:12) {
  cjdj[[names(cj)[i]]] <- cut(cj[, i], fivenum(cj[, i]), cjbq, include.lowest = TRUE)
} 
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
load("cj.rda")
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
