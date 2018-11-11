all_matrix<- read.csv("C:/Users/lenovo/Desktop/网络学习分析课程学习文件/作业/all_matrix.csv",row.names=1)
library(sna)
#gplot()利用以xy坐标表示顶点位置的两列矩阵的数据生成二维图像，displayables值为是否显示顶点标签，lable.cex控制标签文本的大小倍率,vertex.col控制顶点颜色;如果顶点是不同颜色的，可以用向量的形式给出。默认情况下，使用红色(或红色和蓝色，用于双模式数据)。
#基于all_matrix生成社交网络的二维图像，显示节点，节点颜色为黄色，节点大小为0.8倍
gplot(all_matrix,gmode="digraph",displaylabels=TRUE,label.cex=0.8,vertex.col="yellow")

#用network()将矩阵转换为网络格式以便进一步分析
overallnet=network(all_matrix)
overallnet


##################################
###### 节点级分析###########
##################################
#计算节点的度中心性（degree）。顶点的度(degree)是图最基本的结构，是指与它关联的边的数量。有向图会区分入度(in-degree)和出度(out-degree)。gmode表示数据是否有向（digraph为有向，graph非向），cmode表示计算的度的类型（入度/出度）(默认是出入度都计算)
#分别计算出入度
id<-degree(overallnet,gmode="digraph",cmode="indegree")
od<-degree(overallnet,gmode="digraph",cmode="outdegree")
id
od
id+od
#计算中介中心性（betweenness）。中介中心性指的是一个结点担任其它两个结点之间最短路的桥梁的次数。betweenness参数（rescale为真，中心度的和为1）
bet1=betweenness(overallnet,rescale=T)
bet2=betweenness(overallnet)
bet1
bet2
#计算接近中心性（closeness）。接近中心性考量每节结点到其它节点的最短路的平均长度。
clo1=closeness(overallnet,rescale=T)
clo2=closeness(overallnet)
clo1
clo2
#计算节点的特征向量中心度——evcent,这个方法是根据相邻节点的重要性来衡量该节点的价值。
eig1=evcent(overallnet,rescale=T)
eig2=evcent(overallnet)
eig1
eig2

#利用出入度绘制图像
#gplot()，boxed.lables是否将节点标签放在方框中（默认不放）,lable.pos，标签相对于顶点的位置(pos值为0时标签偏离标绘区域中心;1、2、3和4标签分别在节点的下、左、上和右边;pos>=5时标签在节点上)。edge.lwd控制连线宽度
#节点大小与出入度的和成正比。节点的degree值越高，颜色越鲜艳。连线宽度与交互次数成正比。
gplot(overallnet, vertex.cex=(id+od)^0.5/2, gmode="graph",
      boxed.labels=TRUE,label.cex=0.7, label.pos=5, label.col="grey17",
      vertex.col=rgb((id+od)/max(id+od),0,(id+od)/max(id+od)),edge.col="grey17",
      label=network.vertex.names(overallnet),edge.lwd=all_matrix/2,mode = "fruchtermanreingold")
#由此可知，教师T1的出入度总和最高，这可能因为大多数学生欣赏和感谢其在线课程设计和辅导，也可能是由于教师T1的积极参与讨论。

###########################################
######## graph level analysis  ######
###########################################
#整个网络的集中趋势(可简称为中心势)(Centralization)。与个体中心度刻画的是个体特性不同，网络中心势刻画的是整个网络中各个点的差异性程度，因此一个网络只有一个中心势。中心势也可以分为3种：点度中心势，中间中心势，接近中心势。网络点度中心势指的是网络中点的集中趋势。中间中心势也是分析网络整体结构的一个指数，其含义是网络中中间中心性最高的节点的中间中心性与其他节点的中间中心性的差距。该节点与别的节点的差距越大，则网络的中间中心势越高，表示该网络中的节点可能分为多个小团体而且过于依赖某一个节点传递关系，该节点在网络中处于极其重要的地位。接近中心势的含义是，对一个社会网络来说它的值越高，表明网络中节点的差异性越大，反之，则表明网络中节点间的差异越小。
centralization(overallnet,degree)
centralization(overallnet,degree,cmode="outdegree")
centralization(overallnet,degree,cmode="indegree")
#
centralization(overallnet, betweenness)
centralization(overallnet, closeness)
centralization(overallnet, evcent)

#network.size()网络的大小——由19个学生，1个老师组成的20人的网络。
network.size(overallnet)
gden(overallnet,mode="graph") #图的密度density
degree(overallnet)
mean(degree(overallnet)) #average degree
sum(id)/20 #average in degree
sum(od)/20 #average out degree

#transitivity(overallnet)社交网络的交互性（传递性）。
#得出对称（相互）交互，非对称交互和无交互的数目，分别为66，62，62。
dyad.census(overallnet)
#所有可能的交互数目（有向）为380（去除标记为丢失的）.
network.dyadcount(overallnet, na.omit = F)
#交互连线总数为194（去除标记为丢失的）。
network.edgecount(overallnet, na.omit = F)
#连线宽度互易性
grecip(overallnet, measure = "edgewise")
#二元互易性
grecip(overallnet, measure = "dyadic")
#非空白二元互易性
grecip(overallnet, measure = "dyadic.nonnull")
#传递性
gtrans(overallnet)
#网络的层次分数
hierarchy(overallnet, measure = "reciprocity")
#网络的Krackhardt层次结构得分,Krackhardt层次结构被定义为在不对称的可达图中非空双偶的分数。
hierarchy(overallnet, measure = "krackhardt")

#网络中的组件数量
components(overallnet,connected="weak")
#网络的Krackhardt连接度得分
connectedness(overallnet, g=NULL)

# 测地线：从某节点出发，沿图的边（连线）到达另一节点所经过的路径中，各边上权值之和最小的一条路径即测地线。得到所有测地线值的矩阵
geodist(overallnet, inf.replace=Inf, count.paths=TRUE, predecessors=FALSE,
        ignore.eval=TRUE)
geo=geodist(overallnet)
#最短测地线
max(geo$gdist)

## 定义路径长度函数：
averagePathLength<-function(net){
  if(!is.network(net)){stop("Not a Network")}
  gd<-geodist(net)
  if(net%n%"directed"){
    return((1/choose(network.size(net),2))*sum(gd$gdist))
  }
  (1/(2*choose(network.size(net),2)))*sum(gd$gdist)
}

## 最短路径函数：
diameter<-function(net){
  gd<-geodist(net)
  max(gd$gdist)
}

## 计算平均路径长度。
averagePathLength(overallnet)

## 计算最短路径。
diameter(overallnet)

