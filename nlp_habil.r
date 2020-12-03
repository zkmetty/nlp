
###Basic Word Embedding analysis tools

library(dplyr)
library(text2vec)
library(igraph)

#Download pre-trained vector models:
#https://fasttext.cc/docs/en/english-vectors.html



#Use the most common 200k words
wiki <- readLines("wiki-news-300d-1M.vec", n=200000, skip=1)


#Create the vector-space models
wiki <- wiki[2:length(wiki)]

wiki_list <- strsplit(wiki[]," ")
wiki_vec <- matrix(nrow=length(wiki), ncol=300)

rownames(wiki_vec) <- sapply(wiki_list,"[[",1)

for (i in 1:300)
  wiki_vec[,i] <- as.numeric(sapply(wiki_list,"[[",(i+1)))


rm(i)
rm(wiki)
rm(wiki_list)

#What are the closest neighbours of sociology?

s.word <- wiki_vec["sociology",,drop = FALSE]
dist.s <- sim2(x = wiki_vec, y = s.word, method = "cosine", norm = "l2")
dist.s[order(dist.s, decreasing=T),][1:40]

#Distance of political and sociology

sim2(x = wiki_vec["political",,drop = FALSE], y = wiki_vec["sociology",,drop = FALSE], method = "cosine", norm = "l2")

#Distance of political+science and sociology

sim2(x = wiki_vec["political",,drop = FALSE]+wiki_vec["science",,drop = FALSE], y = wiki_vec["sociology",,drop = FALSE], method = "cosine", norm = "l2")

szavak <- c("sociology","psychology","economics","philosophy","linguistics","biology","physics","mathematics","engineering","chemistry")
szavak_vec <- wiki_vec[szavak,]

koz.matrix <- sim2(x =szavak_vec, y = szavak_vec, method = "cosine", norm = "l2")


vec.clust <- hclust(as.dist(1-koz.matrix), "ward.D")
plot(vec.clust)


#Network
tr=0.6
dist.matrix2 <- koz.matrix
dist.matrix2[which(koz.matrix<tr)] <- 0 
dist.matrix2[which(koz.matrix>=tr)] <- 1


dist.matrix.graph <- graph_from_adjacency_matrix(dist.matrix2, "upper", diag=F)

#Megcsináljuk azt, hogy két csoportra vágjuk szét a klaszterezést, és a kisebb csoportot bekaikázzuk
clust.group <- cutree(vec.clust, k = 2)



plot(dist.matrix.graph, vertex.shape="none", vertex.label.font=2, 
     vertex.label.color=c(rep("black",length(V(dist.matrix.graph)))), 
     vertex.label.cex=1.2, edge.color="navyblue",
     mark.groups=list(which(clust.group==1),
                      which(clust.group==2))
     , mark.col=c("#C5E5E7","lightred"))



##Tsne

szodist <- Rtsne(szavak_vec, perplexity=3)

df_sne <-  as.data.frame(bind_cols(szavak,  as.numeric(szodist$Y[,1]),  as.numeric(szodist$Y[,2])))

colnames(df_sne) <- c("terulet","dim1","dim2")


ggplot(df_sne, aes(x=dim1, y= dim2, label = terulet)) +
  geom_point(size=3) + 
  #geom_text(size=7, colour="darkblue") +
  xlab("X") + ylab("Y") + 
  #  xlim(0.3,0.6) +
  #  ylim(0.3,0.6) +
  theme_bw() +
  geom_text_repel(label=df_sne$terulet, size=7) +
  theme(text = element_text(size=20, colour = "black"), plot.title = element_text(size = 30, face = "bold"), legend.position="none")




##
szavak2 <- c("sociologist","psychologist","economist","philosopher","linguist","biologist","physicist","mathematician","engineer","chemist")
szavak_vec2 <- wiki_vec[szavak2,]


d1 <- round(sim2(x = wiki_vec["social",,drop = FALSE], y = szavak_vec, norm = "l2"),2)

d2 <- round(sim2(x = wiki_vec["technical",,drop = FALSE], y = szavak_vec, norm = "l2"),2)


df <- as.data.frame(cbind(szavak, t(d1),t(d2)))

colnames(df) <- c("terulet","soft","hard")



#require(ggrepel)

ggplot(df, aes(x=hard, y= soft, label = terulet)) +
  geom_point(size=3) + 
  #geom_text(size=7, colour="darkblue") +
  xlab("technical") + ylab("social") + 
#  xlim(0.3,0.6) +
#  ylim(0.3,0.6) +
  theme_bw() +
  geom_text_repel(label=df$terulet, size=7) +
  theme(text = element_text(size=20, colour = "black"), plot.title = element_text(size = 30, face = "bold"), legend.position="none")


f1 <- round(sim2(x = wiki_vec["he",,drop = FALSE] + wiki_vec["man",,drop = FALSE], y = szavak_vec2, norm = "l2"),2)
f2 <- round(sim2(x = wiki_vec["she",,drop = FALSE] + wiki_vec["woman",,drop = FALSE], y = szavak_vec2, norm = "l2"),2)

df2 <- as.data.frame(bind_cols(szavak2,  as.numeric(f1),  as.numeric(f2)))


colnames(df2) <- c("szakma","he","she")

ggplot(df2, aes(x=he, y= she, label = szakma)) +
  geom_point(size=3) + 
  #geom_text(size=7, colour="darkblue") +
  xlab("male dimension") + ylab("female dimension") + 
  xlim(0.35,0.55) +
    ylim(0.35,0.55) +
  theme_bw() +
  geom_text_repel(label=df2$szakma, size=7) +
  geom_abline(intercept = 0, slope = 1) +
  theme(text = element_text(size=20, colour = "black"), plot.title = element_text(size = 30, face = "bold"), legend.position="none")


##Korrupció


s.word <- wiki_vec["corruption",,drop = FALSE]
dist.s <- sim2(x = wiki_vec, y = s.word, method = "cosine", norm = "l2")
dist.s[order(dist.s, decreasing=T),][1:40]

c.list <- c("USA", "Germany", "Hungary", "France", "Italy", "Greece", "Spain","Sweden","Denmark",
               "England","Brazil","China","Russia","Romania","Slovakia")

c_vec <- wiki_vec[c.list,]
dist.s <- round(sim2(x = wiki_vec
           , y = wiki_vec["corruption",,drop = FALSE]+ wiki_vec["China",,drop = FALSE]- wiki_vec["country",,drop = FALSE], norm = "l2"),2)

dist.s[order(dist.s, decreasing=T),][1:30]

dist.s <- round(sim2(x = wiki_vec
                     , y = wiki_vec["corruption",,drop = FALSE]+ wiki_vec["Brasil",,drop = FALSE]- wiki_vec["country",,drop = FALSE], norm = "l2"),2)

dist.s[order(dist.s, decreasing=T),][1:30]


##Korrupciós index összevetése a TCI-vel
dist.s <- round(sim2(x = c_vec
                     , y = wiki_vec["corruption",,drop = FALSE], norm = "l2"),2)



tci <- c(75,81,45,70,50,48,57,84,88,82,37,41,29,48,50)


cor(dist.s, tci)

cor.words <- c("corruption","cronyism","malfeasance", "venality","graft","nepotism","bribery")

cor.matrix <- matrix(nrow=length(c.list), ncol=length(cor.words))

for (i in 1:length(cor.words))
  cor.matrix[,i] <- round(sim2(x = c_vec
                               , y = wiki_vec[cor.words[i],,drop = FALSE], norm = "l2"),2)


cor(apply(cor.matrix,1,mean), tci)
cor(apply(cor.matrix[,1:6],1,mean), tci)


pc.corrup <- princomp(cor.matrix)
summary(pc.corrup)
cor(pc.corrup$scores[,1],tci)


dist.s <- round(sim2(x = c_vec
                     , y = wiki_vec["corruption",,drop = FALSE]+ wiki_vec["bribery",,drop = FALSE] +
                      wiki_vec["cronyism",,drop = FALSE]+
                       wiki_vec["malfeasance",,drop = FALSE]+
                       wiki_vec["venality",,drop = FALSE]+
                       wiki_vec["graft",,drop = FALSE]+
                       wiki_vec["nepotism",,drop = FALSE]
                     , norm = "l2"),2)

cor(dist.s, tci)

dist.s <- round(sim2(x = c_vec
                     , y = wiki_vec["corruption",,drop = FALSE]+ wiki_vec["nepotism",,drop = FALSE] +
                       wiki_vec["cronyism",,drop = FALSE]+
                       wiki_vec["malfeasance",,drop = FALSE]+
                       wiki_vec["venality",,drop = FALSE]+
                       wiki_vec["graft",,drop = FALSE]
                     , norm = "l2"),2)

cor(dist.s, tci)

df4 <- bind_cols(c.list, as.numeric(dist.s),as.numeric(tci))


colnames(df4) <- c("orszagok","we","tci")

ggplot(df4, aes(x=we, y= tci, label = orszagok)) +
  geom_point(size=3) + 
  #geom_text(size=7, colour="darkblue") +
  xlab("index - vektortérből képezve") + ylab("tci") + 
  theme_bw() +
  geom_text_repel(label=df4$orszagok, size=7) +
  geom_abline(intercept = 0, slope = 1) +
  theme(text = element_text(size=20, colour = "black"), plot.title = element_text(size = 30, face = "bold"), legend.position="none")

