
library(ggpubr)
library(GGally)
library(viridis)
library(tidyverse)
library(treemapify)
library(ggthemes)
library(ggpubr)
library(forcats)
library(scales)

df <- read.csv("C:/Users/10331/OneDrive/Desktop/Assignment1/ramen-ratings.csv")
#https://www.kaggle.com/residentmario/ramen-ratings

df <- df[,c(-1,-ncol(df))]
df <- df[!duplicated(df), ]
df <- df[df$Stars != "Unrated",]
df <- df[,-2]
df <- df[!(df$Style %in% c("","Bar","Can","Box")),]
df[df$Stars>4.999,"Stars"] = 5
dim(df)
df$Stars = as.numeric(as.character(df$Stars))
str(df)

for (i in colnames(df)){
  nuniq <- nrow(unique(df[i]))
  print(c(i,nuniq))
  sad<-as.data.frame(table(df[i]))
  print(head(sad[order(-sad["Freq"]),],10))
}

sad<-as.data.frame(table(df["Brand"]))
print(sad[order(-sad["Freq"]),])
tmp <- sad[sad$Freq>24,]
df[!(df$Brand %in% tmp$Var1),"Brand"] = "others"

sad<-as.data.frame(table(df["Country"]))
print(sad[order(-sad["Freq"]),])
tmp <- sad[sad$Freq>30,]
df[!(df$Country %in% tmp$Var1),"Country"] = "others"

#Stacked Bar Chart for Style/Brand

df1 <- df[,c("Brand","Style")]
df1 <- as.data.frame(table(df1))

names <- levels(unique(df1$Brand))
for (i in names){
  df1[df1$Brand==i,"Freq"]<-df1[df1$Brand==i,"Freq"]/sum(df1[df1$Brand==i,"Freq"])
}

theme_bluewhite <- function (base_size = 11, base_family = "") {
  theme_bw() %+replace% 
    theme(
      panel.grid.major  = element_line(color = "white"),
      panel.background = element_rect(fill = "transparent"),
      panel.border = element_rect(color = "lightblue", fill = NA),
      axis.line = element_line(color = "lightblue"),
      axis.ticks = element_line(color = "lightblue"),
      legend.position="none",
      plot.margin = margin(0, 0, 0, 0)
    )
}

#Cite to:https://www.datanovia.com/en/blog/ggplot-themes-gallery/

barchart <- ggplot(df1, aes(y=Freq, x=fct_reorder2(Brand, Style=="Style",
                                       Freq), fill=factor(Style, 
                            levels=c("Tray","Cup","Bowl", "Pack")))) + 
  geom_bar(stat="identity") +
  scale_x_discrete("") +
  scale_y_continuous("",labels = scales::percent_format(accuracy = 1), breaks=seq(0, 1, by=0.1),position = "right") +
  theme_bluewhite() +
  theme(axis.text.x=element_text(angle=-30, hjust = 0), legend.position = c(0.994,0.04), 
        legend.direction = "vertical",legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 9),legend.key.size = unit(0.5, "cm"),
        legend.justification = c("right", "bottom"),
        legend.margin = margin(0, 0, 0, 0),
        legend.box.just = "right",
        plot.caption = element_text(size=12, hjust=0.5, face="italic", color="black"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA))

barchart <- barchart + geom_text(aes(label=ifelse(Freq >=0.15, paste(sprintf("%.0f",
                      Freq*100)),"")), position=position_stack(vjust=0.5), color="white") +
  labs(caption ="Ramen Style % by Brands") +
  scale_fill_manual("", labels=c("Tray", "Cup","Bowl", "Pack"), 
                    values=c("#E69F00","#F0E442","#009E73","#0072B2")) 


barchart

# Boxplot per Brands
avg = mean(df$Stars)
q4 = quantile(df$Stars,0.75)
q2 = quantile(df$Stars,0.25)
o1 = q2 - 1.5*(q4-q2)

boxplot <- ggplot(data = df, aes(x=Stars, y=Brand, fill = Brand)) + 
  geom_boxplot() +
  geom_vline(xintercept = avg, linetype = "dashed") +
  geom_vline(xintercept = q4, linetype = "dashed") +
  geom_vline(xintercept = q2, linetype = "dashed") +
  geom_vline(xintercept = o1, linetype = "dashed") +
  theme_bluewhite() +
  annotate("text", label = "Uneatable:(", x = 0.8, y = 1.5, size = 3.5) +
  annotate("text", label = "Unenjoyable", x = 2.5, y = 1.5, size = 3.5) +
  annotate("text", label = "Fantastic!", x = 4.8, y = 20.7, size = 3.5) +
  labs(subtitle ="Ratings by Brands (with quantiles)") +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(size=12, hjust=0.5, face="italic", color="black"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA))

boxplot
  
# Treemap for Brands
num <- as.data.frame(table(df$Brand))
colnames(num) <- c("Brand","Freq")
num <- num[num["Brand"]!="others",]

treemap1 <- ggplot(num, aes(area = Freq,fill = Brand, label = Brand,subgroup = Brand)) +
  geom_treemap() +
  geom_treemap_subgroup_border(color = "white") +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.45, colour =
                               "black", fontface = "italic", min.size = 0) +
  geom_treemap_text(aes(label = label_percent(accuracy = 1)(Freq/sum(Freq))),
                    alpha = 0.35,colour = "black", place = "topleft") +
  theme(legend.position="None", plot.margin = margin(0, 0, 0, 0),
        plot.caption = element_text(size=12, hjust=0.5, face="italic", color="black"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA))+
  labs(caption ="Delegates by Brands")

treemap1

#Cite to:https://cran.r-project.org/web/packages/treemapify/vignettes/introduction-to-treemapify.html

# Treemap for Counties
num2 <- as.data.frame(table(df$Country))
colnames(num2) <- c("Country","Freq")
num2 <- num2[num2["Country"]!="others",]
for (i in num2$Country){
  num2[num2$Country == i, "Stars"] <- mean(df[df$Country == i, "Stars"])
}

treemap2 <- ggplot(num2, aes(area = Freq, fill = Stars ,label = Country, subgroup = Country)) +
  geom_treemap() +
  geom_treemap_subgroup_border(color = "white") +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.45, colour =
                               "black", fontface = "italic", min.size = 0) +
  geom_treemap_text(aes(label = label_percent(accuracy = 1)(Freq/sum(Freq))),
                    alpha = 0.35,colour = "black", place = "topleft") +
  scale_fill_continuous("Rating",low="#132B43",high="#56B1F7") +
  theme(plot.margin = margin(0, 0, 0, 0),
        legend.margin = margin(0,2,0,0),
        legend.box.margin=margin(0,0,0,-7),
        plot.subtitle = element_text(size=12, hjust=0.5, face="italic", color="black"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent"))+
  labs(subtitle ="Delegates by Countries/Ratings")

treemap2

#ggpubr
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

p <- ggplot() + theme_void()

final <- ggarrange(arrangeGrob(boxplot,treemap2, ncol=2, nrow=1, widths=c(1,1)),
                   arrangeGrob(p,treemap1,barchart, ncol=3, nrow=1, widths=c(1,8,9)),
          ncol = 1, nrow = 2, heights = c(9,10),widths = c(1,1))

final <- annotate_figure(final,
                top = text_grob("The Ultimate Ramen Guide", face = "bold", size = 24),
                bottom = text_grob("Source: Kaggle:https://www.kaggle.com/residentmario/ramen-ratings",
                                     hjust = 1, x = 1, face = "italic", size = 10)) +
  bgcolor("#BFD5E3")+
  border("#BFD5E3")

final

ggsave("Ramen.pdf", width = 12, height = 11)
ggsave("Ramen.png", width = 12, height = 11)

