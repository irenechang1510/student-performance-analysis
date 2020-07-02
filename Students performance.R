packages <- c("RCurl","data.table","tidyverse","reshape2", "ggplot2","gridExtra")
sapply(packages, require, character.only=T)

path<- getwd()
path <- file.path(path, "Student")
list.files(path)
math <- fread(file.path(path,"student-mat.csv"), stringsAsFactors=T)
port <- fread(file.path(path,"student-por.csv"), stringsAsFactors=T)

math$average <- round(apply(select(math, G1, G2, G3), 1, mean), 2)
port$average <- round(apply(select(port, G1, G2, G3), 1, mean), 2)

#manipulate
sum(is.na(data)) #no NAs
math$subject<- rep("Math", nrow(math))
port$subject<- rep("Portugese", nrow(port))
data <- rbind(math, port)
#tidy variables
#support categories
data$support <- "0"
data[schoolsup=="yes"& famsup=="yes"]$support<- "both"
data[schoolsup=="yes"& famsup!="yes"]$support<- "school"
data[schoolsup!="yes"& famsup=="yes"]$support<- "family"
data[schoolsup!="yes"& famsup!="yes"]$support<- "none"
data$support <- as.factor(data$support)
data <- select(data, -c("schoolsup","famsup"))

data$Medu <- as.factor(data$Medu)
data$Fedu <- as.factor(data$Fedu)
#split dataset into big categories
family<- select(data, c(5:10, 12, 22, 29:33))
personal<- select(data, -c(5:10, 12, 22))

graph <- function(data, x, y){
	a <- enquo(x)
	b <- enquo(y)
	dt<- enquo(data)
	ggplot(data, aes(!!a, !!b))+
		geom_smooth(method=lm, color="blue",se=T)+
		geom_jitter(alpha=0.7)+
		labs(title= deparse(substitute(data)), subtitle=nrow(data))
}
p1<- graph(math, G1, G3)
p2<- graph(math, G2, G3)
p3<- graph(port, G1, G3)
p4<- graph(port, G2, G3)
grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
#G1 and G2 positively correlates with G3
#There fore, let's see what affects G1, and G2 
#family dataset
family %>%
	gather(key=test, value=grade, -c(1:8, 13)) %>%
	ggplot(aes(x=famsize, y=grade))+geom_boxplot()+
	geom_jitter(size=0.4, alpha=0.8)+
	facet_wrap(~test)+
	xlab("Family size")
family %>%
	gather(key=test, value=grade, -c(1:8, 13)) %>%
	ggplot(aes(x=Pstatus, y=grade))+geom_boxplot()+
	facet_wrap(~test)+ geom_jitter(size=0.4, alpha=0.8)+
	xlab("Parental status")

#difference in mean average score significant?
t.test(average~famsize,data=family, var.equal=T, alternative="greater")
t.test(average~Pstatus,data=family, var.equal=T, alternative="greater")
#difference in proportions getting 0 in final between 2 parental status?
nrow(family[Pstatus=="T" & G3<2.5]) 
nrow(family[Pstatus=="T"]) 
nrow(family[Pstatus=="A" & G3<2.5])
nrow(family[Pstatus=="A"])
prop.test(x=c(50,4), n=c(923,121), correct=T)
#difference in proportions getting 0 in final between 2 famsize?
nrow(family[famsize=="GT3" & G3<2.5])
nrow(family[famsize=="GT3"]) 
nrow(family[famsize=="LE3" & G3<2.5])
nrow(family[famsize=="LE3"])
prop.test(x=c(45,9), n=c(738,306), correct=T)
#Parents' jobs
summary(family)
family %>%
	select(Mjob, Fjob, average)%>%
	gather(key=Parent, value=job, -average) %>%
	ggplot(aes(job, average, fill=Parent))+
	geom_boxplot(varwidth=T, alpha=0.2)+
	xlab("Parents' jobs")+
	scale_fill_discrete(labels=c("Father","Mother"))
#difference in mean across 5 groups?
mdM <- aov(average~Mjob, family)
plot(mdM, 1) #homogeneity of variance met
plot(mdM, 2) 
shapiro.test(residuals(mdM)) #not normally distributed, cannot use anova, use Kruskal-Wallis Test
kruskal.test(average~Mjob, family) #p-value<0.05
##which group is significantly different? 
pairwise.wilcox.test(family$average, family$Mjob, p.adjust.method = "BH")

mdF <- aov(average~Fjob, family)
plot(mdF, 1)
plot(mdF, 2)
shapiro.test(residuals(mdF))
kruskal.test(average~Fjob, family) #p-value<0.05
pairwise.wilcox.test(family$average, family$Fjob, p.adjust.method = "BH")

#parents' education
family %>%
	select(Medu, Fedu, average) %>%
	gather(key=Parent, value=education, -average) %>%
	ggplot(aes(x=factor(education), average, fill=Parent))+
	geom_boxplot(varwidth=T, alpha=0.2)+
	xlab("Parents' education")+
	scale_fill_discrete(labels=c("Father","Mother"))
meM <- aov(average~Medu, family)
plot(meM, 1)
plot(meM, 2)
shapiro.test(residuals(meM))
kruskal.test(average~Medu, family)
pairwise.wilcox.test(family$average, family$Medu, p.adjust.method = "BH")

family %>%
	select(famrel,average) %>%
	ggplot(aes(x=factor(famrel),y=average, fill=factor(famrel)))+
	geom_violin(width=0.5)+
	geom_boxplot(width=0.1, color="black", alpha=0.2)+
	theme(legend.position="none")

