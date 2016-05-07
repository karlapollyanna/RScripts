#!/usr/bin/env Rscript

###################################################################################################

### Script: PCA Analysis 
### Version: 0
### Date: 05/06/2016
### Sponsor: Karla Pollyanna Vieira de Oliveira
### Cellphone: +55 (48) 9133-9078 || +55 (31) 987-677-667
### Email:  karla.biotecnologia@gmail.com || karla@intelab.ufsc.br 
### Lattes: http://lattes.cnpq.br/7673656510731331

##### Checking packages and installing/loading when necessary

Install_And_Load <- function(Required_Packages)
{
  Remaining_Packages <- Required_Packages[!(Required_Packages %in% installed.packages()[,"Package"])];
  if(length(Remaining_Packages)) 
  {
    install.packages(Remaining_Packages);
  }
  for(package_name in Required_Packages)
  {
    library(package_name,character.only=TRUE,quietly=TRUE);
  }
}

Required_Packages = c("corrplot", "PerformanceAnalytics", "scatterplot3d")  
Install_And_Load(Required_Packages)

##### Creates output directory

dir.create("/home/karla/FinalPlot/Llo.PCA")

##### Reading file

PCA <- read.csv("~/CSVs/Llo.PCA.csv", header = TRUE, sep=",")
rows <- nrow(PCA)

##### Defining Variables to be uses

d1 <- PCA[,2:9]
pr=prcomp(d1, cor=TRUE, scores=TRUE, scale=TRUE)

pdf(file="/home/karla/FinalPlot/Llo.PCA/Llo.PCA.VariablesContribution.Choosing.out.pdf")
plot(fviz_pca_var(pr, col.var="contrib") + theme_minimal())
dev.off()

cor.mat <- round(cor(d1),2)

pdf(file="/home/karla/FinalPlot/Llo.PCA/Llo.PCA.VariablesCorrplot.Choosing.out.pdf")
corrplot(cor.mat, type="upper", order="hclust", tl.col="black", tl.srt=45)
dev.off()


##### Defined Variables to be used

pdf(file="/home/karla/FinalPlot/Llo.PCA/Llo.PCA.VariablesContribution.Chosen.out.pdf")
d2 <- PCA[,c(2,6,9)] # INSERT THE COLUMNS NUMBERS PREVIOUSLY DECIDED AND ROWS TOO
pr2 = prcomp(d2, cor=TRUE, scores=TRUE, scale=TRUE)
plot(fviz_pca_var(pr2, col.var="contrib") + theme_minimal())
dev.off()

pdf(file="/home/karla/FinalPlot/Llo.PCA/Llo.PCA.VariablesCorrplot.Chosen.out.pdf")
cor.mat <- round(cor(d2),2)
corrplot(cor.mat, type="upper", order="hclust", tl.col="black", tl.srt=45)
dev.off()


#### 3D Visualization of Raw Data

pdf(file="/home/karla/FinalPlot/Llo.PCA/Llo.PCA.rawdata.out.pdf")
DF <- data.frame(d2, group = sample(PCA[,10],rows, replace = TRUE))
s3d <- with(DF, scatterplot3d(d2, main = "Llo: Main Features", xlab = "RPM", ylab = "Ratio 5p/3p", zlab = "%GC", y.margin.add = 0.7, color = as.numeric(group), pch = 20, angle = 45, scale.y = 0.5))
legend("topright", s3d$xyz.convert(0.5, 0.7, 0.5), pch = 19, yjust=0, legend = levels(DF$group), col = seq_along(levels(DF2$group)))
dev.off()

#### 3D Visualization of PCA components

pdf(file="/home/karla/FinalPlot/Llo.PCA/Llo.PCA.out.pdf")
DF2 <- data.frame(x = pr2$x[,1], y = pr2$x[,2], z = pr$x[,3], group = sample(PCA[,10],rows, replace = TRUE))
s3d2 <- with(DF2, scatterplot3d(x,y,z, main = "Llo: PCA", xlab = "RPM", ylab = "Ratio 5p/3p", zlab = "%GC", y.margin.add = 0.7, color = as.numeric(group), pch = 20, angle = 10, scale.y = 0.5))
legend("topright", s3d$xyz.convert(0.5, 0.7, 0.5), pch = 19, yjust=0, legend = levels(DF2$group), col = seq_along(levels(DF$group)))
dev.off()

pdf(file="/home/karla/FinalPlot/Llo.PCA/Llo.PCA.IndividualsFactorMap.out.pdf")
plot(fviz_pca_ind(pr2, label = "none", habillage=PCA[,10]))
dev.off()

pdf(file="/home/karla/FinalPlot/Llo.PCA/Llo.PCA.IndividualsFactorMap.Ellipses.out.pdf")
plot(fviz_pca_ind(pr2, label="ind", habillage=PCA[,10], addEllipses=TRUE, ellipse.level=0.95))
dev.off()

