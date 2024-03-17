## -----------------------n-of-1--------alpha diversity calculations-----------------------
rm(list=ls())
library(vegan)
data <- readxl::read_xlsx("Source_data_002_unfiltered.xlsx",sheet = "Sheet1") 
alpha <- data[,6:400]
shannon <- diversity(alpha, index = 'shannon', base = exp(1))
gini_simpson <- diversity(alpha, index = 'simpson')
richness <- rowSums(alpha > 0)
individual_alpha <- data.frame(data$participants_id, data$time_point, richness, shannon, gini_simpson)

write.table(individual_alpha, file="individual_alpha_diversity.csv", row.names = FALSE) ##source_data_003 was derived from this results


####dynamics of the alpha diversities across the 12 timepoints
## visualization----------take the shannon index as an example ------------------------------------
rm(list=ls())
library(readr)
data<- readxl::read_xlsx("Figure 2a Source_data.xlsx",sheet = "Sheet1")
data.mean<- readxl::read_xlsx("Figure 2a Source_data.xlsx",sheet = "mean_stratified_by_timepoit")

a <- ggplot(data, aes(x=time_point, y=shannon, group=group)) + 
  geom_line(data=data, aes(x= time_point, y=shannon, group=subject), col = "grey", alpha = 0.6)+
  geom_point(size=3, alpha=0.3, color="green")+
  geom_line(data=data.mean, aes(x=time_point, y=shannon, group=group), color="cadetblue", size=2)+
  
  xlab("Time")+
  ylab("diversity")+
  facet_grid(.~group)+
  
  theme_bw()+
  theme( 
    strip.text.x = element_text(size = 12, face = "bold", angle = 0),
    strip.background = element_rect(color = "transparent", fill="transparent"),
    
    # for legend
    legend.key.height = unit(35, "pt"),
    legend.key.width = unit(35, "pt"),
    legend.text = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    
    # for axis
    axis.title.x = element_text(size=12,face = "bold", color = "black"),
    axis.title.y = element_text(size=12,face = "bold", color = "black"),
    axis.text = element_text(size=10, face = "bold"),
    
    # for background
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    legend.box.background = element_rect(color = "black"))
a

####group level box-plot
rm(list=ls())
library(readr)
dat<- readxl::read_xlsx("Figure 2a Source_data.xlsx",sheet = "combined_HC")
library(tidyverse)
theme_boxplot <- theme(panel.background=element_rect(fill="white",colour="black",size=0.25),
                       axis.line=element_line(colour="black",size=0.5),
                       axis.title=element_text(size=18,face="plain",color="black"),
                       axis.text = element_text(size=18,face="plain",color="black"),
                       legend.position="none")

library("ggplot2")
data <- dat %>% mutate(dist_cat = as.numeric(numeric_group),
                       scat_adj = ifelse(numeric_group == "1", -0.0, 0.0))

p1 <- ggplot(data, aes(factor(numeric_group), mean_shannon)) + 
  geom_boxplot(aes(fill = as.factor(numeric_group)), notch = FALSE, size = 0.4) + 
  geom_jitter(aes(scat_adj+dist_cat, mean_shannon, fill = factor(numeric_group)),
              position=position_jitter(width=0,height=0),
              alpha=1,
              shape=21, size = 1.2) +
  scale_fill_manual(values=c("sandybrown","sandybrown"))+ 
  guides(fill=guide_legend(title="Condition")) +  
  theme_boxplot
p1
p2=p1+geom_line(data=dat, aes(x= numeric_group, y=mean_shannon, group=subject), col = "black", alpha = 0.3); p2



p3 <- ggplot(data, aes(factor(numeric_group), mean_richness)) + 
  geom_boxplot(aes(fill = as.factor(numeric_group)), notch = FALSE, size = 0.4) + 
  geom_jitter(aes(scat_adj+dist_cat, mean_richness, fill = factor(numeric_group)),
              position=position_jitter(width=0,height=0),
              alpha=1,
              shape=21, size = 1.2) +
  scale_fill_manual(values=c("sandybrown","sandybrown"))+ 
  guides(fill=guide_legend(title="Condition")) +  
  theme_boxplot
p3
p4=p3+geom_line(data=dat, aes(x= numeric_group, y=mean_richness, group=subject), col = "black", alpha = 0.3); p4

##pre and post LC
rm(list=ls())
library(readr)
dat<- readxl::read_xlsx("Figure 2a Source_data.xlsx",sheet = "combined_LC")
library(tidyverse)
theme_boxplot <- theme(panel.background=element_rect(fill="white",colour="black",size=0.25),
                       axis.line=element_line(colour="black",size=0.5),
                       axis.title=element_text(size=18,face="plain",color="black"),
                       axis.text = element_text(size=18,face="plain",color="black"),
                       legend.position="none")

library("ggplot2")
data <- dat %>% mutate(dist_cat = as.numeric(numeric_group),
                       scat_adj = ifelse(numeric_group == "1", -0.0, 0.0))

p1 <- ggplot(data, aes(factor(numeric_group), mean_shannon)) + 
  geom_boxplot(aes(fill = as.factor(numeric_group)), notch = FALSE, size = 0.4) + 
  geom_jitter(aes(scat_adj+dist_cat, mean_shannon, fill = factor(numeric_group)),
              position=position_jitter(width=0,height=0),
              alpha=1,
              shape=21, size = 1.2) +
  scale_fill_manual(values=c("sandybrown","sandybrown"))+ 
  guides(fill=guide_legend(title="Condition")) +  
  theme_boxplot
p1
p2=p1+geom_line(data=dat, aes(x= numeric_group, y=mean_shannon, group=subject), col = "black", alpha = 0.3); p2



p3 <- ggplot(data, aes(factor(numeric_group), mean_richness)) + 
  geom_boxplot(aes(fill = as.factor(numeric_group)), notch = FALSE, size = 0.4) + 
  geom_jitter(aes(scat_adj+dist_cat, mean_richness, fill = factor(numeric_group)),
              position=position_jitter(width=0,height=0),
              alpha=1,
              shape=21, size = 1.2) +
  scale_fill_manual(values=c("sandybrown","sandybrown"))+ 
  guides(fill=guide_legend(title="Condition")) +  
  theme_boxplot
p3
p4=p3+geom_line(data=dat, aes(x= numeric_group, y=mean_richness, group=subject), col = "black", alpha = 0.3); p4



####Figure 2b
library(pheatmap)
dat<- data.frame(readxl::read_xlsx("Figure 2b Source_data.xlsx",sheet = "Sheet1")) # Figure D data
rownames(dat)=dat[,1]
data<-subset(dat,select=-c(subject))
data_subset <- as.matrix(data)
pheatmap(data_subset)
my_sample_col <- data.frame(sample =c("L", "W","H", "W","H", "W","L", "W","H", "W","L"))
row.names(my_sample_col) <- colnames(data_subset)
pheatmap(data_subset,cluster_cols  = F,annotation_col = my_sample_col,display_numbers = T)


#--------####Figure 2C--------
library(microbiome)
otu_data<- data.frame(readxl::read_xlsx("Figure 2c Source_data.xlsx",sheet = "micro"))
meta_data<- data.frame(readxl::read_xlsx("Figure 2c Source_data.xlsx",sheet = "meta"))

otu_id = otu_data$`SampleID`
otu_data = data.frame(otu_data[, -1], check.names = FALSE)
rownames(otu_data) = otu_id
assay_data<-as.matrix(otu_data)
class(otu_data)

meta_data = data.frame(meta_data, row.names = colnames(assay_data)) #here,row names must use the assay_data colnames
meta_data$subject=as.factor(meta_data$subject)
meta_data$time=as.factor(meta_data$time)
meta_data$intervention=as.factor(meta_data$intervention)

col_data <-meta_data
se <- TreeSummarizedExperiment(assays = list(counts = assay_data),
                               colData = col_data,
)
se

pseq <- makePhyloseqFromTreeSummarizedExperiment(se)
pseq

Time1 <- divergence(subset_samples(pseq, time == "1"),
                    apply(abundances(subset_samples(pseq, time == "1")), 1, median))

Time2 <- divergence(subset_samples(pseq, time == "2"),
                    apply(abundances(subset_samples(pseq, time == "2")), 1, median))

Time3 <- divergence(subset_samples(pseq, time == "3"),
                    apply(abundances(subset_samples(pseq, time == "3")), 1, median))

Time4 <- divergence(subset_samples(pseq, time == "4"),
                    apply(abundances(subset_samples(pseq, time == "4")), 1, median))

Time5 <- divergence(subset_samples(pseq, time == "5"),
                    apply(abundances(subset_samples(pseq, time == "5")), 1, median))

Time6 <- divergence(subset_samples(pseq, time == "6"),
                    apply(abundances(subset_samples(pseq, time == "6")), 1, median))

Time7 <- divergence(subset_samples(pseq, time == "7"),
                    apply(abundances(subset_samples(pseq, time == "7")), 1, median))

Time8 <- divergence(subset_samples(pseq, time == "8"),
                    apply(abundances(subset_samples(pseq, time == "8")), 1, median))

Time9 <- divergence(subset_samples(pseq, time == "9"),
                    apply(abundances(subset_samples(pseq, time == "9")), 1, median))

Time10 <- divergence(subset_samples(pseq, time == "10"),
                     apply(abundances(subset_samples(pseq, time == "10")), 1, median))

Time11 <- divergence(subset_samples(pseq, time == "11"),
                     apply(abundances(subset_samples(pseq, time == "11")), 1, median))

Time12 <- divergence(subset_samples(pseq, time == "12"),
                     apply(abundances(subset_samples(pseq, time == "12")), 1, median))


l<- list(Time1, Time2,Time3, Time4,Time5, Time6,Time7, Time8,Time9, Time10,Time11,Time12)
df<- melt(l)

df$L1<- factor(df$L1, levels = c('1','2','3','4','5','6','7','8','9','10','11','12'))

p<- ggplot(df, aes(x = L1, y = value)) + geom_boxplot()+ xlab('')
plot(p)

my_comparisons <- list(  c("1", "2"), c("1", "3"), c("1", "4") , c("1", "5") , c("1", "6"), c("1", "7"), c("1", "8"), c("1","9") , c("1", "10"), c("1", "11") , c("1", "12")   )

p<-p + stat_compare_means(comparisons = my_comparisons)

ggsave(p, file='beta_inter_time1.pdf') 

###Figure 2d 
rm(list=ls())
library("RColorBrewer")
library(readr)
display.brewer.pal(n=8, name='Set3') 
set.seed(120)
colors=brewer.pal(n=8, name='Set2')[c(5,6,8)] 
library(readxl)
data <- readxl::read_xlsx("Figure 2d Source_data.xlsx",sheet = "Sheet2")
ggplot (data, aes(as.factor(repeats), Distance))+
  geom_boxplot(aes(fill = as.factor(group)), notch = FALSE, size = 0.4, outlier.colour = "white") + 
  scale_fill_brewer(palette = "Dark2") +
  guides(fill=guide_legend(title="")) +
  theme_bw() +
  stat_summary(mapping=aes(group=as.factor(group)),fun=median, geom="line", size=0.4,  colour="black", 
               position=position_dodge(0.8))

####Figure 3
########## fold change calculations
rm(list=ls())
library(readxl)
library(readr)
HC_set1A <- readxl::read_xlsx("source_data_003_for_foldchange_calculations.xlsx",sheet = "pre_HC_set1")
HC_set1B <- readxl::read_xlsx("source_data_003_for_foldchange_calculations.xlsx",sheet = "post_HC_set1")
HC_set1A <-HC_set1A[,-c(1:2)]
HC_set1B <-HC_set1B[,-c(1:2)]
HC_set1A <-as.data.frame(HC_set1A)
HC_set1B <-as.data.frame(HC_set1B)
rownames(HC_set1A) <- HC_set1A[,1] 
rownames(HC_set1B) <- HC_set1B[,1] 
HC_set1A <-HC_set1A[,-c(1)]
HC_set1B <-HC_set1B[,-c(1)]
delta1 <- HC_set1B-HC_set1A  
ratio=HC_set1B/HC_set1A
write.csv(ratio,file = "foldchanges_for_HC_set1.csv")

HC_set1A <- readxl::read_xlsx("source_data_003_for_foldchange_calculations.xlsx",sheet = "pre_HC_set2")
HC_set1B <- readxl::read_xlsx("source_data_003_for_foldchange_calculations.xlsx",sheet = "post_HC_set2")
HC_set1A <-HC_set1A[,-c(1:2)]
HC_set1B <-HC_set1B[,-c(1:2)]
HC_set1A <-as.data.frame(HC_set1A)
HC_set1B <-as.data.frame(HC_set1B)
rownames(HC_set1A) <- HC_set1A[,1] 
rownames(HC_set1B) <- HC_set1B[,1] 
HC_set1A <-HC_set1A[,-c(1)]
HC_set1B <-HC_set1B[,-c(1)]
delta1 <- HC_set1B-HC_set1A  
ratio=HC_set1B/HC_set1A
write.csv(ratio,file = "foldchanges_for_HC_set2.csv")

rm(list=ls())
HC_set1A <- readxl::read_xlsx("source_data_003_for_foldchange_calculations.xlsx",sheet = "pre_HC_set3")
HC_set1B <- readxl::read_xlsx("source_data_003_for_foldchange_calculations.xlsx",sheet = "post_HC_set3")
HC_set1A <-HC_set1A[,-c(1:2)]
HC_set1B <-HC_set1B[,-c(1:2)]
HC_set1A <-as.data.frame(HC_set1A)
HC_set1B <-as.data.frame(HC_set1B)
rownames(HC_set1A) <- HC_set1A[,1] 
rownames(HC_set1B) <- HC_set1B[,1] 
HC_set1A <-HC_set1A[,-c(1)]
HC_set1B <-HC_set1B[,-c(1)]
delta1 <- HC_set1B-HC_set1A  
ratio=HC_set1B/HC_set1A
write.csv(ratio,file = "foldchanges_for_HC_set3.csv")

##LC  set1
rm(list=ls())
HC_set1A <- readxl::read_xlsx("source_data_003_for_foldchange_calculations.xlsx",sheet = "pre_LC_set1")
HC_set1B <- readxl::read_xlsx("source_data_003_for_foldchange_calculations.xlsx",sheet = "post_LC_set1")
HC_set1A <-HC_set1A[,-c(1:2)]
HC_set1B <-HC_set1B[,-c(1:2)]
HC_set1A <-as.data.frame(HC_set1A)
HC_set1B <-as.data.frame(HC_set1B)
rownames(HC_set1A) <- HC_set1A[,1] 
rownames(HC_set1B) <- HC_set1B[,1] 
HC_set1A <-HC_set1A[,-c(1)]
HC_set1B <-HC_set1B[,-c(1)]
delta1 <- HC_set1B-HC_set1A  
ratio=HC_set1B/HC_set1A
write.csv(ratio,file = "foldchanges_for_LC_set1.csv")

#LC set2
rm(list=ls())
HC_set1A <- readxl::read_xlsx("source_data_003_for_foldchange_calculations.xlsx",sheet = "pre_LC_set2")
HC_set1B <- readxl::read_xlsx("source_data_003_for_foldchange_calculations.xlsx",sheet = "post_LC_set2")
HC_set1A <-HC_set1A[,-c(1:2)]
HC_set1B <-HC_set1B[,-c(1:2)]
HC_set1A <-as.data.frame(HC_set1A)
HC_set1B <-as.data.frame(HC_set1B)
rownames(HC_set1A) <- HC_set1A[,1] 
rownames(HC_set1B) <- HC_set1B[,1] 
HC_set1A <-HC_set1A[,-c(1)]
HC_set1B <-HC_set1B[,-c(1)]
delta1 <- HC_set1B-HC_set1A  
ratio=HC_set1B/HC_set1A
write.csv(ratio,file = "foldchanges_for_LC_set2.csv")


#LC set3
rm(list=ls())
HC_set1A <- readxl::read_xlsx("source_data_003_for_foldchange_calculations.xlsx",sheet = "pre_LC_set3")
HC_set1B <- readxl::read_xlsx("source_data_003_for_foldchange_calculations.xlsx",sheet = "post_LC_set3")
HC_set1A <-HC_set1A[,-c(1:2)]
HC_set1B <-HC_set1B[,-c(1:2)]
HC_set1A <-as.data.frame(HC_set1A)
HC_set1B <-as.data.frame(HC_set1B)
rownames(HC_set1A) <- HC_set1A[,1] 
rownames(HC_set1B) <- HC_set1B[,1] 
HC_set1A <-HC_set1A[,-c(1)]
HC_set1B <-HC_set1B[,-c(1)]
delta1 <- HC_set1B-HC_set1A  
ratio=HC_set1B/HC_set1A
write.csv(ratio,file = "foldchanges_for_LC_set3.csv")


#####visualization for the identified responsive species  
heatmap_motor <- function (matrix, border_color, cellwidth, cellheight, tree_col, 
                           tree_row, treeheight_col, treeheight_row, filename, width, 
                           height, breaks, color, legend, annotation_row, annotation_col, 
                           annotation_colors, annotation_legend, annotation_names_row, 
                           annotation_names_col, main, fontsize, fontsize_row, fontsize_col, 
                           hjust_col, vjust_col, angle_col, fmat, fontsize_number, number_color, 
                           gaps_col, gaps_row, labels_row, labels_col, ...) 
  
{
  lo = pheatmap:::lo(coln = labels_col, rown = labels_row, nrow = nrow(matrix), 
                     ncol = ncol(matrix), cellwidth = cellwidth, cellheight = cellheight, 
                     treeheight_col = treeheight_col, treeheight_row = treeheight_row, 
                     legend = legend, annotation_col = annotation_col, annotation_row = annotation_row, 
                     annotation_colors = annotation_colors, annotation_legend = annotation_legend, 
                     annotation_names_row = annotation_names_row, annotation_names_col = annotation_names_col, 
                     main = main, fontsize = fontsize, fontsize_row = fontsize_row, 
                     fontsize_col = fontsize_col, angle_col = angle_col, gaps_row = gaps_row, 
                     gaps_col = gaps_col, ...)
  res = lo$gt
  mindim = lo$mindim
  if (!is.na(filename)) {
    if (is.na(height)) {
      height = convertHeight(gtable_height(res), "inches", valueOnly = T)
    }
    if (is.na(width)) {
      width = convertWidth(gtable_width(res), "inches", valueOnly = T)
    }
    r = regexpr("\\.[a-zA-Z]*$", filename)
    if (r == -1) 
      stop("Improper filename")
    ending = substr(filename, r + 1, r + attr(r, "match.length"))
    f = switch(ending, pdf = function(x, ...) pdf(x, ...), 
               png = function(x, ...) png(x, units = "in", res = 300, 
                                          ...), jpeg = function(x, ...) jpeg(x, units = "in", 
                                                                             res = 300, ...), jpg = function(x, ...) jpeg(x, 
                                                                                                                          units = "in", res = 300, ...), tiff = function(x, 
                                                                                                                                                                         ...) tiff(x, units = "in", res = 300, compression = "lzw", 
                                                                                                                                                                                   ...), bmp = function(x, ...) bmp(x, units = "in", 
                                                                                                                                                                                                                    res = 300, ...), stop("File type should be: pdf, png, bmp, jpg, tiff"))
    f(filename, height = height, width = width)
    gt = heatmap_motor(matrix, cellwidth = cellwidth, cellheight = cellheight, 
                       border_color = border_color, tree_col = tree_col, 
                       tree_row = tree_row, treeheight_col = treeheight_col, 
                       treeheight_row = treeheight_row, breaks = breaks, 
                       color = color, legend = legend, annotation_col = annotation_col, 
                       annotation_row = annotation_row, annotation_colors = annotation_colors, 
                       annotation_legend = annotation_legend, annotation_names_row = annotation_names_row, 
                       annotation_names_col = annotation_names_col, filename = NA, 
                       main = main, fontsize = fontsize, fontsize_row = fontsize_row, 
                       fontsize_col = fontsize_col, hjust_col = hjust_col, 
                       vjust_col = vjust_col, angle_col = angle_col, fmat = fmat, 
                       fontsize_number = fontsize_number, number_color = number_color, 
                       labels_row = labels_row, labels_col = labels_col, 
                       gaps_col = gaps_col, gaps_row = gaps_row, ...)
    grid.draw(gt)
    dev.off()
    return(gt)
  }
  if (mindim < 3) 
    border_color = NA
  if (!is.na(main)) {
    elem = pheatmap:::draw_main(main, fontsize = 1.3 * fontsize, ...)
    res = gtable_add_grob(res, elem, t = 1, l = 3, name = "main", 
                          clip = "off")
  }
  if (!pheatmap:::is.na2(tree_col) & treeheight_col != 0) {
    elem = pheatmap:::draw_dendrogram(tree_col, gaps_col, horizontal = T)
    res = gtable_add_grob(res, elem, t = 2, l = 3, name = "col_tree")
  }
  if (!pheatmap:::is.na2(tree_row) & treeheight_row != 0) {
    elem = pheatmap:::draw_dendrogram(tree_row, gaps_row, horizontal = F)
    res = gtable_add_grob(res, elem, t = 4, l = 1, name = "row_tree")
  }
  elem = pheatmap:::draw_matrix(matrix, border_color, gaps_row, gaps_col, 
                                fmat, fontsize_number, number_color)
  res = gtable_add_grob(res, elem, t = 4, l = 3, clip = "off", 
                        name = "matrix")
  if (length(labels_col) != 0) {
    pars = list(labels_col, gaps = gaps_col, fontsize = fontsize_col, 
                hjust_col = hjust_col, vjust_col = vjust_col, angle_col = angle_col, 
                ...)
    elem = do.call(pheatmap:::draw_colnames, pars)
    res = gtable_add_grob(res, elem, t = 5, l = 3, clip = "off", 
                          name = "col_names")
  }
  if (length(labels_row) != 0) {
    pars = list(labels_row, gaps = gaps_row, fontsize = fontsize_row, 
                ...)
    elem = do.call(pheatmap:::draw_rownames, pars)
    res = gtable_add_grob(res, elem, t = 4, l = 3, clip = "off", 
                          name = "row_names")
  }
  if (!pheatmap:::is.na2(annotation_col)) {
    converted_annotation = convert_annotations(annotation_col, 
                                               annotation_colors)
    elem = pheatmap:::draw_annotations(converted_annotation, border_color, 
                                       gaps_col, fontsize, horizontal = T)
    res = gtable_add_grob(res, elem, t = 3, l = 3, clip = "off", 
                          name = "col_annotation")
    if (annotation_names_col) {
      elem = pheatmap:::draw_annotation_names(annotation_col, fontsize, 
                                              horizontal = T)
      res = gtable_add_grob(res, elem, t = 3, l = 4, clip = "off", 
                            name = "col_annotation_names")
    }
  }
  if (!pheatmap:::is.na2(annotation_row)) {
    converted_annotation = convert_annotations(annotation_row, 
                                               annotation_colors)
    elem = pheatmap:::draw_annotations(converted_annotation, border_color, 
                                       gaps_row, fontsize, horizontal = F)
    res = gtable_add_grob(res, elem, t = 4, l = 2, clip = "off", 
                          name = "row_annotation")
    if (annotation_names_row) {
      elem = pheatmap:::draw_annotation_names(annotation_row, fontsize, 
                                              horizontal = F, hjust_col = hjust_col, vjust_col = vjust_col, 
                                              angle_col = angle_col)
      res = gtable_add_grob(res, elem, t = 5, l = 2, clip = "off", 
                            name = "row_annotation_names")
    }
  }
  annotation = c(annotation_col[length(annotation_col):1], 
                 annotation_row[length(annotation_row):1])
  annotation = annotation[unlist(lapply(annotation, function(x) !pheatmap:::is.na2(x)))]
  if (length(annotation) > 0 & annotation_legend) {
    elem = pheatmap:::draw_annotation_legend(annotation, annotation_colors, 
                                             border_color, fontsize = fontsize, ...)
    t = ifelse(is.null(labels_row), 4, 3)
    res = gtable_add_grob(res, elem, t = t, l = 6, b = 5, 
                          clip = "off", name = "annotation_legend")
  }
  if (!pheatmap:::is.na2(legend)) {
    elem = pheatmap:::draw_legend(color, breaks, legend, fontsize = fontsize, 
                                  ...)
    t = ifelse(is.null(labels_row), 4, 3)
    res = gtable_add_grob(res, elem, t = t, l = 5, b = 5, 
                          clip = "off", name = "legend")
  }
  return(res)
}

# Modified pheatmap:::lo    
lo <- function (rown, coln, nrow, ncol, cellheight = NA, cellwidth = NA, 
                treeheight_col, treeheight_row, legend, annotation_row, annotation_col, 
                annotation_colors, annotation_legend, annotation_names_row, 
                annotation_names_col, main, fontsize, fontsize_row, fontsize_col, 
                angle_col, gaps_row, gaps_col, ...) 
{
  if (!is.null(coln[1]) | (!pheatmap:::is.na2(annotation_row) & annotation_names_row)) {
    if (!is.null(coln[1])) {
      t = coln
    }
    else {
      t = ""
    }
    tw = strwidth(t, units = "in", cex = fontsize_col/fontsize)
    if (annotation_names_row) {
      t = c(t, colnames(annotation_row))
      tw = c(tw, strwidth(colnames(annotation_row), units = "in"))
    }
    longest_coln = which.max(tw)
    gp = list(fontsize = ifelse(longest_coln <= length(coln), 
                                fontsize_col, fontsize), ...)
    coln_height = unit(1, "grobheight", textGrob(t[longest_coln], 
                                                 rot = angle_col, gp = do.call(gpar, gp))) + unit(10, 
                                                                                                  "bigpts")
  }
  else {
    coln_height = unit(5, "bigpts")
  }
  if (!is.null(rown[1])) {
    t = rown
    tw = strwidth(t, units = "in", cex = fontsize_row/fontsize)
    if (annotation_names_col) {
      t = c(t, colnames(annotation_col))
      tw = c(tw, strwidth(colnames(annotation_col), units = "in"))
    }
    longest_rown = which.max(tw)
    gp = list(fontsize = ifelse(longest_rown <= length(rown), 
                                fontsize_row, fontsize), ...)
    rown_width = unit(1, "grobwidth", textGrob(t[longest_rown], 
                                               rot = 0, gp = do.call(gpar, gp))) + unit(10, "bigpts")
  }
  else {
    rown_width = unit(5, "bigpts")
  }
  gp = list(fontsize = fontsize, ...)
  if (!pheatmap:::is.na2(legend)) {
    longest_break = which.max(nchar(names(legend)))
    longest_break = unit(1.1, "grobwidth", 
                         textGrob(as.character(names(legend))[longest_break], 
                                  gp = do.call(gpar, gp)))
    title_length = unit(1.1, "grobwidth", textGrob("Scale", 
                                                   gp = gpar(fontface = "bold", ...)))
    legend_width = unit(12, "bigpts") + longest_break * 1.2
    legend_width = max(title_length, legend_width)
  }
  else {
    legend_width = unit(0, "bigpts")
  }
  if (is.na(main)) {
    main_height = unit(0, "npc")
  }
  else {
    main_height = unit(1.5, "grobheight", textGrob(main, 
                                                   gp = gpar(fontsize = 1.3 * fontsize, ...)))
  }
  textheight = unit(fontsize, "bigpts")
  if (!pheatmap:::is.na2(annotation_col)) {
    annot_col_height = ncol(annotation_col) * (textheight + 
                                                 unit(2, "bigpts")) + unit(2, "bigpts")
    t = c(as.vector(as.matrix(annotation_col)), colnames(annotation_col))
    annot_col_legend_width = unit(1.2, "grobwidth", textGrob(t[which.max(nchar(t))], 
                                                             gp = gpar(...))) + unit(12, "bigpts")
    if (!annotation_legend) {
      annot_col_legend_width = unit(0, "npc")
    }
  }
  else {
    annot_col_height = unit(0, "bigpts")
    annot_col_legend_width = unit(0, "bigpts")
  }
  if (!pheatmap:::is.na2(annotation_row)) {
    annot_row_width = ncol(annotation_row) * (textheight + 
                                                unit(2, "bigpts")) + unit(2, "bigpts")
    t = c(as.vector(as.matrix(annotation_row)), colnames(annotation_row))
    annot_row_legend_width = unit(1.2, "grobwidth", textGrob(t[which.max(nchar(t))], 
                                                             gp = gpar(...))) + unit(12, "bigpts")
    if (!annotation_legend) {
      annot_row_legend_width = unit(0, "npc")
    }
  }
  else {
    annot_row_width = unit(0, "bigpts")
    annot_row_legend_width = unit(0, "bigpts")
  }
  annot_legend_width = max(annot_row_legend_width, annot_col_legend_width)
  treeheight_col = unit(treeheight_col, "bigpts") + unit(5, 
                                                         "bigpts")
  treeheight_row = unit(treeheight_row, "bigpts") + unit(5, 
                                                         "bigpts")
  if (is.na(cellwidth)) {
    mat_width = unit(1, "npc") - rown_width - legend_width - 
      treeheight_row - annot_row_width - annot_legend_width
  }
  else {
    mat_width = unit(cellwidth * ncol, "bigpts") + length(gaps_col) * 
      unit(4, "bigpts")
  }
  if (is.na(cellheight)) {
    mat_height = unit(1, "npc") - main_height - coln_height - 
      treeheight_col - annot_col_height
  }
  else {
    mat_height = unit(cellheight * nrow, "bigpts") + length(gaps_row) * 
      unit(4, "bigpts")
  }
  gt = gtable(widths = unit.c(treeheight_row, rown_width,  
                              mat_width, treeheight_row, legend_width, annot_legend_width), 
              heights = unit.c(main_height, treeheight_col, annot_col_height, 
                               mat_height, coln_height), vp = viewport(gp = do.call(gpar, 
                                                                                    gp)))
  cw = convertWidth(mat_width - (length(gaps_col) * unit(4, 
                                                         "bigpts")), "bigpts", valueOnly = T)/ncol
  ch = convertHeight(mat_height - (length(gaps_row) * unit(4, 
                                                           "bigpts")), "bigpts", valueOnly = T)/nrow
  mindim = min(cw, ch)
  res = list(gt = gt, mindim = mindim)
  return(res)
}

# Modified pheatmap:::draw_rownames      
draw_rownames <- function (rown, gaps, ...) 
{
  coord = pheatmap:::find_coordinates(length(rown), gaps)
  y = unit(1, "npc") - (coord$coord - 0.5 * coord$size)
  res = textGrob(rown, x = unit(-3, "bigpts"), y = y, vjust = 0.5, 
                 hjust = 1, gp = gpar(...))
  return(res)
}

assignInNamespace(x="draw_rownames", value=draw_rownames, ns="pheatmap")
assignInNamespace(x="lo", value=lo, ns="pheatmap")
assignInNamespace(x="heatmap_motor", value=heatmap_motor, ns="pheatmap") 

####visualization  heatmap
rm(list=ls())
library(RColorBrewer)
library(pheatmap)
library(readxl)
library(ComplexHeatmap)

association <- read_xlsx("Figure 3a Source_data.xlsx",sheet = "Sheet5")
#association <- read_xlsx("Figure 3b Source_data.xlsx",sheet = "Sheet5")

association <- as.data.frame(association)
rownames(association) <- association[,1]
association <- association[,-1] 
association <- as.matrix(association)

#colormap <- colorRampPalette(c("#7FB3D3", "white", "#B01E2A"))(3) ##  
colormap <- colorRampPalette(c("darkcyan", "white", "salmon1"))(3) ##  
#colormap <- colorRampPalette(c("dodgerblue1", "white", "red3"))(3) ##  
colormap <- colorRampPalette(c("deepskyblue2", "white", "hotpink"))(3)  
colormap <- colorRampPalette(c("dodgerblue4", "white", "gold1"))(3) ##  
colormap <- colorRampPalette(c("dodgerblue4", "white", "yellow1"))(3) ##  

p3 <- pheatmap(association,
               color = colormap,
               border_color = "gray", #热图边框颜色，NA即为取消边框
               cellwidth = 18, cellheight = 13,  #设置格子大小
               cluster_cols = F,cluster_row = F,  ##聚类
               treeheight_row =5,treeheight_col=0,  #进化树高度
               legend_breaks = -1:1, row_names_side = "left", # 设置图例的断点
               #number_color="black",
               fontsize_row=14,fontsize_col=14,#设置字体大小
               angle_col = "45")
p3

dev.off()
dev.new()


#******************  PCoA analysis to extract top PC 
rm(list = ls())
library(readxl)
library(FactoMineR)
library(factoextra) 
library(vegan)
library(readr)
library(haven)
data <- read_dta("data_for_PCA.dta")
data <- as.data.frame(data)
rownames(data) = data[,2]
data <- data[,-c(1:7)]
library(bioinfor)
res.pca <- PCA(data,scale.unit = TRUE,ncp=200,graph= F)
eig.val <- get_eigenvalue(res.pca)
eig.val 

fviz_eig(res.pca, addlabels = TRUE, xlim=c(1, 5), ylim = c(0, 12))
fviz_pca_var(res.pca, col.var = "black")
library("corrplot")
corrplot(res.pca$var$cos2, is.corr=FALSE)

fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

ind <- get_pca_ind(res.pca)
ind
fviz_pca_ind(res.pca, pointsize = "cos2", 
             pointshape = 21, fill = "#E7B800",
)
fviz_pca_ind(res.pca,
             geom.ind = "point",  pointsize = "cos2",
             #              # show points only (but not "text")
             col.ind =  "#E7B800", 
)

ind <- get_pca_ind(res.pca)
n <- ind$coord
write.table(n,file = "PCA_results.csv")

library("FactoMineR")
library("factoextra")
library(corrplot)
library(broom)
library(ade4)
library(vegan)
library(ggrepel)

###############################################
pca1 <- PCA(data, ncp = 5, graph = FALSE)
var <- get_pca_var(pca1)
contribution <- var$contrib  
write.table(contribution,file = "contributions_to_each_PC.csv")

get_eigenvalue(res.pca)
fviz_eig(res.pca)
fviz_pca_biplot(res.pca)

y <- data
library(FactoMineR)
library(factoextra) 
dat.pca <- PCA(data, graph = FALSE)
library(scatterplot3d)
individuals <- dat.pca$ind
PCA_values <- individuals$coord
PCA_values <- as.data.frame(PCA_values)
mycolour <- c(rep("#FC4E07", 164)) 

scatterplot3d(PCA_values$Dim.1, PCA_values$Dim.2, PCA_values$Dim.3, color=mycolour,
              pch = c(rep(15,164)), 
              cex.symbols = 1, 
              font.lab = 20,   
              font.axis = 20,
              xlab = 'PC1 (9.9%)', 
              ylab = 'PC2 (6.8%)',
              zlab = 'PC3 (5.3%)',
              main = '3D-PCA', 
              mar=c(3,2.5,3,1.5)+0.1) # 

##Figure 4
##MPG####
rm(list=ls())
library(forestplot)
tabletext <- readxl::read_xlsx("Figure 4 Source_data_MPG.xlsx",sheet = "Sheet3")
sheet2 <- readxl::read_xlsx("Figure 4 Source_data_MPG.xlsx",sheet = "Sheet4")

tabletext <- as.matrix(tabletext)
txt <- tabletext[,c(1:6)]
sheet2 <- as.matrix(sheet2)
value <- sheet2[,c(1:3)]
p1 <- forestplot(txt, value, 
                 graph.pos=3,
                 #hrzl_lines=list("3.5" = gpar(lwd=40, lineend="butt", columns=c(2:7),col="#99999943"),
                 #               "7.5" = gpar(lwd=40, lineend="butt", columns=c(2:7),col="#99999943"),
                 #              "11.5" = gpar(lwd=40, lineend="butt", columns=c(2:7),col="#99999943")),
                 ##fpColors函数设置颜色
                 col=fpColors(box='#458B00',summary="#8B008B",lines = 'black',zero = '#7AC5CD'),
                 xlab="MPG (mmol/L)",#设置x轴标签
                 is.summary = c(F,F,F,F,F,F,F,F,F),
                 txt_gp=fpTxtGp(label=gpar(cex=0.95),
                                ticks=gpar(cex=1.1),
                                xlab=gpar(cex = 1.1)
                 ),
                 boxsize = 0.4,
                 lineheight = unit(8,'mm'),#设置图形中的行距
                 colgap = unit(3.5,'mm'),#设置图形中的列间距
                 zero = 0, #设置参照值
                 lwd.zero = 2,#设置参考线的粗细
                 lwd.ci = 2,#设置区间估计线的粗细
                 lwd.xaxis=2,#设置X轴线的粗细
                 cex=0.9,
                 graphwidth = unit(60,"mm"),
                 ci.vertices=TRUE, ci.vertices.height = 0.15,
                 lty.ci = "solid",
                 xticks=c(-0.2,-0.1,0,0.1,0.2))
p1
dev.off()
dev.new()

##MAGE####
rm(list=ls())
library(forestplot)
tabletext <- readxl::read_xlsx("Figure 4 Source_data_MAGE.xlsx",sheet = "Sheet3")
sheet2 <- readxl::read_xlsx("Figure 4 Source_data_MAGE.xlsx",sheet = "Sheet4")

tabletext <- as.matrix(tabletext)
txt <- tabletext[,c(1:6)]
sheet2 <- as.matrix(sheet2)
value <- sheet2[,c(1:3)]
p2 <- forestplot(txt, value, 
                 graph.pos=3,
                 col=fpColors(box='#458B00',summary="#8B008B",lines = 'black',zero = '#7AC5CD'),
                 xlab="MAGE (mmol/L)",#设置x轴标签
                 is.summary = c(F,F,F,F,F,F,F,F,F),
                 txt_gp=fpTxtGp(label=gpar(cex=0.95),
                                ticks=gpar(cex=1.1),
                                xlab=gpar(cex = 1.1)
                 ),
                 boxsize = 0.4,
                 lineheight = unit(8,'mm'),
                 colgap = unit(3.5,'mm'),
                 zero = 0, 
                 lwd.zero = 2,
                 lwd.ci = 2,
                 lwd.xaxis=2,
                 cex=0.9,
                 graphwidth = unit(60,"mm"),
                 ci.vertices=TRUE, ci.vertices.height = 0.15,
                 lty.ci = "solid",
                 xticks=c(-0.2,-0.1,0,0.1,0.2))
p2
#dev.off()


##TAR####
rm(list=ls())
library(forestplot)
tabletext <- readxl::read_xlsx("Figure 4 Source_data_TAR.xlsx",sheet = "Sheet3")
sheet2 <- readxl::read_xlsx("Figure 4 Source_data_TAR.xlsx",sheet = "Sheet4")

tabletext <- as.matrix(tabletext)
txt <- tabletext[,c(1:6)]
sheet2 <- as.matrix(sheet2)
value <- sheet2[,c(1:3)]
#tiff('p4.tiff',height = 3000,width = 5000,res= 300)
#pdf(file="HPT_forest.pdf",onefile=F)
p3 <- forestplot(txt, value, 
                 graph.pos=3,
                 #hrzl_lines=list("3.5" = gpar(lwd=40, lineend="butt", columns=c(2:7),col="#99999943"),
                 #               "7.5" = gpar(lwd=40, lineend="butt", columns=c(2:7),col="#99999943"),
                 #              "11.5" = gpar(lwd=40, lineend="butt", columns=c(2:7),col="#99999943")),
                 ##fpColors函数设置颜色
                 col=fpColors(box='#458B00',summary="#8B008B",lines = 'black',zero = '#7AC5CD'),
                 xlab="TAR (%)",#设置x轴标签
                 is.summary = c(F,F,F,F,F,F,F,F,F),
                 txt_gp=fpTxtGp(label=gpar(cex=0.95),
                                ticks=gpar(cex=1.1),
                                xlab=gpar(cex = 1.1)
                 ),
                 boxsize = 0.4,
                 lineheight = unit(8,'mm'),#设置图形中的行距
                 colgap = unit(3.5,'mm'),#设置图形中的列间距
                 zero = 0, #设置参照值
                 lwd.zero = 2,#设置参考线的粗细
                 lwd.ci = 2,#设置区间估计线的粗细
                 lwd.xaxis=2,#设置X轴线的粗细
                 cex=0.9,
                 graphwidth = unit(60,"mm"),
                 ci.vertices=TRUE, ci.vertices.height = 0.15,
                 lty.ci = "solid",
                 xticks=c(-0.4,-0.2,0,0.2,0.4,0.6))
p3

####Figure 5b
rm(list=ls())
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(plyr)
library("RColorBrewer")
display.brewer.pal(n=6, name='Set2')  
colors=brewer.pal(n=6, name='Set3')[c(1,2,3)]  

##绘制contribution    和  abundance
data <- read_excel("Figure 5b Source_data.xlsx",sheet = "Sheet1") ##contribution
p <- data%>%
  mutate(Species=fct_reorder(taxa,contributions))%>% 
  ggplot(aes(taxa,contributions,fill=dims))+  #
  geom_bar(stat="identity",position="dodge")+
  facet_grid(.~ dims)+  
  coord_flip()+
  ggtitle("")+ 
  xlab("")+
  ylab("Contributions")+
  scale_fill_manual(values=c("lightblue","lightblue"))+ 
  theme(
    axis.title.x =element_text(),
    legend.position="bottom",
    legend.title=element_blank(),
    axis.text.y=element_text(size=10),
    axis.text.x=element_text(size=10),
    axis.ticks.x=element_blank(),
    axis.ticks.y=element_line(),
    axis.ticks.length=unit(0.05,'cm'),
    axis.line.y=element_line(),
    axis.line.x=element_blank(),
    panel.grid.major.x=element_line(linetype="dashed",colour="white",size=0.1),
    panel.grid.major.y=element_blank(),
    strip.text.x = element_text(size = 26, colour = "Black")
  )

p <- p + theme(axis.text = element_text(size = 18,colour = "black"), 
               axis.title = element_text(size = 18, face = "bold"),
               axis.text.x = element_text(size = 25,  colour = "black"), 
               axis.text.y = element_text(size = 15,face = "italic",
                                          colour = "black"), 
               legend.text = element_text(size = 14), 
               plot.title = element_text(size = 14, face = "bold"))+
  labs(x = NULL)+labs(x = NULL, fill = NULL)+
  theme( panel.grid = element_line(color = 'gray', linetype = 2, size = 1),panel.background = element_rect(color = 'black', fill = 'transparent'), legend.key = element_rect(fill = 'transparent'))  
p
#

data <- read_excel("Figure 5b Source_data.xlsx",sheet = "Sheet2") ##prevalence
p <- data%>%
  mutate(Species=fct_reorder(taxa,abundance))%>%  
  ggplot(aes(taxa,prevalence,fill=dimss))+  
  geom_bar(stat="identity",position="dodge")+
  facet_grid(.~ dimss)+  
  coord_flip()+
  ggtitle("")+ 
  xlab("")+
  ylab("Prevalence (%)")+
  scale_fill_manual(values=c("lightblue","lightblue"))+ 
  theme(
    axis.title.x =element_text(),
    legend.position="bottom",
    legend.title=element_blank(),
    axis.text.y=element_text(size=10),
    axis.text.x=element_text(size=10),
    axis.ticks.x=element_blank(),
    axis.ticks.y=element_line(),
    axis.ticks.length=unit(0.05,'cm'),
    axis.line.y=element_line(),
    axis.line.x=element_blank(),
    panel.grid.major.x=element_line(linetype="dashed",colour="white",size=0.1),
    panel.grid.major.y=element_blank(),
    strip.text.x = element_text(size = 26, colour = "Black")
  )

p <- p + theme(axis.text = element_text(size = 18,colour = "black"), 
               axis.title = element_text(size = 18, face = "bold"),
               axis.text.x = element_text(size = 25,  colour = "black"), 
               axis.text.y = element_text(size = 15,face = "italic",
                                          colour = "black"), 
               legend.text = element_text(size = 14), 
               plot.title = element_text(size = 14, face = "bold"))+
  labs(x = NULL)+labs(x = NULL, fill = NULL)+
  theme( panel.grid = element_line(color = 'gray', linetype = 2, size = 1),panel.background = element_rect(color = 'black', fill = 'transparent'), legend.key = element_rect(fill = 'transparent'))  
p

####Figure 5c
#———————————————————————PC3---------------------------
rm(list=ls())
library(haven)
library(vegan)
#过滤后
data <- read_dta("Figure 5c Source_data.dta")
row.names(data) <- data$SampleID
t<-names(data)
start<-grep("^s9$",t)
end<-grep("^s642$",t)
distance<-as.matrix(vegdist(data[,start:end], method = 'bray'))
############# distance ##########
distance1 <- as.data.frame(distance)
set.seed(123)
a1<-adonis(distance~data$score2_median,permutations = 999, method = "bray"); a1
y <- data
group <- y[,3] 
rownames(group) <- y$SampleID 
library(permute)
library(lattice)
library(vegan)
library(ggplot2)
library(plyr)
distance <- as.matrix(distance)
pcoa <- cmdscale(as.dist(distance), k = 10, eig = T)  

ordiplot(scores(pcoa)[ ,c(1, 2)], type = 't')  
summary(pcoa)
pcoa$eig  
point <- data.frame(pcoa$point)  
pcoa_eig <- (pcoa$eig)[1:2] / sum(pcoa$eig)  
sample_site <- data.frame({pcoa$point})[,1:2]  
sample_site$names <- rownames(y) 
group$names <- rownames(y)
names(sample_site)[1:2] <- c('PCoA1', 'PCoA2') 
sample_site <- merge(sample_site, group, by = "names", all.x = TRUE) 
sample_site$grouping <- factor(sample_site$score1_median, labels = c("Bottom quartile","Top quartile")) 
group_border <- ddply(sample_site, 'grouping', function(df) df[chull(df[[2]], df[[3]]), ]) 
###PCoA
library(RColorBrewer)
library(ggplot2)
library(grid)
library(lattice)
set.seed(120) 
colors=brewer.pal(n=8, name='Set1')[c(2,5)]
pcoa_plot <- ggplot(sample_site, aes(PCoA1, PCoA2, group = grouping)) + xlim(-0.4,0.65)+ylim(-0.6, 0.6)+
  stat_ellipse(level = 0.95, aes(color = grouping), show.legend = F,
               alpha = 0.8,size = 0.8)+
  geom_point(aes(color = grouping), size = 3, alpha = 0.8) + 
  scale_color_manual(values = colors) +
  guides(fill = guide_legend(order = 2), 
         shape = guide_legend(order = 2), 
         color = guide_legend(order = 2),
         family="serif") + 
  annotate("text",x=-0.2,y=0.4,parse=TRUE,size=0.45,label="",family="serif",
           fontface="italic",colour="#005959")+ 
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=25),
        legend.position = 'top',
        axis.line = element_line(size = 1.0),
        axis.text = element_text(size=25),
        axis.title = element_text(size=25),
        aspect.ratio = 1)+

  labs(x = paste('PCoA axis1: ', round(100 * pcoa_eig[1], 2), '%'), 
       y = paste('PCoA axis2: ', round(100 * pcoa_eig[2], 2), '%'),
       title = "")
pcoa_plot

####Figure 6a ####
#####KO composition
rm(list=ls())
library(haven)
library(vegan)
data <- read_dta("Figure 6a Source_data.dta")
row.names(data) <- data$SampleID
t<-names(data)
start<-grep("^k00001$",t)
end<-grep("^k19611$",t)
distance<-as.matrix(vegdist(data[,start:end], method = 'bray'))
############# distance ##########
distance1 <- as.data.frame(distance)
set.seed(123)
a1<-adonis(distance~data$score1_median,permutations = 999, method = "bray"); a1
y <- data
group <- y[,3] 
rownames(group) <- y$SampleID  
library(permute)
library(lattice)
library(vegan)
library(ggplot2)
library(plyr)
distance <- as.matrix(distance)
pcoa <- cmdscale(as.dist(distance), k = 10, eig = T)   

ordiplot(scores(pcoa)[ ,c(1, 2)], type = 't')  
summary(pcoa)
pcoa$eig   
point <- data.frame(pcoa$point)   
pcoa_eig <- (pcoa$eig)[1:2] / sum(pcoa$eig)   
sample_site <- data.frame({pcoa$point})[,1:2]   
sample_site$names <- rownames(y)  
group$names <- rownames(y) 
names(sample_site)[1:2] <- c('PCoA1', 'PCoA2')  
sample_site <- merge(sample_site, group, by = "names", all.x = TRUE)  
sample_site$grouping <- factor(sample_site$score1_median, labels = c("Bottom quartile","Top quartile")) 
group_border <- ddply(sample_site, 'grouping', function(df) df[chull(df[[2]], df[[3]]), ])  
###PCoA画图
library(RColorBrewer)
library(ggplot2)
library(grid)
library(lattice)
set.seed(120)  
 
colors=c("cornflowerblue", "tomato2")
pcoa_plot <- ggplot(sample_site, aes(PCoA1, PCoA2, group = grouping)) + xlim(-0.5,0.6)+ylim(-0.4, 0.4)+
  stat_ellipse(level = 0.95, aes(color = grouping), show.legend = F,
               alpha = 0.8,size = 0.8)+ 
  geom_point(aes(color = grouping), size = 3, alpha = 0.8) +  
  scale_color_manual(values = colors) +
  guides(fill = guide_legend(order = 2), 
         shape = guide_legend(order = 2), 
         color = guide_legend(order = 2),
         family="serif") +  
  annotate("text",x=-0.2,y=0.4,parse=TRUE,size=0.45,label="",family="serif",
           fontface="italic",colour="#005959")+ 
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=25),
        legend.position = 'top',
        axis.line = element_line(size = 1.0),
        axis.text = element_text(size=25),
        axis.title = element_text(size=25),
        aspect.ratio = 1)+
 
  labs(x = paste('PCoA axis1: ', round(100 * pcoa_eig[1], 2), '%'), 
       y = paste('PCoA axis2: ', round(100 * pcoa_eig[2], 2), '%'),
       title = "")
pcoa_plot

####Figure 6e
rm(list=ls())
library(RColorBrewer)
library(pheatmap)
library(readxl)
library(ggplot2)
input_data <- read_excel("Figure 6e Source_data1.xlsx",sheet = "value")
input_data <- as.data.frame(input_data)
rownames(input_data) <- input_data[,1]
data <- as.matrix(input_data[,-1])

input_p <- read_excel("Figure 6e Source_data1.xlsx",sheet = "fdr")
input_p <- as.data.frame(input_p)
rownames(input_p) <-input_p[,1]
data2<-as.matrix(input_p[,-1])
data3<-data2
data3[]<-""
for (i in 1:nrow(data2)) {
  for (j in 1:ncol(data2)) {
    if(data2[i,j]<0.001 )
      data3[i,j]<-"***"
    
    if(data2[i,j]>=0.001&data2[i,j]<0.05)
      data3[i,j]<-"**"
    
    if(data2[i,j]>=0.05&data2[i,j]<0.1)
      data3[i,j]<-"*"
  }    
}
colormap <- colorRampPalette(c("dodgerblue4","white","sandybrown"))(100)
rowgroup=read.table("Figure 6e Source_data2.txt",sep="\t",header=T,check.names=F,row.names=1,quote="")
ann_colors = list( 
  Class= c("Amino acids" = "bisque1", 
           "Benzenoids" = "#FFFF66",
           "Bile acids" = "paleturquoise2",
           "Indoles" = "#B96600", 
           "Carbohydrates" = "#484480",
           "Phenylpropanoic acids" = "blue", 
           "Organic acids"="pink2")
) 



a <- pheatmap(data,color = colormap, 
              annotation_row =rowgroup,
              annotation_colors = ann_colors,
              border_color="snow", #热图边框颜色，NA即为取消边框
              show_rownames = T,
              treeheight_row =0,treeheight_col=0,#进化树高度
              cluster_cols = F,cluster_row = T,##聚类
              cellwidth = 22,cellheight = 33,#设置格子大小
              fontsize = 15,#设置legend字体大小
              fontsize_row=13,fontsize_col=14,
              display_numbers = data3,##设置p值*
              number_color="black",angle_col = "45")
a


###mediation analysis among participants with a HC dietary habit (focus on 14 score-related CAZYmes which were significantly associated with HbA1c)
rm(list = ls())
data <- read_dta("Mediation_analysis_Source_data.dta")
library(mediation)
med<- lm(gt19_z~age_x+as.factor(sex)+BMI_x+LF_score3_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(med) 
outcome<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+LF_score3_z+gt19_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(outcome) 
set.seed(123)
CM<- mediate(med, outcome, sims = 5000, treat = "LF_score3_z", mediator = "gt19_z")
summary(CM)  
outcome_unadjusted<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+LF_score3_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(outcome_unadjusted) 
b<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+gt19_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(b) 

####ce4
med<- lm(ce4_z~age_x+as.factor(sex)+BMI_x+LF_score3_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(med) 
outcome<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+LF_score3_z+ce4_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(outcome) 
set.seed(123)
CM<- mediate(med, outcome, sims = 5000, treat = "LF_score3_z", mediator = "ce4_z")
summary(CM)  
outcome_unadjusted<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+LF_score3_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(outcome_unadjusted) 
b<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+ce4_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(b) 


####GH4
med<- lm(gh4_z~age_x+as.factor(sex)+BMI_x+LF_score3_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(med) 
outcome<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+LF_score3_z+gh4_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(outcome) 
set.seed(1234)
CM<- mediate(med, outcome, sims = 5000, treat = "LF_score3_z", mediator = "gh4_z")
summary(CM)  
outcome_unadjusted<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+LF_score3_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(outcome_unadjusted) 
b<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+gh4_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(b) 


####GT1
med<- lm(gt1_z~age_x+as.factor(sex)+BMI_x+LF_score3_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(med) 
outcome<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+LF_score3_z+gt1_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(outcome) 
set.seed(1234)
CM<- mediate(med, outcome, sims = 5000, treat = "LF_score3_z", mediator = "gt1_z")
summary(CM)  
outcome_unadjusted<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+LF_score3_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(outcome_unadjusted) 
b<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+gt1_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(b) 

####log_pl4
med<- lm(log_pl4_z~age_x+as.factor(sex)+BMI_x+LF_score3_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(med) 
outcome<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+LF_score3_z+log_pl4_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(outcome) 
set.seed(1234)
CM<- mediate(med, outcome, sims = 5000, treat = "LF_score3_z", mediator = "log_pl4_z")
summary(CM)  
outcome_unadjusted<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+LF_score3_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(outcome_unadjusted) 
b<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+log_pl4_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(b) 

####log_gt55
med<- lm(log_gt55_z~age_x+as.factor(sex)+BMI_x+LF_score3_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(med) 
outcome<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+LF_score3_z+log_gt55_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(outcome) 
set.seed(1234)
CM<- mediate(med, outcome, sims = 5000, treat = "LF_score3_z", mediator = "log_gt55_z")
summary(CM)  
outcome_unadjusted<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+LF_score3_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(outcome_unadjusted) 
b<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+gt1_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(b) 


####log_gh8
med<- lm(log_gh8_z~age_x+as.factor(sex)+BMI_x+LF_score3_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(med) 
outcome<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+LF_score3_z+log_gh8_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(outcome) 
set.seed(1234)
CM<- mediate(med, outcome, sims = 5000, treat = "LF_score3_z", mediator = "log_gh8_z")
summary(CM)  
outcome_unadjusted<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+LF_score3_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(outcome_unadjusted) 
b<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+gt1_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(b) 



####gh5
med<- lm(gh5_z~age_x+as.factor(sex)+BMI_x+LF_score3_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(med) 
outcome<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+LF_score3_z+gh5_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(outcome) 
set.seed(1234)
CM<- mediate(med, outcome, sims = 5000, treat = "LF_score3_z", mediator = "gh5_z")
summary(CM)  
outcome_unadjusted<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+LF_score3_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(outcome_unadjusted) 
b<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+gh5_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(b) 



####gt56
med<- lm(log_gt56_z~age_x+as.factor(sex)+BMI_x+LF_score3_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(med) 
outcome<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+LF_score3_z+log_gt56_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(outcome) 
set.seed(1234)
CM<- mediate(med, outcome, sims = 5000, treat = "LF_score3_z", mediator = "log_gt56_z")
summary(CM)  
outcome_unadjusted<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+LF_score3_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(outcome_unadjusted) 
b<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+gt1_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(b) 


####gt25
med<- lm(log_gt25_z~age_x+as.factor(sex)+BMI_x+LF_score3_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(med) 
outcome<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+LF_score3_z+log_gt25_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(outcome) 
set.seed(1234)
CM<- mediate(med, outcome, sims = 5000, treat = "LF_score3_z", mediator = "log_gt25_z")
summary(CM)  
outcome_unadjusted<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+LF_score3_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(outcome_unadjusted) 
b<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+gt1_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(b) 


####gt18
med<- lm(log_gt18_z~age_x+as.factor(sex)+BMI_x+LF_score3_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(med) 
outcome<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+LF_score3_z+log_gt18_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(outcome) 
set.seed(1234)
CM<- mediate(med, outcome, sims = 5000, treat = "LF_score3_z", mediator = "log_gt18_z")
summary(CM)  
outcome_unadjusted<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+LF_score3_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(outcome_unadjusted) 
b<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+gt1_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(b) 

####gh93
med<- lm(log_gh93_z~age_x+as.factor(sex)+BMI_x+LF_score3_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(med) 
outcome<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+LF_score3_z+log_gh93_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(outcome) 
set.seed(1234)
CM<- mediate(med, outcome, sims = 5000, treat = "LF_score3_z", mediator = "log_gh93_z")
summary(CM)  
outcome_unadjusted<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+LF_score3_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(outcome_unadjusted) 
b<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+gt1_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(b) 


####cbm69
med<- lm(log_cbm69_z~age_x+as.factor(sex)+BMI_x+LF_score3_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(med) 
outcome<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+LF_score3_z+log_cbm69_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(outcome) 
set.seed(1234)
CM<- mediate(med, outcome, sims = 5000, treat = "LF_score3_z", mediator = "log_cbm69_z")
summary(CM)  
outcome_unadjusted<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+LF_score3_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(outcome_unadjusted) 
b<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+gt1_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(b) 

####gt107
med<- lm(log_gt107_z~age_x+as.factor(sex)+BMI_x+LF_score3_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(med) 
outcome<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+LF_score3_z+log_gt107_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(outcome) 
set.seed(1234)
CM<- mediate(med, outcome, sims = 5000, treat = "LF_score3_z", mediator = "log_gt107_z")
summary(CM)  
outcome_unadjusted<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+LF_score3_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(outcome_unadjusted) 
b<- lm(hbalc_x~age_x+as.factor(sex)+BMI_x+gt1_z+EA_protein_f_P+as.factor(smoke_x)+as.factor(alc_x)+MET_x, data=data)
summary(b) 
















