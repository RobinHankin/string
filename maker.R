pdf(file="f1.pdf")
source("string1.R")
dev.off()

pdf(file="f2.pdf")
source("string2.R")
dev.off()

pdf(file="f3.pdf")
source("string3.R")
dev.off()

pdf(file="f4.pdf")
source("string4.R")
dev.off()


source("flamm.R")
pdf(file="flamm_nostring.pdf")
flamm(string=FALSE,phi=50,theta=40)
dev.off()

pdf(file="flamm_string.pdf")
flamm(string=TRUE,phi=50,theta=50)
dev.off()


