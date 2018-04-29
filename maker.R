pdf(file="closest_approach.pdf")
source("closest_approach.R")
dev.off()

pdf(file="angle_at_r_equals_2.pdf")
source("angle_at_r_equals_2.R")
dev.off()

pdf(file="one_free_end_r_equals_2.pdf")
source("one_free_end_r_equals_2.R")
dev.off()

pdf(file="one_free_end_fixed_EH_intersection.pdf")
source("one_free_end_fixed_EH_intersection.R")
dev.off()


source("flamm.R")
pdf(file="flamm_nostring.pdf")
flamm(string=FALSE,phi=50,theta=40)
dev.off()

pdf(file="flamm_string.pdf")
flamm(string=TRUE,phi=50,theta=50)
dev.off()


