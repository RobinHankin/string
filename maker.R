pdf(file="closest_approach_nonselfintersecting.pdf")
source("closest_approach.R")
dev.off()

pdf(file="closest_approach_selfintersecting.pdf")
source("closest_approach2.R")
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
flamm(string=TRUE,addlegend=FALSE,phi=50,theta=50)
dev.off()

pdf(file="light_start_at_r_equals_1.pdf")
source("light_start_at_r_equals_1.R")
dev.off()

pdf(file="light_start_at_r_equals_2.pdf")
source("light_start_at_r_equals_2.R")
dev.off()

pdf(file="light_closest_approach.pdf")
source("light_closest_approach.R")
dev.off()


pdf(file="illustrative_relativistic_trajectory.pdf")
source("illustrative_relativistic_trajectory.R")
dev.off()




