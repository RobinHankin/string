all: string.tex closest_approach.R closest_approach2.R angle_at_r_equals_2.R one_free_end_r_equals_2.R one_free_end_fixed_EH_intersection.R radius_of_curvature_switch.R flamm.R 
	R CMD BATCH maker.R
	pdflatex string_microarticle

R: closest_approach.R closest_approach2.R angle_at_r_equals_2.R one_free_end_r_equals_2.R one_free_end_fixed_EH_intersection.R radius_of_curvature_switch.R flamm.R flamm_string.R
	R CMD BATCH maker.R

pdf: string_ajp.tex
	pdflatex string_ajp


clean:
	rm *.pdf
