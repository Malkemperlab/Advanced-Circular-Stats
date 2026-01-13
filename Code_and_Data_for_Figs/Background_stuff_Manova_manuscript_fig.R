#background stuff for MANOVA paper 

#all the test names 
name_vector<- c("Rayleigh","HR","MANOVA_intercept","MANOVA_random_intercept",
                "MANOVA_linear_interceptP","MANOVA_random_linear_interceptP",
                "MANOVA_linear_group_interceptP","MANOVA_random_linear_group_interceptP",
                "MANOVA_linear_covariateP","MANOVA_random_linear_covariateP",
                "MANOVA_linear_group_covariateP", "MANOVA_random_linear_group_covariateP",
                "MANOVA_linear_group_groupP","MANOVA_random_linear_group_groupP",
                "LM_P_intercept","LM_lin_P_intercept","LM_lin_P_lin",	"LM_lin_group_P_intercept",
                "LM_lin_group_P_lin",	"LM_lin_group_P_group","Model_comp")

#define colors 
Col <- rainbow(length(name_vector))
Col[c(3,4)] <-c("black","#800000")
Col[20] <-"purple"
Col[21] <-"bisque"

#adding a "col" 
name_vector_col <- paste(name_vector, "col", sep="_")

#the name plus col is now associated with a color
colors_plot<-mapply(function(x,y) {assign(x, y)},name_vector_col,Col)
