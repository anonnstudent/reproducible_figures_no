## ---------------------------
##
## Script name: plotting.R
##
## Purpose of script: 
##      Plotting graphs used within analysis of palmer penguin data set.
##
## Date Created: 2023-12-05
##
##
## ---------------------------


# Exploratory graph looking at distribution of body mass by penguin species and sex 

plot_body_mass <- function(body_mass_data){
  
  body_mass_data %>% 
    ggplot(aes(species, body_mass_g)) + 
    geom_violin(scale = "count", width = 0.8, trim = FALSE) +  #creates violin plot
    geom_jitter(shape = 16, aes(colour = sex), alpha = 0.5, width = 0.3, size = 2) +  # adds individual points to plot
    labs(x = "Species", y = "Body Mass (g)", colour = "Sex", title = "Body Mass Distribution by Species and Sex") +
    scale_colour_manual(labels = c("Female", "Male"), values = c("hotpink", "deepskyblue")) +  # sets legend names and colours
    theme_bw()
}


# Function to produce a graph showing the body mass means of different sexes and penguin species

plot_mean_body_mass <- function(means_data){
  
  means_data %>% 
    ggplot(aes(x = factor(species_sex, levels = c("Adelie", "Chinstrap", "Gentoo", "MALE", "FEMALE")), # specifies order of characters on xaxis
               y = mean_body_mass, fill = species_sex)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7, alpha = 0.8, show.legend = FALSE) + # adds bars to figure
    geom_errorbar(aes(ymin = mean_body_mass - se, ymax = mean_body_mass + se),
                  position = position_dodge(width = 0.7), width = 0.25, colour = "grey20") + # adds error bar to figure 
    labs(title = "Mean Body Mass by Species and Sex",
         x = "Species", y = "Mean Body Mass (g)") +
    theme_bw()+ 
    scale_x_discrete(labels = c(MALE = "Male", FEMALE = "Female")) + # Changes labeling of xaxis characters
    scale_fill_manual(values = c("tomato", "goldenrod1", "hotpink", "seagreen3", "deepskyblue")) +  # chooses colours for graph
    annotate("text", x = "Adelie", y = 5700, label = "***", hjust = 0, size = 4) +  # adds significance
    annotate("segment", x = "Adelie", xend = "Gentoo", y = 5650, yend = 5650, size = 0.3) +  # adds significance
    annotate("text", x = "MALE", y = 5100, label = "***", hjust = 0, size = 4) +  # adds significance
    annotate("segment", x = "MALE", xend = "FEMALE", y = 5050, yend = 5050, size = 0.3) +  # adds significance
    scale_y_continuous(breaks = seq(0, 6000, by = 1000), labels = seq(0, 6000, by = 1000))  # determines yaxis 
}



# Function that saves figures to a .png file 

save_figure_png <- function(figure, filename, width, height, res, scaling){
  agg_png(filename, 
          width  =  width, 
          height  =  height, 
          units   =  "cm", 
          res     =  res, 
          scaling =  scaling)
  print(figure)
  dev.off()
}




# function to calculate mean and se of body mass of categories within a column (to use in figure)

find_means <- function(body_mass_data, column) {
  body_mass_data %>%
    group_by_at(vars({{ column }})) %>%
    summarize(mean_body_mass = mean(body_mass_g), se = std.error(body_mass_g))
}




# function to combine two sets of mean data calculated (to be used in plot)

combine_data <- function(species_means, sex_means) {
  bind_rows(
    species_means %>% mutate(species_sex = paste(species, sep = "_")),
    sex_means %>% mutate(species_sex = paste(sex, sep = "_"))
  ) %>% 
    select(-species, -sex)
}



