# Summarize and Plot

This repository contains a function `summarize_and_plot_all` that summarizes numeric data and generates various plots for visualization. The function allows users to specify which variables to analyze, the types of plots to create, and how to group the data. Its a quick way to gather results for further exploration of data. 

Key features:
- Summarizes data with mean, median, standard deviation, max, and min.
- Generates box plots, scatter plots, density plots, histograms, and bar charts.
- Supports custom colors for the plots.
- Can create combined plots for multiple variables by using `y_var`.

## Requirements
- R (version 4.0 or higher)
- Required packages: `dplyr`, `ggplot2`, `gridExtra`
  
# Function Detailed Description 

The `summarize_and_plot` function generates summary statistics (mean, median, standard deviation, max, min) of specified numeric columns grouped by one or more variables, and creates plots (e.g., box, scatter, density, histogram, or bar plots) for each column or as combined plots, depending on the specified combined_plot argument. I designed the `summarize_and_plot`function to be a quick and efficient data exploration tool, that allows for an overview of key trends before a more in-depth analysis. Additionally, the reasoning behind the name is that it combines summary statistics and customization of basics plots rapidly asses relationships.


```{r, message=FALSE, warning=FALSE}
library(dplyr)
library(ggplot2)
library(gridExtra)

#' Summarize and plot Function for multiple variables.
#'
#' This function summarizes specified numeric variables in a dataset and 
#' creates various types of plots (box, scatter, density, histogram, bar).
#'
#' @param data A data frame containing the variables to analyze.
#' @param x_var A string specifying the independent variable (categorical).
#' @param y_vars A character vector of dependent variables (numeric).
#' @param plot_types A character vector of plot types to create.
#' @param y_var_colors An optional named vector of colors for each y_var.
#' @param group_by An optional variable for grouping the summary.
#' @param combined_plot A boolean indicating if combined plots should be created.
#' @return A list containing the summary table and generated plots.
#' @examples
#' x_var <- "cyl"
#' y_vars <- c("mpg", "hp", "wt")
#' plot_types <- c("box", "scatter", "density", "histogram", "bar")
#' custom_colors <- c(mpg = "purple", hp = "orange", wt = "green")
#' result <- summarize_and_plot(mtcars, x_var, y_vars, plot_types, 
#'                                   y_var_colors = custom_colors, combined_plot = TRUE)
summarize_and_plot <- function(data, x_var, y_vars, plot_types, 
                                   y_var_colors = NULL, group_by = NULL, combined_plot = FALSE) {
  
  # Ensure columns are numeric for summaries
  data_numeric <- data %>% 
    mutate(across(all_of(y_vars), as.numeric))
  
  # group by if group_by is specified
  summary_table <- data_numeric %>%
    group_by(across(all_of(c(x_var, group_by)))) %>%
    summarize(across(all_of(y_vars), list(mean = ~mean(.x, na.rm = TRUE), 
                                          median = ~median(.x, na.rm = TRUE), 
                                          sd = ~sd(.x, na.rm = TRUE), 
                                          max = ~max(.x, na.rm = TRUE), 
                                          min = ~min(.x, na.rm = TRUE))),
              .groups = 'drop')
  
  # Use default colors if y_var_colors not provided
  if (is.null(y_var_colors)) {
    y_var_colors <- setNames(rainbow(length(y_vars)), y_vars)
  }
  
  # Store plots in a named list
  plots <- list()
  
  # create individual plots
  create_plot <- function(y_var, plot_type) {
    p <- switch(plot_type,
                "box" = ggplot(data, aes_string(x = x_var, 
                                                y = y_var,
                                                color = shQuote(y_var))) + 
                  geom_boxplot() + 
                  scale_color_manual(values = y_var_colors) + 
                  labs(title = paste("Boxplot of",
                                     y_var, "by", x_var)),
                
                "scatter" = ggplot(data, aes_string(x = x_var,
                                                    y = y_var, 
                                                    color = shQuote(y_var))) + 
                  geom_point() + 
                  scale_color_manual(values = y_var_colors) + 
                  labs(title = paste("Scatter plot of", y_var, "by", x_var)),
                
                "density" = ggplot(data, aes_string(x = y_var, 
                                                    color = shQuote(y_var))) + 
                  geom_density() + 
                  scale_color_manual(values = y_var_colors) + 
                  labs(title = paste("Density plot of", y_var)),
                
                "histogram" = ggplot(data, aes_string(x = y_var,
                                                      fill = shQuote(y_var))) + 
                  geom_histogram(bins = 30, color = "black") + 
                  scale_fill_manual(values = y_var_colors) + 
                  labs(title = paste("Histogram of", y_var)),
                
                "bar" = ggplot(data, aes_string(x = x_var,
                                                y = y_var, 
                                                fill = shQuote(y_var))) + 
                  geom_bar(stat = "identity", 
                           color = "black") + 
                  scale_fill_manual(values = y_var_colors) + 
                  labs(title = paste("Bar chart of", y_var, "by", x_var))
    )
    return(p)
  }
  
  # individual or combined plots based on argument
  for (plot_type in plot_types) {
    if (combined_plot) {
      # combined plot with all y_vars in a single plot
      p <- ggplot(data, aes_string(x = x_var)) + 
        labs(title = paste("Combined",
                           plot_type, 
                           "of", 
                           paste(y_vars, collapse = ", "), "by", x_var))
      
      for (y_var in y_vars) {
        p <- switch(plot_type,
                    "box" = p + geom_boxplot(aes_string(y = y_var, 
                                                        fill = shQuote(y_var)),
                                             color = y_var_colors[y_var],
                                             alpha = 0.5),
                    
                    "scatter" = p + geom_point(aes_string(y = y_var,
                                                          color = shQuote(y_var))) +
                      scale_color_manual(values = y_var_colors),
                    
                    "density" = p + geom_density(aes_string(x = y_var, 
                                                            color = shQuote(y_var)),
                                                 alpha = 0.5) +
                      scale_color_manual(values = y_var_colors),
                    
                    "histogram" = p + geom_histogram(aes_string(x = y_var, 
                                                                fill = shQuote(y_var)),
                                                     bins = 30,
                                                     color = "black",
                                                     alpha = 0.5, 
                                                     position = "identity") +
                      scale_fill_manual(values = y_var_colors),
                    
                    "bar" = p + geom_bar(aes_string(y = y_var,
                                                    fill = shQuote(y_var)),
                                         stat = "identity", 
                                         position = "dodge",
                                         color = "black") +
                      scale_fill_manual(values = y_var_colors)
        )
      }
      
      plots[[paste("Combined", plot_type, sep = "_")]] <- p
    } else {
      #  individual plots for each y_var
      for (y_var in y_vars) {
        plot_name <- paste(plot_type, y_var, sep = "_")
        p <- create_plot(y_var, plot_type)
        plots[[plot_name]] <- p
      }
    }
  }
  
  # Display all plots (not needed but useful)
  grid.arrange(grobs = plots, 
               ncol = 2)  # Adjust as needed
  
  # Return summary table and named list of plots
  return(list(summary_table = summary_table, 
              plots = plots))
}
```


# Examples for usage 

Dataset *vancouver_trees* from library(`datateachr`)

```{r}

library(datateachr)


# Load vancouver_trees dataset
data("vancouver_trees")

# Define parameters for function
x_var <- "neighbourhood_name"
y_vars <- c("diameter", "height_range_id")
plot_types <- c("box", "histogram", "Combined_density")

# Customize colors for each y_var
y_var_colors <- c(diameter = "darkgreen",
                  height_range_id = "blue")

# Run the function 
result_vancouver <- summarize_and_plot(
  data = vancouver_trees, 
  x_var = x_var, 
  y_vars = y_vars, 
  plot_types = plot_types, 
  y_var_colors = y_var_colors, 
  combined_plot = FALSE ## true or false: will make plot with multiple y_var
)

print(result_vancouver$summary_table)

# Assuming the result from summarize_and_plot is stored in result_vancouver
summary_table <- result_vancouver$summary_table

result_vancouver$plots$box_diameter

```
Results for all plots:

![1](https://github.com/user-attachments/assets/61c208e3-c338-41ef-ac87-7a0ef7cd5dd7)

Summary of Variables:

![table1](https://github.com/user-attachments/assets/628cb268-9cdd-4e60-abbf-4b5624eeaaf8)

Individual plot:

![2](https://github.com/user-attachments/assets/60cd3990-7563-4713-9c09-8dc36fb7baa3)

There is too much going on so we can also use `filter()` and use the function `summarize_and_plot()` as follows: 

```{r}

## PLot

# Filter for selected neighborhoods
filtered_data <- vancouver_trees %>%
  filter(neighbourhood_name %in% c("DOWNTOWN", "KITSILANO"))

# Define parameters from data
x_var <- "neighbourhood_name"
y_vars <- c("diameter")
plot_types <- c("box")

# Run function on the filtered data
result_filtered <- summarize_and_plot(
  data = filtered_data,
  x_var = x_var,
  y_vars = y_vars,
  plot_types = plot_types,
  combined_plot = FALSE 
)


print(result_filtered$summary_table)

```

![3](https://github.com/user-attachments/assets/a512b195-f4f5-4d84-895c-e3888eac7e45)

![image](https://github.com/user-attachments/assets/badf4e9c-5e4d-4707-bb8c-0ae0395c04b8)

# Example using *iris* dataset:

```{r}
# iris dataset prameters
x_var <- "Species"
y_vars <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
plot_types <- c("box", "density")

result_iris <- summarize_and_plot(
  data = iris, 
  x_var = x_var, 
  y_vars = y_vars, 
  plot_types = plot_types, 
  y_var_colors = c(Sepal.Length = "darkblue", ## another way to set colors
                   Sepal.Width = "darkgreen",
                   Petal.Length = "darkred", 
                   Petal.Width = "purple"), 
  group_by = "Species", 
  combined_plot = FALSE
)

#  summary table
print(result_iris$summary_table)

# individual plot
result_iris$plots[["box_Sepal.Length"]]


```

![4](https://github.com/user-attachments/assets/82f4a8fe-637f-4ef4-b16a-21bb68e78de8)

Selected Individual plot:

![5](https://github.com/user-attachments/assets/0cec1cf2-4fd9-4543-be08-1c71ac04d80a)

Summary Table:

![table3](https://github.com/user-attachments/assets/15ed72d3-1f50-432f-9773-749933422268)

# Example using *diamonds* dataset

Shows if `combined_plot = TRUE` but no multiple `y_vars` were provided.

```{r}

# parameters
x_var <- "carat"
y_vars <- "price"
plot_types <- c("scatter", "box")

result_diamonds <- summarize_and_plot(
  data = diamonds, 
  x_var = x_var, 
  y_vars = y_vars, 
  plot_types = plot_types, 
  y_var_colors = c(carat = "blue",
                   price = "black"), 
  combined_plot = TRUE
)

#  summary table
print(result_diamonds$summary_table)

# Access combined bar and scatter plots
result_diamonds$plots$Combined_bar 
result_diamonds$plots$Combined_scatter

```

![6](https://github.com/user-attachments/assets/c989fe75-4ace-4a82-b393-b0fa4b335db1)

![7](https://github.com/user-attachments/assets/ee6bd78f-457f-4a9e-9b22-b415cca75b42)

![table4](https://github.com/user-attachments/assets/1436e8f5-c591-4956-8297-7a299fe9aee6)


# Test using `testthat` library : Dataset *mtcars*

```{r}
library(testthat)

test_summarize_and_plot <- function(data, x_var, y_vars, plot_types, group_by = NULL, combined_plot = FALSE) {
  test_that("summarize_and_plot produces correct output", {
    # Whatever is provided as arguments
    result <- summarize_and_plot(data, 
                                 x_var, 
                                 y_vars, 
                                 plot_types, 
                                 group_by = group_by, 
                                 combined_plot = combined_plot)
    
    # Check that result is a list with two elements: summary_table and plots
    expect_type(result, "list")
    expect_named(result, c("summary_table",
                           "plots"))
    
    #  structure and content
    summary_table <- result$summary_table
    expect_s3_class(summary_table, 
                    "data.frame")
    
    # Checks expected column names in summary table
    summary_cols <- c(x_var, group_by, unlist(sapply(y_vars, function(y) paste0(y, c("_mean", 
                                                                                     "_median", 
                                                                                     "_sd", 
                                                                                     "_max", 
                                                                                     "_min")))))
    summary_cols <- summary_cols[!is.na(summary_cols)]  # Remove NULL
    expect_true(all(summary_cols %in% names(summary_table)))
    
    #no missing values main summary columns
    for (y_var in y_vars) {
      expect_true(all(!is.na(summary_table[[paste0(y_var, "_mean")]])))
      expect_true(all(!is.na(summary_table[[paste0(y_var, "_max")]])))
    }
    
    # named list with expected plot types
    plots <- result$plots
    expect_type(plots, "list")
    
    # Verify each plot for each y_var
    for (plot_name in names(plots)) {
      plot <- plots[[plot_name]]
      expect_s3_class(plot, "ggplot")
    }
  })
}

# Example test with mtcars
test_summarize_and_plot(mtcars, "cyl", 
                            c("mpg", 
                              "hp",
                              "wt"),
                            plot_types = c("box", "histogram"))


```
![test2](https://github.com/user-attachments/assets/c6ef8d65-9e45-4493-b639-b9d26a03ec48)

```{r}
# Example test with iris
test_summarize_and_plot(iris, 
                        "Species", 
                        c("Sepal.Length", 
                          "Sepal.Width", 
                          "Petal.Length",
                          "Petal.Width"), 
                        plot_types = c("box", 
                                       "density"))
```
![test1](https://github.com/user-attachments/assets/36c30355-1a6f-4978-9487-fe9e0287345a)

The `summarize_and_plot` function has basic clean up abilities such as making the `y_var`'s numeric and grouping. As this is v.1.1 there are still some kinks like further personalizing each graph and using different variables to make plots from the selected `y_vars` and `x_vars` rather than just the default, but from the test, this is a good start to exploring data rapidly and efficiently before diving in. 


