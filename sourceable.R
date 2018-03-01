# Define a ggplot2 object as "sourceable", which means that it knows
# how to keep track of its source code (parameter must be a ggplot2 object)
sourceable <- function(x) {
  stopifnot(methods::is(x, "ggplot"))
  attr(x, "source_code") <- deparse(substitute(x))
  attr(x, "source_deps") <- list()
  class(x) <- c("sourceable", class(x))
  x
}

# Overwrite the plus operator so that if a "sourceable" object is used,
# the source code is kept
`+` <- function(e1, e2) {
  if (methods::is(e1, "sourceable")) {
    res <- base::`+`(e1, e2)
    new_code <- trimws(deparse(substitute(+e2)))
    new_code <- paste(new_code, collapse = "")
    attr(res, "source_code") <- paste(attr(res, "source_code"), new_code)
    res
  }
  # If we're not dealing with a "sourceable" object, carry on with regular +
  else {
    base::`+`(e1, e2)
  }
}

# Attach dependecies to the source code (any input variables are automatically
# attached)
attach_source_dep <- function(x, deps) {
  stopifnot(methods::is(x, "sourceable"))
  if (length(deps) == 0) {
    return(x)
  }
  
  # Keep a reference to the parent frame, where the dependency objet should be
  # evaluated
  parentFrame <- parent.frame(1)
  
  # Create a list of all the dependencies and their values
  new_deps <- lapply(deps, function(dep) {
    eval(parse(text = dep), envir = parentFrame)
  })
  new_deps <- setNames(new_deps, deps)
  
  attr(x, "source_deps") <- append(attr(x, "source_deps"), new_deps)
  x
}

# Retrieve the source code of a "sourceable" ggplot2
get_source_code <- function(x) {
  stopifnot(methods::is(x, "sourceable"))
  
  # Get the plot code and add a line break after every + sign
  plot_code <- attr(x, "source_code")
  plot_code <- gsub("\\+[[:space:]]*", "\\+\n  ", plot_code)
  
  # Get the code for any input variables used in the plot
  input_code <- ""
  if (exists("input", envir = parent.frame())) {
    input_vars <- stringr::str_extract_all(plot_code, "input\\$[[:alnum:]]*")[[1]]
    input_vars <- sub("input\\$", "", input_vars)
    input_list <- reactiveValuesToList(get("input", envir = parent.frame()))
    if (length(input_vars) > 0) {
      input_code <- paste0("input <- list()\n")
      for (input_var in input_vars) {
        input_var_value <- capture.output(
          dput(get(input_var, envir = as.environment(input_list))))
        input_var_value <- paste(trimws(input_var_value), collapse = "")
        input_code <- paste0(input_code,
                             "input$", input_var, " <- ", input_var_value, "\n") 
      }
      input_code <- paste0(input_code, "\n")
    }
  }

  # Get the code for the dependency variables
  dep_code <- ""
  dep_vars <- attr(x, "source_deps")
  if (length(dep_vars) > 0) {
    for (dep_var in names(dep_vars)) {
      dep_var_value <- capture.output(dput(dep_vars[[dep_var]]))
      dep_var_value <- paste(trimws(dep_var_value), collapse = "")
      dep_code <- paste0(dep_code,
                         dep_var, " <- ", dep_var_value, "\n")
    }
    dep_code <- paste0(dep_code, "\n")
  }
  
  # Paste together the input variables, dependency variables, and plot code
  full_code <- paste0(input_code, dep_code, plot_code)
  full_code
}