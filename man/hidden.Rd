% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/initital_state.R
\name{hidden}
\alias{hidden}
\alias{disabled}
\title{Sets the initial state of all given tags}
\usage{
hidden(...)

disabled(...)
}
\arguments{
\item{...}{tags to add to the ui, can be a single element or nested tagLists}
}
\value{
tags with the state change
}
\description{
Hidden only hides elements on the top level, so showing these
elements will also show all children of them.

Disabled will recursively traverse the given elements and its
children and set all inputs, buttons, selects and textfields to be disabled.
}
\note{
If you have trouble with these functions, please make sure that you
either - use any dqshiny element in your UI - load the package with
\code{\link{library}} - use \code{\link{init}} at the beginning of your UI!
}
\examples{
## Only run examples in interactive R sessions
if (interactive()) {

library(shiny)
shinyApp(
  ui = fluidPage(
    init(),
    actionButton("btn1", "Toggle Display"),
    actionButton("btn2", "Toggle State"),
    hidden(disabled(actionButton("btn3", "Hello")))
  ),
  server = function(input, output) {
    observeEvent(input$btn1, toggle("btn3"))
    observeEvent(input$btn2, toggle_state("btn3"))
  }
)

}
}
\author{
richard.kunze
}
