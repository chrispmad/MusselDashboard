intextInput = function(inputId, label = NULL, 
                       choices = c('Arnold','cats',4), 
                       selected = NULL,
                       textColour = 'purple',
                       backgroundColour = 'lightblue',
                       borderRoundness = '10%',
                       padding = '2px'){
  
  if(is.null(label)) stop("Label must not be NULL")
  
  # if(is.null(selected)) selected = choices[1]
  # Collapse vector of options into a single string separated by commas.
  choices_as_str <- paste(
    choices,
    collapse = ',')
  
  styleArgs = paste0('color:',textColour,'; background-color:',backgroundColour,';border-radius:',borderRoundness,';padding:',padding,';')
  # Output is a barely formatted chunk of HTML. But it works!
  HTML(paste0("<div id = '",inputId,"', choices = ",choices_as_str," class = 'expandable-word' style = '",styleArgs,"; display:-webkit-inline-box;'>",label,"</div>"))
}