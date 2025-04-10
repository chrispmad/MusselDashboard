selectParagraph = function(text = list(NULL),
                           inputs = list(NULL)){
  
  if(length(inputs) >= length(text)){
    stop("Number of inputs should be at most equal to number of text spacers.")
  }
  
  output = ''

  for(i in 1:length(text)){
    
    # Check for spaces / commas / colons etc..
    text_with_spaces = text[[i]]
    if(i == 1){
      if(!text_with_spaces[length(text_with_spaces)] %in% c(" ",",",":")){
        text_with_spaces = paste0(text_with_spaces," ")
      }
    }
    if(i > 1 && i < length(text)){
      if(!text_with_spaces[1] %in% c(" ",",",":")){
        text_with_spaces = paste0(" ",text_with_spaces)
      }
      if(!text_with_spaces[length(text_with_spaces)] %in% c(" ",",",":")){
        text_with_spaces = paste0(text_with_spaces," ")
      }
    }

    text_for_paste = paste0("<p style='display:inline;'>",text_with_spaces,"</p>")
    
    # Add input, if it exists, to the output string.
    if(i <= length(inputs)){
      input_for_paste = inputs[[i]]
    } else {
      input_for_paste = ''
    }
    output = paste0(output, text_for_paste, input_for_paste)
  }
  
  # This is the javascript required to get the input to talk to Shiny.
  div(
    htmlwidgets::onStaticRenderComplete("
      $(document).ready(function() {

    Array.from(document.getElementsByClassName('expandable-word')).forEach((element) => {
      
      element.addEventListener('click', function() {
        
        var inputId = $(this).attr('id');
        $(this).css('border','');
        var originalTitle = $(this).attr('label');
        var choices = $(this).attr('choices');
        var choices_split = choices.split(',');
        
        var wordLinks = '';
        
        for (var i = 0; i < choices_split.length; i++) {
          wordLinks += '<div><a href=\"#\" selector = ' + inputId + ' class=\"word-link\" data-value=\"' + choices_split[i] + '\">' + choices_split[i] + '</a></div>';
        }
        
        $('#' + inputId).html('<div class=\"word-list\">' + wordLinks + '</div>');
        
        // Trying to remove gold border after click; not working yet
        $('#' + inputId).css('border','');
        $('#' + inputId).css('padding', '2px');
      });
    });
                                      
  $('.expandable-word').hover(
    function() {
      $(this).css('border', '2px solid gold');
      $(this).css('padding', '0px');
    },
    function() {
      $(this).css('border','');
      $(this).css('padding', '2px');
    }
  );
  
  // When user clicks on one of the choices, do the following:
    $(document).on('click', '.word-link', function() {
      
      // Grab attributes, data etc. and send to Shiny.
      var inputId = $(this).attr('selector');
      var selectedWord = $(this).data('value');
      Shiny.setInputValue(inputId, selectedWord);
      $('#' + inputId).html(selectedWord);
    });
  
  $('#' + inputId).change(function() {
    var selectedOption = $(this).val();
    $('#expandable-word').css('border','');
  });
  });"),
  htmltools::HTML(output)
  )
}
# 
# selectParagraph_full = function(selectParagraph){
#   
# }