#' Determining the mode from a vector of numbers.
#' 
#' \code{ClassMode} returns the most commonly occurring number from a vector of numbers.
#' 
#' \code{ClassMode} creates a frequency table based on a vector.  This table is then sorted by highest frequency
#' and returns the top element.  Note, in the case of a tie, the first element is chosen as opposed to a random tiebreaker or returning 
#' both elements.  For the sake of developing Random Forests of SDTs, this is a safer option.
#' 
#' @param classes A character vector of elements to find the mode.
#' @return The mode found.
#' 
#' @export
#' 
#' @examples 
#' Input = c("A", "C", "B", "B", "A", "B")
#' ClassMode(Input)

ClassMode  = function(classes){
  stopifnot(is.vector(classes))
  
  freqtab = table(classes)
  modes = freqtab[max(freqtab)==freqtab]
  themode = names(modes[1]) #in case mult. modes, this is safer
  return(themode)
}
