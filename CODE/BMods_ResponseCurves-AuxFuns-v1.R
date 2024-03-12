

## ---------------------------------------------------------------------------- ##

"+" = function(x,y) {
  if(is.character(x) || is.character(y)) {
    return(paste(x , y, sep=""))
  } else {
    .Primitive("+")(x,y)
  }
}

go_up_filepath <- function(filepath, n) {
  # Ensure n is non-negative
  if(n < 0) {
    stop("n must be non-negative")
  }
  # Go up n levels
  for(i in 1:n) {
    filepath <- dirname(filepath)
  }
  return(filepath)
}


get_sp_name_ <- function(x){
  return(unlist(strsplit(x,"_"), use.names = FALSE)[1])
}

get_sp_name <- function(x) sapply(x,FUN = get_sp_name_,simplify = TRUE, USE.NAMES = FALSE)

get_algo_name_ <- function(x){
  return(unlist(strsplit(x,"_"), use.names = FALSE)[4])
}

get_algo_name <- function(x) sapply(x,FUN = get_algo_name_,simplify = TRUE, USE.NAMES = FALSE)


format_sp_name_ <- function(x) {
  
  # Find the position where the species name starts (first uppercase 
  # letter after the first character)
  splitPos <- regexpr("[A-Z]", substring(x, 2)) + 1
  
  # Split the string into genus and species
  genus <- substr(x, 1, splitPos - 1)
  species <- substr(x, splitPos, nchar(x))
  
  # Capitalize the first letter of the genus and make the rest uppercase
  genusFormatted <- toupper(substr(genus, 1, 1)) + tolower(substr(genus, 2, nchar(genus)))
  
  # Make the species part lowercase
  speciesFormatted <- tolower(species)
  
  # Combine and return the formatted name
  formattedName <- paste(genusFormatted, speciesFormatted)
  return(formattedName)
}

format_sp_name <- function(x) sapply(x,FUN = format_sp_name_, simplify = TRUE, USE.NAMES = FALSE)

