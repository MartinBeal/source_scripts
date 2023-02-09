## Martin Beal, July 2022

## function to split a string by a pattern and create columns in a data.frame 
# from the split. E.g. to turn a string column into multiple columns via cbind

# cols argument is the index of (split) columns to be returned
# colnames are the new names to give the returned columns (must be same length as cols)

str2col <- function(x, pattern, cols=NULL, colnames=NULL){
  df <- data.frame(do.call(rbind, str_split(x, pattern=pattern)))
  if(!is.null(cols)){
    df <- df[,cols]
    if(!is.null(colnames)){
      colnames(df)[cols] <- colnames
      }
  }
  return(df)
}

#e.g. 
# str2col(councoord4$address, pattern=", ", cols=1:2, colnames=c("county", "country"))
