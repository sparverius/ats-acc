datatype hey =
  | HEYx of string
  | HEYy of string
  
  
fn
get_hey(x: hey): void = 
  case+ x of 
  | HEYx(i) => ()