## -------------------------------makeCacheMatrix------------------------##
## makeCacheMatrix function to cache inverse of a square matrix 
##supplied as an argument to cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  
  #check if cacheVariable is defined
  if(!exists("invMatrix"))
  {
    invMatrix<<-NULL
    #print("invMatrix value doesn't exist in memory. def invMatrix with NULL value")
  }
  
  
  cacheSolve(x)
  
  print(invMatrix)
  
}

##----------------------------cachSolve----------------------------------------##
## cacheSolve function to check whether matrix inverse exists in cache else
## calculate inverse of the matrix 

cacheSolve <- function(x, ...) {
  
  #check if cacheVariable is not null Value. 
  #if cacheVariable is null, compute the matrix inverse
  #also store matrix inverse values in another variable for future comparison
  if(is.null(invMatrix))
  {
    invMatrix<<-solve(x)
    staticInvMatrix<<-invMatrix
    #print("matrix is currently null. computing inverse of matrix now.")
    
  }
  else
  {
    ## if cacheVariable exists, check if the original values are maintained
    ## else recompute inverse of matrix
    if(!identical(invMatrix,staticInvMatrix)) 
    {
      ##solving the matrix again as the values are changed
      invMatrix<<-solve(x)
      #print("values changed in the matrix. hence recomputed")
      
    }
  }
  return(invMatrix)
  ## Return a matrix that is the inverse of 'x'
}
