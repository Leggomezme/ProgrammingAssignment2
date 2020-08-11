## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inversa<- NULL     #initialization of two objects, x and inversa.
makeVector(x = numeric()) {            
  set<- function (z){ 
    x <<- z ## Assign the input argument to the x object in the parent environment
    inversa <<- NULL ##Assign the value of NULL to the inversa object in the parent environment
  }
  get<- function()x
  setsolve <- function(solve) inversa <<- solve # sets the input argument to the value of inversa in the main environment.
  getsolve<- function() inversa ## to get the inversa value
  list(set = set, get = get,setsolve = setsolve,getsolve = getsolve) #The last section of code assigns each of these functions as an element within a list
  
} 


## Write a short comment describing this function

cachesolve<- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
   ## Next, the function attempts to retrieve a inverse from the object passed in as the argument. First, it calls the getsolve() function on the input object.
  inversa <- x$getsolve()
  if (!is.null(inversa)) { #Then check if the result is NULL
    message("getting cached data")
    return(inversa) ## returns the value of the inverse to the parent environment by printing the inverse object.
  } #If the result of! Is.null (inversa) is FALSE, cachesolve () gets the vector of the input object, calculates solve ()
  datos <- x$get()
  inversa <- solve(datos, ...)
  x$setsolve(inversa)
  inversa #and then returns the value of the inverse to the parent environment by printing the inverse object.
}
