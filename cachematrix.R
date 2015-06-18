
#Notes

#I used the following script to test this application:
#https://class.coursera.org/rprog-015/forum/thread?thread_id=447.
#The functions do work with this script.

#I tried to keep the the structure of the examples given.

makeCacheMatrix <- function(x = matrix()) {
  
  #The first function, makeVector creates a special "vector", which is really a list containing a function to
  #set the value of the vector
  #get the value of the vector
  #set the value of the inverse
  #get the value of the inverse matrix
  
  #Basically this comes from makeVector example but with mean changed to solve.
  
  m<-NULL
  
  #set the value of the vector
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  #get the value of the vector
  get<-function() x
  
  #set the value of the inverse
  setincache<-function(solve) m<<- solve
  
  #get the value of the inverse matrix
  getmatrix<-function() m
  list(set=set, get=get,
       setincache=setincache,
       getmatrix=getmatrix)
}


#The following function calculates the mean of the special "vector"
#created with the above function. 
#However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the mean from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and sets the value 
#of the mean in the cache via the setincache function.

cacheSolve <- function(x=matrix(), ...) {
  
  #Get value of cached inverse matrix
  m<-x$getmatrix()
  
  #Check to see what has been retrived 
  if(!is.null(m)){
    #If it exists then return the vector
    message("getting cached data")
    return(m)
  }
  #If not then get the vector
  mymatrix <- x$get() 
  #Then retrieve the inverse
  m<-solve(mymatrix, ...)
  #and put it into the cache
  x$setincache(m)
  #and return it
  return(m)
}