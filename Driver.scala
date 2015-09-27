package mSketch
import Jama.Matrix
import scala.collection.mutable.ListBuffer
import math.pow

object Driver extends App{
  println("Begin Norm Experiment")
  
  //Create a large 10000 x 100 random matrix 
  val m1 = Matrix.random(10000,100)
  val sketch = new FD
  
  val skDiffLS = new ListBuffer[Double]  // Create a List Buffer to hold ||A^TA-B^TB||
  val upBoundLS = new ListBuffer[Double] // Create a List Buffer to hold ||A||_F^2 
  
  //Original Bound
  for(l <- 50 to 100) {
    if(l % 5 == 0){ //Evaluate on groups of 5
      val fd = sketch.FFD(m1, l)  
      val sketchedDiff = m1.transpose.times(m1).minus(fd.transpose.times(fd)).norm2
      val upperBound = 2 * pow(m1.normF,2) / l
      skDiffLS.append(sketchedDiff)
      upBoundLS.append(upperBound)
      
      println(l + ":  " + (sketchedDiff <= upperBound))
    }
  }
  // Display actual values of list
  println("Original Bound")
  println(skDiffLS.toList)
  println(upBoundLS.toList)
  
  // Clear Lists for use in next experiment
  skDiffLS.clear
  upBoundLS.clear
  
  // Create Lists to hold Execution times
  val fdTime = new ListBuffer[Long]
  val svdTime = new ListBuffer[Long]
  
  // Compare timing vs Error for fixed l = 750
  println("Begin Timing Experiment")
  val l = 75
  val n = 100
  val start = 10000
  val step = 10000
  val last = 100000
  for(m <- start to last){
    if(m % step == 0){
      val mat = Matrix.random(m,n)
    
      // Execute and time
      val fd = timer(sketch.FFD(mat, l))
      val svd = timer(mat.svd)
      fdTime.append(fd._2)
      svdTime.append(svd._2)
      println(m + "/" + last)
    }
    

    
//    // Compare Error
//    val fdDiff = m1.transpose.times(m1).minus(fd._1.transpose.times(fd._1)).norm2
//    val svdDiff = 2 * pow(m1.normF,2) / l
//    skDiffLS.append(fdDiff)
//    upBoundLS.append(svdDiff)
    
    
  }
  println("Timing Difference")
  println(fdTime.toList)
  println(svdTime.toList)
  
//  println("Error Difference")
//  println(skDiffLS.toList)
//  println(upBoundLS.toList)
  
  
  
  /**
   * This method takes in a chunck of code, and times its
   * execution.
   * @param code - A command to be executed and timed
   * @return - tuple _1 is executed code and _2 is the time to execute
   */
  private def timer[A](code: => A): (A,Long) = {
    val start = System.nanoTime()
    val execute = code
    val end = System.nanoTime()
    
    (execute, end - start)
  }
}