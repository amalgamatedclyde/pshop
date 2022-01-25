package scalashop

import org.scalameter._
import common._

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */
  
 
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
  // TODO implement this method using the `boxBlurKernel` method
//    println("source", src.width, src.height)
//    println("dest", dst.width, dst.height)
    var x = from
    while(x < end){
      var y = 0
      while(y<=src.height-1){
//        println("feeding", x ,y)
        dst.update(x, y, boxBlurKernel(src, x, y, radius))
        y+=1
        }
    x+=1
    }
//    for(i<-List(0,1,2);
//        j<-List(0,1,2)){
//      println(i,j,dst(i,j))}
  }


  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   */

     def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Int = {
  // TODO implement using the `task` construct and the `blur` method
     if (numTasks==1){
      blur(src, dst, 0, src.width, radius)
      return 1
    }
    if (src.width/numTasks < 1){
      val numTasks = src.width
    }
      if (numTasks==src.width){
      val tasks = List.range(0, src.width).map(c=> task { blur(src, dst, c, c+1, radius) })
      tasks.map(_.join)
      return tasks.length
    }
     if (src.height*src.width <=2048){
      blur(src, dst, 0, src.height, radius)
      return 1
    }

    else{
    val stripStarts = List.range(0, src.width-1, src.width/numTasks)
    val strips = for(ind<-0 until stripStarts.length-1) yield (stripStarts(ind), stripStarts(ind+1))
    val tasks = (strips.init.map(c=> task { blur(src, dst, c._1, c._2, radius) }).toList):::List(task { blur(src, dst, strips.last._1, src.width, radius) })
    tasks.map(_.join)
    return tasks.length
    }
     
}
}

