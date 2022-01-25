
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
//  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
//    // TODO implement using while loops
//   val buf = scala.collection.mutable.ArrayBuffer.empty[Tuple2[Int,Int]]
//   for(u<-x-radius until x+radius){
//   for(v<-y-radius until y+radius){
//     buf += Tuple2(clamp(u, 0, src.width-1), clamp(v, 0, src.height-1))}}
//   val pixels = buf.distinct
//   var rChannel = 0; var gChannel = 0; var bChannel = 0; var aChannel = 0; 
//   for (pix<-pixels) {
//     rChannel += red(src(pix._1, pix._2))
//     gChannel += green(src(pix._1, pix._2))
//     bChannel += blue(src(pix._1, pix._2))
//     aChannel += alpha(src(pix._1, pix._2))
//   }
//   rgba(rChannel/pixels.length, gChannel/pixels.length, bChannel/pixels.length, aChannel/pixels.length)
//  }
//  


//def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
//    // TODO implement using while loops
//   if(radius==0) return src(x,y)
//   var rChannel = 0; var gChannel = 0; var bChannel = 0; var aChannel = 0; var pixels = 0;
//
//   (List.range(clamp(x-radius, 0, src.width), clamp(x+radius+1, 0, src.width)) zip 
//       List.range(clamp(y-radius, 0, src.height), clamp(y+radius+1, 0, src.height))).distinct.
//       map(t=>
//        {rChannel += red(src(t._1, t._2))
//         gChannel += green(src(t._1, t._2))
//         bChannel += blue(src(t._1, t._2))
//         aChannel += alpha(src(t._1, t._2))
//         pixels += 1}) 
//   rgba(rChannel/pixels, gChannel/pixels, bChannel/pixels, aChannel/pixels)
//   }
   
//  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
//    // TODO implement using while loops
//   if(radius==0) return src(x,y)
//   var rChannel = 0; var gChannel = 0; var bChannel = 0; var aChannel = 0; var pixels = 0;
//   var uv = scala.collection.mutable.ArrayBuffer.empty[Tuple2[Int,Int]]
//   for(u<-clamp(x-radius, 0, src.width) until clamp(x+radius+1, 0, src.width)){
//     for(v<-clamp(y-radius, 0, src.height) until clamp(y+radius+1, 0, src.height)){
//       if(!(uv contains (u, v))){
//         uv.append((u, v))
//         rChannel += red(src(u, v))
//         gChannel += green(src(u, v))
//         bChannel += blue(src(u, v))
//         aChannel += alpha(src(u, v))
//         pixels +=1
//       }
//       }
//   }
//   rgba(rChannel/pixels, gChannel/pixels, bChannel/pixels, aChannel/pixels)
//  }
  
//    def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
//    // TODO implement using while loops
//   if(radius==0) return src(x,y)
//   var rChannel = 0; var gChannel = 0; var bChannel = 0; var aChannel = 0; var pixels = 0;
//   for(u<-clamp(x-radius, 0, src.width) until clamp(x+radius+1, 0, src.width)){
//     for(v<-clamp(y-radius, 0, src.height) until clamp(y+radius+1, 0, src.height)){
//         rChannel += red(src(u, v))
//         gChannel += green(src(u, v))
//         bChannel += blue(src(u, v))
//         aChannel += alpha(src(u, v))
//         pixels +=1
//       }
//      }
//   rgba(rChannel/pixels, gChannel/pixels, bChannel/pixels, aChannel/pixels)
//  }
//  
   def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    // TODO implement using while loops
//     println("coords", x,y)
     if(radius==0) return src(x,y)
//     val coords = scala.collection.mutable.ArrayBuffer.empty[Tuple2[Int,Int]]
     var rChannel = 0L; var gChannel = 0L; var bChannel = 0L; var aChannel = 0L; var pixels = 0;
     var v = clamp(y-radius, 0, src.height)
     while(v <= clamp(y+radius, 0, src.height-1)){
       var u = clamp(x-radius, 0, src.width)
       while(u <= clamp(x+radius, 0, src.width-1)){
//         println("u,v", u,v)
         rChannel += red(src(u, v))
         gChannel += green(src(u, v))
         bChannel += blue(src(u, v))
         aChannel += alpha(src(u, v))
         pixels +=1
         u+=1
         }
       v+=1
//       println("pixels", pixels)
     }
//     println(x, y, rgba((rChannel/pixels).toInt, (gChannel/pixels).toInt, (bChannel/pixels).toInt, (aChannel/pixels).toInt))
     rgba((rChannel/pixels).toInt, (gChannel/pixels).toInt, (bChannel/pixels).toInt, (aChannel/pixels).toInt)
    }
//     def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
//    // TODO implement using while loops
//     if(radius==0) return src(x,y)
//     var rChannel = 0L; var gChannel = 0L; var bChannel = 0L; var aChannel = 0L; var pixels = 0;
//     if(y < src.height-1){
//     var u = x-radius
//     while(u <= x+radius){
//       var v = y-radius
//       while(v <= y+radius){
////         println("(" + u +", " + v + ")", x,y)
//         rChannel += red(src(clamp(u, 0, src.width-1), clamp(v, 0, src.width-1)))
//         gChannel += green(src(clamp(u, 0, src.width-1), clamp(v, 0, src.width-1)))
//         bChannel += blue(src(clamp(u, 0, src.width-1), clamp(v, 0, src.width-1)))
//         aChannel += alpha(src(clamp(u, 0, src.width-1), clamp(v, 0, src.width-1)))
//         pixels +=1
//         v+=1
//         }
//       u+=1
//     }}
//     rgba((rChannel/pixels).toInt, (gChannel/pixels).toInt, (bChannel/pixels).toInt, (aChannel/pixels).toInt)
//  }
}