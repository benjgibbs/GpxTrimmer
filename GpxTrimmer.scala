
package gpxtrimmer

import scala.xml.XML
import scala.xml.Node
import java.util.ArrayList

object GpxTrimmer {

  def main(args: Array[String]) = {
    test()
    
    val xml = XML.loadFile("/Users/benjgibbs/Desktop/toLyndhurst.gpx")

    val allPoints = new ArrayList[Point]()
    
    for(x <- (xml \ "rte" \ "rtept"))
      allPoints.add(new Point(pos(x,"lat"), pos(x,"lon")))

//    for( p <- allPoints.toArray())
//      println(p)
    
    val filteredPoints = allPoints
    var maxTurn = 10.0
    val turnInc = 1.0

    while(filteredPoints.size() > 100){
      var i = 0
      while(i < (filteredPoints.size() - 2)){
        if(filteredPoints.get(i).equals(filteredPoints.get(i+1)))
          filteredPoints.remove(i+1)
        else if(turnIsLessThan(
            filteredPoints.get(i),
            filteredPoints.get(i+1),
            filteredPoints.get(i+2), maxTurn)) 
          filteredPoints.remove(i+1)
        else 
          i += 1
      }
      maxTurn += turnInc
    }
    
    println("Num Points: " + filteredPoints.size + ", Min turn: " + maxTurn)
  }

  def hypotenuse(x: Point, y:Point) = {
    val a1 = (y.lat - x.lat)
    val a2 = (y.long - x.long)
    Math.sqrt(a1*a1+a2*a2)
  }

  def turnIsLessThan(x: Point, y: Point, z: Point, maxTurn: Double) : Boolean= {
    val a = hypotenuse(x,y)
    val b = hypotenuse(y,z)
    val c = hypotenuse(x,z)
    var k = (a*a+b*b-c*c)/(2*a*b)
    

    if ( k > 1.0 ) k = 1.0
    else if( k < -1.0) k = -1.0
    
    val turn = 180.0 - Math.toDegrees(Math.acos(k))
    
//    println("["+a+", "+b+", "+c+"]")
//    println("["+x+", "+y+", "+z+"]")
//    println("Turn  is: " + turn)
    turn < maxTurn
  }
  
  def pos(xml: Node, prop: String) :Double = (xml \ ("@"+prop)).text.toDouble
  case class Point(lat: Double, long: Double){ }
  
  def test() = {
    for (i <- List(
        (Point(0,0),Point(0,1),Point(1,1),90),
        (Point(0,0),Point(1,0),Point(1,1),90),
        (Point(0,0),Point(1,1),Point(2,1),45)
      )) {
      if(!turnIsLessThan(i._1,i._2,i._3,i._4 + 0.01) || 
        turnIsLessThan(i._1,i._2,i._3,i._4 - 0.01)) {
          println("Error in " + i)
          assert(false)
        }
    }
  }
}
