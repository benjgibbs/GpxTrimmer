
package gpxtrimmer

import scala.xml.XML
import scala.xml.Elem
import scala.xml.Node
import java.util.ArrayList

object GpxTrimmer {
  val MAX_POINTS= 100

  def main(args: Array[String]) = {
    test()

    val inFile="/Users/benjgibbs/Code/GpxTrimmer/input/"+ args(0) + ".gpx"
    val outFile="/Users/benjgibbs/Code/GpxTrimmer/output/"+args(0) + "_truncated.gpx"
    val rteName=args(0)
    
    val xml = XML.loadFile(inFile)

    var allPoints : List[Point] = Nil
    
    for(x <- (xml \ "rte" \ "rtept"))
      allPoints = Point(pos(x,"lat"), pos(x,"lon"), name(x)) :: allPoints

    println(allPoints.reverse)

    XML.save(outFile,updateXml(xml,rteName,filterPoints(allPoints.reverse)))
    println("Wrote: " + outFile)
  }
  
  def updateXml(xml: Node, name: String, points: List[Point]) : Node= {
  <gpx version="1.1"
       creator="GMapToGPX 6.4b - http://www.elsewhere.org/GMapToGPX/"
       xmlns="http://www.topografix.com/GPX/1/1"
       xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
       xsi:schemaLocation="http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd">
     <rte>
        <name>{name}</name>
        <cmt>Trunced by GpxTrimmer</cmt>
        {for(p <- points) yield p.toXml()}
      </rte>
    </gpx>
  }

  def filterPoints(allPoints: List[Point]) = {  
    var minTurn = 10.0
    val turnInc = 0.1
    var points = allPoints
    
    def runThrough(points: List[Point]) :List[Point]= {
        points match {  
        case x :: y :: z :: rest =>
          if(x==y||y==z) runThrough(x :: z :: rest)
          else if(x==z) runThrough(rest)
          else if(turnIsLessThan(x,y,z,minTurn)) x :: runThrough(List(z) ::: rest)
          else x :: y :: z :: runThrough(rest)
        case a @ _ => a
      }
    }

    while(points.size > MAX_POINTS) {
      points = runThrough(points)
      println("Num Points: " + points.size + ", Min turn: " + minTurn)
      minTurn += turnInc
      if(minTurn > 359.0)
        throw new RuntimeException("Turn is too big: " + points)
    }
    points
  }

  def pos(xml: Node, prop: String) :Double = (xml \ ("@"+prop)).text.toDouble
  def name(xml: Node) : String = (xml \ ("name")).text

  def hypotenuse(x: Point, y:Point) = {
    val a1 = (y.lat - x.lat)
    val a2 = (y.long - x.long)
    Math.sqrt(a1*a1+a2*a2)
  }

  def turnIs(x: Point, y: Point, z: Point) = {
    val a = hypotenuse(x,y)
    val b = hypotenuse(y,z)
    val c = hypotenuse(x,z)
    var k = (a*a+b*b-c*c)/(2*a*b)
    
    if(k > 1.0 || k < -1.0){
      println("["+a+", "+b+", "+c+"]")
      println("["+x+", "+y+", "+z+"]")
      println("k  is: " + k)
      assert(false)
    }
    180.0 - Math.toDegrees(Math.acos(k))
  }

  def turnIsLessThan(x: Point, y: Point, z: Point, maxTurn: Double) : Boolean= {
    turnIs(x,y,z) < maxTurn
  }
  
  object Point{ def apply(lat: Double, long: Double) = new Point(lat,long) }
  case class Point(lat: Double, long: Double,name: String){ 
   def this(lat: Double, long: Double) = this(lat, long, "") 
   def == (p : Point) = {
    Math.abs(lat - p.lat) < 0.000000001 && Math.abs(long - p.long) < 0.00000001
   }
   def toXml() : Node = {
     <rtept lat={lat.toString()} lon={long.toString()}>
       <name>{name}</name>
     </rtept>
   }
  }
  
  def test() = {
    for (i <- List(
        (Point(0,0),Point(0,1),Point(1,1),90),
        (Point(0,0),Point(1,0),Point(1,1),90),
        (Point(0,0),Point(1,1),Point(0,1),135),
        (Point(0,0),Point(-1,1),Point(0,2),90),
        (Point(0,0),Point(1,1),Point(2,1),45)
      )) {
      val tol = 0.000001
      if(!turnIsLessThan(i._1,i._2,i._3,i._4 + tol) || 
        turnIsLessThan(i._1,i._2,i._3,i._4 - tol)) {
          println("Error in " + i)
          println("We think turn  of "+ i._4+" is " + turnIs(i._1, i._2,i._3))
          assert(false)
        }
    }
  }
}
