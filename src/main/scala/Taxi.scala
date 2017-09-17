/*
  Microsoft T-drive data available at https://www.microsoft.com/en-us/research/publication/t-drive-trajectory-data-sample/

 */

import java.io.File
import java.time.{Duration, Instant}

import scala.collection.mutable.ListBuffer


case class Taxi(val id: String, val waypoints: Vector[Waypoint]) {

  override def toString: String = {
    waypoints.mkString("\n")
  }

  def breakTrack(): List[Track] = {

    //criteria determined here
    val tooSlow: Vector[Boolean] = Taxi.instant_vel(waypoints).map(v => v < 5 || v > 130)//not too fast, not too slow
    val tooLong: Vector[Boolean] = Taxi.time_diff(waypoints).map(t => t > 90)//not to far apart
    val tooSharp: Vector[Boolean] = Taxi.turnMagnitude(waypoints).map(turns => turns > 45)//no quick turns
    val combined: Vector[Boolean] = (tooSlow, tooLong, tooSharp).zipped.map((v, t, turns) => v || t || turns)

    val tooRando: Vector[Boolean] = Vector.fill(combined.length)(scala.util.Random.nextInt(10)).map(r => r == 0)
    val withRando: Vector[Boolean] = (combined, tooRando).zipped.map((c, r) => c || r)


    /*
    Finds continuous sequences of "false" values in a Vector[Boolean]
     */
    def findContinuousTracks(boolSeq: Vector[Boolean]):List[(Int, Int)] = {

      var left=0
      var right=1

      val pairs = new ListBuffer[(Int, Int)]()

      while (left < boolSeq.length - 2 && right < boolSeq.length - 1) {

        //find first false
        while (boolSeq(left) && left < boolSeq.length - 2) {
          left += 1
        }

        //find next true
        right = left + 1
        while (!boolSeq(right) && right < boolSeq.length-1) {
          right += 1
        }

        if (right < boolSeq.length-1){
          pairs += ((left,right))
          left = right //start next loop
          right = left + 1
        }
      }
      pairs.toList
    }

    def filterTracks(input: List[(Int,Int)]): List[Track] = {
      //filter track segments with length > threshold
      val threshold = 4 //at least 10 waypoints

      val filtered = input.filter { case(left,right) => (right-left) > threshold}

      val tracks = new ListBuffer[Track]
      var counter = 0

      for (f <- filtered) {
        tracks += new Track(id + "_" + counter.toString, waypoints.slice(f._1, f._2 + 1))//slice does not include last element
        counter += 1
      }
      tracks.toList
    }

    filterTracks(findContinuousTracks(withRando))
  }//end of breakTrack method

  /*
  Converts Taxi object to one long track; needed to use Track utility functions like writeWKT
   */
  def toTrack(): Track = {
    new Track(id, waypoints)
  }



}//end of Taxi class

//companion object
object Taxi {

  def apply(id: String, waypoints: Vector[Waypoint]) = new Taxi(id, waypoints)

  /*
  Reads in a T-drive input file and creates a Taxi object.
   */
  def readCSV(inputFile:String):List[Taxi] = {
    readCSV_timed(new File(inputFile), Instant.MIN, Instant.MAX)
  }

  /*
  Only loads waypoints within specified time range
   */

  def readCSV_timed(inputFile: File, start: Instant = Instant.MIN, end: Instant = Instant.MAX): List[Taxi] = {
    val bufferedSource = io.Source.fromFile(inputFile)
    val waypoints = new ListBuffer[Waypoint]()
    var ID = ""

    val maxSampleRate = Duration.ofSeconds(30)//no more than every 30 seconds
    var nextSampleAt = start

    for (line <- bufferedSource.getLines) {
      val Array(id, dateTime, long, lat) = line.split(",").map(_.trim)
      ID = id
      val Array(date, time) = dateTime.split(" ").map(_.trim)
      val timeInst = Instant.parse(date + "T" + time + "Z")
      if (timeInst.isAfter(nextSampleAt) && timeInst.isBefore(end)){
        waypoints += Waypoint(lat.toFloat, long.toFloat, timeInst)
        nextSampleAt = timeInst.plus(maxSampleRate)
      }
    }

    bufferedSource.close

    val taxi:List[Taxi] = new Taxi(ID, waypoints.toVector) :: Nil

    taxi
  }

  /*
  Reads an entire directory of Taxi files, converts them to broken tracks, and returns a single List[Track]
   */

  def readDIR(inputDIR:String): List[Track] = {
    readDIR_timed(inputDIR, Instant.MIN, Instant.MAX)
  }

  def readDIR_timed(inputDIR:String, start: Instant, end: Instant): List[Track] = {

    def getListOfFiles(dir: File, extensions: List[String]): List[File] = {
      dir.listFiles.filter(_.isFile).toList.filter { file =>
        extensions.exists(file.getName.endsWith(_))
      }
    }

    val okFileExtensions = List("txt")
    val files = getListOfFiles(new File(inputDIR), okFileExtensions)

    val taxis: List[Taxi] = files.flatMap(f => Taxi.readCSV_timed(f,start,end))
    val tracks: List[Track] = taxis.flatMap(_.breakTrack())

    tracks
  }

  /*
Function calculates distance covered between waypoints and amount of time between waypoints.
Is used to approximate instantaneous velocity in km/h.

 */
  def instant_vel(waypoints: Vector[Waypoint]): Vector[Float] = {

    val D = Taxi.distances(waypoints)
    val T = Taxi.time_diff(waypoints)

    val list = new ListBuffer[Float]()

    for ((d,t) <- D zip T) {
      list += (d/t * 3.6).toFloat //converts to km/h
    }
    list.toVector
  }

  def distances(waypoints: Vector[Waypoint]): Vector[Float] = {

    val list = new ListBuffer[Float]()

    for (i <- 1 until waypoints.length) { //dont start with 0 b/c calculating differences
      list += Waypoint.haversine(waypoints(i-1), waypoints(i))
    }

    list.toVector
  }

  def time_diff(waypoints: Vector[Waypoint]): Vector[Long] = {

    val list = new ListBuffer[Long]()

    for (i <- 1 until waypoints.length) { //dont start with 0 b/c calculating differences
      list += Duration.between(waypoints(i-1).time, waypoints(i).time).getSeconds
    }

    list.toVector
  }

  /*
  Calculates bearings [0, 360)
   */
  def bearings(waypoints: Vector[Waypoint]): Vector[Float] = {

    val bear = new ListBuffer[Float]()

    for (i <- 1 until waypoints.length) {
      val temp = Waypoint.bearingOpt(waypoints(i-1), waypoints(i))
      if (temp.isDefined){
        bear += temp.get
      } else { //maintain bearing
        if (i>1) {
          bear += bear.last
        } else { //default to 0
          bear += 0
        }
      }
    }

    bear.toVector
  }

  /*
  Calculates angle between consecutive bearings; uses modulo math to cross 360 -> 0
   */
  def turnMagnitude(waypoints: Vector[Waypoint]): Vector[Float] = {

    val turns = new ListBuffer[Float]()
    val bear = bearings(waypoints)

    for (i <- 1 until bear.length){
      turns += minAngle(bear(i-1), bear(i))
    }

    def minAngle(a1: Float, a2: Float): Float = {

      var smaller = a1
      var larger = a2
      if (a1 > a2){
        smaller = a2
        larger = a1
      }

      (larger - smaller) min (smaller + 360 - larger)
    }

    turns.toVector
  }


}// end of companion object Taxi

