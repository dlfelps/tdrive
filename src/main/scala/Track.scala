import java.time.{Duration, ZoneId}
import java.io.{File, FileInputStream, FileOutputStream, PrintWriter}
import java.time.temporal.ChronoUnit

import com.esotericsoftware.kryo.io.{Input, Output}
import com.twitter.chill.ScalaKryoInstantiator

import scala.collection.mutable.ListBuffer


case class Track(val id: String, val waypoints: Vector[Waypoint]) {

  override def toString: String = {
    id
  }

  def printSummary(): Unit = {
    println(f"Track_id=$id, total_distance=$d%.0f meters, ave_speed=${aveSpeed()}%.0f km/h")
  }

  def aveSpeed(): Float = {
    val d = Taxi.distances(waypoints).sum
    val t = Duration.between(waypoints.head.time, waypoints.last.time).getSeconds
    (d/t*3.6).toFloat
  }

  def formatWKT(): String = {

    val buf = new StringBuilder()

    buf ++= s"$id;LINESTRING("

    for (w <- waypoints){
      buf ++= w.formatWKT()
      buf ++= ","
    }

    //remove last comma
    val buf2 = buf.dropRight(1)

    val aSpeed = aveSpeed()
    buf2 ++= f");$aSpeed%.0f;"//add average speed of track
    buf2.toString()

  }

  def duration(): Long = {
    val start = waypoints.head.time
    val end = waypoints.last.time

    start.until(end, ChronoUnit.SECONDS)
  }


}//end class Track

//companion object
object Track {

  def apply(id: String, waypoints: Vector[Waypoint]) = new Track(id, waypoints)

  def writeWKT(trackList: List[Track], outputFile: String): Unit = {

    val writer = new PrintWriter(new File(outputFile))

    for (t <- trackList){
      writer.println(t.formatWKT())
    }

    writer.close()

  }

  /*
  Uses Twitter Chill for Scala (kryo) to serialize objects.
   */
  def serializeTrackList(trackList: List[Track], outputFile: String): Unit = {


    val instantiator = new ScalaKryoInstantiator
    instantiator.setRegistrationRequired(false)

    val kryo = instantiator.newKryo()
    val output = new Output(new FileOutputStream(outputFile))
    kryo.writeObject(output, trackList)
    output.close()

  }

  def deserializeTrackList(inputFile: String): List[Track] = {

    val instantiator = new ScalaKryoInstantiator
    instantiator.setRegistrationRequired(false)

    val kryo = instantiator.newKryo()

    val input = new Input( new FileInputStream(inputFile))
    val deser = kryo.readObject(input, classOf[List[Track]])
    input.close()
    deser
  }

  /*
  Puts all tracks occuring on the same day, but does not change the time.
   */
  def compressDay(trackList: List[Track]): List[Track] = {

    //get the day of the first instant (int representing day of the year)
    //error if spans more than a year
    val targetDay = trackList(0).waypoints(0).time.atZone(ZoneId.of("UTC")).getDayOfYear

    val newTracks = new ListBuffer[Track]()

    for (t <- trackList) {
      newTracks += compressDaySub(t, targetDay)
    }

    def compressDaySub(track: Track, targetDay: Int): Track = {

      val newPoints = new ListBuffer[Waypoint]()

      val today = track.waypoints(0).time.atZone(ZoneId.of("UTC"))
      val offset = today.getDayOfYear - targetDay

      for (w <- track.waypoints) {
        val newday = {
          if (offset >= 0)
            w.time.minus(offset, ChronoUnit.DAYS)
          else w.time.plus(-offset, ChronoUnit.DAYS)
        }

        newPoints += Waypoint(w.lat, w.long, newday)
      }

      new Track(track.id, newPoints.toVector)
    }

    newTracks.toList
  }



  def compressHour(trackList: List[Track]): List[Track] = {

    //sets all tracks to start between midnight and 1 AM
    //tracks can carry over beyond 1 AM depending on start time and duration

    val newTracks = new ListBuffer[Track]()

    for (t <- trackList) {
      newTracks += compressHourSub(t)
    }

    def compressHourSub(track: Track): Track = {

      val newPoints = new ListBuffer[Waypoint]()

      val today = track.waypoints(0).time.atZone(ZoneId.of("UTC"))
      val offset = today.getHour

      for (w <- track.waypoints) {
        val newday = w.time.minus(offset, ChronoUnit.HOURS)
        newPoints += Waypoint(w.lat, w.long, newday)
      }

      new Track(track.id, newPoints.toVector)
    }

    newTracks.toList
  }



}//end companion object Track