
object Hello extends App {


  val tracks = Taxi.readDIR("/data/tdrive")

  Track.writeWKT(tracks, "broken_tracks.txt")

}



object Normal extends App {

  val tracks = {
    if (false) Taxi.readDIR("/data/tdrive")
      else Track.deserializeTrackList("raw.ser")
  }

  Track.serializeTrackList(tracks, "raw.ser")

  println(tracks.length)

  val tracks2=Track.compressDay(tracks)
  val tracks3=Track.compressHour(tracks2)


}
