import org.scalatest.FlatSpec

class TrackSpec extends FlatSpec {

  def fixture =
    new {
      val taxi = Taxi.readCSV("/data/tdrive/1131.txt")
      val tracks = taxi(0).breakTrack()
    }

  behavior of "Tracks with Kryo"

  it should "be saved/loaded properly" in {
    val f = fixture
    Track.serializeTrackList(f.tracks, "testSerial.ser")
    val tracks2 = Track.deserializeTrackList("testSerial.ser")

    assert(f.tracks ==  tracks2)
  }

}