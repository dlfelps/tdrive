import org.scalatest.FlatSpec
import java.time.Instant

class WaypointSpec extends FlatSpec {

  def fixture =
    new {
      val w1 = Waypoint("116.45804".toFloat, "39.86973".toFloat, Instant.parse("2008-02-02T13:30:49Z"))
      val w2 = Waypoint("116.47083".toFloat, "39.91227".toFloat, Instant.parse("2008-02-02T14:09:12Z"))
    }

  behavior of "A Waypoint"

  it should "be created properly" in {
    val f = fixture
    assert(f.w1 !=  f.w2)
  }

  it should "calculate distances" in {
    val f = fixture
    assert(Waypoint.haversine(f.w1, f.w2) > 0)
  }

}