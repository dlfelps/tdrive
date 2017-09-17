import java.time.Instant
import java.lang.Math.atan2
import scala.math._


case class Waypoint (val lat: Float, val long: Float, val time: Instant) {

  override def toString(): String = {
    s"($lat,$long) @ $time"
  }

  def formatWKT(): String = {
    s"$long $lat"
  }

}

//companion object
object Waypoint {

  def apply(lat:Float, long:Float, time:Instant) = new Waypoint(lat, long, time)

  def haversine(w1: Waypoint, w2: Waypoint): Float = {

    if ((w1.lat == w2.lat) && (w1.long == w2.long)) {
      (0.0).toFloat
    } else {

      val dLat = (w2.lat - w1.lat).toRadians
      val dLon = (w2.long - w1.long).toRadians

      val a = pow(sin(dLat / 2), 2) + pow(sin(dLon / 2), 2) * cos(w1.lat.toRadians) * cos(w2.lat.toRadians)
      val c = 2 * asin(sqrt(a))
      (c * 6371000).toFloat //Earth radius in m (6.371 million)
    }
  }

  def bearingOpt(w1: Waypoint, w2: Waypoint): Option[Float] = {

    if ((w1.lat == w2.lat) && (w1.long == w2.long)) {
      None
    } else {

      val dLon = (w2.long - w1.long).toRadians

      val x = cos(w2.lat.toRadians) * sin(dLon)
      val y = cos(w1.lat.toRadians) * sin(w2.lat.toRadians) - sin(w1.lat.toRadians) * cos(w2.lat.toRadians) * cos(dLon)

      val b = atan2(x, y).toDegrees
      val clock = (b + 360) % 360
      Some(clock.toFloat)

    }
  }

}//end of companion Waypoint object
