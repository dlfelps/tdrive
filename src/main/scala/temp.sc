
val tracks = Track.deserializeTrackList("/home/dlfelps/IdeaProjects/tdrive/raw.ser")

val durs = tracks.map(_.duration())

val len = tracks.map(_.waypoints.length)


s"The average track lasts ${durs.sum/durs.length/60.0} minutes."

s"The median track lasts ${durs(durs.length/2)/60.0} minutes."

s"The longest track lasts ${durs.last/60.0} minutes."

Track.writeWKT(tracks, "broken_tracks.txt")










