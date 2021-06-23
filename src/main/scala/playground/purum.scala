package playground

import sttp.client.quick._

val reg = "\\.*?2\\.(\\d+)".r

@main def purum() = 
  val res = quickRequest.get(uri"https://apod.nasa.gov/htmltest/gifcity/e.2mil").send()

  val digs = res.body match {
    case reg(sub) => sub
  }
