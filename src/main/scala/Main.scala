package itc

import org.eclipse.jetty.server.Server
import org.eclipse.jetty.servlet.ServletHandler

object Main {

  def main(args: Array[String]): Unit = {

    val port    = sys.env.get("PORT").fold(8080)(_.toInt)
    val server  = new Server(port)
    val handler = new ServletHandler()

    server.setHandler(handler)
//         handler.addServletWithMapping(HelloServlet.class, "/*");
    server.start()
    server.join()

  }

}