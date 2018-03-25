import java.io._
import org.apache.http.{HttpEntity, HttpResponse}
import org.apache.http.client._
import org.apache.http.client.methods.HttpGet
import org.apache.http.impl.client.DefaultHttpClient
import scala.collection.mutable.StringBuilder
import scala.xml.XML
import org.apache.http.params.HttpConnectionParams
import org.apache.http.params.HttpParams

import scala.io.StdIn



object ContentReader extends App {


    /**
     * Returns the text (content) from a REST URL as a String.
     * Returns a blank String if there was a problem.
     * This function will also throw exceptions if there are problems trying
     * to connect to the url.
     *
     * @param url A complete URL, such as "http://foo.com/bar"
     * @param connection_timeout The connection timeout [ms]
     * @param socket_timeout The socket timeout [ms]
     */
    
    def getRestContent(url: String,
                       connection_timeout: Int,
                       socket_timeout: Int): String = {
    
        val httpClient = buildHttpClient(connection_timeout, socket_timeout)
        val httpResponse = httpClient.execute(new HttpGet(url))
        val entity = httpResponse.getEntity
        var content = ""
    
        if (entity != null) {
            val inputStream = entity.getContent
            content = io.Source.fromInputStream(inputStream).getLines.mkString
            inputStream.close
        }
    
        httpClient.getConnectionManager.shutdown
        content
    }
    
    private def buildHttpClient(connection_timeout: Int, 
                                socket_timeout: Int): DefaultHttpClient = {
    
        val httpClient = new DefaultHttpClient
        val httpParams = httpClient.getParams
    
        HttpConnectionParams.setConnectionTimeout(httpParams, connection_timeout)
        HttpConnectionParams.setSoTimeout(httpParams, socket_timeout)
        httpClient.setParams(httpParams)
        httpClient
    }


    //val input  = readLine()
    //val result = getRestContent(input, 10000, 10000)


    // this is works despite all above not so
    val xml = XML.load("http://feeds.bbci.co.uk/news/world/us_and_canada/rss.xml")
}
