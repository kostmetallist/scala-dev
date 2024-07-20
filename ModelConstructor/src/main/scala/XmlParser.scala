package ru.ispras.dst

import  scala.xml._
import  scala.collection.mutable.ListBuffer


object XmlParser extends App {


    def fillEventsInfo(events_list: ListBuffer[MnpEvent]): Unit = {

        val xml  = XML.loadFile("data/mnp_export.xml")
        val rows = xml \\ "RESULTS" \\ "ROW"

        rows.foreach{node =>

                     var req_id       = (node.head \\ "COLUMN").filter(_ \@ "NAME" == "REQUEST_ID").text

                     val event_id     = (node.head \\ "COLUMN").filter(_ \@ "NAME" == "ID").text.toLong
                     val proc_inst_id = (node.head \\ "COLUMN").filter(_ \@ "NAME" == "PROCESSINSTANCEID").text
                     val np_id        = (node.head \\ "COLUMN").filter(_ \@ "NAME" == "NPID").text
                     val event_name   = (node.head \\ "COLUMN").filter(_ \@ "NAME" == "EVENT_NAME").text
                     val timestamp    = (node.head \\ "COLUMN").filter(_ \@ "NAME" == "TIMESTAMP").text
                     val sys_name     = (node.head \\ "COLUMN").filter(_ \@ "NAME" == "SYSTEM_NAME").text
                     val xml_msg      = (node.head \\ "COLUMN").filter(_ \@ "NAME" == "XML_MESSAGE").text

                     val index = req_id.indexOf('|')

                     if (index != -1) {

                        req_id = req_id.slice(0, index)
                     }

                     events_list += new MnpEvent(event_id, req_id, proc_inst_id, 
                                                 np_id, event_name, timestamp, 
                                                 sys_name, xml_msg)}
    }
}

case class MnpEvent(id: Long, requestId: String, processId: String, 
                    npId: String, eventName: String, timestamp: String, 
                    systemName: String, message: String)
